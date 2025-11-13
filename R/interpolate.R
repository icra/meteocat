#' Interpola dades de les estacions meteorològiques
#' @param stations_data Dades climàtiques de les estacions, obtingudes amb \link{download_var}
#' @param stations_features Covariables de les estacions, obtingudes amb \link{calculate_features}
#' @param grid_features Covariables de la malla de punts, obtingudes amb \link{calculate_features}
#' @param allow_negative Si és FALSE, els valors negatius es converteixen a zero.
#' @param smooth Si és TRUE, es fa un smooth dels resultats utilitzant una mitjana mòbil de 3x3
#' @param area_estudi Arxiu ràster amb l'àrea d'estudi. Només necessari si `smooth = TRUE`.
#' @param method Opció per utilitzar regressió linial o regressió lasso en la interpolació.
#' @returns Una tidytable amb els valors de la variable interpolada a cada punt de la malla
#' @export

interpolate <- function(
  stations_data,
  stations_features,
  grid_features,
  allow_negative = TRUE,
  smooth = TRUE,
  area_estudi,
  method = c("lm", "lasso")
) {
  stations <- stations_data |>
    tidytable::inner_join(stations_features) |>
    sf::st_as_sf()

  if ("ZE" %in% stations$id_station) {
    stations$id_station[stations$id_station == "ZE"] <- "Z8"
  }

  method <- rlang::arg_match(method)

  if (method == "lm") {
    interpolate_month <- interpolate_lm_month
  } else if (method == "lasso") {
    if (!(requireNamespace("glmnet", quietly = TRUE))) {
      rlang::abort("Cal instal·lar `glmnet` per utilitzar lasso")
    }
    interpolate_month <- interpolate_lasso_month
  } else {
    rlang::abort()
  }

  months <- c(paste0("m0", 1:9), paste0("m", 10:12))

  grid_temp <- tidytable::tidytable(id = 1:nrow(grid_features))

  for (month in months) {
    grid_temp <- grid_temp |>
      tidytable::mutate(
        !!rlang::sym(month) := interpolate_month(stations, grid_features, month)
      )
  }

  # Valors negatius no són possibles per ppt
  if (!allow_negative) {
    grid_temp <- grid_temp |>
      tidytable::mutate(tidytable::across(tidytable::starts_with("m"), \(x) {
        tidytable::if_else(x < 0, 0, x)
      }))
  }

  if (smooth) {
    grid_matrix <- grid_features |>
      sf::st_as_sf() |>
      sf::st_coordinates() |>
      as.matrix()
    grid_temp <- grid_temp |>
      tidytable::mutate(tidytable::across(tidytable::starts_with("m"), \(temp) {
        smooth_var(temp, grid_features, area_estudi, grid_matrix)
      })) |>
      tidytable::fill(tidytable::everything(), .direction = "downup")
  }

  grid_temp |> tidytable::select(!id)
}

interpolate_lm_month <- function(stations, grid_features, month) {
  formula <- paste0(month, " ~ ", "z + slope + aspect + dist_sea + cos_lat")

  # fit a linear model and extract the residuals
  mod <- lm(formula, data = stations)
  stations$glm_residuals <- mod$residuals

  # fit the variogram
  vario <- gstat::variogram(glm_residuals ~ 1, data = stations)
  vgm_vario <- gstat::vgm("Exp")
  fitted_vario <- gstat::fit.variogram(vario, vgm_vario)

  # interpolate using linear modal and kriging for residuals
  lm_pred <- predict(mod, newdata = grid_features)
  kr_pred <- gstat::krige(
    glm_residuals ~ 1,
    stations,
    newdata = grid_features,
    model = fitted_vario,
    debug.level = 0,
    beta = 0
  )
  (kr_pred$var1.pred + lm_pred) |> unname()
}

interpolate_lasso_month <- function(stations, grid_features, month) {
  x <- stations |>
    tidytable::select(z, slope, aspect, dist_sea, cos_lat) |>
    as.matrix()
  y <- stations[[month]]

  # mod <- lm(formula, data = tt$train)
  find_lambda <- glmnet::cv.glmnet(x, y, alpha = 1)
  best_lambda <- find_lambda$lambda.min

  mod <- glmnet::glmnet(x, y, aplha = 1, lambda = best_lambda)

  # extract residuals
  stations$glm_residuals <- y - predict(mod, newx = x)

  # fit the variogram
  vario <- gstat::variogram(glm_residuals ~ 1, data = stations)
  vgm_vario <- gstat::vgm("Exp")
  fitted_vario <- gstat::fit.variogram(vario, vgm_vario)

  # predict
  new_x <- grid_features |>
    tidytable::select(z, slope, aspect, dist_sea, cos_lat) |>
    as.matrix()

  glm_predict <- predict(mod, newx = new_x)
  kr_pred <- gstat::krige(
    glm_residuals ~ 1,
    stations,
    newdata = grid_features,
    model = fitted_vario,
    debug.level = 0,
    beta = 0
  )
  kr_pred$var1.pred + glm_predict
}

smooth_var <- function(temp, grid_features, template_r, grid_matrix) {
  grid_features$field <- temp
  grid_features <- grid_features['field']

  r <- grid_features |>
    stars::st_rasterize(stars::read_stars(template_r))

  terra::rast(r) |>
    terra::focal(w = 3, fun = "mean") |>
    terra::extract(grid_matrix, method = "simple") |>
    unlist() |>
    as.vector() |>
    unname()
}

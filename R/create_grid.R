#' Calcula les dades de la malla
#' @param grid_sf Objecte sf de punts, obtingut amb \link{create_grid_sf}` o data.frama amb coordenades com l'obtingut amb \link[meteocat]{create_grid_tidy}
#' @param mde Arxiu ràster que conté un model digital d'elevacions. Es pot obtenir amb \link[meteocat]{get_mde}.
#' @param slope Arxiu raster de pendents. Es pot obtenir amb \link{get_terrain}.
#' @param aspect Arxiu raster d'orientacions. Es pot obtenir amb \link{get_terrain}.
#' @param aspect Arxiu raster d'orientacions. Es pot obtenir amb \link{get_terrain}.
#' @param coastline Objecte sf que representa la línia de costa.
#' @returns Una tidytable amb les mateixes files que `grid_sf` i amb columnes corresponents a les dades utilitzades per \link{interpolate}
#' @export

calculate_features <- function(grid_sf, mde, slope, aspect, coastline) {
  if ("id_station" %in% colnames(grid_sf)) {
    grid_sf <- grid_sf |>
      tidytable::distinct(id_station, .keep_all = TRUE)
  }

  if (inherits(grid_sf, "sf")) {
    grid_sf_xy <- grid_sf |> sf::st_transform(4326) |> sf::st_coordinates()
  } else {
    cols <- c("x", "X", "y", "Y")
    if (sum(cols %in% grid_sf) < 2) {
      rlang::abort(
        "grid_sf ha de ser un objecte sf o tenir coordenades com a x i y"
      )
    }
    grid_sf_xy <- grid_sf |>
      tidytable::select(any_of(c("x", "X", "y", "Y"))) |>
      as.matrix()
    grid_sf <- sf::st_as_sf(
      grid_sf,
      coords = c("X", "Y"),
      crs = terra::crs(terra::rast(mde))
    )
  }
  # TODO: Convertir a 4326 per calcular cos_lat
  grid_sf$z <- terra::extract(terra::rast(mde), grid_sf_xy)[, 1]
  grid_sf$slope <- terra::extract(terra::rast(slope), grid_sf_xy)[, 1]
  grid_sf$aspect <- terra::extract(terra::rast(aspect), grid_sf_xy)[, 1]
  grid_sf$dist_sea <- distance_to_sea(grid_sf, coastline)
  grid_sf$cos_lat <- cos(grid_sf_xy[, 2] * pi / 180)

  grid_sf |>
    tidytable::select(tidytable::any_of(c(
      "id_station",
      "z",
      "slope",
      "aspect",
      "dist_sea",
      "cos_lat",
      "geometry"
    )))
}

#' Crea una malla de punts per la interpolació
#' @param area_estudi Arxiu raster amb l'àrea d'estudi
#' @returns Un objecte sf amb un punt per cada píxel del raster `area_estudi`
#' @export

create_grid_sf <- function(area_estudi) {
  res <- stars::read_stars(area_estudi) |>
    sf::st_as_sf(as_points = TRUE, merge = FALSE)
  res["geometry"]
}

#' Crea una tidytable de coordenades a partir d'una malla
#' @param grid_sf Objecte sf de punts, obtingut amb \link[meteocat]{create_grid_sf}`
#' @returns Una tidytable amb coordenades i un identificador únic.
#' @export

create_grid_tidy <- function(grid_sf) {
  grid_sf |>
    sf::st_coordinates() |>
    tidytable::as_tidytable() |>
    tidytable::mutate(id = tidytable::row_number(), .before = 1)
}

#' Calcula la distància al mar de cada punt de la malla
#' @param grid_sf Objecte sf de punts, obtingut amb \link[meteocat]{create_grid_sf}`
#' @param coastline Objecte sf representant la línia de costa
#' @returns Un vector numèric amb les distàncies
#' @export

distance_to_sea <- function(grid_sf, coastline) {
  coastline <- coastline |>
    sf::st_coordinates() |>
    as.data.frame() |>
    sf::st_as_sf(coords = c("X", "Y"), crs = sf::st_crs(coastline))

  nearest_sea <- sf::st_nearest_feature(grid_sf, coastline)

  as.numeric(sf::st_distance(
    grid_sf,
    coastline[nearest_sea, ],
    by_element = TRUE
  ))
}

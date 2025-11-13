#' Descarrega una variable per un any concret
#' @param code_variable Codi de la variable obtingut com \link{download_code_variables}.
#' @param year Any que es vol descarregar
#' @param api_key La clau API del meteocat. Si es guarda a .Renviron com a 'meteocat_key' s'estira automàticament amb `get_api_key()`
#' @returns Una tidytable amb una columna per mes i una fila per estació.
#' @export
#' @details Funció dissenyada per ús intern a la funció `donwload_var()`.

download_var_year <- function(code_variable, year, api_key = get_api_key()) {
  url_base <- "https://api.meteo.cat/xema/v1/variables/estadistics/mensuals/"

  req <- httr2::request(url_base) |>
    httr2::req_url_path_append(code_variable) |>
    httr2::req_headers("x-api-key" = api_key) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_url_query(any = year) |>
    httr2::req_perform()

  if (length(httr2::resp_body_json(req)) == 0) {
    rlang::warn(paste(
      "No hi ha dades per l'any",
      year,
      "per la variable",
      code_variable
    ))
    return(NULL)
  }
  df <- suppressMessages(
    httr2::resp_body_json(req) |>
      tidytable::map_dfr(\(x) {
        df <- tidytable::tidytable(id_station = x$codiEstacio)
        .valors <- tidytable::map_dfc(x$valors, \(y) {
          m <- paste0("m", stringr::str_sub(y$data, 6, 7))

          tidytable::tidytable(
            !!m := y$valor
          )
        })
        tidytable::bind_cols(df, .valors)
      }) |>
      tidytable::select(
        id_station,
        tidytable::any_of(c(
          "m01",
          "m02",
          "m03",
          "m04",
          "m05",
          "m06",
          "m07",
          "m08",
          "m09",
          "m10",
          "m11",
          "m12"
        ))
      )
  )
  df |> tidytable::select(order(colnames(df)))
}

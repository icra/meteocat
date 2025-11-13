#' Descarrega els codis de les variables
#' @param api_key La clau API del meteocat. Si es guarda a .Renviron com a 'meteocat_key' s'estira automàticament amb `get_api_key()`
#' @returns Una tidytable amb codi, nom, unitat, acrònim, tipus i decimals.
#' @export

download_code_variables <- function(api_key = get_api_key()) {
  req <- httr2::request(
    "https://api.meteo.cat/xema/v1/variables/estadistics/mensuals/metadades"
  ) |>
    httr2::req_headers("x-api-key" = api_key) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_perform()

  json <- req |> httr2::resp_body_json()

  tidytable::map_dfr(json, \(x) {
    tidytable::tidytable(
      codi = x$codi,
      nom = x$nom,
      unitat = x$unitat,
      acronim = x$acronim,
      tipus = x$tipus,
      decimals = x$decimals
    )
  })
}

#' Descarrega dades de les estacions de la XEMA
#' @param api_key La clau API del meteocat. Si es guarda a .Renviron com a 'meteocat_key' s'estira automàticament amb `get_api_key()`
#' @returns Un objecte sf amb les estacions meteorològiques de la XEMA
#' @export

download_stations <- function(api_key = get_api_key()) {
  req <- httr2::request("https://api.meteo.cat/xema/v1/estacions/metadades") |>
    httr2::req_headers("x-api-key" = api_key) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_perform()

  json <- req |> httr2::resp_body_json()

  tidytable::map_dfr(json, \(x) {
    tidytable::tidytable(
      id_station = x$codi,
      nom = x$nom,
      tipus = x$tipus,
      lat = x$coordenades$latitud,
      lon = x$coordenades$longitud,
      emplacament = x$emplacament,
      altitud = x$altitud,
      codi_municipi = x$municipi$codi,
      municipi = x$municipi$nom,
      xarxa = x$nom,
      estats = x$estats
    )
  }) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(25831)
}

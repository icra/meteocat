#' Descarrega la mitjana d'una sèrie multianual
#' @param variable Acrònim de la variable obtinguda de `download_code_variables()`
#' @param years Un vector amb tots els anys que volem incloure a la sèrie temporal
#' @param variables La taula de variables que s'obté amb `download_code_variables()`
#' @param api_key La clau API del meteocat. Si es guarda a .Renviron com a 'meteocat_key' s'estira automàticament amb `get_api_key()`
#' @returns Una tidytable amb una columna per mes i una fila per estació.
#' @export
#' @details La funció retorna el valor mitjà dels anys inclosos a la sèrie pels quals hi ha dades. Si una estació no té dades per cap dels anys, s'elimina de la taula.

download_var <- function(
  variable,
  years,
  variables = download_code_variables(),
  api_key = get_api_key()
) {
  code_variable <- variables[variables$acronim == variable, ] |>
    tidytable::pull(codi)

  if (length(code_variable) == 0) {
    rlang::abort(
      "La variable no es correspon amb cap acrònim del meteocat"
    )
  }

  tidytable::map_dfr(years, \(year) {
    download_var_year(code_variable, year)
  }) |>
    tidytable::summarize(
      tidytable::across(tidytable::where(is.numeric), \(x) {
        mean(x, na.rm = TRUE)
      }),
      .by = id_station
    ) |>
    tidytable::drop_na(tidytable::everything())
}

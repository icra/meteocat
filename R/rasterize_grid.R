#' Converteix la malla de punts a raster
#' @param punts_sf Objecte \code{sf} que conté la malla de punts, si \code{df = NULL}, `punts_sf` ha de contenir `field`.
#' @param field Caràcter amb el nom del camp a rasteritzar de `punts_sf` o de `df`.
#' @param template Arxiu ràster per utilitzar com a plantilla.
#' @param df `data.frame` que conté `field` si aquest no es troba a `punts_sf`.
#' @return Objecte `stars`.
#' @export

rasterize_grid <- function(punts_sf, field, template, df = NULL) {
  r <- stars::read_stars(template)

  if (!is.null(df) && !inherits(df, "data.frame")) {
    df <- data.frame(df)
    colnames(df) <- field
  }

  if (!is.null(df)) {
    if (nrow(df) != nrow(punts_sf)) {
      rlang::abort("punts_sf i df no tenen la mateixa llargada")
    }
  }
  punts_sf[[field]] <- df[[field]]

  stars::st_rasterize(punts_sf[field], r)
}

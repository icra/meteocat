#' Obté un model digital d'elevacions per l'àrea d'estudi
#' @param area_estudi Arxiu raster amb l'àrea d'estudi
#' @param path Destí on es vol guardar el raster
#' @param zoom La resolució del raster. `?elevatr::get_elev_raster` per més info.
#' @param ... altres arguments per `elevatr::get_elev_raster`.
#' @export
#' @returns El destí de l'arxiu de forma invisible
get_mde <- function(area_estudi, path, zoom = 12, ...) {
  if (!(requireNamespace("elevatr", quietly = TRUE))) {
    rlang::abort("Cal instal·lar `elevatr`")
  }
  as(
    elevatr::get_elev_raster(terra::rast(area_estudi), zoom, ...),
    "SpatRaster"
  ) |>
    terra::writeRaster(path, overwrite = TRUE)

  invisible(path)
}

#' Obté variables derivades del model digital d'elevacions
#' @param mde Arxiu ràster amb un model digital d'elevacions
#' @param var Variable a calcular. Una o més d'aquestes opcions: slope, aspect, TPI, TRI, TRIriley, TRIrmsd, roughness, flowdir. Consulta \link[terra]{terrain} per més info.
#' @param path Destí on es vol guardar el ràster resultant.
#' @export
#' @returns El destí de l'arxiu de forma invisible.

get_terrain <- function(
  mde,
  var = c(
    'slope',
    'aspect',
    'TPI',
    'TRI',
    'TRIriley',
    'TRIrmsd',
    'roughness',
    'flowdir'
  ),
  path
) {
  var <- rlang::arg_match(var)
  terra::terrain(terra::rast(mde), v = var, unit = "degrees") |>
    terra::writeRaster(path, overwrite = TRUE)

  invisible(path)
}

#' Plot a single spectrum
#'
#' @param x `Spectra(1)`.
#' @param type `character(1)`, type of plot ("l" for profile/"h" for centroided)
#' @param ... passed to `plot.default`.
#'
#' length(SpectraObject) == 1
#' type = "l" for profile (default), type = "h" for centroided
#'
#' @noRd
#' @example
#' spd <- DataFrame(msLevel = c(1L, 2L), rtime = c(1.1, 1.2))
#' spd$mz <- list(c(100, 103.2, 104.3, 106.5), c(45.6, 120.4, 190.2))
#' spd$intensity <- list(c(200, 400, 34.2, 17), c(12.3, 15.2, 6.8))
#'
#' s <- Spectra(spd)
.plotSingleSpectrum <- function(x,
    type = if (isTRUE(isCentroided(x[1L]))) "h" else "l", ...) {
    v <- as.list(x[1L])[[1L]]
    plot(x = v[, "mz"], y = v[, "intensity"], type = type)
}

# length(SpectraObject) > 1
# stacked above each other, shared x-axis
.plotStackedSpectra <- function(x, ...) {
}

# length(SpectraObject) > 1
# plot each spectra beside messey above each other, shared x-axis
.plotCoPlotSpectra <- function(x, ...) {
}

# SpectraObject(1), SpectraObject(1)
# comparison plot (the second one up-side down with a shared x-axis)
.plotMirrorSpectra <- function(x, y, ...) {
}

# length(SpectraObject) > 1
# rt: x-axis
# mz: y-axis
# intensity: color coded
.plotLcMsMap <- function(x, ...) {
}

# length(SpectraObject) == 1
# type = "l" for profile (default), type = "h" for centroided
.plotSingleSpectrum <- function(x, ...) {
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

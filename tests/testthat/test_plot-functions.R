#' @examples
#' # each base::graphics plot function must be wrapped by an anonymous function
#' # that could be called by `vdiffr::expect_doppelganger()`
#' # use `manage_cases()` to add new/verify changed plots
#' vdiffr::manage_cases(filter = "plot")

spd <- DataFrame(msLevel = c(1L, 2L), rtime = c(1.1, 1.2))
spd$mz <- list(c(100, 103.2, 104.3, 106.5), c(45.6, 120.4, 190.2))
spd$intensity <- list(c(200, 400, 34.2, 17), c(12.3, 15.2, 6.8))

s <- Spectra(spd)

test_that(".plotSingleSpectrum", {
    vdiffr::expect_doppelganger(
        "plotSingleSpectrum", function().plotSingleSpectrum(s)
    )
})

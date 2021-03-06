test_that("ProcessingStep constructor", {
    ps <- new("ProcessingStep", FUN = "mean", ARGS = list(na.rm = TRUE))
    expect_match(capture.output(show(ps))[1],
                 "Object of class \"ProcessingStep\"")
    expect_error(new("ProcessingStep", FUN = "aaaa"))
    ps <- ProcessingStep(mean, list(na.rm = TRUE))
    expect_true(validObject(ps))
})

test_that("ProcessingStep executeProcessingStep", {
    ps <- ProcessingStep("sum", list(c(1, 4, 5)))
    expect_identical(executeProcessingStep(ps), 10)

    ps <- ProcessingStep("sum", list(c(1, 4, 5, NA)))
    ## Pass optional arguments to ...
    expect_identical(executeProcessingStep(ps, na.rm = TRUE), 10)
})

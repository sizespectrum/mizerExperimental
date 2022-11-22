# set up according to 
# https://rstudio.github.io/shinytest2/articles/use-package.html#applications-in-inst
# at the bottom of that section.
library(shinytest2)

test_that("tuneParams gadget works", {
    # Don't run these tests on the CRAN build servers
    skip_on_cran()
    
    appdir <- "apps/tuneParams/"
    test_app(appdir)
})

# To create new tests run
# shinytest2::record_test("tests/testthat/apps/tuneParams/", 
#                          seed = 1, allow_no_input_binding = TRUE)
# The `allow_no_input_binding` is needed because of plotly.

# To review the snapshots use
# testthat::snapshot_review(path = "tests/testthat/apps/tuneParams/tests/testthat/")

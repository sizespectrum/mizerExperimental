library(shinytest2)

test_that("{shinytest2} recording: tuneParams", {
  app <- AppDriver$new(variant = platform_variant(), name = "tuneParams", height = 919, 
      width = 978)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(scale_bkgrd_by = 0.5)
  app$click("sp_steady")
  app$expect_values()
})

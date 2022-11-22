library(shinytest2)


test_that("{shinytest2} recording: tuneParams", {
  app <- AppDriver$new(name = "tuneParams", height = 919, width = 978, seed = 1)
  app$set_inputs(`plotly_afterplot-A` = "\"plotSpectra\"", allow_no_input_binding_ = TRUE, 
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":692,\"height\":400}", allow_no_input_binding_ = TRUE, 
      priority_ = "event")
  app$expect_values()
})


test_that("{shinytest2} recording: second", {
  app <- AppDriver$new(name = "second", seed = 1, height = 919, width = 978)
  app$set_inputs(`plotly_afterplot-A` = "\"plotSpectra\"", allow_no_input_binding_ = TRUE, 
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":692,\"height\":400}", allow_no_input_binding_ = TRUE, 
      priority_ = "event")
  app$set_inputs(scale_bkgrd_by = 0.5)
  app$click("sp_steady")
  app$click("undo_all")
  app$click("sp_steady")
  app$expect_values()
})

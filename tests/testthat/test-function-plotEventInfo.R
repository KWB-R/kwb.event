#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("plotEventInfo() works", {

  expect_error(
    kwb.event:::plotEventInfo()
    # Argument "eventInfo" fehlt (ohne Standardwert)
  )

})


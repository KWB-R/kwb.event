#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("plotMergedEventInfoForValidation() works", {

  expect_error(
    kwb.event:::plotMergedEventInfoForValidation()
    # Argument "mergedEvents" fehlt (ohne Standardwert)
  )

})


#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".setTimeUnitToSeconds() works", {

  expect_error(
    kwb.event:::.setTimeUnitToSeconds()
    # Argument "events" fehlt (ohne Standardwert)
  )

})


#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".hasTimeColumns() works", {

  expect_error(
    kwb.event:::.hasTimeColumns()
    # Argument "events" fehlt (ohne Standardwert)
  )

})


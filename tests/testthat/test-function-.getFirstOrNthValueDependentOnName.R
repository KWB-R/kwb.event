#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".getFirstOrNthValueDependentOnName() works", {

  expect_error(
    kwb.event:::.getFirstOrNthValueDependentOnName()
    # Argument "myname" fehlt (ohne Standardwert)
  )

})


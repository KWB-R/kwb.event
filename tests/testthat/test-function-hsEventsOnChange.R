#library(testthat)

test_that("hsEventsOnChange() works", {
  expect_warning(kwb.event:::hsEventsOnChange(1))
})

test_that("eventsOnChange() works", {
  
  f <- kwb.event:::eventsOnChange
  
  expect_identical(f(1:3), data.frame(
    iBeg = 1:3,
    iEnd = 1:3
  ))
  
  expect_identical(f(1:3, include.value = TRUE), data.frame(
    iBeg = 1:3,
    iEnd = 1:3,
    value = 1:3
  ))

  expect_identical(f(1:3, numberOnly = TRUE), 3L)
})

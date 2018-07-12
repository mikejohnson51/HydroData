context("utility functions")

test_that("cida server throws correct errors", {
  expect_error(query_cida(AOI::getAOI(clip= list("UCSB", 1, 1)), type = "test") , "Type not found.", fixed = TRUE)
})

test_that("make sure data exists", {
  test1 = is.null(HydroData::usgsStations)
  test2 = is.null(HydroData::snotel)
  test3 = is.null(HydroData::ap)

  vec = c(test1, test2, test3)

  print(any(!isTRUE(vec)))
  check = any(!isTRUE(vec))
  expect_true(check)
})

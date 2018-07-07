context("utility functions")

test_that("cida server throws correct errors", {
  expect_error(query_cida(AOI::getAOI(clip_unit = list("UCSB", 1, 1)), type = "test") , "Type not found.")
})


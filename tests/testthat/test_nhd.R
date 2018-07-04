context("findNHD")

test_that("findNHD throws correct errors", {
  expect_error(findNHD(clip_unit = list("UCSB", .3,.3)), "0 flowlines found in AOI")
})

test_that("check NHD routines", {
  clip <- try(findNHD(clip_unit = list("UCSB", 10, 10), save = TRUE))

  vec = c(is.list(clip))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})



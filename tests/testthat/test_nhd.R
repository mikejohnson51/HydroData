context("findNHD")

test_that("findNHD throws correct errors", {
  expect_error(findNHD(clip_unit = list("UCSB", .3,.3)), "0 NHD found")
})

test_that("check NHD routines", {
  clip  <- findNHD(clip_unit = list("Denver", 10, 10), save = TRUE)
  clip1 <- findNHD(clip_unit = list("Denver", 10, 10), save = FALSE)

  vec = c(is.list(clip), is.list(clip1))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})

test_that("check closest COMID routines", {
  clip  <- try(findNearestCOMID(location = c(37, -113), n = 5))

  vec = c(is.list(clip))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})



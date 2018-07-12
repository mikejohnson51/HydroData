context("findNHD")

test_that("findNHD throws correct errors", {
  expect_error(findNHD(clip = list("UCSB", .3,.3)), "O features found in this AOI.")
})

test_that("check NHD routines", {
  clip  <- findNHD(clip = list("Denver", 10, 10), save = TRUE)
  clip1 <- findNHD(clip = list("Denver", 10, 10), save = FALSE, ids = T)

  vec = c(is.list(clip), is.list(clip1))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})

test_that("check closest COMID routines", {
  clip  <- try(findNearestCOMID(location = c(37, -113), n = 5))
  clip1 <- try(findNearestCOMID("UCSB", n = 10))

  vec = c(is.list(clip), is.list(clip1))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})



context("findUSGS")

test_that("USGS throws correct errors", {
  expect_error(findGHCN(clip = list("UCSB", .3,.3)), "0 stations found in AOI")
  expect_error(findNearestGHCN(location = c(37, -113), n = 5, PARAM = "PPT"), "PPT is not a valid GHCN parameter")

})

test_that("check USGS routines", {
  ids <- try(findUSGS(clip = list("UCSB", 40, 40), ids = TRUE))
  save <- try(findUSGS(state = 'TX', county = 'Harris', boundary = FALSE, save = TRUE))
  bounds <- try(findUSGS(clip = list("UCSB", 40, 40), boundary  = TRUE))


  vec = c( is.list(ids), is.list(save), is.list(bounds))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})


# test_that("check closest USGS routines", {
#   clip  <- try(findNearestGHCN(location = c(37, -113), n = 5, PARAM = "PRCP"))
#   clip1 <- try(findNearestGHCN("UCSB", n = 10))
#
#   vec = c(is.list(clip), is.list(clip1))
#   print(!inherits(vec,"try-error"))
#   check = !inherits(vec,"try-error")
#   expect_true(check)
# })


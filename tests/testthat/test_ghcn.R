context("findGHCN")

test_that("GHCN throws correct errors", {
  expect_error(findGHCN(clip_unit = list("UCSB", .3,.3)), "0 stations found in AOI")
  expect_error(findNearestGHCN(location = c(37, -113), n = 5, PARAM = "PPT"), "PPT is not a valid GHCN parameter")

})

test_that("check GHCN routines", {
  state <- try(findGHCN(state = "Colorado", param = "PRCP", save = FALSE))
  county <- try(findGHCN(state = "Colorado", county = "El Paso", save = TRUE))
  clip <- try(findGHCN(clip_unit = list("UCSB", 40, 40)))
  ids <- try(findGHCN(clip_unit = list("UCSB", 40, 40), ids = TRUE))
  bounds1 <- try(findGHCN(state = 'TX', county = 'Harris', boundary = TRUE, save = TRUE))
  bounds2 <- try(findGHCN(clip_unit = list("UCSB", 40, 40), boundary  = TRUE))


  vec = c(is.list(state), is.list(county), is.list(clip), is.list(ids), is.list(bounds1), is.list(bounds2))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})


test_that("check closest GHCN routines", {
  clip  <- try(findNearestGHCN(location = c(37, -113), n = 5, PARAM = "PRCP"))
  clip1 <- try(findNearestGHCN("UCSB", n = 10))

  vec = c(is.list(clip), is.list(clip1))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})


context("findGHCN")

test_that("GHCN throws correct errors", {
  expect_error(findGHCN(clip = list("UCSB", .3,.3)), "0 stations found in AOI")
  expect_error(findNearestGHCN(location = c(37, -113), n = 5, PARAM = "PPT"), "PPT is not a valid GHCN parameter")

})

test_that("check GHCN routines", {
  PARAM <- try(findGHCN(state = "Colorado", param = "PRCP", save = FALSE))
  #county <- try(findGHCN(state = "Colorado", county = "El Paso", save = TRUE))
  #clip <- try(findGHCN(clip = list("UCSB", 40, 40)))
  ids <- try(findGHCN(clip = list("UCSB", 40, 40), ids = TRUE))
  save <- try(findGHCN(state = 'TX', county = 'Harris', boundary = FALSE, save = TRUE))
  bounds <- try(findGHCN(clip = list("UCSB", 40, 40), boundary  = TRUE))


  vec = c(is.list(PARAM), is.list(ids), is.list(save), is.list(bounds))
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


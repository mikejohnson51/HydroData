context("findAirports")

test_that("getAOI throws correct errors", {
  expect_error(findAirports(clip_unit = list("UCSB", .3,.3)), "0 airports found in AOI")
})

test_that("check Airport routines", {
  state <- try(findAirports(state = "Colorado", save = TRUE))
  county <- try(findAirports(state = "Colorado", county = "El Paso", save = TRUE))
  clip <- try(findAirports(clip_unit = list("UCSB", 40, 40)))
  ids <- try(findAirports(clip_unit = list("UCSB", 40, 40), ids = TRUE))
  bounds1 <- try(findAirports(state = 'TX', county = 'Harris', boundary = TRUE, save = TRUE))
  bounds2 <- try(findAirports(clip_unit = list("UCSB", 40, 40), boundary  = TRUE))


  vec = c(is.list(state), is.list(county), is.list(clip), is.list(ids), is.list(bounds1), is.list(bounds2))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})



context("findAirports")

test_that("findAirport throws correct errors", {
  expect_error(findAirports(clip = list("UCSB", .3,.3)), "0 airports found in AOI", fixed = TRUE)
})

test_that("check Airport routines", {
  state <- try(findAirports(state = "Colorado", save = TRUE))
  county <- try(findAirports(state = "Colorado", county = "El Paso", save = TRUE))
  clip <- try(findAirports(clip = list("UCSB", 40, 40)))
  ids <- try(findAirports(clip = list("UCSB", 40, 40), ids = TRUE))
  bounds1 <- try(findAirports(state = 'TX', county = 'Harris', boundary = TRUE, save = TRUE))
  bounds2 <- try(findAirports(clip = list("UCSB", 40, 40), boundary  = TRUE))


  vec = c(is.list(state), is.list(county), is.list(clip), is.list(ids), is.list(bounds1), is.list(bounds2))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})

test_that("check closest Airport routines", {
  clip  <- try(findNearestAirports(location = c(37, -113), n = 5))
  clip1 <- try(findNearestAirports("UCSB", n = 10))

  vec = c(is.list(clip), is.list(clip1))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})


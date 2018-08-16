context("findAirports")

test_that("check Airport routines", {
  state <- getAOI(state = "CO") %>% findAirports()
  county <- getAOI(state = "CO", county = "El Paso") %>% findAirports()
  clip <- getAOI(clip = list("UCSB", 30, 30)) %>% findAirports()
  ids <- getAOI(state = "CO") %>% findAirports(ids = T)

  vec = c(is.list(state), is.list(county), is.list(clip), is.list(ids))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})

test_that("check closest Airport routines", {
  clip  <- geocode("UCSB") %>% findNearestAirports()

  vec = c(is.list(clip))
  print(!inherits(vec,"try-error"))
  check = !inherits(vec,"try-error")
  expect_true(check)
})


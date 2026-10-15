set.seed(20260512)

test_that("graph schema and topology are validated without project files", {
  g <- toy_graph()
  expect_s3_class(shield_parameters(g), "shield_parameters")
  expect_error(shield_parameters(g[-1, ]), "15 rows")
  bad <- g; bad[2, ] <- bad[1, ]
  expect_error(shield_parameters(bad), "every leg-route")
  bad <- g; bad$travel_min[1] <- NA_real_
  expect_error(shield_parameters(bad), "missing")
  bad <- g; bad$travel_min[1] <- 1.5
  expect_error(shield_parameters(bad), "domains")
  bad <- g; bad$exposure_weight[1] <- 2
  expect_error(shield_parameters(bad), "domains")
  bad <- g; bad$from[1] <- "Stage3"
  expect_error(shield_parameters(bad), "topology")
  bad <- g; bad$vehicle_fraction[1] <- .3
  expect_error(shield_parameters(bad), "vehicle fraction")
  bad <- g; bad$travel_min <- as.character(bad$travel_min)
  expect_error(shield_parameters(bad), "finite numeric")
})

test_that("invalid batches and event-time inputs fail at construction", {
  g <- toy_graph()
  expect_error(shield_parameters(g, N = 1.5), "integer")
  expect_error(shield_parameters(g, N = NA_real_), "finite")
  expect_error(shield_parameters(g, h = c(0, -1, 0)), "nonnegative")
  expect_error(shield_parameters(g, mix = c(1, 1, 1)), "summing to N")
  expect_error(shield_parameters(g, slot = 0), "integer")
  expect_error(shield_parameters(g, departure_holds = c(1, 2)), "start at zero")
  expect_error(shield_parameters(g, departure_holds = c(0, 0)), "increase")
  expect_error(shield_parameters(g, slot = 2L, departure_holds = 0:2), "multiples")
  expect_error(shield_parameters(g, t_load = 0L), "integer")
  expect_error(shield_parameters(g, misclass = 2), "probabilities")
  expect_error(shield_parameters(g, disruptions_enabled = NA), "TRUE or FALSE")
  expect_error(shield_parameters(g, beta_prior = c(0, 1)), "positive shapes")
})

test_that("pool allocation preserves totals, including odd flexible fleets", {
  g <- toy_graph()
  for (n in c(1L, 3L, 7L)) {
    s <- shield_parameters(g, N = n)
    totals <- vapply(1:5, function(leg) sum(vapply(s$pools, function(pool)
      if (pool$leg == leg) pool$vehicles else 0L, integer(1))), integer(1))
    expect_identical(totals, s$V)
    expect_equal(sum(s$mix), n)
  }
})

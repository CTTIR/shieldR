set.seed(20260512)

toy_graph <- function() {
  example_scenario()$graph[c("leg", "from", "to", "route", "travel_min",
                            "exposure_weight", "vehicle_fraction")]
}

stochastic_scenario <- function(N = 6L, ...) {
  graph <- toy_graph()
  graph$travel_min <- rep(c(1, 2, 3), 5)
  graph$exposure_weight <- rep(c(0.3, 0.2, 0.1), 5)
  shield_parameters(graph, N = N, horizon = 40L, h = c(.03, .02, .01),
    esc32 = .01, esc21 = .01, misclass = .2, disruption_risk = c(low = .1, high = .2),
    q = .4, lambda = 2, beta = 3, atk_window = 3L,
    disruptions_enabled = TRUE, ...)
}

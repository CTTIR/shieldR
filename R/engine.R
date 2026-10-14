# Five-stage network simulation model.

init_rng_full <- function() {
  if (RNGkind()[1L] != "L'Ecuyer-CMRG")
    stop("simulate_once_full requires the RNG kind set by run_scenario_full")
  base <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  policy <- parallel::nextRNGStream(base)
  disruption <- parallel::nextRNGStream(policy)
  e <- new.env(parent = emptyenv())
  e$item <- base
  e$policy <- policy
  e$disruption <- disruption
  e
}

draw_rng_full <- function(rng, stream, n) {
  old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  assign(".Random.seed", rng[[stream]], envir = .GlobalEnv)
  on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  value <- stats::runif(n)
  rng[[stream]] <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  value
}

simulate_once_full <- function(policy, disruption_model, params) {
  if (!is.null(params$item_model)) return(simulate_once_item_v1(policy, disruption_model, params))
  state <- init_items_full(params)
  rng <- init_rng_full()
  theta <- init_theta_full(params)
  base_disruption_risk <- unname(params$disruption_risk[params$disruption_risk_level])
  n_leg <- params$n_leg
  pools <- params$pools
  n_pool <- length(pools)

  route_history <- replicate(n_leg,
    data.frame(time = integer(0), route = integer(0)), simplify = FALSE)
  vehicle_free <- lapply(pools, function(x) rep(0, x$vehicles))
  departure_time <- lapply(pools, function(x) rep(NA_real_, x$vehicles))
  assignment_time <- lapply(pools, function(x) rep(NA_real_, x$vehicles))
  departure_item <- lapply(pools, function(x) rep(NA_integer_, x$vehicles))
  departure_route <- lapply(pools, function(x) rep(NA_integer_, x$vehicles))
  departure_dest <- lapply(pools, function(x) rep(NA_integer_, x$vehicles))
  departure_hold <- lapply(pools, function(x) rep(NA_integer_, x$vehicles))

  n_disruption <- 0L
  n_disruption_destructive <- 0L
  n_dispatch <- integer(n_leg)
  route_count <- matrix(0L, n_leg, params$R)
  destination_count <- integer(length(NODE))
  busy_minutes <- numeric(n_leg)
  holds <- integer(0)

  for (time in 0:params$horizon) {
    ## 1a. Record physical stage arrival before unloading. This is the
    ## endpoint boundary; item hazard stops at physical Stage-2 arrival.
    physical <- which(state$status == 1L & state$viable &
                      !is.na(state$dest) & !is.na(state$t_arrival) &
                      state$t_arrival <= time)
    if (length(physical)) {
      dest <- state$dest[physical]
      at_stage1 <- dest %in% c(NODE[["Stage1_a"]], NODE[["Stage1_b"]])
      at_stage2 <- dest == NODE[["Stage2"]]
      at_stage3 <- dest == NODE[["Stage3"]]
      if (any(at_stage1)) state$t_stage1[physical[at_stage1]] <- time
      if (any(at_stage2)) state$t_stage2[physical[at_stage2]] <- time
      if (any(at_stage3)) state$t_stage3[physical[at_stage3]] <- time
      state$t_arrival[physical] <- NA_real_
    }

    ## 1b. A casualty becomes available at a node after unloading.
    arrived <- which(state$status == 1L & state$viable &
                     !is.na(state$dest) & !is.na(state$t_eta) &
                     state$t_eta <= time)
    if (length(arrived)) {
      dest <- state$dest[arrived]
      state$node[arrived] <- dest
      state$status[arrived] <- ifelse(dest == params$terminal_node, 2L, 0L)
      state$dest[arrived] <- NA_integer_
      state$t_eta[arrived] <- NA_real_
    }

    if (time == params$horizon) break

    ## 2. Cancel reservations whose item failed while loading/holding.
    ## The vehicle was occupied until cancellation, but no traversal, disruption
    ## observation or realised-route count is created by a cancelled trip.
    for (pool_i in seq_len(n_pool)) {
      reserved <- which(!is.na(departure_time[[pool_i]]))
      cancelled <- reserved[!state$viable[departure_item[[pool_i]][reserved]]]
      if (length(cancelled)) {
        leg <- pools[[pool_i]]$leg
        busy_minutes[leg] <- busy_minutes[leg] + sum(
          pmax(0, time - assignment_time[[pool_i]][cancelled]))
        vehicle_free[[pool_i]][cancelled] <- time
        departure_time[[pool_i]][cancelled] <- NA_real_
        assignment_time[[pool_i]][cancelled] <- NA_real_
        departure_item[[pool_i]][cancelled] <- NA_integer_
        departure_route[[pool_i]][cancelled] <- NA_integer_
        departure_dest[[pool_i]][cancelled] <- NA_integer_
        departure_hold[[pool_i]][cancelled] <- NA_integer_
      }
    }

    ## Once every casualty is failed or has physically reached Stage 3, all
    ## endpoints are fixed. Vehicle cycles were booked at departure, so the
    ## remaining empty return travel needs no minute-by-minute processing.
    if (all(!state$viable | !is.na(state$t_stage3))) break

    ## Resolve live departures. Allocations use a start-of-minute history
    ## snapshot so simultaneous departures cannot react to one another.
    history_snapshot <- route_history
    departed_this_minute <- replicate(n_leg, integer(0), simplify = FALSE)
    for (pool_i in seq_len(n_pool)) {
      pool <- pools[[pool_i]]
      leg <- pool$leg
      going <- which(!is.na(departure_time[[pool_i]]) &
                     departure_time[[pool_i]] <= time)
      for (vehicle in going) {
        item <- departure_item[[pool_i]][vehicle]
        route <- departure_route[[pool_i]][vehicle]
        dest <- departure_dest[[pool_i]][vehicle]

        if (params$disruptions_enabled) {
          allocation <- disruption_model_response_full(history_snapshot[[leg]], params,
                                               disruption_model)
          disruption_u <- draw_rng_full(rng, "disruption", 2L)
          disrupted <- disruption_u[1L] <
            disruption_prob_full(leg, route, allocation, base_disruption_risk, params)
        } else {
          disruption_u <- draw_rng_full(rng, "disruption", 2L)
          disrupted <- FALSE
        }
        theta <- update_theta_full(theta, leg, route, disrupted)
        departed_this_minute[[leg]] <- c(departed_this_minute[[leg]], route)

        disruption_delay <- 0L
        if (disrupted) {
          n_disruption <- n_disruption + 1L
          if (state$viable[item]) {
            if (disruption_u[2L] < params$q) {
              state$viable[item] <- FALSE
              state$status[item] <- 3L
              n_disruption_destructive <- n_disruption_destructive + 1L
            } else {
              disruption_delay <- params$att_delay
            }
          } else {
            disruption_delay <- params$att_delay
          }
        }

        travel <- params$leg_time[leg, route]
        if (state$viable[item]) {
          state$t_arrival[item] <- time + travel + disruption_delay
          state$t_eta[item] <- state$t_arrival[item] + params$t_unload
          state$dest[item] <- dest
        }
        cycle_after_departure <- travel + disruption_delay + params$t_unload + travel
        vehicle_free[[pool_i]][vehicle] <- time + cycle_after_departure
        busy_minutes[leg] <- busy_minutes[leg] + max(
          0, min(params$horizon, time + cycle_after_departure) -
            assignment_time[[pool_i]][vehicle])

        n_dispatch[leg] <- n_dispatch[leg] + 1L
        route_count[leg, route] <- route_count[leg, route] + 1L
        destination_count[dest] <- destination_count[dest] + 1L
        holds <- c(holds, departure_hold[[pool_i]][vehicle])

        departure_time[[pool_i]][vehicle] <- NA_real_
        assignment_time[[pool_i]][vehicle] <- NA_real_
        departure_item[[pool_i]][vehicle] <- NA_integer_
        departure_route[[pool_i]][vehicle] <- NA_integer_
        departure_dest[[pool_i]][vehicle] <- NA_integer_
        departure_hold[[pool_i]][vehicle] <- NA_integer_
      }
    }
    for (leg in seq_len(n_leg)) {
      routes <- departed_this_minute[[leg]]
      if (length(routes)) route_history[[leg]] <- rbind(
        route_history[[leg]], data.frame(time = rep.int(time, length(routes)),
                                        route = routes))
    }

    ## 3. Item information is refreshed before this minute's decisions.
    if (time %% params$reassess == 0L) {
      state <- reassess_items_full(
        state, params,
        u_error = draw_rng_full(rng, "item", params$N),
        u_shift = draw_rng_full(rng, "item", params$N))
      if (time == 0L && !is.null(params$decision_controls))
        state$obs_initial <- state$obs
    }

    ## 4. Current plus committed downstream demand prevents batch herding.
    waiting <- tabulate(state$node[state$viable & state$status == 0L],
                        nbins = length(NODE))
    travelling_dest <- state$dest[state$viable & state$status == 1L &
                                  !is.na(state$dest)]
    reserved_dest <- integer(0)
    for (pool_i in seq_len(n_pool)) {
      scheduled <- which(!is.na(departure_dest[[pool_i]]))
      scheduled <- scheduled[
        state$viable[departure_item[[pool_i]][scheduled]]]
      reserved_dest <- c(reserved_dest,
                         departure_dest[[pool_i]][scheduled])
    }
    demand <- waiting + tabulate(c(travelling_dest, reserved_dest),
                                 nbins = length(NODE))

    scheduled_by_leg <- replicate(n_leg,
      data.frame(time = numeric(0), route = integer(0)), simplify = FALSE)
    for (pool_i in seq_len(n_pool)) {
      scheduled <- which(!is.na(departure_time[[pool_i]]))
      if (length(scheduled)) {
        leg <- pools[[pool_i]]$leg
        scheduled_by_leg[[leg]] <- rbind(scheduled_by_leg[[leg]], data.frame(
          time = departure_time[[pool_i]][scheduled],
          route = departure_route[[pool_i]][scheduled]))
      }
    }

    if (time %% params$slot == 0L) for (pool_i in seq_len(n_pool)) {
      pool <- pools[[pool_i]]
      leg <- pool$leg
      if (sum(waiting[pool$origins]) == 0L) next
      free <- which(vehicle_free[[pool_i]] <= time &
                    is.na(departure_time[[pool_i]]))
      for (vehicle in free) {
        decision <- decide_transport_full(policy, state, theta, leg,
          pool$origins, params, demand,
          time = time, disruption_model = disruption_model, history = route_history[[leg]],
          scheduled = scheduled_by_leg[[leg]],
          decision_u = draw_rng_full(rng, "policy", 1L))
        if (is.null(decision)) break
        item <- decision$item
        origin <- state$node[item]
        waiting[origin] <- waiting[origin] - 1L
        demand[origin] <- demand[origin] - 1L
        demand[decision$dest] <- demand[decision$dest] + 1L

        state$status[item] <- 1L
        state$t_eta[item] <- NA_real_
        state$t_arrival[item] <- NA_real_
        state$dest[item] <- NA_integer_
        assignment_time[[pool_i]][vehicle] <- time
        departure_time[[pool_i]][vehicle] <-
          time + params$t_load + decision$hold
        departure_item[[pool_i]][vehicle] <- item
        departure_route[[pool_i]][vehicle] <- decision$route
        departure_dest[[pool_i]][vehicle] <- decision$dest
        departure_hold[[pool_i]][vehicle] <- decision$hold
        vehicle_free[[pool_i]][vehicle] <- Inf
        scheduled_by_leg[[leg]] <- rbind(scheduled_by_leg[[leg]], data.frame(
          time = departure_time[[pool_i]][vehicle],
          route = decision$route))

      }
    }

    ## 5. Hazard and escalation continue during waiting, holding and transit.
    state <- update_item_state_full(
      state, 1, params,
      u_loss = draw_rng_full(rng, "item", params$N),
      u_escalate = draw_rng_full(rng, "item", params$N))
  }

  ## Count occupancy of reservations still loading at administrative
  ## censoring. Completed trips were already booked at departure.
  for (pool_i in seq_len(n_pool)) {
    reserved <- which(!is.na(assignment_time[[pool_i]]))
    if (length(reserved)) {
      leg <- pools[[pool_i]]$leg
      busy_minutes[leg] <- busy_minutes[leg] + sum(
        pmax(0, params$horizon - assignment_time[[pool_i]][reserved]))
    }
  }

  n <- params$N
  reached1 <- sum(!is.na(state$t_stage1))
  reached2 <- sum(!is.na(state$t_stage2))
  reached3 <- sum(!is.na(state$t_stage3))
  result <- data.frame(
    viable_stage1 = reached1 / n,
    viable_stage2 = reached2 / n,
    viable_stage3 = reached3 / n,
    n_stage1 = reached1, n_stage2 = reached2, n_stage3 = reached3,
    t_stage2_med = if (reached2) stats::median(state$t_stage2, na.rm = TRUE) else NA_real_,
    t_stage3_med = if (reached3) stats::median(state$t_stage3, na.rm = TRUE) else NA_real_,
    n_dispatch = sum(n_dispatch), n_disruption = n_disruption,
    n_disruption_loss = n_disruption_destructive,
    n_pending = sum(state$viable & is.na(state$t_stage3)),
    hold_mean = if (length(holds)) mean(holds) else 0,
    hold_rate = if (length(holds)) mean(holds > 0L) else 0,
    stringsAsFactors = FALSE)
  for (leg in seq_len(n_leg)) {
    result[[paste0("util_L", leg)]] <-
      busy_minutes[leg] / (params$V[leg] * params$horizon)
    for (route in seq_len(params$R))
      result[[paste0("routeL", leg, params$routes[route])]] <-
        route_count[leg, route]
    total_route <- sum(route_count[leg, ])
    route_probability <- if (total_route) route_count[leg, ] / total_route
                         else rep(0, params$R)
    positive <- route_probability > 0
    result[[paste0("entropy_L", leg)]] <- if (total_route)
      -sum(route_probability[positive] * log(route_probability[positive])) /
        log(params$R) else NA_real_
  }
  result$dest_Transfer_A <- destination_count[NODE[["Transfer_A"]]]
  result$dest_Transfer_B <- destination_count[NODE[["Transfer_B"]]]
  result$dest_Stage1_a <- destination_count[NODE[["Stage1_a"]]]
  result$dest_Stage1_b <- destination_count[NODE[["Stage1_b"]]]
  result
}

run_scenario_full <- function(policy, disruption_model, params, seed,
                              cell = NA_integer_) {
  ## Reset each replication independently. Matching A/B cells use the same
  ## cell seed and therefore the same vector of replicate seeds.
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(seed)
  replicate_seeds <- sample.int(.Machine$integer.max, params$n_rep)
  rows <- lapply(seq_len(params$n_rep), function(replication) {
    set.seed(replicate_seeds[replication])
    simulate_once_full(policy, disruption_model, params)
  })
  result <- do.call(rbind, rows)
  cbind(data.frame(
    cell = cell, N = params$N, disruption_risk = params$disruption_risk_level,
    band = params$hazard_band, lambda = params$lambda,
    disruption_model = disruption_model, policy = policy,
    disruptions = params$disruptions_enabled,
    rep = seq_len(params$n_rep), cell_seed = seed,
    replicate_seed = replicate_seeds,
    stringsAsFactors = FALSE), result)
}

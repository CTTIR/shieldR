#' Configure the first servicing and fixed-horizon item extension
#'
#' The caller supplies coefficients and explicit processing and observation
#' assumptions for abstract logistics items. The model tracks degradation and
#' other loss processes, finite processing capacity, and noisy item specifications.
#' The condition index is dimensionless and synthetic. Coefficients and thresholds
#' require validation for each application; this is not a calibrated materials model.
#' @param parameters A shield_parameters object.
#' @param model Named list: degradation_scale, early_other, late_other, tau,
#'   residual, prep (min/mode/max), processing (min/mode/max), failure,
#'   capacity, fragility (three positive weights), sensitivity, specificity,
#'   persistence, missing, refresh, delivery, useful_updates, item_age.
#'   Rates are cumulative coefficients; tau and all times are minutes.
#' @param planner Same schema, fixed across truth sensitivity conditions.
#' @return A new parameter object with an opt-in item configuration.
#' @export
with_item_model <- function(parameters, model, planner = model) {
  if (!inherits(parameters, "shield_parameters")) stop("invalid parameters")
  required <- c("degradation_scale", "early_other", "late_other", "tau", "residual",
    "prep", "processing", "failure", "capacity", "fragility", "sensitivity",
    "specificity", "persistence", "missing", "refresh", "delivery",
    "useful_updates", "item_age")
  for (x in list(model, planner)) {
    if (!is.list(x) || !all(required %in% names(x))) stop("incomplete item model")
    for (nm in c("degradation_scale", "early_other", "late_other", "item_age"))
      check_scalar(x[[nm]], nm, 0)
    for (nm in c("tau", "capacity", "refresh")) check_scalar(x[[nm]], nm, 1,
      integer = nm != "tau")
    check_scalar(x$delivery, "delivery", 0, integer = TRUE)
    for (nm in c("residual", "failure", "sensitivity", "specificity", "persistence", "missing"))
      check_probability(x[[nm]], nm, 1L)
    for (nm in c("prep", "processing")) {
      z <- x[[nm]]
      if (!is.numeric(z) || length(z) != 3L || any(!is.finite(z)) ||
          any(z < 0) || any(diff(z) < 0)) stop("invalid service triangle")
    }
    if (!is.numeric(x$fragility) || length(x$fragility) != 3L ||
        any(!is.finite(x$fragility) | x$fragility <= 0)) stop("invalid fragility")
    if (!is.logical(x$useful_updates) || length(x$useful_updates) != 1L ||
        is.na(x$useful_updates)) stop("useful_updates must be logical")
  }
  if (parameters$horizon + model$item_age > 1440)
    stop("transport horizon plus damage age must not exceed 24 hours")
  parameters$item_model <- model
  parameters$item_planner <- planner
  if (is.null(parameters$decision_controls))
    parameters <- with_decision_controls(parameters)
  parameters
}

item_triangle_v1 <- function(u, x) {
  if (x[3] == x[1]) return(rep(x[1], length(u)))
  f <- (x[2] - x[1]) / (x[3] - x[1])
  ifelse(u <= f, x[1] + sqrt(u * (x[3] - x[1]) * (x[2] - x[1])),
    x[3] - sqrt((1-u) * (x[3] - x[1]) * (x[3] - x[2])))
}

item_gamma_v1 <- function(t, tau) {
  t <- pmax(0, t)
  tail <- (t/tau) * exp(-t/tau)
  tail[is.infinite(t)] <- 0
  -expm1(-t/tau) - tail
}

item_hazard_v1 <- function(t, control, fragility, model) {
  # Integrals from damage. Inf control means no effective stabilization.
  g <- item_gamma_v1(t, model$tau)
  gc <- item_gamma_v1(pmin(t, control), model$tau)
  degradation <- fragility * model$degradation_scale * (model$residual*g + (1-model$residual)*gc)
  other <- fragility * (model$early_other * pmin(t,1440)/1440 +
    model$late_other * pmax(0,t-1440)/(43200-1440))
  cbind(degradation = degradation, other = other)
}

#' Fit aggregate serviced-batch loss constraints
#'
#' Uses deterministic expected competing risks on a fixed supplied service
#' sample. Fits degradation and early/late residual coefficients. Numerical
#' interval integration is explicit. Aggregate fitting is not item
#' validation and does not identify unserviced pre-service trajectories.
#' @param model Item model configuration; initial coefficient values positive.
#' @param control Vector of damage-to-stabilization times, Inf for failures.
#' @param fragility Positive vector of corresponding fragility weights.
#' @param targets Named vector: loss_24, loss_30, degradation_24.
#' @param intake Nonnegative damage-to-intake offset in minutes.
#' @param step Integration interval in minutes.
#' @return List with fitted model, expected outcomes and optimizer convergence.
#' @export
calibrate_item_model <- function(model, control, fragility, targets,
    intake = 0, step = 5) {
  check_scalar(intake, "intake", 0)
  check_scalar(step, "step", .01)
  if (length(control) != length(fragility) || !length(control) ||
      anyNA(control) || any(control < intake) ||
      any(!is.finite(fragility) | fragility <= 0)) stop("invalid calibration batch")
  if (!all(c("loss_24", "loss_30", "degradation_24") %in% names(targets)))
    stop("missing calibration targets")
  check_probability(unname(targets), "targets", 3L)
  evaluate <- function(coef) {
    m <- model; m[c("degradation_scale", "early_other", "late_other")] <- as.list(exp(coef))
    surv <- rep(1,length(control)); degradation <- rep(0,length(control))
    previous <- item_hazard_v1(intake,control,fragility,m)
    times <- sort(unique(c(seq(intake, intake+1440, by=step), intake+1440)))
    for (t in times[-1]) {
      current <- item_hazard_v1(t,control,fragility,m)
      d <- pmax(current-previous,0); total <- rowSums(d)
      degradation <- degradation + surv * (-expm1(-total)) * d[,1]/pmax(total,1e-300)
      surv <- surv*exp(-total); previous <- current
    }
    h30 <- rowSums(item_hazard_v1(intake+43200,control,fragility,m) -
      item_hazard_v1(intake,control,fragility,m))
    c(loss_24=1-mean(surv),loss_30=1-mean(exp(-h30)),degradation_24=mean(degradation))
  }
  fit <- stats::optim(log(unlist(model[c("degradation_scale","early_other","late_other")])),
    function(z) sum((evaluate(z)-targets[c("loss_24","loss_30","degradation_24")])^2),
    method="Nelder-Mead",control=list(maxit=500,reltol=1e-12))
  model[c("degradation_scale","early_other","late_other")] <- as.list(exp(fit$par))
  h <- item_hazard_v1(control,control,fragility,model) -
    item_hazard_v1(intake,control,fragility,model)
  w <- exp(-rowSums(h)); w[!is.finite(control)] <- 0
  ord <- order(control); cw <- cumsum(w[ord])/sum(w)
  quantiles <- vapply(c(.25,.5,.75),function(p)
    control[ord[which(cw>=p)[1]]]-intake,numeric(1))
  list(model=model, expected=evaluate(fit$par), convergence=fit$convergence,
    objective=fit$value, integration_step=step, stabilization_quantiles=quantiles)
}

item_init_v1 <- function(state, params, rng) {
  m <- params$item_model; n <- params$N
  u <- matrix(draw_rng_full(rng,"item",n*10L),nrow=n)
  state$item_specs <- data.frame(fragility=m$fragility[state$cls0],
    degradation=rep(TRUE,n), threshold_degradation=-log(pmax(u[,1],1e-15)),
    threshold_other=-log(pmax(u[,2],1e-15)), diag_u=u[,3],
    persistent=u[,4]<m$persistence, missing_u=u[,5],
    prep=ceiling(item_triangle_v1(u[,6],m$prep)),
    duration=pmax(1,ceiling(item_triangle_v1(u[,7],m$processing))),
    failure=u[,8]<m$failure, condition_index_error=(u[,9]-.5)*16,
    ready=rep(Inf,n), start=rep(Inf,n), completion=rep(Inf,n),
    stabilization=rep(Inf,n), loss=rep(Inf,n), cause=rep("",n),
    obs_time=rep(NA_real_,n), obs_condition_index=rep(NA_real_,n), obs_degradation=rep(NA,n),
    obs_missing=rep(TRUE,n))
  state$item_time <- m$item_age
  state$item_pending <- list()
  state$item_capacity_minutes <- 0
  state$item_exposure <- 0
  state
}

item_observe_v1 <- function(state, time, params, rng) {
  m <- params$item_model; c <- state$item_specs; n <- params$N
  # Every scheduled observation draws the same number, regardless of viability.
  u <- matrix(draw_rng_full(rng,"item",n*3L),nrow=n)
  current <- m$item_age+time
  diagnostic <- ifelse(c$persistent,c$diag_u,u[,1])
  missing <- ifelse(c$persistent,c$missing_u,u[,2]) < m$missing
  condition_index <- pmax(40, 115-20*c$fragility - .035*current*c$fragility*
    (current<c$stabilization)) + c$condition_index_error + (u[,3]-.5)*8
  positive <- ifelse(c$degradation, diagnostic<m$sensitivity, diagnostic>m$specificity)
  if (time == 0 || m$useful_updates) {
    state$item_pending[[length(state$item_pending)+1L]] <- list(
      delivery=time+m$delivery, measured=current,condition_index=condition_index,positive=positive,missing=missing)
  }
  state
}

item_deliver_v1 <- function(state, time, params) {
  pending <- state$item_pending
  initial_delivery <- FALSE
  ready <- which(vapply(pending,function(x) x$delivery<=time,logical(1)))
  for (j in ready) {
    x <- pending[[j]]; good <- !x$missing
    initial_delivery <- initial_delivery || x$measured == params$item_model$item_age
    state$item_specs$obs_missing <- x$missing
    state$item_specs$obs_condition_index[good] <- x$condition_index[good]
    state$item_specs$obs_degradation[good] <- x$positive[good]
    state$item_specs$obs_time[good] <- x$measured
  }
  if (length(ready)) state$item_pending <- pending[-ready]
  # Missing information uses an explicit uncertain middle prior, never normal BP.
  c <- state$item_specs
  score <- ifelse(is.na(c$obs_condition_index),2L,ifelse(c$obs_condition_index<80,1L,ifelse(c$obs_condition_index<100,2L,3L)))
  score[!is.na(c$obs_degradation) & c$obs_degradation] <- pmin(score[!is.na(c$obs_degradation) & c$obs_degradation],2L)
  state$obs <- as.integer(score)
  if (is.null(state$item_initial) || initial_delivery) {
    state$item_initial <- c[c("obs_time","obs_condition_index","obs_degradation","obs_missing")]
    state$obs_initial <- state$obs
  }
  state
}

item_service_v1 <- function(state, time, params) {
  c <- state$item_specs; now <- time+params$item_model$item_age
  # Readiness requires physical arrival AND completed unloading.
  eligible <- state$viable & state$node==NODE[["Stage2"]] & state$status==0L &
    !is.finite(c$ready)
  c$ready[eligible] <- now+c$prep[eligible]
  completed <- state$viable & c$completion<=now & !is.finite(c$stabilization) & !c$failure
  c$stabilization[completed] <- c$completion[completed]
  occupied <- sum(state$viable & c$start<=now & c$completion>now)
  waiting <- which(state$viable & c$ready<=now & !is.finite(c$start))
  # Common local FIFO service across arms; dispatch observations never deny service.
  free <- max(0,params$item_model$capacity-occupied)
  if (length(waiting) && free>0) {
    chosen <- utils::head(waiting[order(c$ready[waiting],waiting)],free)
    c$start[chosen] <- now; c$completion[chosen] <- now+c$duration[chosen]
  }
  state$item_specs <- c
  state
}

item_advance_v1 <- function(state, end, params) {
  c <- state$item_specs; start <- state$item_time
  if (end<=start) return(state)
  idx <- which(state$viable)
  if (length(idx)) {
    h <- item_hazard_v1(end,c$stabilization[idx],c$fragility[idx],params$item_model)
    h0 <- item_hazard_v1(params$item_model$item_age,rep(Inf,length(idx)),
      c$fragility[idx],params$item_model)
    exceeded <- h-h0 >= cbind(c$threshold_degradation[idx],c$threshold_other[idx])
    for (k in which(rowSums(exceeded)>0)) {
      i <- idx[k]; roots <- rep(Inf,2)
      for (cause in which(exceeded[k,])) {
        threshold <- c(c$threshold_degradation[i],c$threshold_other[i])[cause]+h0[k,cause]
        roots[cause] <- stats::uniroot(function(t)
          item_hazard_v1(t,c$stabilization[i],c$fragility[i],params$item_model)[1,cause]-threshold,
          c(start,end),tol=1e-7,extendInt="no")$root
      }
      c$loss[i] <- min(roots); c$cause[i] <- c("degradation","other")[which.min(roots)]
      state$viable[i] <- FALSE; state$status[i] <- 3L
      state$t_eta[i] <- state$t_arrival[i] <- NA_real_; state$dest[i] <- NA_integer_
    }
  }
  state$item_specs <- c; state$item_time <- end
  state
}

item_finish_v1 <- function(state, params) {
  m <- params$item_model
  # Finish the existing facility queue, with competing losses, after transport.
  repeat {
    c <- state$item_specs; now <- state$item_time
    pending <- state$viable & is.finite(c$ready) & (!is.finite(c$start) | c$completion>now)
    if (!any(pending) || now>=43200) break
    future <- c(c$ready[pending & c$ready>now],c$completion[pending & c$completion>now])
    # A loss can release an occupied server; one-minute steps retain that event.
    next_time <- min(now+1, future,43200)
    state <- item_advance_v1(state,next_time,params)
    state <- item_service_v1(state,next_time-m$item_age,params)
  }
  item_advance_v1(state,43200,params)
}

item_endpoints_v1 <- function(state, params) {
  c <- state$item_specs; n <- params$N
  effective <- is.finite(c$stabilization) & c$stabilization<c$loss
  viable_items <- c$loss>43200
  eligible_time <- effective & viable_items
  data.frame(viable_30d=mean(viable_items),viable_24h=mean(c$loss>1440),
    loss_degradation=mean(c$cause=="degradation"),loss_other=mean(c$cause=="other"),
    loss_second_hit=mean(c$cause=="second_hit"),
    stabilization_access=mean(effective),never_stabilization=mean(!effective),
    loss_before_stabilization=mean(!viable_items & !effective),
    stabilization_viable_items=sum(eligible_time),
    t_stabilization_med=if(any(eligible_time)) stats::median(c$stabilization[eligible_time]) else NA_real_,
    t_processing_med=if(any(eligible_time)) stats::median(c$start[eligible_time]) else NA_real_,
    processing_utilization=sum(pmax(0,pmin(c$completion,c$loss,43200)-pmin(c$start,43200)))/
      (params$item_model$capacity*43200),
    exposure_proxy=state$item_exposure/n)
}

#' Independently replay a serviced intake calibration batch
#'
#' Uses the stochastic item event engine with unlimited procedural capacity
#' and no transport or disruption_risk. All items are eligible at intake. Reports
#' outcomes on the intake clock, which differs from the chain's damage clock.
#' This is a numerical calibration check, not external validation.
#' @param model Item configuration.
#' @param N Positive batch size.
#' @param intake Damage-to-intake offset in minutes.
#' @param seed Integer seed, default 20260512.
#' @return Named vector of intake-clock loss and degradation loss risks.
#' @export
simulate_item_reference <- function(model, N=100L, intake=0, seed=20260512L) {
  model$capacity <- N; model$item_age <- intake
  p <- example_scenario(N=N);p$horizon <- 1L
  p <- with_item_model(p,model)
  with_shield_rng(seed,{
    s <- item_init_v1(init_items_full(p),p,init_rng_full())
    s$node[] <- NODE[["Stage2"]];s$t_stage2[] <- 0
    s <- item_service_v1(s,0,p)
    for(t in seq_len(1440L)) {
      s <- item_advance_v1(s,intake+t,p)
      s <- item_service_v1(s,t,p)
    }
    d24 <- mean(s$item_specs$loss<=intake+1440)
    b24 <- mean(s$item_specs$cause=="degradation")
    s <- item_finish_v1(s,p)
    s <- item_advance_v1(s,intake+43200,p)
    c(loss_24=d24,loss_30=mean(s$item_specs$loss<=intake+43200),degradation_24=b24)
  })
}

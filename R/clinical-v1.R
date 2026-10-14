#' Configure the first treatment and fixed-horizon clinical extension
#'
#' The caller supplies calibrated cause-specific coefficients and explicit
#' service and observation assumptions. This bleeding-eligible adult research
#' model is not a validated prognosis, triage rule or treatment recommendation.
#' Routine care is identical across allocation arms. Other-injury mortality is
#' a residual component, not an additional independently calibrated TBI model.
#' @param parameters A shield_parameters object.
#' @param model Named list: bleed_scale, early_other, late_other, tau,
#'   residual, prep (min/mode/max), procedure (min/mode/max), failure,
#'   capacity, severity (three positive weights), sensitivity, specificity,
#'   persistence, missing, refresh, delivery, useful_updates, injury_age.
#'   Rates are cumulative coefficients; tau and all times are minutes.
#' @param planner Same schema, fixed across truth sensitivity conditions.
#' @return A new parameter object with an opt-in clinical configuration.
#' @export
with_clinical_model <- function(parameters, model, planner = model) {
  if (!inherits(parameters, "shield_parameters")) stop("invalid parameters")
  required <- c("bleed_scale", "early_other", "late_other", "tau", "residual",
    "prep", "procedure", "failure", "capacity", "severity", "sensitivity",
    "specificity", "persistence", "missing", "refresh", "delivery",
    "useful_updates", "injury_age")
  for (x in list(model, planner)) {
    if (!is.list(x) || !all(required %in% names(x))) stop("incomplete clinical model")
    for (nm in c("bleed_scale", "early_other", "late_other", "injury_age"))
      check_scalar(x[[nm]], nm, 0)
    for (nm in c("tau", "capacity", "refresh")) check_scalar(x[[nm]], nm, 1,
      integer = nm != "tau")
    check_scalar(x$delivery, "delivery", 0, integer = TRUE)
    for (nm in c("residual", "failure", "sensitivity", "specificity", "persistence", "missing"))
      check_probability(x[[nm]], nm, 1L)
    for (nm in c("prep", "procedure")) {
      z <- x[[nm]]
      if (!is.numeric(z) || length(z) != 3L || any(!is.finite(z)) ||
          any(z < 0) || any(diff(z) < 0)) stop("invalid service triangle")
    }
    if (!is.numeric(x$severity) || length(x$severity) != 3L ||
        any(!is.finite(x$severity) | x$severity <= 0)) stop("invalid severity")
    if (!is.logical(x$useful_updates) || length(x$useful_updates) != 1L ||
        is.na(x$useful_updates)) stop("useful_updates must be logical")
  }
  if (parameters$horizon + model$injury_age > 1440)
    stop("transport horizon plus injury age must not exceed 24 hours")
  parameters$clinical_model <- model
  parameters$clinical_planner <- planner
  if (is.null(parameters$decision_controls))
    parameters <- with_decision_controls(parameters)
  parameters
}

clinical_triangle_v1 <- function(u, x) {
  if (x[3] == x[1]) return(rep(x[1], length(u)))
  f <- (x[2] - x[1]) / (x[3] - x[1])
  ifelse(u <= f, x[1] + sqrt(u * (x[3] - x[1]) * (x[2] - x[1])),
    x[3] - sqrt((1-u) * (x[3] - x[1]) * (x[3] - x[2])))
}

clinical_gamma_v1 <- function(t, tau) {
  t <- pmax(0, t)
  tail <- (t/tau) * exp(-t/tau)
  tail[is.infinite(t)] <- 0
  -expm1(-t/tau) - tail
}

clinical_hazard_v1 <- function(t, control, severity, model) {
  # Integrals from injury. Inf control means no effective hemostasis.
  g <- clinical_gamma_v1(t, model$tau)
  gc <- clinical_gamma_v1(pmin(t, control), model$tau)
  bleed <- severity * model$bleed_scale * (model$residual*g + (1-model$residual)*gc)
  other <- severity * (model$early_other * pmin(t,1440)/1440 +
    model$late_other * pmax(0,t-1440)/(43200-1440))
  cbind(bleed = bleed, other = other)
}

#' Fit aggregate treated-cohort mortality constraints
#'
#' Uses deterministic expected competing risks on a fixed supplied service
#' sample. Fits bleeding and early/late residual coefficients. Numerical
#' interval integration is explicit. Aggregate fitting is not clinical
#' validation and does not identify untreated prehospital trajectories.
#' @param model Clinical model configuration; initial coefficient values positive.
#' @param control Vector of injury-to-hemostasis times, Inf for failures.
#' @param severity Positive vector of corresponding severity weights.
#' @param targets Named vector: mortality_24, mortality_30, bleeding_24.
#' @param admission Nonnegative injury-to-admission offset in minutes.
#' @param step Integration interval in minutes.
#' @return List with fitted model, expected outcomes and optimizer convergence.
#' @export
calibrate_clinical_model <- function(model, control, severity, targets,
    admission = 0, step = 5) {
  check_scalar(admission, "admission", 0)
  check_scalar(step, "step", .01)
  if (length(control) != length(severity) || !length(control) ||
      anyNA(control) || any(control < admission) ||
      any(!is.finite(severity) | severity <= 0)) stop("invalid calibration cohort")
  if (!all(c("mortality_24", "mortality_30", "bleeding_24") %in% names(targets)))
    stop("missing calibration targets")
  check_probability(unname(targets), "targets", 3L)
  evaluate <- function(coef) {
    m <- model; m[c("bleed_scale", "early_other", "late_other")] <- as.list(exp(coef))
    surv <- rep(1,length(control)); bleeding <- rep(0,length(control))
    previous <- clinical_hazard_v1(admission,control,severity,m)
    times <- sort(unique(c(seq(admission, admission+1440, by=step), admission+1440)))
    for (t in times[-1]) {
      current <- clinical_hazard_v1(t,control,severity,m)
      d <- pmax(current-previous,0); total <- rowSums(d)
      bleeding <- bleeding + surv * (-expm1(-total)) * d[,1]/pmax(total,1e-300)
      surv <- surv*exp(-total); previous <- current
    }
    h30 <- rowSums(clinical_hazard_v1(admission+43200,control,severity,m) -
      clinical_hazard_v1(admission,control,severity,m))
    c(mortality_24=1-mean(surv),mortality_30=1-mean(exp(-h30)),bleeding_24=mean(bleeding))
  }
  fit <- stats::optim(log(unlist(model[c("bleed_scale","early_other","late_other")])),
    function(z) sum((evaluate(z)-targets[c("mortality_24","mortality_30","bleeding_24")])^2),
    method="Nelder-Mead",control=list(maxit=500,reltol=1e-12))
  model[c("bleed_scale","early_other","late_other")] <- as.list(exp(fit$par))
  h <- clinical_hazard_v1(control,control,severity,model) -
    clinical_hazard_v1(admission,control,severity,model)
  w <- exp(-rowSums(h)); w[!is.finite(control)] <- 0
  ord <- order(control); cw <- cumsum(w[ord])/sum(w)
  quantiles <- vapply(c(.25,.5,.75),function(p)
    control[ord[which(cw>=p)[1]]]-admission,numeric(1))
  list(model=model, expected=evaluate(fit$par), convergence=fit$convergence,
    objective=fit$value, integration_step=step, hemostasis_quantiles=quantiles)
}

clinical_init_v1 <- function(state, params, rng) {
  m <- params$clinical_model; n <- params$N
  u <- matrix(draw_rng_full(rng,"clinical",n*10L),nrow=n)
  state$clinical <- data.frame(severity=m$severity[state$cls0],
    bleed=rep(TRUE,n), threshold_bleed=-log(pmax(u[,1],1e-15)),
    threshold_other=-log(pmax(u[,2],1e-15)), diag_u=u[,3],
    persistent=u[,4]<m$persistence, missing_u=u[,5],
    prep=ceiling(clinical_triangle_v1(u[,6],m$prep)),
    duration=pmax(1,ceiling(clinical_triangle_v1(u[,7],m$procedure))),
    failure=u[,8]<m$failure, bp_error=(u[,9]-.5)*16,
    ready=rep(Inf,n), start=rep(Inf,n), completion=rep(Inf,n),
    hemostasis=rep(Inf,n), death=rep(Inf,n), cause=rep("",n),
    obs_time=rep(NA_real_,n), obs_bp=rep(NA_real_,n), obs_bleed=rep(NA,n),
    obs_missing=rep(TRUE,n))
  state$clinical_time <- m$injury_age
  state$clinical_pending <- list()
  state$clinical_capacity_minutes <- 0
  state$clinical_exposure <- 0
  state
}

clinical_observe_v1 <- function(state, time, params, rng) {
  m <- params$clinical_model; c <- state$clinical; n <- params$N
  # Every scheduled observation draws the same number, regardless of survival.
  u <- matrix(draw_rng_full(rng,"clinical",n*3L),nrow=n)
  current <- m$injury_age+time
  diagnostic <- ifelse(c$persistent,c$diag_u,u[,1])
  missing <- ifelse(c$persistent,c$missing_u,u[,2]) < m$missing
  bp <- pmax(40, 115-20*c$severity - .035*current*c$severity*
    (current<c$hemostasis)) + c$bp_error + (u[,3]-.5)*8
  positive <- ifelse(c$bleed, diagnostic<m$sensitivity, diagnostic>m$specificity)
  if (time == 0 || m$useful_updates) {
    state$clinical_pending[[length(state$clinical_pending)+1L]] <- list(
      delivery=time+m$delivery, measured=current,bp=bp,positive=positive,missing=missing)
  }
  state
}

clinical_deliver_v1 <- function(state, time, params) {
  pending <- state$clinical_pending
  initial_delivery <- FALSE
  ready <- which(vapply(pending,function(x) x$delivery<=time,logical(1)))
  for (j in ready) {
    x <- pending[[j]]; good <- !x$missing
    initial_delivery <- initial_delivery || x$measured == params$clinical_model$injury_age
    state$clinical$obs_missing <- x$missing
    state$clinical$obs_bp[good] <- x$bp[good]
    state$clinical$obs_bleed[good] <- x$positive[good]
    state$clinical$obs_time[good] <- x$measured
  }
  if (length(ready)) state$clinical_pending <- pending[-ready]
  # Missing information uses an explicit uncertain middle prior, never normal BP.
  c <- state$clinical
  score <- ifelse(is.na(c$obs_bp),2L,ifelse(c$obs_bp<80,1L,ifelse(c$obs_bp<100,2L,3L)))
  score[!is.na(c$obs_bleed) & c$obs_bleed] <- pmin(score[!is.na(c$obs_bleed) & c$obs_bleed],2L)
  state$obs <- as.integer(score)
  if (is.null(state$clinical_initial) || initial_delivery) {
    state$clinical_initial <- c[c("obs_time","obs_bp","obs_bleed","obs_missing")]
    state$obs_initial <- state$obs
  }
  state
}

clinical_service_v1 <- function(state, time, params) {
  c <- state$clinical; now <- time+params$clinical_model$injury_age
  # Readiness requires physical arrival AND completed unloading.
  eligible <- state$alive & state$node==NODE[["Stage2"]] & state$status==0L &
    !is.finite(c$ready)
  c$ready[eligible] <- now+c$prep[eligible]
  completed <- state$alive & c$completion<=now & !is.finite(c$hemostasis) & !c$failure
  c$hemostasis[completed] <- c$completion[completed]
  occupied <- sum(state$alive & c$start<=now & c$completion>now)
  waiting <- which(state$alive & c$ready<=now & !is.finite(c$start))
  # Common local FIFO care across arms; dispatch observations never deny care.
  free <- max(0,params$clinical_model$capacity-occupied)
  if (length(waiting) && free>0) {
    chosen <- utils::head(waiting[order(c$ready[waiting],waiting)],free)
    c$start[chosen] <- now; c$completion[chosen] <- now+c$duration[chosen]
  }
  state$clinical <- c
  state
}

clinical_advance_v1 <- function(state, end, params) {
  c <- state$clinical; start <- state$clinical_time
  if (end<=start) return(state)
  idx <- which(state$alive)
  if (length(idx)) {
    h <- clinical_hazard_v1(end,c$hemostasis[idx],c$severity[idx],params$clinical_model)
    h0 <- clinical_hazard_v1(params$clinical_model$injury_age,rep(Inf,length(idx)),
      c$severity[idx],params$clinical_model)
    exceeded <- h-h0 >= cbind(c$threshold_bleed[idx],c$threshold_other[idx])
    for (k in which(rowSums(exceeded)>0)) {
      i <- idx[k]; roots <- rep(Inf,2)
      for (cause in which(exceeded[k,])) {
        threshold <- c(c$threshold_bleed[i],c$threshold_other[i])[cause]+h0[k,cause]
        roots[cause] <- stats::uniroot(function(t)
          clinical_hazard_v1(t,c$hemostasis[i],c$severity[i],params$clinical_model)[1,cause]-threshold,
          c(start,end),tol=1e-7,extendInt="no")$root
      }
      c$death[i] <- min(roots); c$cause[i] <- c("bleeding","other")[which.min(roots)]
      state$alive[i] <- FALSE; state$status[i] <- 3L
      state$t_eta[i] <- state$t_arrival[i] <- NA_real_; state$dest[i] <- NA_integer_
    }
  }
  state$clinical <- c; state$clinical_time <- end
  state
}

clinical_finish_v1 <- function(state, params) {
  m <- params$clinical_model
  # Finish the existing facility queue, with competing deaths, after transport.
  repeat {
    c <- state$clinical; now <- state$clinical_time
    pending <- state$alive & is.finite(c$ready) & (!is.finite(c$start) | c$completion>now)
    if (!any(pending) || now>=43200) break
    future <- c(c$ready[pending & c$ready>now],c$completion[pending & c$completion>now])
    # A death can release an occupied server; one-minute steps retain that event.
    next_time <- min(now+1, future,43200)
    state <- clinical_advance_v1(state,next_time,params)
    state <- clinical_service_v1(state,next_time-m$injury_age,params)
  }
  clinical_advance_v1(state,43200,params)
}

clinical_endpoints_v1 <- function(state, params) {
  c <- state$clinical; n <- params$N
  effective <- is.finite(c$hemostasis) & c$hemostasis<c$death
  survivors <- c$death>43200
  eligible_time <- effective & survivors
  data.frame(surv_30d=mean(survivors),surv_24h=mean(c$death>1440),
    death_bleeding=mean(c$cause=="bleeding"),death_other=mean(c$cause=="other"),
    death_second_hit=mean(c$cause=="second_hit"),
    hemostasis_access=mean(effective),never_hemostasis=mean(!effective),
    death_before_hemostasis=mean(!survivors & !effective),
    hemostasis_survivors=sum(eligible_time),
    t_hemostasis_med=if(any(eligible_time)) stats::median(c$hemostasis[eligible_time]) else NA_real_,
    t_procedure_med=if(any(eligible_time)) stats::median(c$start[eligible_time]) else NA_real_,
    procedure_utilization=sum(pmax(0,pmin(c$completion,c$death,43200)-pmin(c$start,43200)))/
      (params$clinical_model$capacity*43200),
    exposure_proxy=state$clinical_exposure/n)
}

#' Independently replay a treated admission calibration cohort
#'
#' Uses the stochastic clinical event engine with unlimited procedural capacity
#' and no transport or disruption_risk. All patients are eligible at admission. Reports
#' outcomes on the admission clock, which differs from the chain's injury clock.
#' This is a numerical calibration check, not external validation.
#' @param model Clinical configuration.
#' @param N Positive cohort size.
#' @param admission Injury-to-admission offset in minutes.
#' @param seed Integer seed, default 20260512.
#' @return Named vector of admission-clock mortality and bleeding death risks.
#' @export
simulate_clinical_reference <- function(model, N=100L, admission=0, seed=20260512L) {
  model$capacity <- N; model$injury_age <- admission
  p <- example_scenario(N=N);p$horizon <- 1L
  p <- with_clinical_model(p,model)
  with_shield_rng(seed,{
    s <- clinical_init_v1(init_patients_full(p),p,init_rng_full())
    s$node[] <- NODE[["Stage2"]];s$t_stage2[] <- 0
    s <- clinical_service_v1(s,0,p)
    for(t in seq_len(1440L)) {
      s <- clinical_advance_v1(s,admission+t,p)
      s <- clinical_service_v1(s,t,p)
    }
    d24 <- mean(s$clinical$death<=admission+1440)
    b24 <- mean(s$clinical$cause=="bleeding")
    s <- clinical_finish_v1(s,p)
    s <- clinical_advance_v1(s,admission+43200,p)
    c(mortality_24=d24,mortality_30=mean(s$clinical$death<=admission+43200),bleeding_24=b24)
  })
}

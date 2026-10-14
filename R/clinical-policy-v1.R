clinical_observed_survival_v1 <- function(obs, time, control_time, model) {
  # Synthetic fixed forecast. Only observed fields enter, never latent severity.
  severity <- if (is.na(obs$obs_bp)) mean(model$severity) else
    pmin(max(model$severity)*2,pmax(min(model$severity)/2,(115-obs$obs_bp)/20))
  recognized <- if (is.na(obs$obs_bleed)) .5 else if (obs$obs_bleed) 1 else .25
  a <- clinical_hazard_v1(43200,control_time,severity,model)
  b <- clinical_hazard_v1(time,rep(Inf,length(control_time)),severity,model)
  exp(-pmax(0,recognized*(a[,1]-b[,1])+a[,2]-b[,2]))
}

clinical_decide_v1 <- function(policy,state,theta,leg,origins,params,demand,
    time=0L,disruption_model="static",history=NULL,scheduled=NULL,decision_u=NULL) {
  waiting <- which(state$alive & state$status==0L & state$node %in% origins)
  # Definitive care is completed before a patient is committed to the last leg.
  if (leg==5L) waiting <- waiting[state$clinical$completion[waiting] <=
    time+params$clinical_model$injury_age]
  if (!length(waiting)) return(NULL)
  obs <- if(params$decision_controls$clinical=="initial_observed")
    state$clinical_initial else state$clinical
  m <- params$clinical_planner; now <- time+params$clinical_model$injury_age
  remaining <- if(leg<=4L) sum(rowMeans(params$leg_time)[leg:4L]) +
    (5-leg)*(params$t_load+params$t_unload) else 0
  facility_wait <- sum(state$alive & state$clinical$ready<=now &
    state$clinical$completion>now)*mean(m$procedure)/m$capacity
  expected_control <- now+remaining+mean(m$prep)+mean(m$procedure)+facility_wait
  if (policy %in% c("P0","P2")) {
    priority <- state$obs_initial[waiting]
    patient <- waiting[order(priority,waiting)][1]
  } else {
    gain <- vapply(waiting,function(i) {
      s <- clinical_observed_survival_v1(obs[i,],now,c(expected_control,expected_control+30),m)
      s[1]-s[2]
    },numeric(1))
    patient <- waiting[order(-gain,waiting)][1]
  }
  dests <- params$leg_dests[[leg]]
  if (policy!="P3" || leg==5L) {
    dest <- dests[which.min(destination_load(demand,dests,params))]
    return(list(patient=patient,route=1L,dest=as.integer(dest),hold=0L))
  }
  disruption_risk <- theta_hat_full(theta)[leg,]
  if(is.null(history)) history <- data.frame(time=integer(),route=integer())
  if(is.null(scheduled)) scheduled <- data.frame(time=numeric(),route=integer())
  reactive <- params$decision_controls$anticipation=="reactive"
  current_log <- disruption_model_response_full(history,params,disruption_model,log_scale=TRUE)
  choices <- expand.grid(route=seq_len(params$R),dest_i=seq_along(dests),
    hold=params$departure_holds,KEEP.OUT.ATTRS=FALSE)
  loss <- rep(Inf,nrow(choices))
  for (hold in params$departure_holds) {
    ix <- which(choices$hold==hold)
    if(reactive) projected <- pmin(params$p_cap,disruption_risk) else {
      prior <- scheduled[scheduled$time<time+params$t_load+hold,,drop=FALSE]
      hist <- if(nrow(prior)) rbind(history,prior[order(prior$time),,drop=FALSE]) else history
      future_log <- disruption_model_response_full(hist,params,disruption_model,log_scale=TRUE)
      projected <- exp(pmin(log(params$p_cap),log(disruption_risk)+future_log-current_log))
    }
    allowed <- projected[choices$route[ix]] <= projected[1]+1e-12
    care <- expected_control+hold+params$leg_time[leg,choices$route[ix]]-
      mean(params$leg_time[leg,])+params$kappa_node[dests[choices$dest_i[ix]]]*
      demand[dests[choices$dest_i[ix]]]
    survival <- clinical_observed_survival_v1(obs[patient,],now,care,m)
    loss[ix[allowed]] <- 1-survival[allowed]*(1-params$q*projected[choices$route[ix[allowed]]])
  }
  logw <- -params$beta*loss
  prob <- exp(logw-max(logw)); prob <- prob/sum(prob)
  if(is.null(decision_u)) decision_u <- stats::runif(1)
  pick <- which(decision_u<=cumsum(prob))[1]
  list(patient=patient,route=as.integer(choices$route[pick]),
    dest=as.integer(dests[choices$dest_i[pick]]),hold=as.integer(choices$hold[pick]))
}

clinical_fixture_v1 <- function(N=6L) {
  p <- example_scenario(N=N)
  m <- list(bleed_scale=.2,early_other=.02,late_other=.1,tau=40,residual=.1,
    prep=c(1,2,3),procedure=c(2,3,4),failure=0,capacity=1L,severity=c(1.2,1,.8),
    sensitivity=.7,specificity=.95,persistence=1,missing=0,refresh=2L,delivery=0L,
    useful_updates=TRUE,injury_age=0)
  with_clinical_model(p,m)
}

trace_clinical_v1 <- function(p,policy="P3",seed=20260512L) {
  p$clinical_trace <- TRUE
  x <- simulate_replication(p,policy,"static",seed)
  list(result=x,state=attr(x,"clinical_trace"))
}

test_that("clinical configuration validates and old mode is unchanged", {
  p <- clinical_fixture_v1()
  expect_null(example_scenario()$clinical_model)
  bad <- p$clinical_model;bad$capacity <- 0
  expect_error(with_clinical_model(example_scenario(),bad),"capacity")
  bad <- p$clinical_model;bad$missing <- 2
  expect_error(with_clinical_model(example_scenario(),bad),"missing")
  expect_identical(simulate_replication(example_scenario(),"P3","static",20260512L),
    simulate_replication(example_scenario(),"P3","static",20260512L))
})

test_that("arrival, readiness, procedure and effective control are distinct", {
  p <- clinical_fixture_v1(10L)
  p$clinical_model$bleed_scale <- p$clinical_model$early_other <- p$clinical_model$late_other <- 0
  x <- trace_clinical_v1(p);s<-x$state;c<-s$clinical
  expect_true(all(s$t_stage2+p$t_unload <= c$ready))
  expect_true(all(c$ready<=c$start))
  expect_true(all(c$start<c$completion))
  expect_equal(c$completion,c$hemostasis)
  expect_true(all(s$t_stage3>=c$completion | is.na(s$t_stage3)))
  for(t in seq(0,max(c$completion)))
    expect_lte(sum(c$start<=t & c$completion>t),p$clinical_model$capacity)
  expect_equal(x$result$surv_30d,1)
  expect_equal(x$result$never_hemostasis,0)
})

test_that("risk persists after arrival and after hemostasis", {
  p <- clinical_fixture_v1(20L)
  p$clinical_model$bleed_scale <- 0;p$clinical_model$early_other<-0
  p$clinical_model$late_other <- 8
  x <- trace_clinical_v1(p)
  expect_equal(x$result$surv_stage2,1)
  expect_lt(x$result$surv_30d,x$result$surv_24h)
  expect_true(all(x$state$clinical$death>=1440))
  expect_equal(x$result$death_bleeding,0)
})

test_that("competing death prevents treatment and releases capacity", {
  p <- clinical_fixture_v1(20L);p$clinical_model$bleed_scale<-20
  x<-trace_clinical_v1(p);c<-x$state$clinical
  expect_true(all(c$start[is.finite(c$start)]<c$death[is.finite(c$start)]))
  expect_true(all(c$hemostasis[is.finite(c$hemostasis)]<c$death[is.finite(c$hemostasis)]))
  expect_equal(x$result$surv_30d+x$result$death_bleeding+x$result$death_other+
    x$result$death_second_hit,1)
})

test_that("missing and frozen information cannot stop biology", {
  p<-clinical_fixture_v1(10L);p$clinical_model$missing<-1
  x<-trace_clinical_v1(p);expect_true(all(is.na(x$state$clinical$obs_bp)))
  expect_true(all(x$state$clinical$obs_missing))
  p$clinical_model$useful_updates<-FALSE
  a<-with_decision_controls(p,"updated","reactive")
  b<-with_decision_controls(p,"initial_observed","reactive")
  expect_equal(simulate_replication(a,"P3","static",20260512L),
    simulate_replication(b,"P3","static",20260512L))
})

test_that("planner uses observed data and keeps truth coefficients separate", {
  p<-clinical_fixture_v1();s<-trace_clinical_v1(p)$state
  obs<-s$clinical_initial[1,]
  value<-clinical_observed_survival_v1(obs,10,c(20,40),p$clinical_planner)
  expect_gte(value[1],value[2])
  p$clinical_model$bleed_scale<-5
  expect_equal(value,clinical_observed_survival_v1(obs,10,c(20,40),p$clinical_planner))
})

test_that("clinical seed pairing and RNG preservation hold", {
  p<-clinical_fixture_v1();set.seed(20260512);before<-.Random.seed
  a<-simulate_replication(p,"P3","static",20260512L)
  expect_identical(before,.Random.seed)
  expect_identical(a,simulate_replication(p,"P3","static",20260512L))
  p$disruptions_enabled<-TRUE
  expect_equal(a,simulate_replication(p,"P3","static",20260512L))
})

test_that("persistent recognition preserves the specified initial marginal", {
  p<-clinical_fixture_v1(10000L)
  x<-with_shield_rng(20260512L,{
    s<-clinical_init_v1(init_patients_full(p),p,init_rng_full())
    r<-init_rng_full();s<-clinical_observe_v1(s,0,p,r)
    first<-s$clinical_pending[[1]]$positive
    s<-clinical_observe_v1(s,2,p,r)
    list(first=first,second=s$clinical_pending[[2]]$positive)
  })
  expect_lt(abs(mean(x$first)-p$clinical_model$sensitivity),.02)
  expect_identical(x$first,x$second)
})

test_that("delayed initial measurements reach frozen policies once delivered", {
  p<-clinical_fixture_v1();p$clinical_model$delivery<-3L
  x<-with_shield_rng(20260512L,{
    r<-init_rng_full();s<-clinical_init_v1(init_patients_full(p),p,r)
    s<-clinical_observe_v1(s,0,p,r)
    s<-clinical_deliver_v1(s,0,p)
    before<-s$clinical_initial
    s<-clinical_deliver_v1(s,3,p)
    initial<-s$clinical_initial
    s<-clinical_observe_v1(s,4,p,r);s<-clinical_deliver_v1(s,7,p)
    list(before=before,initial=initial,latest=s$clinical_initial,current=s$clinical)
  })
  expect_true(all(is.na(x$before$obs_time)))
  expect_true(all(x$initial$obs_time==0))
  expect_identical(x$initial,x$latest)
  expect_true(all(x$current$obs_time==4))
})

\name{with_decision_controls}
\alias{with_decision_controls}
\title{Configure matched item-information and anticipation controls}
\usage{
with_decision_controls(parameters,
  item = c("updated", "initial_observed"),
  anticipation = c("known_model", "reactive"))
}
\arguments{
\item{parameters}{A \code{shield_parameters} object.}
\item{item}{Use updated observations or retain the first observed class.}
\item{anticipation}{Use the specified-response forecast or current risk estimates.}
}
\value{A new parameter object with recorded \code{decision_controls}.}
\description{Separates item information use and P3 anticipation while keeping
item deterioration, observation draws and decision architecture unchanged.}
\details{All controlled policies start from the first noisy observation. The
initial-observation arm retains this signal; updated arms use the latest
scheduled observation. P3 reactive controls update risk estimates but do not
forecast changes from committed future departures. Candidate actions, weights,
queues and randomization remain identical. The anticipation switch is unused
by P0, P1 and P2. Uncontrolled parameters retain legacy behavior. No servicing
model or equilibrium solution is introduced, and existing reservations are not
replanned after a new item observation. Use a separate experiment identifier
and distinct seeds for independent policy arms.}
\examples{
set.seed(20260512)
p <- with_decision_controls(example_scenario(N = 2L),
  item = "initial_observed", anticipation = "reactive")
simulate_shield(p, policy = "P3", n_rep = 1L)
}

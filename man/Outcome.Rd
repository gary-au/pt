\name{Outcome}
\alias{Outcome}
\alias{Outcome-class}
\title{The S4 Outcome class}
\description{
  The Outcome class represents an outcome of a gamble. Two
  fields store visible information used for calculations.
  The field \code{objective_consequence} stores the
  objective consequence of the outcome. The field
  \code{probability_string} stores the probability string
  associated with the objective consequence. A string is
  used in order to avoid rounding errors associated with
  the floating point representation of numbers. Other
  fields are internal calculational variables used for
  implementing different theories of risky decision making.
}
\note{
  The subjective_value is the utility of an outcome.
}
\section{Slots}{
  \describe{ \item{\code{outcome_id}:}{Object of class
  \code{"numeric"}, containing the unique id of the
  outcome} \item{\code{position}:}{Object of class
  \code{"numeric"}, containing the position of the outcome}
  \item{\code{objective_consequence}:}{Object of class
  \code{"numeric"}, containing the objective consequence}
  \item{\code{probability}:}{Object of class
  \code{"numeric"}, containing the probability of the
  outcome} \item{\code{probability_string}:}{Object of
  class \code{"character"}, containing the probability of
  the outcome stored as a string}
  \item{\code{rank}:}{Object of class \code{"numeric"},
  containing the rank of the outcome}
  \item{\code{decision_weight}:}{Object of class
  \code{"numeric"}, containing the decision weight of the
  outcome} \item{\code{subjective_value}:}{Object of class
  \code{"numeric"}, containing the subjective value
  (utility) of the outcome} \item{\code{w}:}{Object of
  class \code{"numeric"}, containing the RDU w of the
  outcome} }
}


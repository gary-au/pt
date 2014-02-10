\name{create_choices}
\alias{create_choices}
\title{create_choices}
\usage{
  create_choices(choice_id_vector, gamble_id_vector,
    outcome_id_vector, objective_consequence_vector,
    probability_string_vector)
}
\arguments{
  \item{choice_id_vector}{vector, a vector containing the
  choice_id of each objective_consequence.}

  \item{gamble_id_vector}{vector, a vector containing the
  gamble_id of each objective_consequence.}

  \item{outcome_id_vector}{vector, a vector containing the
  outcome_id of each objective_consequence.}

  \item{objective_consequence_vector}{vector, a vector
  containing the objective consequences.}

  \item{probability_string_vector}{vector, a vector
  containing the probability_string of each objective
  consequence.}
}
\description{
  Create choices using five vectors.
}
\details{
  This function creates a new instance of a Choices class.
  The inputs are five vectors, representing the properties
  of each outcome.
}
\examples{
choice_id_vector <- c(1, 1, 1, 1, 1, 1, 1, 1)

gamble_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)

outcome_id_vector <- c(1, 2, 3, 4, 1, 2, 3, 4)

objective_consequence_vector <- c(7, 7, 84, 90, 7, 10, 90, 90)

probability_string_vector <- c("0.1", "0.3", "0.3", "0.3", "0.1", "0.3", "0.3", "0.3")

my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector,
	outcome_id_vector=outcome_id_vector,
	objective_consequence_vector=objective_consequence_vector,
	probability_string_vector=probability_string_vector)

my_choices
}


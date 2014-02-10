\name{compareEV}
\alias{compareEV}
\alias{compareEV,Choices-method}
\title{compareEV}
\description{
  Compare the expected value (EV) of choice gambles.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the EV for each gamble in the choices.

choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)

gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)

outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)

objective_consequence_vector <- c(2500, 2400, 0, 2400,
	2500, 0, 2400, 0)

probability_string_vector <- c("0.33", "0.66", "0.01", "1.0",
	"0.33", "0.67", "0.34", "0.66")

my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector,
	outcome_id_vector=outcome_id_vector,
	objective_consequence_vector=objective_consequence_vector,
	probability_string_vector=probability_string_vector)

my_choices

compareEV(my_choices, digits=4)
}


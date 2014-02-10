\name{comparePRT}
\alias{comparePRT}
\alias{comparePRT,Choices-method}
\title{comparePRT}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{gamma}{numeric, the gamma parameter in Viscusi's
  theory.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
  Compare choice gambles under Viscusi's (1989) Prospective
  reference theory (PRT).
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the PRT for each gamble in the choices.

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

my_utility <- create_utility(utility_function="power",
	parameters=c(alpha=0.631, beta=0.631, lambda=1))

gamma <- 0.676

comparePRT(my_choices,
	utility=my_utility,
	gamma=gamma,
	digits=4)
}
\references{
  Viscusi, W. K. (1989). Prospective reference theory:
  Toward an explanation of the paradoxes. Journal of Risk
  and Uncertainty, 2(3), 235-263.
}


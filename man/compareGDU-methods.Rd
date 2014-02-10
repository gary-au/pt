\name{compareGDU}
\alias{compareGDU}
\alias{compareGDU,Choices-method}
\title{compareGDU}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{probability_weighting_specification}{Probability_Weighting,
  an instance of a Probability_Weighting class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
  Compare choice gambles under Luce's (2000) (Lower)
  Gains-decompositions utility (GDU) theory.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the GDU for each gamble in the choices.

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

my_pwf <-
	create_probability_weighting(probability_function="compound_invariance",
	parameters=c(alpha=0.542, beta=1.382))

my_utility <- create_utility(utility_function="power",
	parameters=c(alpha=1, beta=1, lambda=1))

compareGDU(my_choices,
	probability_weighting_specification=my_pwf,
	utility=my_utility,
	digits=4)
}
\references{
  Luce, R. D. (2000). Utility of gains and losses:
  Measurement-theoretical and experimental approaches.
  Mahwah, NJ: Lawrence Erlbaum Associates.
}


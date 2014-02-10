\name{comparePT}
\alias{comparePT}
\alias{comparePT,Choices-method}
\title{comparePT}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{probability_weighting_specification_for_positive_outcomes}{Probability_Weighting,
  an instance of a Probability_Weighting class.}

  \item{probability_weighting_specification_for_negative_outcomes}{Probability_Weighting,
  an instance of a Probability_Weighting class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
  Compare choice gambles under Tversky and Kahneman's
  (1992) (Cumulative) prospect theory (PT).
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the PT for each gamble in the choices.

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

tk_1992_utility <- create_utility(utility_function="power",
	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))

tk_1992_positive_probability_weighting <-
	create_probability_weighting(probability_function=
	"Tversky_Kahneman_1992",
	parameters=c(alpha=0.61))

tk_1992_negative_probability_weighting <-
	create_probability_weighting(probability_function=
	"Tversky_Kahneman_1992",
	parameters=c(alpha=0.69))

comparePT(my_choices,
	probability_weighting_specification_for_positive_outcomes=
	tk_1992_positive_probability_weighting,
	probability_weighting_specification_for_negative_outcomes=
	tk_1992_negative_probability_weighting,
	utility=tk_1992_utility,
	digits=4)
}
\references{
  Tversky, A., & Kahneman, D. (1992). Advances in prospect
  theory: Cumulative representation of uncertainty. Journal
  of Risk and Uncertainty, 5(4), 297-323.

  Wakker, P. P. (2010). Prospect theory: For risk and
  ambiguity. Cambridge, UK: Cambridge University Press.
}


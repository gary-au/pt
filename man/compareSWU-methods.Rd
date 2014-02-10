\name{compareSWU}
\alias{compareSWU}
\alias{compareSWU,Choices-method}
\title{compareSWU}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{probability_weighting_specification}{Probability_Weighting,
  an instance of a Probability_Weighting class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
  Compare choice gambles under Edwards' (1954, 1962)
  Subjective Weighted Utility (SWU).
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the SWU for each gamble in the choices.

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
	parameters=c(alpha=0.4, beta=0.4, lambda=1))

my_pwf <-
create_probability_weighting(probability_function="linear_in_log_odds",
	parameters=c(alpha=0.4, beta=0.4))

compareSWU(my_choices,
	probability_weighting_specification=my_pwf,
	utility=my_utility,
	digits=4)
}
\references{
  Edwards, W. (1954). The theory of decision making.
  Psychological Bulletin, 51(4), 380-417.

  Edwards, W. (1962). Subjective probabilities inferred
  from decisions. Psychological Review, 69(2), 109-135.

  Birnbaum, M. H. (1999). The paradoxes of Allais,
  stochastic dominance, and decision weights. In J.
  Shanteau, B. A. Mellers & D. A. Schum (Eds.), Decision
  science and technology: Reflections on the contributions
  of Ward Edwards (pp. 27-52). Norwell, MA: Kluwer Academic
  Publishers.
}


\name{compareEU}
\alias{compareEU}
\alias{compareEU,Choices-method}
\title{compareEU}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
  Compare the expected utility (EU) of choice gambles.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the EU for each gamble in the choices.

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
parameters=c(alpha=1.0, beta=1.0, lambda=1.0))

compareEU(my_choices, utility=my_utility, digits=4)
}
\references{
  von Neumann, J., & Morgenstern, O. (1947). Theory of
  games and economic behavior (2nd ed.). Princeton, NJ:
  Princeton University Press.

  Bernoulli, D. (1954). Exposition of a new theory on the
  measurement of risk. Econometrica, 22(1), 23-36.

  Bernoulli, D. (1738). Specimen theoriae novae de mensura
  sortis. Commentarii Academiae Scientiarum Imperialis
  Petropolitanae, 5, 175-192.
}


\name{compareRAM}
\alias{compareRAM}
\alias{compareRAM,Choices-method}
\title{compareRAM}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{branch_weighting_vector_list}{list, a list of
  branch weighting vectors.}

  \item{probability_weighting_specification}{Probability_Weighting,
  an instance of a Probability_Weighting class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
  Compare choice gambles under Birnbaum's (2008) configural
  weight RAM theory.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the RAM for each gamble in the choices.

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

# note that the maximum number of outcomes in the gambles is 3,
# so branch weights for 3 outcomes need to be provided.

branch_weighting_vector_list <- list(c(1),
	c(0.3738, 0.6262),
	c(0.16, 0.33, 0.51))

my_utility <- create_utility(utility_function="linear",
	parameters=c(lambda=1))

power_probability_weighting <-
	create_probability_weighting(probability_function="power",
	parameters=c(alpha=0.7, beta=1))

compareRAM(my_choices,
	branch_weighting_vector_list=branch_weighting_vector_list,
	probability_weighting_specification=power_probability_weighting,
	utility=my_utility,
	digits=4)
}
\references{
  Birnbaum, M. H. (2008). New paradoxes of risky decision
  making. Psychological Review, 115(2), 463-501.
}


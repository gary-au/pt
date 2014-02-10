\name{create_utility}
\alias{create_utility}
\title{create_utility}
\usage{
  create_utility(utility_function, parameters)
}
\arguments{
  \item{utility_function}{text, the utility function
  string}

  \item{parameters}{vector, a vector of parameters for the
  utility_function}
}
\description{
  This function creates an instance of a Utility class.
}
\details{
  The following functional forms are currently implemented:

  linear (requires 1 parameter)

  exponential (requires 2 parameters)

  power (requires 3 parameters)

  general_power (requires 3 parameters)

  general_linear (requires 2 parameters)
}
\examples{
# This example creates a power utility function.

tk_1992_utility <- create_utility(utility_function="power",
	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))

# This example creates a linear utility function.

my_linear_utility <- create_utility(utility_function="linear",
	parameters=c(lambda=1))
}
\references{
  Tversky, A., & Kahneman, D. (1992). Advances in prospect
  theory: Cumulative representation of uncertainty. Journal
  of Risk and Uncertainty, 5(4), 297-323.

  Wakker, P. P. (2010). Prospect theory: For risk and
  ambiguity. Cambridge, UK: Cambridge University Press.

  Stott, H. P. (2006). Cumulative prospect theory's
  functional menagerie. Journal of Risk and Uncertainty,
  32(2), 101-130.

  Birnbaum, M. H. (2008). New paradoxes of risky decision
  making. Psychological Review, 115(2), 463-501.
}


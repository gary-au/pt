\docType{package}
\name{pt-package}
\alias{pt}
\alias{pt-package}
\title{pt: An R package for Prospect Theory}
\description{
  A package for comparing prospect theory to other risky
  decision making theories.
}
\details{
  pt: An R package for Prospect Theory

  The pt package provides the ability to create, save and
  visualise choices. The results for different risky
  decision making theories can be calculated for these
  choice situations.

  The fastest way to get started is to either create
  choices from the command line using the create_choices
  function or load in choices from previously prepared
  external text files using the create_choices_from_file
  function.

  Once the choices are in R, it is possible to visualise
  them using drawChoices. Newly created choices can be
  saved to text files using save_choices.

  The predictions of various risky decision making theories
  can then be run on choices using the following functions:

  compareEV (for expected value)

  compareEU (expected utility)

  compareRDU (rank-dependent utility)

  comparePT (prospect theory)

  compareSWU (subjectively weighted utility)

  compareSWAU (subjectively weighted average utility)

  compareRAM (rank-affected multiplicative weights utility)

  compareTAX ((special) transfer of attention exchange
  utility)

  compareGDU ((lower) gains decomposition utility)

  comparePRT (prospective reference theory utility)

  Visualisation functions include:

  drawSimplex (draws the Marschak-Machina unit probability
  simplex)

  plot_pwf (draws a single probability weighting function)

  plot_pwf_family (draws families of probability weighting
  functions)

  plot_risk_premium (draws the risk premium)

  plot_utility_function (draws the utility function)
}
\author{
  Gary Au \email{gary.au@unimelb.edu.au}

  Maintainer: Gary Au \email{gary.au@unimelb.edu.au}
}
\keyword{package}


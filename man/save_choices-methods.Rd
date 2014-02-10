\name{save_choices}
\alias{save_choices}
\alias{save_choices,Choices-method}
\title{save_choices}
\arguments{
  \item{my_choices}{Choices, an instance of a Choices
  class.}

  \item{output_file}{text, the output file for saving
  my_choices.}

  \item{choice_id_header}{text, the column name for the
  choice_id field in the output file.}

  \item{gamble_id_header}{text, the column name for the
  gamble_id field in the output file.}

  \item{outcome_id_header}{text, the column name for the
  outcome_id field in the output file.}

  \item{probability_header}{text, the column name for the
  probability field in the output file.}

  \item{objective_consequence_header}{text, the column name
  for the objective_consequence field in the output file.}

  \item{DELIMITER}{text, the delimiter character used to
  separate the columns in the output file.}
}
\description{
  Saves a Choices object to an external text file.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and saves them to an external text file.

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

save_choices(my_choices,
	output_file="saved_choices.txt",
	choice_id_header="choice_id",
	gamble_id_header="gamble_id",
	outcome_id_header="outcome_id",
	probability_header="probability",
	objective_consequence_header="objective_consequence",
	DELIMITER="\\\\t")

# after finishing with the file, delete to keep the workspace tidy
rm(my_choices)
}


library("pt")

########################	
# These routines test saving and loading choices to/from external text files.
########################

choice_id_vector <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_id_vector <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequence_vector <- c(7, 7, 84, 90, 7, 10, 90, 90)
probability_string_vector <- c("0.1", "0.3", "0.3", "0.3", "0.1", "0.3", "0.3", "0.3")
my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector, 
	outcome_id_vector=outcome_id_vector, 
	objective_consequence_vector=objective_consequence_vector, 
	probability_string_vector=probability_string_vector)
my_choices

save_choices(my_choices, 
	output_file="test_save_choices.txt",
	choice_id_header="choice_id",
	gamble_id_header="gamble_id",
	outcome_id_header="outcome_id",
	probability_header="probability",
	objective_consequence_header="objective_consequence",
	DELIMITER="\t")

rm(my_choices)

my_choices <- create_choices_from_file(input_file="test_save_choices.txt",
	choice_id_header="choice_id",
	gamble_id_header="gamble_id",
	outcome_id_header="outcome_id",
	objective_consequence_header="objective_consequence",	
	probability_header="probability",
	DELIMITER="\t")
my_choices

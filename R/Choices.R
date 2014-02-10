########################
#
# Choices, a S4 class
#
########################	

#' The Choices class
#' 
#' The Choices class contains a vector of Gambles class objects.
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{choices}:}{Object of class \code{"vector"}, containing Gambles objects for decision makers to choose from.}
#'  }
#'
#' @note Choices contains a vector of Gambles class objects. Each vector element stores a decision making choice.  
#' @name Choices 
#' @rdname Choices
#' @exportClass Choices
#' @aliases Choices-class
setClass(Class = "Choices",
	representation = representation
	(
		choices = "vector"
	),
	# check for input consistency when creating new Choices objects using the "new" constructor
	validity = function(object)
	{
		# this is the Choices inspector
		
		for (index1 in 1: length(choices))
		{
			my_gambles_vector <- get_gambles(choices[index1])
			
			for (index2 in 1:length(my_gambles_vector))
			{
				my_gamble <- my_gambles_vector[index2]
				
				# make sure probabilities of all outcomes sum to <= 1	
				probability_sum = sum(sapply(my_gamble@outcomes, get_probability))

				if (probability_sum < 0 | probability_sum > 1)
				{
					stop(paste("gamble id: ", get_gamble_id(my_gamble), " sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
				}
				else
				{			
				}	
			}
								
		}

		return (TRUE)
	}
)



########################
# show choice
########################	

# override existing show function
setMethod(f = "show",
	signature = "Choices",
	definition = function(object)
	{
		df <- data.frame(row.names=NULL, stringsAsFactors=FALSE)	
		
		for (a in 1:length(object@choices))
		{	
			my_gambles_vector <- get_gambles(object@choices[[a]])
			
			for (n in 1:length(my_gambles_vector))
			{
				a_gamble <- my_gambles_vector[[n]]	
				
				for (m in 1:get_number_of_outcomes(my_gambles_vector[[n]]))
				{	
					an_outcome <- a_gamble@outcomes[[m]]
					
					gamble_id <- get_gamble_id(a_gamble)
					outcome_id <- get_outcome_id(an_outcome)
					probability_string <- get_probability_string(an_outcome)					
					objective_consequence <- format(get_objective_consequence(an_outcome), digits=16, scientific=FALSE)


					df <-rbind(df, data.frame("cid"=a, "gid"=gamble_id, "oid"=outcome_id, "pr"=probability_string, "oc"=objective_consequence))					
								
				}
			}
		}
		print (df)
	}
)



########################
# There are two ways to create an instance of a Choices class.
# The first way is by reading in choices data from an external text file. (create_choices_from_file)
# The second way is to create a choice using a series of four vectors. (create_choices)
########################	

#' @name create_choices_from_file
#' @title create_choices_from_file
#' @description Create an instance of a Choices class using data from an external text file.
#' @details This function is used to create a new instance of a Choices class from an external text file.
#' This file has at least 5 columns, delimited by the DELIMITER character string.
#' Each row of the file contains an individual outcome. The last line of the file needs to be
#' a blank row. An example input file describing the Allais constant ratio paradox looks like this,
#' with the DELIMITER being a "\\t".
#' 
#' choice_id	gamble_id	outcome_id	probability	objective_consequence
#' 
#' 1	1	1	1	3000
#' 
#' 1	2	1	0.8	4000
#' 
#' 1	2	2	0.2	0
#' 
#' 2	1	1	0.25	3000
#' 
#' 2	1	2	0.75	0
#' 
#' 2	2	1	0.2	4000
#' 
#' 2	2	2	0.8	0
#' 
#' 
#' ------
#' 
#' Note that the last line is a blank row.
#'  
#' @usage create_choices_from_file(input_file, choice_id_header, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
#' @param input_file text, the input_file.
#' @param choice_id_header text, the column name of the choice_id variable.
#' @param gamble_id_header text, the column name of the gamble_id variable.
#' @param outcome_id_header text, the column name of the outcome_id variable.
#' @param objective_consequence_header text, the column name of the objective_consequence variable.
#' @param probability_header, the column name of the probability_string variable.
#' @param DELIMITER text, the delimeter character separating the fields in the input file.
#' @examples
#' 
#' # This example loads up the choices for the Allais constant ratio paradox, which
#' # are available as text files in the pt package.
#' 
#' my_input_file <- system.file("external", "allais_constant_ratio_paradox.txt", package="pt")
#' 
#' my_choices <- create_choices_from_file(input_file=my_input_file, 
#' 	choice_id_header="choice_id", 
#' 	gamble_id_header="gamble_id", 
#' 	outcome_id_header="outcome_id", 
#' 	objective_consequence_header="objective_consequence", 
#' 	probability_header="probability", 
#' 	DELIMITER="\t")
#' 	
#' my_choices
#' 
#' @export
create_choices_from_file <- function(input_file, choice_id_header, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
{
	object <- new(Class = "Choices")
		
	object <- read_choices_data_file(object, input_file, choice_id_header, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)	
	
	return (object)

}

#' @name create_choices
#' @title create_choices
#' @description Create choices using five vectors.
#' @details This function creates a new instance of a Choices class. The inputs are five vectors, representing
#' the properties of each outcome.
#' @usage create_choices(choice_id_vector, gamble_id_vector, outcome_id_vector, objective_consequence_vector, probability_string_vector)
#' @param choice_id_vector vector, a vector containing the choice_id of each objective_consequence.
#' @param gamble_id_vector vector, a vector containing the gamble_id of each objective_consequence.
#' @param outcome_id_vector vector, a vector containing the outcome_id of each objective_consequence.
#' @param objective_consequence_vector vector, a vector containing the objective consequences.
#' @param probability_string_vector vector, a vector containing the probability_string of each objective consequence.
#' @examples
#' choice_id_vector <- c(1, 1, 1, 1, 1, 1, 1, 1)
#' 
#' gamble_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 4, 1, 2, 3, 4)
#' 
#' objective_consequence_vector <- c(7, 7, 84, 90, 7, 10, 90, 90)
#' 
#' probability_string_vector <- c("0.1", "0.3", "0.3", "0.3", "0.1", "0.3", "0.3", "0.3")
#' 
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' @export
create_choices <- function(choice_id_vector, gamble_id_vector, outcome_id_vector, objective_consequence_vector, probability_string_vector)
{

	# perform validity checks on the input
	
	# firstly the choice_id_vector and all other vectors must be the same length
	
	choice_id_vector_length <- length(choice_id_vector)
	gamble_id_vector_length <- length(gamble_id_vector)
	outcome_id_vector_length <- length(outcome_id_vector)	
	objective_consequence_vector_length <- length(objective_consequence_vector)
	probability_string_vector_length <- length(probability_string_vector)
	

	if (choice_id_vector_length != gamble_id_vector_length)
	{
		stop(paste("choice_id_vector has length: ", choice_id_vector_length, " and gamble_id_vector has length: ", gamble_id_vector_length, "\n"));			
	}
	
	if (choice_id_vector_length != outcome_id_vector_length)
	{
		stop(paste("choice_id_vector has length: ", choice_id_vector_length, " and outcome_id_vector has length: ", outcome_id_vector_length, "\n"));			
	}	

	if (choice_id_vector_length != objective_consequence_vector_length)
	{
		stop(paste("choice_id_vector has length: ", choice_id_vector_length, " and objective_consequence_vector has length: ", objective_consequence_vector_length, "\n"));			
	}
	
	if (choice_id_vector_length != probability_string_vector_length)
	{
		stop(paste("choice_id_vector has length: ", choice_id_vector_length, " and probability_string_vector has length: ", probability_string_vector_length, "\n"));			
	}	

	
	# create a gamble with no outcomes, but perform further checks on probability sums
	
	object <- new(Class = "Choices")

	cid_choice_vector <- c()
	gamid_gamble_vector <- c()
	outc_gamble_vector <- c()
	prob_string_gamble_vector <- c()
	obj_cons_gamble_vector <- c()
	old_choice_id <- choice_id_vector[1]

	for (index in 1:length(choice_id_vector))
	{	
		
		# create a new choice
		if (choice_id_vector[index] != old_choice_id)
		{	
			my_gambles <- create_gambles_v3(gamid_gamble_vector, outc_gamble_vector, obj_cons_gamble_vector, prob_string_gamble_vector)
			object@choices <- append(object@choices, my_gambles)
			old_choice_id <- choice_id_vector[index]

			cid_choice_vector <- c()
			gamid_gamble_vector <- c()
			outc_gamble_vector <- c()
			prob_string_gamble_vector <- c()
			obj_cons_gamble_vector <- c()
			
			choice_id <- choice_id_vector[index]
			gamble_id <- gamble_id_vector[index]
			outcome_id <- outcome_id_vector[index]	
			objective_consequence <- objective_consequence_vector[index]
			probability_string <- probability_string_vector[index]
			
			cid_choice_vector <- append(cid_choice_vector, choice_id)			
			gamid_gamble_vector <- append(gamid_gamble_vector, gamble_id)
			outc_gamble_vector <- append(outc_gamble_vector, outcome_id)	
			obj_cons_gamble_vector <- append(obj_cons_gamble_vector, objective_consequence)
			prob_string_gamble_vector <- append(prob_string_gamble_vector, probability_string)			
		}
		else
		{
			choice_id <- choice_id_vector[index]			
			gamble_id <- gamble_id_vector[index]
			outcome_id <- outcome_id_vector[index]	
			objective_consequence <- objective_consequence_vector[index]
			probability_string <- probability_string_vector[index]

			cid_choice_vector <- append(cid_choice_vector, choice_id)	
			gamid_gamble_vector <- append(gamid_gamble_vector, gamble_id)
			outc_gamble_vector <- append(outc_gamble_vector, outcome_id)	
			obj_cons_gamble_vector <- append(obj_cons_gamble_vector, objective_consequence)
			prob_string_gamble_vector <- append(prob_string_gamble_vector, probability_string)
			
			if (index == length(choice_id_vector))
			{
				my_gambles <- create_gambles_v3(gamid_gamble_vector, outc_gamble_vector, obj_cons_gamble_vector, prob_string_gamble_vector)
				object@choices <- append(object@choices, my_gambles)
			}
		}
	}

		
	return (object)	
}


########################
# read decision from external text file
########################	

# declare a custom function
setGeneric(name = "read_choices_data_file",
	def = function(object, ...)
	{
		standardGeneric("read_choices_data_file")
	}
)


setMethod(f = "read_choices_data_file",
	signature = "Choices",
	definition = function(object, input_file, choice_id_header, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
	{

		#read in outcomes from external file
		my_data_frame <- data.frame(read.table(file = input_file, header = TRUE, stringsAsFactors = FALSE, sep = DELIMITER))

		cid_choice_vector <- c()
		gamid_gamble_vector <- c()
		outc_gamble_vector <- c()
		prob_string_gamble_vector <- c()
		obj_cons_gamble_vector <- c()
		old_choice_id <- eval(parse(text = my_data_frame[1, choice_id_header]))
				
		for (index in 1:nrow(my_data_frame))
		{
			choice_id <- eval(parse(text = my_data_frame[index, choice_id_header]))						
			gamble_id <- eval(parse(text = my_data_frame[index, gamble_id_header]))			
			outcome_id <- eval(parse(text = my_data_frame[index, outcome_id_header]))	
			objective_consequence <- my_data_frame[index, objective_consequence_header]			
			probability_string <- as.character(format(my_data_frame[index, probability_header], digits = 16, scientific=FALSE))

			# create a new choice
			if (choice_id != old_choice_id)
			{	
				my_gambles <- create_gambles_v3(gamid_gamble_vector, outc_gamble_vector, obj_cons_gamble_vector, prob_string_gamble_vector)
				object@choices <- append(object@choices, my_gambles)
				old_choice_id <- choice_id
				
				cid_choice_vector <- c()
				gamid_gamble_vector <- c()
				outc_gamble_vector <- c()
				prob_string_gamble_vector <- c()
				obj_cons_gamble_vector <- c()
				
				cid_choice_vector <- append(cid_choice_vector, choice_id)
				gamid_gamble_vector <- append(gamid_gamble_vector, gamble_id)
				outc_gamble_vector <- append(outc_gamble_vector, outcome_id)	
				obj_cons_gamble_vector <- append(obj_cons_gamble_vector, objective_consequence)
				prob_string_gamble_vector <- append(prob_string_gamble_vector, probability_string)			
			}
			else
			{	
				cid_choice_vector <- append(cid_choice_vector, choice_id)				
				gamid_gamble_vector <- append(gamid_gamble_vector, gamble_id)
				outc_gamble_vector <- append(outc_gamble_vector, outcome_id)	
				obj_cons_gamble_vector <- append(obj_cons_gamble_vector, objective_consequence)
				prob_string_gamble_vector <- append(prob_string_gamble_vector, probability_string)
				
				if (index == nrow(my_data_frame))
				{
					my_gambles <- create_gambles_v3(gamid_gamble_vector, outc_gamble_vector, obj_cons_gamble_vector, prob_string_gamble_vector)
					object@choices <- append(object@choices, my_gambles)
				}
			}
		}
		
		return (object)
	}
)


########################
# support functions
########################	

########################
# get choices
########################	

# declare a custom function to return the choices
setGeneric(name = "get_choices",
	def = function(object, ...)
	{
		standardGeneric("get_choices")
	}
)

# provide implementation of custom function to return the choices
setMethod(f = "get_choices",
	signature = "Choices",
	definition = function(object)
	{	
		return (object@choices)
	}
)

########################
# computational functions for comparing theories
########################	

########################
# ev related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareEV",
	def = function(object, ...)
	{
		standardGeneric("compareEV")
	}
)

#' @rdname compareEV-methods
#' @aliases compareEV,Choices-method
#' @name compareEV
#' @title compareEV
#' @description Compare the expected value (EV) of choice gambles.
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the EV for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' compareEV(my_choices, digits=4)
#' 
#' @export
setMethod(f = "compareEV",
	signature = "Choices",
	definition = function(object, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_expected_value(object@choices[[n]], digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev")]
			summary_df <- rbind(summary_df, df)
		}		

		return (summary_df)

	}
)


########################
# EU related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareEU",
	def = function(object, ...)
	{
		standardGeneric("compareEU")
	}
)

#' @rdname compareEU-methods
#' @aliases compareEU,Choices-method
#' @name compareEU
#' @title compareEU
#' @description Compare the expected utility (EU) of choice gambles.
#' @param object Choices, an instance of a Choices class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' von Neumann, J., & Morgenstern, O. (1947). Theory of games and economic behavior (2nd ed.). Princeton, NJ: Princeton University Press.
#' 
#' Bernoulli, D. (1954). Exposition of a new theory on the measurement of risk. Econometrica, 22(1), 23-36.
#' 
#' Bernoulli, D. (1738). Specimen theoriae novae de mensura sortis. Commentarii Academiae Scientiarum Imperialis Petropolitanae, 5, 175-192.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the EU for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' my_utility <- create_utility(utility_function="power",
#' parameters=c(alpha=1.0, beta=1.0, lambda=1.0))
#' 
#' compareEU(my_choices, utility=my_utility, digits=4)
#' 
#' @export
setMethod(f = "compareEU",
	signature = "Choices",
	definition = function(object, utility, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_expected_utility(object@choices[[n]], utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "eu", "euce", "eurp")]
			summary_df <- rbind(summary_df, df)
		}

		return (summary_df)

	}
)


########################
# SWU related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareSWU",
	def = function(object, ...)
	{
		standardGeneric("compareSWU")
	}
)

#' @rdname compareSWU-methods
#' @aliases compareSWU,Choices-method
#' @name compareSWU
#' @title compareSWU
#' @description Compare choice gambles under Edwards' (1954, 1962) Subjective Weighted Utility (SWU).
#' @param object Choices, an instance of a Choices class.
#' @param probability_weighting_specification Probability_Weighting, an instance of a Probability_Weighting class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Edwards, W. (1954). The theory of decision making. Psychological Bulletin, 51(4), 380-417.
#' 
#' Edwards, W. (1962). Subjective probabilities inferred from decisions. Psychological Review, 69(2), 109-135.
#' 
#' Birnbaum, M. H. (1999). The paradoxes of Allais, stochastic dominance, and decision weights. In J. Shanteau, B. A. Mellers & D. A. Schum (Eds.), Decision science and technology: Reflections on the contributions of Ward Edwards (pp. 27-52). Norwell, MA: Kluwer Academic Publishers.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the SWU for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' my_utility <- create_utility(utility_function="power",
#' 	parameters=c(alpha=0.4, beta=0.4, lambda=1))
#'
#' my_pwf <-
#' create_probability_weighting(probability_function="linear_in_log_odds",
#' 	parameters=c(alpha=0.4, beta=0.4))
#'
#' compareSWU(my_choices,
#'	probability_weighting_specification=my_pwf,
#'	utility=my_utility,
#'	digits=4)
#' 
#' @export
setMethod(f = "compareSWU",
	signature = "Choices",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_swu(object@choices[[n]], probability_weighting_specification, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "swu", "swuce", "swurp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)
	}
)

########################
# swau related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareSWAU",
	def = function(object, ...)
	{
		standardGeneric("compareSWAU")
	}
)

#' @rdname compareSWAU-methods
#' @aliases compareSWAU,Choices-method
#' @name compareSWAU
#' @title compareSWAU
#' @description Compare choices under Subjectively weighted average utility (SWAU).
#' @param object Choices, an instance of a Choices class.
#' @param probability_weighting_specification Probability_Weighting, an instance of a Probability_Weighting class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Karmarkar, U. S. (1978). Subjectively weighted utility: A descriptive extension of the expected utility model. Organizational Behavior & Human Performance, 21(1), 61-72.
#' 
#' Karmarkar, U. S. (1979). Subjectively weighted utility and the Allais Paradox. Organizational Behavior & Human Performance, 24(1), 67-72.
#' 
#' Viscusi, W. K. (1989). Prospective reference theory: Toward an explanation of the paradoxes. Journal of Risk and Uncertainty, 2(3), 235-263.
#' 
#' Lattimore, P. K., Baker, J. R., & Witte, A. D. (1992). The influence of probability on risky choice: A parametric examination. Journal of Economic Behavior and Organization, 17(3), 377-400.
#' 
#' Birnbaum, M. H. (1999). The paradoxes of Allais, stochastic dominance, and decision weights. In J. Shanteau, B. A. Mellers & D. A. Schum (Eds.), Decision science and technology: Reflections on the contributions of Ward Edwards (pp. 27-52). Norwell, MA: Kluwer Academic Publishers.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the SWAU for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' my_utility <- create_utility(utility_function="power",
#' 	parameters=c(alpha=0.4, beta=0.4, lambda=1))
#'
#' my_pwf <-
#' create_probability_weighting(probability_function="linear_in_log_odds",
#' 	parameters=c(alpha=0.4, beta=0.4))
#'
#' compareSWAU(my_choices,
#'	probability_weighting_specification=my_pwf,
#'	utility=my_utility,
#'	digits=4)
#'	
#' @export
setMethod(f = "compareSWAU",
	signature = "Choices",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	

		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_swau(object@choices[[n]], probability_weighting_specification, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "swau", "swauce", "swaurp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)
	}
)


########################
# RDU related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareRDU",
	def = function(object, ...)
	{
		standardGeneric("compareRDU")
	}
)

#' @rdname compareRDU-methods
#' @aliases compareRDU,Choices-method
#' @name compareRDU
#' @title compareRDU
#' @description Compare choice gambles under Quiggin's (1993) Rank-dependent utility (RDU).
#' @param object Choices, an instance of a Choices class.
#' @param probability_weighting_specification Probability_Weighting, an instance of a Probability_Weighting class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Quiggin, J. (1982). A theory of anticipated utility. Journal of Economic Behavior & Organization, 3(4), 323-343.
#' 
#' Quiggin, J. (1985). Subjective utility, anticipated utility, and the Allais paradox. Organizational Behavior and Human Decision Processes, 35(1), 94-101.
#' 
#' Quiggin, J. (1993). Generalized expected utility theory: The rank-dependent model. Boston, MA: Kluwer Academic Publishers.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the RDU for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' tk_1992_utility <- create_utility(utility_function="power",
#' 	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))
#' 	
#' tk_1992_positive_probability_weighting <-
#'	create_probability_weighting(probability_function=
#'	"Tversky_Kahneman_1992",
#'	parameters=c(alpha=0.61))
#'	
#' compareRDU(my_choices,
#'	probability_weighting_specification=
#'	tk_1992_positive_probability_weighting,
#'	utility=tk_1992_utility,
#'	digits=4)
#'	
#' @export
setMethod(f = "compareRDU",
	signature = "Choices",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	
		
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			results_list <- compare_gambles_under_rdu(object@choices[[n]], probability_weighting_specification, utility, digits)
			
			for (m in 1:length(results_list))
			{
				df_list <- results_list[[m]]
				df <- df_list$summary
				df
				df$cid <- n
				df <- df[c("cid", "gid", "ev", "rdu", "rduce", "rdurp")]	
				
				summary_df <- rbind(summary_df, df)
			}
		}
		
		return (summary_df)

	}
)

########################
# PT related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "comparePT",
	def = function(object, ...)
	{
		standardGeneric("comparePT")
	}
)

#' @rdname comparePT-methods
#' @aliases comparePT,Choices-method
#' @name comparePT
#' @title comparePT
#' @description Compare choice gambles under Tversky and Kahneman's (1992) (Cumulative) prospect theory (PT).
#' @param object Choices, an instance of a Choices class.
#' @param probability_weighting_specification_for_positive_outcomes Probability_Weighting, an instance of a Probability_Weighting class.
#' @param probability_weighting_specification_for_negative_outcomes Probability_Weighting, an instance of a Probability_Weighting class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#'  
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the PT for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' tk_1992_utility <- create_utility(utility_function="power",
#'	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))
#'	
#' tk_1992_positive_probability_weighting <-
#'	create_probability_weighting(probability_function=
#'	"Tversky_Kahneman_1992",
#'	parameters=c(alpha=0.61))
#'	
#' tk_1992_negative_probability_weighting <-
#'	create_probability_weighting(probability_function=
#'	"Tversky_Kahneman_1992",
#'	parameters=c(alpha=0.69))
#'	
#' comparePT(my_choices,
#'	probability_weighting_specification_for_positive_outcomes=
#'	tk_1992_positive_probability_weighting,
#'	probability_weighting_specification_for_negative_outcomes=
#'	tk_1992_negative_probability_weighting,
#'	utility=tk_1992_utility, 
#'	digits=4)
#'	
#' @export
setMethod(f = "comparePT",
	signature = "Choices",
	definition = function(object, 
		probability_weighting_specification_for_positive_outcomes, 
		probability_weighting_specification_for_negative_outcomes, 
		utility, 
		digits)
	{	
	
		summary_df <- data.frame(row.names=NULL, stringsAsFactors=FALSE)
		
		for (n in 1:length(object@choices))
		{	
			results_list <- compare_gambles_under_pt(object@choices[[n]], probability_weighting_specification_for_positive_outcomes, probability_weighting_specification_for_negative_outcomes, utility, digits)
			
			for (m in 1:length(results_list))
			{
				df_list <- results_list[[m]]
				df <- df_list$summary
				df$cid <- n
				df <- df[c("cid", "gid", "ev", "pt", "ptce", "ptrp")]	
				
				summary_df <- rbind(summary_df, df)
			}

		}

		return (summary_df)

	}
)


########################
# TAX related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareTAX",
	def = function(object, ...)
	{
		standardGeneric("compareTAX")
	}
)

#' @rdname compareTAX-methods
#' @aliases compareTAX,Choices-method
#' @name compareTAX
#' @title compareTAX
#' @description Compare choice gambles under Birnbaum's (2008) configural weight (special) TAX theory.
#' @param object Choices, an instance of a Choices class.
#' @param probability_weighting_specification Probability_Weighting, an instance of a Probability_Weighting class.
#' @param utility Utility, an instance of a Utility class.
#' @param delta numeric, the delta parameter in Birnbaum's TAX theory.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#'
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the TAX for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' my_utility <- create_utility(utility_function="linear",
#'	parameters=c(lambda=1))
#'	
#' power_probability_weighting <-
#'	create_probability_weighting(probability_function="power",
#'	parameters=c(alpha=0.7, beta=1))
#'	
#' compareTAX(my_choices,
#'	probability_weighting_specification=power_probability_weighting,
#'	utility=my_utility, 
#'	delta=-1, 
#'	digits=4)
#'	
#' @export
setMethod(f = "compareTAX",
	signature = "Choices",
	definition = function(object, probability_weighting_specification, utility, delta, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_tax(object@choices[[n]], probability_weighting_specification, utility, delta, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "tax", "taxce", "taxrp")]
			summary_df <- rbind(summary_df, df)
		}

		return (summary_df)

	}
)

########################
# RAM related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareRAM",
	def = function(object, ...)
	{
		standardGeneric("compareRAM")
	}
)

#' @rdname compareRAM-methods
#' @aliases compareRAM,Choices-method
#' @name compareRAM
#' @title compareRAM
#' @description Compare choice gambles under Birnbaum's (2008) configural weight RAM theory.
#' @param object Choices, an instance of a Choices class.
#' @param branch_weighting_vector_list list, a list of branch weighting vectors.
#' @param probability_weighting_specification Probability_Weighting, an instance of a Probability_Weighting class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#'
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the RAM for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' # note that the maximum number of outcomes in the gambles is 3,
#' # so branch weights for 3 outcomes need to be provided. 
#' 
#' branch_weighting_vector_list <- list(c(1),
#'	c(0.3738, 0.6262),
#'	c(0.16, 0.33, 0.51))
#'	
#' my_utility <- create_utility(utility_function="linear",
#'	parameters=c(lambda=1))
#'	
#' power_probability_weighting <-
#'	create_probability_weighting(probability_function="power",
#'	parameters=c(alpha=0.7, beta=1))
#'	
#' compareRAM(my_choices,
#'	branch_weighting_vector_list=branch_weighting_vector_list,
#'	probability_weighting_specification=power_probability_weighting,
#'	utility=my_utility, 
#'	digits=4)
#' 
#' @export
setMethod(f = "compareRAM",
	signature = "Choices",
	definition = function(object, branch_weighting_vector_list, probability_weighting_specification, utility, digits)
	{	
		
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_ram(object@choices[[n]], branch_weighting_vector_list, probability_weighting_specification, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "ramu", "ramuce", "ramurp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)

	}
)

########################
# GDU related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "compareGDU",
	def = function(object, ...)
	{
		standardGeneric("compareGDU")
	}
)

#' @rdname compareGDU-methods
#' @aliases compareGDU,Choices-method
#' @name compareGDU
#' @title compareGDU
#' @description Compare choice gambles under Luce's (2000) (Lower) Gains-decompositions utility (GDU) theory.
#' @param object Choices, an instance of a Choices class.
#' @param probability_weighting_specification Probability_Weighting, an instance of a Probability_Weighting class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Luce, R. D. (2000). Utility of gains and losses: Measurement-theoretical and experimental approaches. Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the GDU for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' my_pwf <-
#'	create_probability_weighting(probability_function="compound_invariance",
#'	parameters=c(alpha=0.542, beta=1.382))
#'	
#' my_utility <- create_utility(utility_function="power",
#'	parameters=c(alpha=1, beta=1, lambda=1))
#'	
#' compareGDU(my_choices,
#'	probability_weighting_specification=my_pwf,
#'	utility=my_utility,
#'	digits=4)
#'
#' @export
setMethod(f = "compareGDU",
	signature = "Choices",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_gdu(object@choices[[n]], probability_weighting_specification, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "gdu", "gduce", "gdurp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)

	}
)


########################
# PRT related functions
########################	

# declare a custom function to compare choices
setGeneric(name = "comparePRT",
	def = function(object, ...)
	{
		standardGeneric("comparePRT")
	}
)

#' @rdname comparePRT-methods
#' @aliases comparePRT,Choices-method
#' @name comparePRT
#' @title comparePRT
#' @description Compare choice gambles under Viscusi's (1989) Prospective reference theory (PRT).
#' @param object Choices, an instance of a Choices class.
#' @param utility Utility, an instance of a Utility class.
#' @param gamma numeric, the gamma parameter in Viscusi's theory.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Viscusi, W. K. (1989). Prospective reference theory: Toward an explanation of the paradoxes. Journal of Risk and Uncertainty, 2(3), 235-263.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the PRT for each gamble in the choices.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' my_utility <- create_utility(utility_function="power",
#'	parameters=c(alpha=0.631, beta=0.631, lambda=1))
#'	
#' gamma <- 0.676
#'	
#' comparePRT(my_choices,
#'	utility=my_utility,
#'	gamma=gamma,
#'	digits=4)
#'
#' @export
setMethod(f = "comparePRT",
	signature = "Choices",
	definition = function(object, utility, gamma, digits)
	{	
		
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_prt(object@choices[[n]], utility, gamma, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "prtu", "prtuce", "prturp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)

	}
)

########################	
# I/O wrappers
########################	

# declare a custom function to save_choices
setGeneric(name = "save_choices",
	def = function(object, ...)
	{
		standardGeneric("save_choices")
	}
)

#' @rdname save_choices-methods
#' @aliases save_choices,Choices-method
#' @name save_choices
#' @title save_choices
#' @description Saves a Choices object to an external text file.
#' @param my_choices Choices, an instance of a Choices class.
#' @param output_file text, the output file for saving my_choices.
#' @param choice_id_header text, the column name for the choice_id field in the output file.
#' @param gamble_id_header text, the column name for the gamble_id field in the output file.
#' @param outcome_id_header text, the column name for the outcome_id field in the output file.
#' @param probability_header text, the column name for the probability field in the output file.
#' @param objective_consequence_header text, the column name for the objective_consequence field in the output file.
#' @param DELIMITER text, the delimiter character used to separate the columns in the output file.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and saves them to an external text file.
#' 
#' choice_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_id_vector <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_id_vector <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequence_vector <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_string_vector <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- create_choices(choice_id_vector=choice_id_vector,
#' 	gamble_id_vector=gamble_id_vector, 
#' 	outcome_id_vector=outcome_id_vector, 
#' 	objective_consequence_vector=objective_consequence_vector, 
#' 	probability_string_vector=probability_string_vector)
#' 	
#' my_choices
#' 
#' save_choices(my_choices, 
#'	output_file="saved_choices.txt",
#'	choice_id_header="choice_id",
#'	gamble_id_header="gamble_id",
#'	outcome_id_header="outcome_id",
#'	probability_header="probability",
#'	objective_consequence_header="objective_consequence",
#'	DELIMITER="\\t")
#' 
#' # after finishing with the file, delete to keep the workspace tidy
#' rm(my_choices)
#' 
#' @export
setMethod(f = "save_choices",
	signature = "Choices",
	definition = function(object, 
		output_file, 
		choice_id_header, 
		gamble_id_header, 
		outcome_id_header, 
		probability_header, 
		objective_consequence_header, 
		DELIMITER)
	{	
		df <- data.frame(row.names = NULL, stringsAsFactors = FALSE)
		
		row_index <- 1
	
		# extract class data into data.frame		
		for (i in 1:length(object@choices))
		{
			choice_id <- i
			my_gambles <- object@choices[[i]]
			
			my_gambles_vector <- get_gambles(my_gambles)
					
			for (j in 1:length(my_gambles_vector))
			{
				my_gamble <- my_gambles_vector[[j]]
				gamble_id <- get_gamble_id(my_gamble)
				
				outcome_vector <- get_outcomes(my_gamble)
				
				for (k in 1:length(outcome_vector))
				{
					outcome_id <- get_outcome_id(outcome_vector[[k]])
					objective_consequence <- get_objective_consequence(outcome_vector[[k]])
					probability_string <- get_probability_string(outcome_vector[[k]])	
					
					#[rows, cols]
					df[row_index, 1] <- choice_id
					df[row_index, 2] <- gamble_id
					df[row_index, 3] <- outcome_id
					df[row_index, 4] <- probability_string		
					df[row_index, 5] <- objective_consequence
					
					row_index <- row_index + 1
				}		
			}

		}
		
		names(df) <- c(choice_id_header, gamble_id_header, outcome_id_header, probability_header, objective_consequence_header)
			
		write.table(df,
			file = output_file,
			append = FALSE,
			quote = FALSE,
			sep = DELIMITER,
            	row.names = FALSE,
            	col.names = TRUE)
		
		return (invisible())
	}
)

#' @name create_vsd_choices
#' @title create_vsd_choices
#' @description Create choice situations that can elicit violations of (first-order) stochastic dominance in decision makers, using Birbaum's (1997) recipe.
#' @details Given a binary gamble G0, this function creates a pair of three outcome gambles G+ and G-
#' and a pair of four outcome gambles GS+, GS- that can elicit vsd behaviour in decision makers. e.g.
#' 
#' G0 = (96, 0.9; 12, 0.1)
#' 
#' G+ = (12, 0.05; 14, 0.05; 96, 0.9) and G- = (12, 0.1; 90, 0.05; 96, 0.85)
#' 
#' where G+ dominates G0 and G- is dominated by G0.
#' 
#' GS+ = (12, 0.05; 14, 0.05; 96, 0.05; 96, 0.85) and GS- = (12, 0.05; 12, 0.05; 90, 0.05; 96, 0.85)
#' 
#' 
#' @examples
#' my_choices_list <- create_vsd_choices(x=12, y=96, p="0.1", q="0.9", x_plus=14, y_minus=90, r="0.05")
#' 
#' original_choice <- my_choices_list[[1]]
#' 
#' original_choice
#' 
#' pair_of_three_outcome_choices <- my_choices_list[[2]]
#' 
#' pair_of_three_outcome_choices
#' 
#' pair_of_four_outcome_choices <- my_choices_list[[3]]
#' 
#' pair_of_four_outcome_choices
#' 
#' @references
#' Figure 5, p. 475 from Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' Birnbaum, M. H. (1997). Violations of monotonicity in judgment and decision making. In A. A. J. Marley (Ed.), Choice, decision, and measurement: Essays in honor of R. Duncan Luce (pp. 73-100). Mahwah, NJ: Erlbaum.
#' 
#' @usage
#' create_vsd_choices(x, y, p, q, x_plus, y_minus, r)
#' @param x numeric, x is one of the objective consequences in the original binary gamble G0.
#' @param y numeric, y is the other objective consequences in the original binary gamble G0.
#' @param p text, p is a probability string associated with the objective consequence x.
#' @param q text, q is a probability string associated with the objective consequence y.
#' @param x_plus numeric, x_plus
#' @param y_minus numeric, y_minus
#' @param r numeric, r the g_minus probability offset
#' @export
create_vsd_choices <- function(x, y, p, q, x_plus, y_minus, r)
{
	if ((x < 0) || (y < 0))
	{
		cat("y > x > 0\n")
		return (invisible())
	}
	else if (x > y)
	{
		cat("y > x > 0\n")
		return (invisible())		
	}
	
	np <- as.numeric(p)
	nq <- as.numeric(q)
	nr <- as.numeric(r)
	
	#g0
	choice_id_vector <- c(1, 1)	
	gamble_id_vector <- c(1, 1)
	outcome_id_vector <- c(1, 2)
	objective_consequence_vector <- c(x, y)
	probability_string_vector <- c(as.character(np),
		as.character(nq))		
	g0 <- create_choices(choice_id_vector=choice_id_vector,
		gamble_id_vector=gamble_id_vector, 		
		outcome_id_vector=outcome_id_vector, 
		objective_consequence_vector=objective_consequence_vector, 
		probability_string_vector=probability_string_vector)	
		
	#gplusminus
	choice_id_vector <- c(1, 1, 1, 1, 1, 1)
	gamble_id_vector <- c(1, 1, 1, 2, 2, 2)
	outcome_id_vector <- c(1, 2, 3, 1, 2, 3)
	objective_consequence_vector <- c(x, x_plus, y, 
		x, y_minus, y)
	probability_string_vector <- c(		
		as.character(eval(parse(text="np-nr"))), 
		as.character(eval(parse(text="np-nr"))), 	
		as.character(nq), 
		
		as.character(np), 
		as.character(eval(parse(text="np-nr"))),		
		as.character(eval(parse(text="nq-nr"))))
	
	gplusminus <- create_choices(choice_id_vector=choice_id_vector,
		gamble_id_vector=gamble_id_vector, 
		outcome_id_vector=outcome_id_vector, 
		objective_consequence_vector=objective_consequence_vector, 
		probability_string_vector=probability_string_vector)		
	
	#gsplusminus
	choice_id_vector <- c(1, 1, 1, 1, 1, 1, 1, 1)
	gamble_id_vector <- c(1, 1, 1, 1, 2, 2, 2, 2)
	outcome_id_vector <- c(1, 2, 3, 4, 1, 2, 3, 4)
	objective_consequence_vector <- c(x, x_plus, y, y,
		x, x, y_minus, y)
		
	probability_string_vector <- c(		
		as.character(eval(parse(text="np-nr"))), 
		as.character(eval(parse(text="np-nr"))),
		as.character(eval(parse(text="np-nr"))), 		
		as.character(eval(parse(text="nq-nr"))), 		
	
		as.character(eval(parse(text="np-nr"))), 
		as.character(eval(parse(text="np-nr"))),
		as.character(eval(parse(text="np-nr"))), 		
		as.character(eval(parse(text="nq-nr"))))
	
	gsplusminus <- create_choices(choice_id_vector=choice_id_vector,
		gamble_id_vector=gamble_id_vector, 
		outcome_id_vector=outcome_id_vector, 
		objective_consequence_vector=objective_consequence_vector, 
		probability_string_vector=probability_string_vector)
	
	
	my_list <- list("g0"=g0, "gplusminus"=gplusminus, "gsplusminus"=gsplusminus)

	return (my_list)	
}
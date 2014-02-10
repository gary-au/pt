########################
#
# Gamble, a S4 class
#
########################	

#' The Gamble class
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{gamble_id}:}{Object of class \code{"numeric"}, containing the unique id of the gamble.}
#'    \item{\code{outcomes}:}{Object of class \code{"vector"}, containing the possible outcomes of the gamble.}
#'  }
#'
#' @note The Gamble class represents a gamble, i.e. a choice option containing various outcomes whose probabilities need to sum to unity.
#' @name Gamble 
#' @rdname Gamble
#' @exportClass Gamble
#' @aliases Gamble-class
setClass(Class = "Gamble",
	representation = representation
	(
		gamble_id = "numeric",
		outcomes = "vector",
	
		expected_value = "numeric",
		expected_utility = "numeric",
		rdu_value = "numeric",	
		pt_value = "numeric",
			
		certainty_equivalent = "numeric",
		risk_premium = "numeric"		
	),
	# check for input consistency when creating new Outcome objects using "new" constructor
	validity = function(object)
	{
		cat("~~~ Gamble: inspector ~~~ \n")
		
		# make sure probabilities of all outcomes sum to <= 1	
		probability_sum = sum(sapply(object@outcomes, get_probability))
# 		print(probability_sum)
		
# 		TOLERANCE <- 0.000000000000001
# 		
# 		if (abs(1.0 - probability_sum) < TOLERANCE)
# 		{
		if (probability_sum < 0 | probability_sum > 1)
		{
			stop(paste("sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
		}
		else
		{			
		}			
		return (TRUE)
	}
)

########################	
# display functions
########################	


setMethod(f = "show",
	signature = "Gamble",
	definition = function(object)
	{
		df <- data.frame(row.names=NULL, stringsAsFactors=FALSE)
				
		for (n in 1:(length(object@outcomes)))
		{	
			gamble_id <- object@gamble_id
			outcome_id <- get_outcome_id(object@outcomes[[n]])
			objective_consequence <- get_objective_consequence(object@outcomes[[n]])			
			probability_string <- get_probability_string(object@outcomes[[n]])			
			df <-rbind(df, data.frame("gid"=gamble_id, "oid"=outcome_id, "oc"=objective_consequence, "pr"=probability_string))		
		}
		
		print (df)
	}
)

########################	
# stand alone consistency checks
########################	

# declare a custom function
setGeneric(name = "run_probability_sum_check",
	def = function(object)
	{
		standardGeneric("run_probability_sum_check")
	}
)


setMethod(f = "run_probability_sum_check",
	signature = "Gamble",
	definition = function(object)
	{
		
		probability_sum = sum(sapply(object@outcomes, get_probability))
	
		if (probability_sum < 0 | probability_sum > 1)
		{
			cat(paste("sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));

		}
		else
		{
			cat("probabilities sum to unity.\n")		
		}
		return (invisible())
	}
)

########################	
# I/O wrappers
########################	

# declare a custom function to save_gamble
setGeneric(name = "save_gamble",
	def = function(object, ...)
	{
		standardGeneric("save_gamble")
	}
)


setMethod(f = "save_gamble",
	signature = "Gamble",
	definition = function(object, output_file, gamble_id_header, outcome_id_header, probability_header, objective_consequence_header, DELIMITER)
	{	
		df <- data.frame(row.names = NULL, stringsAsFactors = FALSE)
	
		# extract class data into data.frame		
		for (i in 1:length(object@outcomes))
		{
			gamble_id <- get_gamble_id(object)
			outcome_id <- get_outcome_id(object@outcomes[[i]])
			objective_consequence <- get_objective_consequence(object@outcomes[[i]])
			probability_string <- get_probability_string(object@outcomes[[i]])

			#[rows, cols]
			df[i, 1] <- gamble_id
			df[i, 2] <- outcome_id
			df[i, 3] <- probability_string
			df[i, 4] <- objective_consequence
		}
		
		names(df) <- c(gamble_id_header, outcome_id_header, probability_header, objective_consequence_header)
		
		# write.table		
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

########################	
# create an empty gamble with no outcomes
########################	


create_gamble <- function()
{

	new(Class = "Gamble")
}


create_gamble_v3 <- function(gamble_id, outcome_id_vector, objective_consequence_vector, probability_string_vector)
{

	# perform validity checks on the input
	
	# firstly the objective_consequence_vector and probability_string_vector must be the same length
	
	objective_consequence_vector_length <- length(objective_consequence_vector)
	probability_string_vector_length <- length(probability_string_vector)
	
	if (objective_consequence_vector_length != probability_string_vector_length)
	{
		stop(paste("objective_consequence_vector has length: ", objective_consequence_vector_length, " and probability_string_vector has length: ", probability_string_vector_length, "\n"));			
	}
	
	# secondly the probabilities in probability_vector must sum to 1

	probability_vector <- unlist(lapply(probability_string_vector, function(probability_string_vector) eval(parse(text=probability_string_vector))))
	
	probability_sum <- sum(probability_vector)
	
	if (probability_sum < 0 | probability_sum > 1)
	{
		stop(paste("sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
	}	
	
	# if all checks pass, create a gamble with no outcomes
	
	object <- new(Class = "Gamble")
	
	object@gamble_id <- gamble_id
	
	for (n in 1:length(objective_consequence_vector))
	{
		outcome_id <- outcome_id_vector[n]
		
		probability_string <- probability_string_vector[n]				
		objective_consequence <- objective_consequence_vector[n]

		my_outcome <- create_outcome(outcome_id = outcome_id, position = n, objective_consequence = objective_consequence, probability_string = probability_string, rank = 0, decision_weight = 0.0, subjective_value = 0.0, w = 0.0)

		object@outcomes <- append(object@outcomes, my_outcome)

	}
		
	return (object)	
}





########################
# gamble_id related functions
########################	

# declare a custom function to retrieve gamble id
setGeneric(name = "get_gamble_id",
	def = function(object)
	{
		standardGeneric("get_gamble_id")
	}
)

# provide implementation of custom function to retrieve gamble id
setMethod(f = "get_gamble_id",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@gamble_id)
	}
)

# declare a custom function to assign gamble id
setGeneric(name = "set_gamble_id<-",
	def = function(object, value)
	{
		standardGeneric("set_gamble_id<-")
	}
)

# provide implementation of custom function to assign gamble id	
setReplaceMethod(f = "set_gamble_id",
	signature = "Gamble",
	definition = function(object, value)
	{
		object@gamble_id <- value
		return (object)
	}
)


########################
# get number of outcomes
########################	

# declare a custom function to retrieve number of outcomes
setGeneric(name = "get_number_of_outcomes",
	def = function(object)
	{
		standardGeneric("get_number_of_outcomes")
	}
)

# provide implementation of custom function to retrieve number of outcomes
setMethod(f = "get_number_of_outcomes",
	signature = "Gamble",
	definition = function(object)
	{
		return (length(object@outcomes))
	}
)

########################
# get outcomes
########################	

# declare a custom function to retrieve outcomes
setGeneric(name = "get_outcomes",
	def = function(object)
	{
		standardGeneric("get_outcomes")
	}
)

# provide implementation of custom function to retrieve outcomes
setMethod(f = "get_outcomes",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@outcomes)
	}
)


########################
# sort outcomes
########################	

# declare a custom function to sort outcomes from highest to lowest objective_consequence
setGeneric(name = "sort_outcomes",
	def = function(object, ...)
	{
		standardGeneric("sort_outcomes")
	}
)

# provide implementation of custom function to sort outcomes from highest to lowest objective_consequence
setMethod(f = "sort_outcomes",
	signature = "Gamble",
	definition = function(object, high_to_low_flag)
	{
	
		number_of_outcomes <- length(object@outcomes)
		
		if (number_of_outcomes > 1)
		{		
			my_unsorted_objective_consequences <- sapply(object@outcomes, get_objective_consequence)
			if (high_to_low_flag == TRUE)
			{
				my_sorted_objective_consequences <- sort(my_unsorted_objective_consequences, decreasing = TRUE, index.return = TRUE)				
			}
			else
			{
				my_sorted_objective_consequences <- sort(my_unsorted_objective_consequences, decreasing = FALSE, index.return = TRUE)							
			}
			
			store <- c(object@outcomes[my_sorted_objective_consequences$ix[1]])
			for (n in 2:length(my_sorted_objective_consequences$ix))
			{
				store <- c(store, object@outcomes[my_sorted_objective_consequences$ix[n]])
			}
			
			for (n in 1:length(store))
			{
				set_rank(store[[n]]) <- n
			}
			
			object@outcomes <- store
		}
		
		return (object)
	}
)


########################
# get probability of an outcome
########################	

# declare a custom function to retrieve probability_string of an outcome
setGeneric(name = "get_gamble_outcome_probability_string",
	def = function(object, ...)
	{
		standardGeneric("get_gamble_outcome_probability_string")
	}
)

# provide implementation of custom function to retrieve probability_string of an outcome
setMethod(f = "get_gamble_outcome_probability_string",
	signature = "Gamble",
	definition = function(object, index)
	{	
		return (get_probability_string(object@outcomes[[index]]))
	}
)

# declare a custom function to retrieve probability of an outcome
setGeneric(name = "get_gamble_outcome_probability",
	def = function(object, ...)
	{
		standardGeneric("get_gamble_outcome_probability")
	}
)

# provide implementation of custom function to retrieve probability of an outcome
setMethod(f = "get_gamble_outcome_probability",
	signature = "Gamble",
	definition = function(object, index)
	{	
		return (get_probability(object@outcomes[[index]]))
	}
)


# declare a custom function
setGeneric(name = "sum_outcome_probabilities",
	def = function(object, ...)
	{
		standardGeneric("sum_outcome_probabilities")
	}
)

# provide implementation of custom function
setMethod(f = "sum_outcome_probabilities",
	signature = "Gamble",
	definition = function(object, start_outcome_index, end_outcome_index)
	{
	
		sum_of_probabilities <- 0.0
	
		if (end_outcome_index  >= start_outcome_index)
		{
			sum_of_probabilities <- sum(sapply(object@outcomes[start_outcome_index:end_outcome_index], get_probability))
		}
		return (sum_of_probabilities)	
	}
)


########################
# set_g_objective_consequence
########################	

# declare a custom function to set_g_objective_consequence	
setGeneric(name = "set_g_objective_consequence<-",
	def = function(object, outcome_id, value)
	{
		standardGeneric("set_g_objective_consequence<-")
	}
)


setReplaceMethod(f = "set_g_objective_consequence",
	signature = "Gamble",
	definition = function(object, outcome_id, value)
	{
		
		# need to search for the index slot with the relevant outcome_id
		outcome_index <- outcome_id
		
		for (n in 1:length(object@outcomes))
		{
			if (get_outcome_id(object@outcomes[[n]]) == outcome_id)
			{
				outcome_index <- n
				break
			}
		}
		
		
		set_objective_consequence(object@outcomes[[outcome_index]]) <- value
		return (object)
	}
)

########################
# set_g_ocv2
########################	

# declare a custom function to set_g_ocv2
setGeneric(name = "set_g_ocv2<-",
	def = function(object, outcome_ids, value)
	{
		standardGeneric("set_g_ocv2<-")
	}
)


setReplaceMethod(f = "set_g_ocv2",
	signature = "Gamble",
	definition = function(object, outcome_ids, value)
	{		
		for (m in 1:length(outcome_ids))
		{
			for (n in 1:length(object@outcomes))
			{
				if (get_outcome_id(object@outcomes[[n]]) == outcome_ids[m])
				{
					set_objective_consequence(object@outcomes[[n]]) <- value[m]
					break
				}
			}			
		}
		return (object)
	}
)


########################
# set_g_objective_consequences<-
########################	

# declare a custom function to set_g_objective_consequences	
setGeneric(name = "set_g_objective_consequences<-",
	def = function(object, value)
	{
		standardGeneric("set_g_objective_consequences<-")
	}
)


setReplaceMethod(f = "set_g_objective_consequences",
	signature = "Gamble",
	definition = function(object, value)
	{	
		for (index in 1:length(value))
		{
			set_objective_consequence(object@outcomes[[index]]) <- value[index]		
		}
		return (object)
	}
)

########################
# set_g_probabilities<-
########################	

# declare a custom function to set_g_probabilities	
setGeneric(name = "set_g_probabilities<-",
	def = function(object, value)
	{
		standardGeneric("set_g_probabilities<-")
	}
)


setReplaceMethod(f = "set_g_probabilities",
	signature = "Gamble",
	definition = function(object, value)
	{	
		
		probability_vector <- unlist(lapply(value, function(value) eval(parse(text=value))))
		
		probability_sum = sum(probability_vector)
		
		if (probability_sum < 0 | probability_sum > 1)
		{
			cat(paste("sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));

		}
		else
		{
			for (index in 1:length(value))
			{
				set_probability_string(object@outcomes[[index]]) <- value[index]					
			}	

		}
		return (object)
	}
)

########################
# get objective_consequence of an outcome
########################	

# declare a custom function to retrieve objective_consequence of an outcome
setGeneric(name = "get_gamble_outcome_objective_consequence",
	def = function(object, ...)
	{
		standardGeneric("get_gamble_outcome_objective_consequence")
	}
)

# provide implementation of custom function to retrieve objective_consequence of an outcome
setMethod(f = "get_gamble_outcome_objective_consequence",
	signature = "Gamble",
	definition = function(object, index)
	{	
		return (get_objective_consequence(object@outcomes[[index]]))
	}
)

########################
# get subjective_value of an outcome
########################	

# declare a custom function to retrieve subjective_value of an outcome
setGeneric(name = "get_gamble_outcome_subjective_value",
	def = function(object, ...)
	{
		standardGeneric("get_gamble_outcome_subjective_value")
	}
)

# provide implementation of custom function to retrieve subjective_value of an outcome
setMethod(f = "get_gamble_outcome_subjective_value",
	signature = "Gamble",
	definition = function(object, index)
	{	
		return (get_subjective_value(object@outcomes[[index]]))
	}
)

########################
# expected_value related functions
########################	

# declare a custom function to retrieve expected_value
setGeneric(name = "get_expected_value",
	def = function(object)
	{
		standardGeneric("get_expected_value")
	}
)

# provide implementation of custom function to retrieve expected_value
setMethod(f = "get_expected_value",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@expected_value)
	}
)

# declare a custom function to assign expected_value
setGeneric(name = "set_expected_value<-",
	def = function(object, value)
	{
		standardGeneric("set_expected_value<-")
	}
)

# provide implementation of custom function to assign expected_value	
setReplaceMethod(f = "set_expected_value",
	signature = "Gamble",
	definition = function(object, value)
	{
		object@expected_value <- value
		return (object)
	}
)

# declare a custom function to compute expected value
setGeneric(name = "compute_expected_value",
	def = function(object)
	{
		standardGeneric("compute_expected_value")
	}
)


setMethod(f = "compute_expected_value",
	signature = "Gamble",
	definition = function(object)
	{	
	
		expected_value <- sum(sapply(object@outcomes, get_probability) * sapply(object@outcomes, get_objective_consequence))
	
		return (expected_value)
	}
)


########################
# value function related functions
########################	

# declare a custom function
setGeneric(name = "apply_value_function",
	def = function(object, ...)
	{
		standardGeneric("apply_value_function")
	}
)

# provide implementation of custom function
setMethod(f = "apply_value_function",
	signature = "Gamble",
	definition = function(object, utility)
	{
	
		for (n in 1:(length(object@outcomes)))
		{	
			subjective_value <- compute_utility(utility, get_objective_consequence(object@outcomes[[n]]))
		
			set_subjective_value(object@outcomes[[n]]) <- compute_utility(utility, get_objective_consequence(object@outcomes[[n]]))
		}	

		return (object)
	}
)

########################
# certainty_equivalent related functions
########################	

# declare a custom function to retrieve certainty_equivalent
setGeneric(name = "get_certainty_equivalent",
	def = function(object)
	{
		standardGeneric("get_certainty_equivalent")
	}
)

# provide implementation of custom function to retrieve certainty_equivalent
setMethod(f = "get_certainty_equivalent",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@certainty_equivalent)
	}
)

# declare a custom function to assign certainty_equivalent
setGeneric(name = "set_certainty_equivalent<-",
	def = function(object, value)
	{
		standardGeneric("set_certainty_equivalent<-")
	}
)

# provide implementation of custom function to assign certainty_equivalent	
setReplaceMethod(f = "set_certainty_equivalent",
	signature = "Gamble",
	definition = function(object, value)
	{
		object@certainty_equivalent <- value
		return (object)
	}
)

########################
# risk_premium related functions
########################	

# declare a custom function to retrieve risk_premium
setGeneric(name = "get_risk_premium",
	def = function(object)
	{
		standardGeneric("get_risk_premium")
	}
)

# provide implementation of custom function to retrieve risk_premium
setMethod(f = "get_risk_premium",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@risk_premium)
	}
)

# declare a custom function to assign risk_premium
setGeneric(name = "set_risk_premium<-",
	def = function(object, value)
	{
		standardGeneric("set_risk_premium<-")
	}
)

# provide implementation of custom function to assign risk_premium	
setReplaceMethod(f = "set_risk_premium",
	signature = "Gamble",
	definition = function(object, value)
	{
		object@risk_premium <- value
		return (object)
	}
)

# declare a custom function
setGeneric(name = "compute_risk_premium",
	def = function(object, ...)
	{
		standardGeneric("compute_risk_premium")
	}
)

# provide implementation of custom function
setMethod(f = "compute_risk_premium",
	signature = "Gamble",
	definition = function(object, expected_value, certainty_equivalent)
	{
	
		risk_premium <- expected_value - certainty_equivalent
		
		return (risk_premium)
	}
)

########################
# expected_utility related functions
########################	

# declare a custom function to retrieve expected_utility
setGeneric(name = "get_expected_utility",
	def = function(object)
	{
		standardGeneric("get_expected_utility")
	}
)

# provide implementation of custom function to retrieve expected_utility
setMethod(f = "get_expected_utility",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@expected_utility)
	}
)

# declare a custom function to assign expected_utility
setGeneric(name = "set_expected_utility<-",
	def = function(object, value)
	{
		standardGeneric("set_expected_utility<-")
	}
)

# provide implementation of custom function to assign expected_utility	
setReplaceMethod(f = "set_expected_utility",
	signature = "Gamble",
	definition = function(object, value)
	{
		object@expected_utility <- value
		return (object)
	}
)


# declare a custom function
setGeneric(name = "compute_utilities",
	def = function(object, ...)
	{
		standardGeneric("compute_utilities")
	}
)

# provide implementation of custom function to compute ranks
setMethod(f = "compute_utilities",
	signature = "Gamble",
	definition = function(object, utility_family)
	{

		for (n in 1:length(object@outcomes))
		{
			set_subjective_value(object@outcomes[[n]]) <- compute_utility(utility_family, get_objective_consequence(object@outcomes[[n]]))				
		}		
		
		return (object)
	}
)


########################
# pt_value related functions
########################	

# declare a custom function to retrieve pt_value
setGeneric(name = "get_pt_value",
	def = function(object)
	{
		standardGeneric("get_pt_value")
	}
)

# provide implementation of custom function to retrieve pt_value
setMethod(f = "get_pt_value",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@pt_value)
	}
)

# declare a custom function to assign pt_value
setGeneric(name = "set_pt_value<-",
	def = function(object, value)
	{
		standardGeneric("set_pt_value<-")
	}
)

# provide implementation of custom function to assign pt_value	
setReplaceMethod(f = "set_pt_value",
	signature = "Gamble",
	definition = function(object, value)
	{
		object@pt_value <- value
		return (object)
	}
)

# declare a custom function
setGeneric(name = "compute_pt_value",
	def = function(object)
	{
		standardGeneric("compute_pt_value")
	}
)

# provide implementation of custom function
setMethod(f = "compute_pt_value",
	signature = "Gamble",
	definition = function(object)
	{
	
		pt <- 0.0	
		
		df <- data.frame(row.names = NULL, stringsAsFactors = FALSE)		
	
		for (n in 1:(length(object@outcomes)))
		{	
			pt <- pt + get_decision_weight(object@outcomes[[n]]) * get_subjective_value(object@outcomes[[n]])

			#[row, col]			
			df[n, 1] <- get_gamble_id(object)
			df[n, 2] <- get_outcome_id(object@outcomes[[n]])
			df[n, 3] <- get_objective_consequence(object@outcomes[[n]])
			df[n, 4] <- get_probability(object@outcomes[[n]])
			df[n, 5] <- get_decision_weight(object@outcomes[[n]])
			df[n, 6] <- get_subjective_value(object@outcomes[[n]])
			df[n, 7] <- pt							
		}
		
		colnames(df) <- c("gid", "od", "oc", "pr", "dw", "sv", "pt")
		
	
		return (df)
	}
)




########################	
# PT calculations
########################	

# declare a custom function
setGeneric(name = "compute_prospect",
	def = function(object, ...)
	{
		standardGeneric("compute_prospect")
	}
)


setMethod(f = "compute_prospect",
	signature = "Gamble",
	definition = function(object, probability_weighting_specification_for_positive_outcomes, 
probability_weighting_specification_for_negative_outcomes, utility_family, digits)
	{

		#order gambles from highest to lowest, i.e. perform complete sign-ranking.
		
		object <- sort_outcomes(object, high_to_low_flag=TRUE)	

		#compute weighting

		for (i in 1:(length(object@outcomes)))
		{
			my_objective_consequence <- sapply(object@outcomes[i], get_objective_consequence)
			if (sapply(object@outcomes[i], get_objective_consequence) >= 0.0)
			{
				lower_sum <- sum_outcome_probabilities(object, 1, i - 1)
				upper_sum <- sum_outcome_probabilities(object, 1, i)
				positive_weighting <- compute_probability_weighting(probability_weighting_specification_for_positive_outcomes, sum_outcome_probabilities(object, 1, i)) - compute_probability_weighting(probability_weighting_specification_for_positive_outcomes, sum_outcome_probabilities(object, 1, i - 1))
				set_decision_weight(object@outcomes[[i]]) <- positive_weighting
			}
			else
			{
				lower_sum <- sum_outcome_probabilities(object, i + 1, length(object@outcomes))
				upper_sum <- sum_outcome_probabilities(object, i, length(object@outcomes))
				negative_weighting <- compute_probability_weighting(probability_weighting_specification_for_negative_outcomes, sum_outcome_probabilities(object, i, length(object@outcomes))) - compute_probability_weighting(probability_weighting_specification_for_negative_outcomes, sum_outcome_probabilities(object, i + 1, length(object@outcomes)))
				set_decision_weight(object@outcomes[[i]]) <- negative_weighting
			}
		}		
		
		#determine the utility of each outcome value (apply the value function)
		object <- apply_value_function(object, utility_family)				
		
		pt_df <- compute_pt_value(object)
		
		pt_value <- pt_df[nrow(pt_df), ncol(pt_df)]	
		
		set_pt_value(object) <- pt_value
		
		# compute expected value
		
		expected_value <- compute_expected_value(object)	
		set_expected_value(object) <- expected_value
		
		# compute certainty equivalent
		
		certainty_equivalent <- compute_certainty_equivalent(utility_family, pt_value)
		set_certainty_equivalent(object) <- certainty_equivalent

		# compute risk premium
		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)
		set_risk_premium(object) <- risk_premium
		
		summary_df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits=digits, scientific=FALSE),
			format(pt_value, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE),
			format(risk_premium, digits=digits, scientific=FALSE),
			row.names=NULL, 
			stringsAsFactors=FALSE)
		
		colnames(summary_df) <- c("gid", "ev", "pt", "ptce", "ptrp")
		
		df_list <- list("calculations"=pt_df, "summary"=summary_df)
		
		return (df_list)
	}
)

########################	
# RDU calculations
########################	

# declare a custom function to assign rdu_value
setGeneric(name = "set_rdu_value<-",
	def = function(object, value)
	{
		standardGeneric("set_rdu_value<-")
	}
)

# provide implementation of custom function to assign rdu_value	
setReplaceMethod(f = "set_rdu_value",
	signature = "Gamble",
	definition = function(object, value)
	{
		object@rdu_value <- value
		return (object)
	}
)

# declare a custom function to retrieve rdu_value
setGeneric(name = "get_rdu_value",
	def = function(object)
	{
		standardGeneric("get_rdu_value")
	}
)

# provide implementation of custom function to retrieve rdu_value
setMethod(f = "get_rdu_value",
	signature = "Gamble",
	definition = function(object)
	{
		return (object@rdu_value)
	}
)

# declare a custom function
setGeneric(name = "compute_rdu",
	def = function(object, ...)
	{
		standardGeneric("compute_rdu")
	}
)

setMethod(f = "compute_rdu",
	signature = "Gamble",
	definition = function(object, probability_weighting_specification, utility_family, input_file, DELIMITER)
	{
		# following Wakker (2010) p. 165, 5.6 Calculating rank-dependent utility:
		# rank outcomes from best to worst
		# for each outcome, calculate the rank r
		# for all ranks, calculate their w values
		# for each outcome a, calculate the marginal w contribution of its outcome probability p to its rank; i.e., calculate w(p ? r)  w(r).
		# determine the utility of each outcome
		# multiply the utility of each outcome by its decision weight and sum the results
	
		# read in a data file
		object <- read_single_Gamble_data_file(object, input_file, DELIMITER)
	
		#order gambles from highest to lowest, i.e. perform complete sign-ranking.
		
		object <- sort_outcomes(object, high_to_low_flag=TRUE)	
	
		# compute rank r for each outcome
		
		object <- compute_ranks(object)			
	
		# for all ranks, calculate the w of each rank
		
		object <- compute_ws(object, probability_weighting_specification)
		
		# calculate decision weights of each outcome
	
		object <- compute_rdu_decision_weights(object)
	
		# calculate utility of each outcome
		
		object <- compute_utilities(object, utility_family)
	
		# multiply each outcome utility by the decision weight and sum the results
		
		rdu_value <- compute_rdu_value(object)	
		set_rdu_value(object) <- rdu_value
		
		# compute expected value
		
		expected_value <- compute_expected_value(object)		
		set_expected_value(object) <- expected_value
		
		# compute certainty equivalent
		
		certainty_equivalent <- compute_certainty_equivalent(utility_family, rdu_value)
		set_certainty_equivalent(object) <- certainty_equivalent
		
		# compute risk premium
		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)
		set_risk_premium(object) <- risk_premium
		
		return (object)		
	}
)

# declare a custom function
setGeneric(name = "compute_ranks",
	def = function(object, ...)
	{
		standardGeneric("compute_ranks")
	}
)

# provide implementation of custom function to compute ranks
setMethod(f = "compute_ranks",
	signature = "Gamble",
	definition = function(object)
	{

		set_rank(object@outcomes[[1]]) <- 0
		
		number_of_outcomes <- length(object@outcomes)
		
		if (number_of_outcomes > 1)
		{	
			for (n in 2:length(object@outcomes))
			{
				set_rank(object@outcomes[[n]]) <- sum(sapply(object@outcomes[1:(n - 1)], get_probability))
			}			
		}
		
		return (object)
	}
)

# declare a custom function
setGeneric(name = "compute_ws",
	def = function(object, ...)
	{
		standardGeneric("compute_ws")
	}
)

# provide implementation of custom function to compute decision weight
setMethod(f = "compute_ws",
	signature = "Gamble",
	definition = function(object, probability_weighting)
	{

		for (n in 1:length(object@outcomes))
		{
			set_w(object@outcomes[[n]]) <- compute_probability_weighting(probability_weighting, 
get_rank(object@outcomes[[n]]))
		}	
		
		return (object)
	}
)

# declare a custom function
setGeneric(name = "compute_rdu_decision_weights",
	def = function(object, ...)
	{
		standardGeneric("compute_rdu_decision_weights")
	}
)

# provide implementation of custom function to compute rdu decision weights
setMethod(f = "compute_rdu_decision_weights",
	signature = "Gamble",
	definition = function(object)
	{

		number_of_outcomes <- length(object@outcomes)
		
		if (number_of_outcomes > 1)
		{	
			for (n in 2:length(object@outcomes))
			{
				set_decision_weight(object@outcomes[[(n - 1)]]) <- (get_w(object@outcomes[[n]]) - 
get_w(object@outcomes[[(n - 1)]]))
			}
			
		}
		
		set_decision_weight(object@outcomes[[length(object@outcomes)]]) <- (1 - 
get_w(object@outcomes[[length(object@outcomes)]]))

			
		return (object)
	}
)


# declare a custom function
setGeneric(name = "compute_rdu_value",
	def = function(object, ...)
	{
		standardGeneric("compute_rdu_value")
	}
)

# provide implementation of custom function to compute rdu_value
setMethod(f = "compute_rdu_value",
	signature = "Gamble",
	definition = function(object)
	{
	
		rdu <- 0
			
	
		df <- data.frame(row.names = NULL, stringsAsFactors = FALSE)
		
		for (n in 1:(length(object@outcomes)))
		{	
			rdu <- rdu + (get_decision_weight(object@outcomes[[n]]) * get_subjective_value(object@outcomes[[n]]))

			#[row, col]
			df[n, 1] <- get_gamble_id(object)
			df[n, 2] <- get_outcome_id(object@outcomes[[n]])			
			df[n, 3] <- get_objective_consequence(object@outcomes[[n]])
			df[n, 4] <- get_probability(object@outcomes[[n]])
			df[n, 5] <- get_rank(object@outcomes[[n]])
			df[n, 6] <- get_w(object@outcomes[[n]])
			df[n, 7] <- get_decision_weight(object@outcomes[[n]])
			df[n, 8] <- get_subjective_value(object@outcomes[[n]])
			df[n, 9] <- rdu		
		}
	
		colnames(df) <- c("gid", "oid", "oc", "pr", "rnk", "w", "dw", "sv", "rdu")
	
		return (df)
	}
)

# declare a custom function
setGeneric(name = "compute_rdu_value_for_gamble",
	def = function(object, ...)
	{
		standardGeneric("compute_rdu_value_for_gamble")
	}
)


setMethod(f = "compute_rdu_value_for_gamble",
	signature = "Gamble",
	definition = function(object, probability_weighting_specification, utility_family, digits)
	{

		#order gambles from highest to lowest, i.e. perform a complete sign-ranking.
		
		object <- sort_outcomes(object, high_to_low_flag=TRUE)	
		
		# compute rank r for each outcome
		
		object <- compute_ranks(object)			
		
		# for all ranks, calculate the w of each rank
		
		object <- compute_ws(object, probability_weighting_specification)
		
		# calculate decision weights of each outcome
	
		object <- compute_rdu_decision_weights(object)
		
		# calculate utility of each outcome
		
		object <- compute_utilities(object, utility_family)
		
		# multiply each outcome utility by the decision weight and sum the results
		
		rdu_df <- compute_rdu_value(object)
		rdu_value <- rdu_df[nrow(rdu_df), ncol(rdu_df)]
		set_rdu_value(object) <- rdu_value
		
		# compute expected value
		
		expected_value <- compute_expected_value(object)
		set_expected_value(object) <- expected_value
		
		# compute certainty equivalent
		
		certainty_equivalent <- compute_certainty_equivalent(utility_family, rdu_value)
		set_certainty_equivalent(object) <- certainty_equivalent
		
		# compute risk premium
		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)
		set_risk_premium(object) <- risk_premium

		summary_df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits=digits, scientific=FALSE), 
			format(rdu_value, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE), 
			format(risk_premium, digits=digits, scientific=FALSE),
			row.names=NULL, stringsAsFactors=FALSE)
		colnames(summary_df) <- c("gid", "ev", "rdu", "rduce", "rdurp")
		summary_df
		
		df_list <- list("calculations"=rdu_df, "summary"=summary_df)
		
		return (df_list)
	}
)



########################	
# EU calculations
########################	

# declare a custom function
setGeneric(name = "compute_expected_utility_for_gamble",
	def = function(object, ...)
	{
		standardGeneric("compute_expected_utility_for_gamble")
	}
)


setMethod(f = "compute_expected_utility_for_gamble",
	signature = "Gamble",
	definition = function(object, utility_family, digits)
	{

		
		#Determine the utility of each outcome value (apply the value function)
		object <- apply_value_function(object, utility_family)		
		expected_utility <- sum(sapply(object@outcomes, get_subjective_value) * sapply(object@outcomes, get_probability))
		set_expected_utility(object) <- expected_utility		
		
		# compute expected value
		
		expected_value <- compute_expected_value(object)
		set_expected_value(object) <- expected_value
		
		# compute certainty equivalent
		
		certainty_equivalent <- compute_certainty_equivalent(utility_family, expected_utility)
		set_certainty_equivalent(object) <- certainty_equivalent	

		# compute risk premium
		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)
		set_risk_premium(object) <- risk_premium
		
		df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits = digits, scientific = FALSE),
			format(expected_utility, digits = digits, scientific = FALSE),
			format(certainty_equivalent, digits = digits, scientific = FALSE),
			format(risk_premium, digits = digits, scientific = FALSE),
			row.names=NULL, stringsAsFactors=FALSE)
		names(df) <- c("gid", "ev", "eu", "ce", "rp")
		
		return (df)
	}
)


########################
#
# subjectively weighted utility (SWU) model 
#
########################

# declare a custom function
setGeneric(name = "compute_swu",
	def = function(object, ...)
	{
		standardGeneric("compute_swu")
	}
)


setMethod(f = "compute_swu",
	signature = "Gamble",
	definition = function(object,  probability_weighting_specification, utility, digits)
	{

		probability_vector <- NULL
		objective_consequence_vector <- NULL
		
		for (n in 1:length(object@outcomes))
		{
			objective_consequence_vector <- c(objective_consequence_vector, get_objective_consequence(object@outcomes[[n]]))
			probability_vector <- c(probability_vector, get_probability(object@outcomes[[n]]))
		}	
		
		swu <- 0
		for (n in 1:length(objective_consequence_vector))
		{
			w <- compute_probability_weighting(probability_weighting_specification, probability_vector[n])
			u <- compute_utility(utility, objective_consequence_vector[n])
			
			swu <- swu + w * u
		}			
		

		
		# compute expected value		
		expected_value <- compute_expected_value(object)			
		
		# compute certainty equivalent
		certainty_equivalent <- compute_certainty_equivalent(utility, swu)
		
		# compute risk premium		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)		
					
		summary_df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits=digits, scientific=FALSE),
			format(swu, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE),	
			format(risk_premium, digits=digits, scientific=FALSE),	
			row.names=NULL, stringsAsFactors=FALSE)
		colnames(summary_df) <- c("gid", "ev", "swu", "swuce", "swurp")

		return (summary_df)
	}
)


########################
#
# subjectively weighted average utility (SWAU) model 
#
########################


# declare a custom function
setGeneric(name = "compute_swau",
	def = function(object, ...)
	{
		standardGeneric("compute_swau")
	}
)


setMethod(f = "compute_swau",
	signature = "Gamble",
	definition = function(object,  probability_weighting_specification, utility, digits)
	{
		probability_vector <- NULL
		objective_consequence_vector <- NULL
		
		for (n in 1:length(object@outcomes))
		{
			objective_consequence_vector <- c(objective_consequence_vector, get_objective_consequence(object@outcomes[[n]]))
			probability_vector <- c(probability_vector, get_probability(object@outcomes[[n]]))
		}	
		

		
		probability_vector <- NULL
		objective_consequence_vector <- NULL
		
		for (n in 1:length(object@outcomes))
		{
			objective_consequence_vector <- c(objective_consequence_vector, get_objective_consequence(object@outcomes[[n]]))
			probability_vector <- c(probability_vector, get_probability(object@outcomes[[n]]))
		}	
		
		numerator <- 0
		sum_w <- 0
		for (n in 1:length(objective_consequence_vector))
		{
			w <- compute_probability_weighting(probability_weighting_specification, probability_vector[n])
			u <- compute_utility(utility, objective_consequence_vector[n])
			
			numerator <- numerator + w * u
			sum_w <- sum_w + w
		}			
		
		swau <- numerator / sum_w
		

		
		# compute expected value		
		expected_value <- compute_expected_value(object)			
				
		# compute certainty equivalent
		certainty_equivalent <- compute_certainty_equivalent(utility, swau)
		
		# compute risk premium		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)			
		
		summary_df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits=digits, scientific=FALSE),
			format(swau, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE),	
			format(risk_premium, digits=digits, scientific=FALSE),	
			row.names=NULL, stringsAsFactors=FALSE)
		colnames(summary_df) <- c("gid", "ev", "swau", "swauce", "swaurp")

		return (summary_df)
	}
)

########################
#
# lower gains decomposition utility (GDU) model 
#
########################


# declare a custom function
setGeneric(name = "compute_gdu",
	def = function(object, ...)
	{
		standardGeneric("compute_gdu")
	}
)

setMethod(f = "compute_gdu",
	signature = "Gamble",
	definition = function(object, probability_weighting_specification, utility, digits)
	{

		
		probability_vector <- NULL
		objective_consequence_vector <- NULL		
		
		for (n in 1:length(object@outcomes))
		{
			objective_consequence_vector <- c(objective_consequence_vector, get_objective_consequence(object@outcomes[[n]]))
			probability_vector <- c(probability_vector, get_probability(object@outcomes[[n]]))
		}		
		
		df <- data.frame(objective_consequence_vector, probability_vector)
		df
		df <- df[order(df$objective_consequence_vector, df$probability_vector), ]
		df
		dim(df)
		# reverse the order of the rows
		df <- df[dim(df)[1]:1, ]

		
		objective_consequence_vector <- df$objective_consequence_vector
		objective_consequence_vector
		probability_vector <- df$probability_vector
		probability_vector		
		

		# sum all probabilities from highest to second lowest and feed into compute_probability_weighting

		if (length(objective_consequence_vector) == 1)
		{
			u_x <- compute_utility(utility, objective_consequence_vector[1])			
			

			gdu <- u_x
		}
		else if (length(objective_consequence_vector) == 2)
		{
			w_x <- compute_probability_weighting(probability_weighting_specification, probability_vector[1])
			u_x <- compute_utility(utility, objective_consequence_vector[1])
			u_y <- compute_utility(utility, objective_consequence_vector[2])
			
			gdu <- w_x * u_x + (1.0 - w_x) * u_y
		}
		else if (length(objective_consequence_vector) == 3)
		{
	
			u_z <- compute_utility(utility, objective_consequence_vector[3])
			x <- objective_consequence_vector[1]
			p <- probability_vector[1]			
			y <- objective_consequence_vector[2]
			q <- probability_vector[2]

			w_x <- compute_probability_weighting(probability_weighting_specification, p/(p+q))
			u_x <- compute_utility(utility, objective_consequence_vector[1])
			u_y <- compute_utility(utility, objective_consequence_vector[2])		
			
			two_outcome_result <- w_x * u_x + (1.0 - w_x) * u_y
			
			w_p_q <- compute_probability_weighting(probability_weighting_specification, p+q)
			
			gdu <- w_p_q * two_outcome_result + (1.0 - w_p_q) * u_z
		
		}
		else if (length(objective_consequence_vector) == 4)
		{
			u_A <- compute_utility(utility, objective_consequence_vector[4])
			x <- objective_consequence_vector[1]
			p <- probability_vector[1]			
			y <- objective_consequence_vector[2]
			q <- probability_vector[2]
			z <- objective_consequence_vector[3]
			r <- probability_vector[3]
			
			w_x_y <- compute_probability_weighting(probability_weighting_specification, (p+q)/(p+q+r))
			u_x_y <- compute_utility(utility, objective_consequence_vector[1] + objective_consequence_vector[2])	
			u_z <- compute_utility(utility, objective_consequence_vector[3])	
			
			two_outcome_result <- w_x_y * u_x_y + (1.0 - w_x_y) * u_z
			
			w_p_q_r <- compute_probability_weighting(probability_weighting_specification, p+q+r)
			
			gdu <- w_p_q_r * two_outcome_result + (1.0 - w_p_q_r) * u_A
		}
		else
		{
			
			
			u_A <- compute_utility(utility, objective_consequence_vector[length(objective_consequence_vector)])
			x <- objective_consequence_vector[1]
			p <- probability_vector[1]			
			y <- objective_consequence_vector[2]
			q <- probability_vector[2]
			z <- objective_consequence_vector[3]
			r <- probability_vector[3]
			
			numerator <- sum(probability_vector[1:(length(probability_vector)-2)])
			denominator <- sum(probability_vector[1:(length(probability_vector)-1)])
			ratio <- numerator/denominator
			
			x_y <- sum(probability_vector[1:(length(objective_consequence_vector)-2)])
			
			w_x_y <- compute_probability_weighting(probability_weighting_specification, ratio)
			u_x_y <- compute_utility(utility, x_y)	
			u_z <- compute_utility(utility, objective_consequence_vector[length(objective_consequence_vector)-2])	
			
			two_outcome_result <- w_x_y * u_x_y + (1.0 - w_x_y) * u_z
			
			w_p_q_r <- compute_probability_weighting(probability_weighting_specification,
				sum(probability_vector[1:(length(probability_vector)-1)]))
			
			gdu <- w_p_q_r * two_outcome_result + (1.0 - w_p_q_r) * u_A			
	
		}
		

		# compute expected value		
		expected_value <- compute_expected_value(object)
		
		# compute certainty_equivalent
		certainty_equivalent <- compute_certainty_equivalent(utility, gdu)

		# compute risk premium		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)
	
		summary_df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits=digits, scientific=FALSE),
			format(gdu, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE),	
			format(risk_premium, digits=digits, scientific=FALSE),	
			row.names=NULL, stringsAsFactors=FALSE)
		colnames(summary_df) <- c("gid", "ev", "gdu", "gduce", "gdurp")

		return (summary_df)
	}
)



########################
#
# RAM
#
########################	

 
# declare a custom function
setGeneric(name = "compute_ram_model",
	def = function(object, ...)
	{
		standardGeneric("compute_ram_model")
	}
)


setMethod(f = "compute_ram_model",
	signature = "Gamble",
	definition = function(object, branch_weighting_vector, probability_weighting_specification, utility, digits)
	{
		probability_vector <- NULL
		objective_consequence_vector <- NULL		
		
		for (n in 1:length(object@outcomes))
		{
			objective_consequence_vector <- c(objective_consequence_vector, get_objective_consequence(object@outcomes[[n]]))
			probability_vector <- c(probability_vector, get_probability(object@outcomes[[n]]))
		}		
		
		# sort outcomes from lowest to highest (rank order, including probabilities)
		# also need to keep branch_weighting_vector sorted in alignment as well


		df <- data.frame(objective_consequence_vector, probability_vector)
		df
		df <- df[order(objective_consequence_vector, probability_vector), ]
		df
		df$branch_weighting_vector <- rev(branch_weighting_vector)
		df
		
		objective_consequence_vector <- df$objective_consequence_vector
		probability_vector <- df$probability_vector
		branch_weighting_vector <- df$branch_weighting_vector
		
		##############
		# test if all elements in objective_consequence_vector are negative
		# if so, take absolute values and use reflection (Birnbaum, 2008, p470)
		negative_vector_flag <- TRUE
		
		for (index in 1:length(objective_consequence_vector))
		{
			if (objective_consequence_vector[index] > 0)
			{
				negative_vector_flag <- FALSE
				break
			}
		}
		
		if (negative_vector_flag == TRUE)
		{
			objective_consequence_vector <- sapply(objective_consequence_vector, function(x) abs(x))
			objective_consequence_vector <- rev(objective_consequence_vector)
			probability_vector <- rev(probability_vector)
		}
		##############		
		

		t <- c()
		for (i in 1:length(probability_vector))
		{
			t <- append(t, compute_probability_weighting(probability_weighting_specification, probability_vector[i]))
			
		}		
		

		u <- c()
		for (i in 1:length(objective_consequence_vector))
		{
			u <- append(u, compute_utility(utility, objective_consequence_vector[i]))		
		}		
		
		# utility
		ramu <- sum(branch_weighting_vector * t * u) / sum(branch_weighting_vector * t)


		##############
		# test if all elements in objective_consequence_vector are negative
		# if so, take absolute values and use reflection (Birnbaum, 2008, p470)		
		if (negative_vector_flag == TRUE)
		{
			ramu <- -ramu
		}
		##############		
		
		# compute expected value		
		expected_value <- compute_expected_value(object)			
		
		# compute certainty equivalent
		certainty_equivalent <- compute_certainty_equivalent(utility, ramu)
		
		# compute risk premium		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)		
		
		summary_df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits=digits, scientific=FALSE),
			format(ramu, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE),	
			format(risk_premium, digits=digits, scientific=FALSE),				
				row.names=NULL, stringsAsFactors=FALSE)
		colnames(summary_df) <- c("gid", "ev", "ram", "ramce", "ramrp")

		return (summary_df)	
	}
)


########################
#
# special transfer of attention model (TAX) model
#
########################	

# declare a custom function
setGeneric(name = "compute_tax_model",
	def = function(object, ...)
	{
		standardGeneric("compute_tax_model")
	}
)


setMethod(f = "compute_tax_model",
	signature = "Gamble",
	definition = function(object, probability_weighting_specification, utility, delta, digits)
	{
	
		probability_vector <- NULL
		objective_consequence_vector <- NULL		
		
		for (n in 1:length(object@outcomes))
		{
			objective_consequence_vector <- c(objective_consequence_vector, get_objective_consequence(object@outcomes[[n]]))
			probability_vector <- c(probability_vector, get_probability(object@outcomes[[n]]))
		}		
		
		# sort outcomes from lowest to highest (rank order, including probabilities)

		df <- data.frame(objective_consequence_vector, probability_vector)
		df
		df <- df[order(objective_consequence_vector, probability_vector), ]
		df
		
		objective_consequence_vector <- df$objective_consequence_vector
		probability_vector <- df$probability_vector
		
		##############
		# test if all elements in objective_consequence_vector are negative
		# if so, take absolute values and use reflection (Birnbaum, 2008, p471)
		negative_vector_flag <- TRUE
		
		for (index in 1:length(objective_consequence_vector))
		{
			if (objective_consequence_vector[index] > 0)
			{
				negative_vector_flag <- FALSE
				break
			}
		}
		
		if (negative_vector_flag == TRUE)
		{
			objective_consequence_vector <- sapply(objective_consequence_vector, function(x) abs(x))
			objective_consequence_vector <- rev(objective_consequence_vector)
			probability_vector <- rev(probability_vector)
		}
		##############
		
		# t represents how a branch weight depends on its probability

		t <- c()
		for (i in 1:length(probability_vector))
		{
			t <- append(t, compute_probability_weighting(probability_weighting_specification, probability_vector[i]))
			
		}	
		

		u <- c()
		for (i in 1:length(objective_consequence_vector))
		{
			u <- append(u, compute_utility(utility, objective_consequence_vector[i]))		
		}		
		
		numerator1 <- sum(t * u)
		
		numerator2 <- 0
		if (length(objective_consequence_vector) > 1)
		{
			for (i in 2:(length(objective_consequence_vector)))
			{                        
				for (j in 1:(i-1))
				{
					if (delta < 0)
					{
						omega <- (delta * t[i])/(length(objective_consequence_vector) + 1)
					}
					else if (delta >= 0)
					{
						omega <- (delta * t[j])/(length(objective_consequence_vector) + 1)				
					}
					numerator2 <- numerator2 + (u[i] - u[j]) * omega
				}
			}		
		}
		denominator <- sum(t)
		
		# utility	
		taxu <- (numerator1 + numerator2)/denominator
		

		
		##############
		# test if all elements in objective_consequence_vector are negative
		# if so, take absolute values and use reflection (Birnbaum, 2008, p471)		
		if (negative_vector_flag == TRUE)
		{
			taxu <- -taxu
		}
		##############
		
		
		# compute expected value		
		expected_value <- compute_expected_value(object)			
	
		# compute certainty equivalent
		certainty_equivalent <- compute_certainty_equivalent(utility, taxu)
		
		# compute risk premium		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)	
		
		summary_df <- data.frame(get_gamble_id(object), 
			format(expected_value, digits=digits, scientific=FALSE),
			format(taxu, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE), 
			format(risk_premium, digits=digits, scientific=FALSE), 
			row.names=NULL, stringsAsFactors=FALSE)
		colnames(summary_df) <- c("gid", "ev", "tax", "taxce", "taxrp")

		return (summary_df)
	}
)


########################
#
# Viscusi's (1989) prt
#
########################	


# declare a custom function
setGeneric(name = "compute_prt",
	def = function(object, ...)
	{
		standardGeneric("compute_prt")
	}
)


setMethod(f = "compute_prt",
	signature = "Gamble",
	definition = function(object, utility, gamma, digits)
	{
		# extract 2 vectors from the gamble outcomes
		probability_vector <- NULL
		objective_consequence_vector <- NULL
		
		for (n in 1:length(object@outcomes))
		{
			objective_consequence_vector <- c(objective_consequence_vector, get_objective_consequence(object@outcomes[[n]]))
			probability_vector <- c(probability_vector, get_probability(object@outcomes[[n]]))
		}				
		
		# utility power function	
		u <- c()
		for (i in 1:length(objective_consequence_vector))
		{
			u <- append(u, compute_utility(utility, objective_consequence_vector[i]))		
		}		
		
		
		term1 <- gamma * sum(probability_vector * u)
		
		term2 <- (1.0 - gamma) * sum(u) / length(u)
		
		
		# utility	
		prtu <- term1 + term2
		
		# compute expected value		
		expected_value <- compute_expected_value(object)
		
		# compute certainty_equivalent
		certainty_equivalent <- compute_certainty_equivalent(utility, prtu)

		# compute risk premium		
		risk_premium <- compute_risk_premium(object, expected_value, certainty_equivalent)		
		
		summary_df <- data.frame(get_gamble_id(object),
			format(expected_value, digits=digits, scientific=FALSE),
			format(prtu, digits=digits, scientific=FALSE),
			format(certainty_equivalent, digits=digits, scientific=FALSE), 
			format(risk_premium, digits=digits, scientific=FALSE), 
			row.names=NULL, stringsAsFactors=FALSE)				
		colnames(summary_df) <- c("gid", "ev", "prtu", "prtuce", "prturp")

		return (summary_df)
	}
)


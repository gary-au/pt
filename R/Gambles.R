########################
#
# Gambles, a S4 class
#
########################	

#' The Gambles class

#' @section Slots:
#'  \describe{
#'    \item{\code{gambles}:}{Object of class \code{"vector"}, containing the gambles.}
#'  }
#'
#' @note The Gambles class represents a set of gambles.
#' @name Gambles 
#' @rdname Gambles
#' @exportClass Gambles
#' @aliases Gambles-class
setClass(Class = "Gambles",
	representation = representation
	(
		gambles = "vector"
	),
	# check for input consistency when creating new Gambles objects using "new" constructor
	validity = function(object)
	{
		# run the Gambles inspector

		for (index in 1: length(gambles))
		{
			my_gamble <- gambles[index]
			
			# make sure probabilities of all outcomes sum to <= 1	
			probability_sum = sum(sapply(my_gamble@outcomes, get_probability))

			if (probability_sum < 0 | probability_sum > 1)
			{
				stop(paste("sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
			}
			else
			{			
			}						
		}

		return (TRUE)
	}
)


########################
# show gambles
########################	

setMethod(f = "show",
	signature = "Gambles",
	definition = function(object)
	{

		df <- data.frame(row.names=NULL, stringsAsFactors=FALSE)		
		
		for (n in 1:length(object@gambles))
		{

			for (m in 1:get_number_of_outcomes(object@gambles[[n]]))
			{
		
				a_gamble <- object@gambles[[n]]				
				an_outcome <- a_gamble@outcomes[[m]]
	
				gamble_id <- get_gamble_id(object@gambles[[n]])
				outcome_id <- get_outcome_id(an_outcome)
				objective_consequence <- format(get_objective_consequence(an_outcome), digits=16, scientific=FALSE)
				probability_string <- get_probability_string(an_outcome)
			
				df <-rbind(df, data.frame("gid"=gamble_id, "oid"=outcome_id, "oc"=objective_consequence, "pr"=probability_string))					
				
			}			
		}
		print (df)
	}
)

########################
# create a gambles container
########################	


create_gambles <- function()
{

	new(Class = "Gambles")
}


create_gambles_v3 <- function(gamble_id_vector, outcome_id_vector, objective_consequence_vector, probability_string_vector)
{

	# perform validity checks on the input
	
	# firstly the gamble_id_vector and all other vectors must be the same length
	
	gamble_id_vector_length <- length(gamble_id_vector)
	outcome_id_vector_length <- length(outcome_id_vector)	
	objective_consequence_vector_length <- length(objective_consequence_vector)
	probability_string_vector_length <- length(probability_string_vector)

	if (gamble_id_vector_length != outcome_id_vector_length)
	{
		stop(paste("gamble_id_vector_length has length: ", gamble_id_vector_length, " and outcome_id_vector has length: ", outcome_id_vector_length, "\n"));			
	}
	
	if (gamble_id_vector_length != objective_consequence_vector_length)
	{
		stop(paste("gamble_id_vector_length has length: ", gamble_id_vector_length, " and objective_consequence_vector has length: ", objective_consequence_vector_length, "\n"));			
	}
	
	if (gamble_id_vector_length != probability_string_vector_length)
	{
		stop(paste("gamble_id_vector_length has length: ", gamble_id_vector_length, " and probability_string_vector has length: ", probability_string_vector_length, "\n"));			
	}	

	
	# create a gamble with no outcomes, but perform further checks on probability sums...
	
	object <- new(Class = "Gambles")
	
	outc_gamble_vector <- c()
	prob_string_gamble_vector <- c()
	obj_cons_gamble_vector <- c()
	old_gamble_id <- 1
	
	for (n in 1:length(gamble_id_vector))
	{
			
		gamble_id <- gamble_id_vector[n]
		outcome_id <- outcome_id_vector[n]		
	
		if (n == 1)
		{
			old_gamble_id <- gamble_id
		}
		
		# create a new gamble
		if (gamble_id != old_gamble_id)
		{

			# the probabilities for each gamble id the in probability_vector must sum to 1
			
			probability_vector <- unlist(lapply(prob_string_gamble_vector, function(prob_string_gamble_vector) eval(parse(text=prob_string_gamble_vector))))			
			probability_sum <- sum(probability_vector)
	
			if (probability_sum < 0 | probability_sum > 1)
			{
				stop(paste("gamble id: ", old_gamble_id, " sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
			}				
			
			my_gamble <- create_gamble_v3(old_gamble_id, outc_gamble_vector, obj_cons_gamble_vector, prob_string_gamble_vector)
			object@gambles <- append(object@gambles, my_gamble)
	
			set_gamble_id(object@gambles[[length(object@gambles)]]) <- old_gamble_id
			
			old_gamble_id <- gamble_id
			
			outc_gamble_vector <- c()
			prob_string_gamble_vector <- c()
			obj_cons_gamble_vector <- c()

			outcome_id <- outcome_id_vector[n]			
			probability_string <- probability_string_vector[n]
			objective_consequence <- objective_consequence_vector[n]			
			
			outc_gamble_vector <- append(outc_gamble_vector, outcome_id)	
			prob_string_gamble_vector <- append(prob_string_gamble_vector, probability_string)
			obj_cons_gamble_vector <- append(obj_cons_gamble_vector, objective_consequence)
			
			if (n == length(gamble_id_vector))
			{
				# the probabilities for each gamble id the in probability_vector must sum to 1	
				probability_vector <- unlist(lapply(prob_string_gamble_vector, function(prob_string_gamble_vector) eval(parse(text=prob_string_gamble_vector))))			
				probability_sum <- sum(probability_vector)				

				if (probability_sum < 0 | probability_sum > 1)
				{
					stop(paste("sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
				}				
				
				my_gamble <- create_gamble_v3(gamble_id, outc_gamble_vector, obj_cons_gamble_vector, prob_string_gamble_vector)
				object@gambles <- append(object@gambles, my_gamble)
		
				set_gamble_id(object@gambles[[length(object@gambles)]]) <- old_gamble_id	
			}
		}
		else
		{

			outcome_id <- outcome_id_vector[n]			
			probability_string <- probability_string_vector[n]
			objective_consequence <- objective_consequence_vector[n]			
			
			outc_gamble_vector <- append(outc_gamble_vector, outcome_id)
			prob_string_gamble_vector <- append(prob_string_gamble_vector, probability_string)
			obj_cons_gamble_vector <- append(obj_cons_gamble_vector, objective_consequence)

			if (n == length(gamble_id_vector))
			{
				# the probabilities for each gamble id the in probability_vector must sum to 1			
				probability_vector <- unlist(lapply(prob_string_gamble_vector, function(prob_string_gamble_vector) eval(parse(text=prob_string_gamble_vector))))			
				probability_sum <- sum(probability_vector)
				
				if (probability_sum < 0 | probability_sum > 1)
				{
					stop(paste("gamble_id: ", gamble_id, " sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
				}				
				
				my_gamble <- create_gamble_v3(gamble_id, outc_gamble_vector, obj_cons_gamble_vector, prob_string_gamble_vector)
				object@gambles <- append(object@gambles, my_gamble)
		
				set_gamble_id(object@gambles[[length(object@gambles)]]) <- old_gamble_id	
			}
		}	
	}

		
	return (object)	
}



########################
# read gambles from external text file
########################	

# declare a custom function
setGeneric(name = "read_multiple_gambles_data_file",
	def = function(object, ...)
	{
		standardGeneric("read_multiple_gambles_data_file")
	}
)


setMethod(f = "read_multiple_gambles_data_file",
	signature = "Gambles",
	definition = function(object, input_file, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
	{

		#read in outcomes from external file
		my_data_frame <- data.frame(read.table(file = input_file, header = TRUE, stringsAsFactors = FALSE, sep = DELIMITER))

		objective_consequence_vector <- c()
		gamble_id_vector <- c()
		outcome_id_vector <- c()
		
		for (n in 1:nrow(my_data_frame))
		{
			gamble_id <- eval(parse(text = my_data_frame[n, gamble_id_header]))
			
			outcome_id <- eval(parse(text = my_data_frame[n, outcome_id_header]))			
			probability_string <- as.character(format(my_data_frame[n, probability_header], digits = 16, scientific=FALSE))
			objective_consequence <- my_data_frame[n, objective_consequence_header]

			my_outcome <- create_outcome(outcome_id = outcome_id, position = n, objective_consequence = objective_consequence, probability_string = probability_string, rank = 0, decision_weight = 0.0, subjective_value = 0.0, w = 0.0)
	
			if ((gamble_id %in% gamble_id_vector) == FALSE)
			{
		
				gamble_id_vector <- append(gamble_id_vector, gamble_id)
				
				my_gamble <- create_gamble()
				
				set_gamble_id(my_gamble) <- gamble_id
				
	
				my_gamble@outcomes <- append(my_gamble@outcomes, my_outcome)
				
				object@gambles <- append(object@gambles, my_gamble)
			}
			else
			{
				a_gamble <- object@gambles[[gamble_id]]
				a_gamble@outcomes <- append(a_gamble@outcomes, my_outcome)
				object@gambles[gamble_id] <- a_gamble
			}
		
		}
		
		return (object)
	}
)


########################
# support functions
########################	

########################
# get number of gambles
########################	

# declare a custom function to return the number of gambles
setGeneric(name = "get_number_of_gambles",
	def = function(object, ...)
	{
		standardGeneric("get_number_of_gambles")
	}
)

# provide implementation of custom function to return the number of gambles
setMethod(f = "get_number_of_gambles",
	signature = "Gambles",
	definition = function(object)
	{	
		return (length(object@gambles))
	}
)

########################
# get gamble ids
########################	

# declare a custom function to get_gamble_ids
setGeneric(name = "get_gamble_ids",
	def = function(object, ...)
	{
		standardGeneric("get_gamble_ids")
	}
)

# provide implementation of custom function to get_gamble_ids
setMethod(f = "get_gamble_ids",
	signature = "Gambles",
	definition = function(object)
	{	
		gamble_ids <- c()
		
		for (i in 1:length(object@gambles))
		{
			gamble_id <- get_gamble_id(object@gambles[[i]])
			
			gamble_ids <- c(gamble_ids, gamble_id)
		}
		
		return (gamble_ids)
	}
)


########################
# get total number of gamble outcomes (useful for drawing gamble diagrams)
########################	

# declare a custom function to return the total number of outcomes across all gambles
setGeneric(name = "get_total_number_of_gamble_outcomes",
	def = function(object, ...)
	{
		standardGeneric("get_total_number_of_gamble_outcomes")
	}
)

# provide implementation of custom function to return the total number of outcomes across all gambles
setMethod(f = "get_total_number_of_gamble_outcomes",
	signature = "Gambles",
	definition = function(object)
	{	
		total_number_of_gamble_outcomes <- 0
		
		for (n in 1:length(object@gambles))
		{	
			total_number_of_gamble_outcomes <- total_number_of_gamble_outcomes + get_number_of_outcomes(object@gambles[[n]])
		}
		
		return (total_number_of_gamble_outcomes)
	}
)


########################
# get number of outcomes for a specified gamble
########################	

# declare a custom function to return the number of outcomes for specified gamble index 
setGeneric(name = "get_number_of_gamble_outcomes",
	def = function(object, ...)
	{
		standardGeneric("get_number_of_gamble_outcomes")
	}
)

# provide implementation of custom function to return the number of outcomes for specified gamble index
setMethod(f = "get_number_of_gamble_outcomes",
	signature = "Gambles",
	definition = function(object, index)
	{	
		number_of_outcomes <- get_number_of_outcomes(object@gambles[[index]])
		
		return (number_of_outcomes)
	}
)

########################
# get gambles vector
########################	

# declare a custom function to return the gambles vector 
setGeneric(name = "get_gambles",
	def = function(object, ...)
	{
		standardGeneric("get_gambles")
	}
)

# provide implementation of custom function to return the gambles vector 
setMethod(f = "get_gambles",
	signature = "Gambles",
	definition = function(object)
	{	
		return (object@gambles)
	}
)

########################
# get a gamble object via specified gamble index
########################	

# declare a custom function to return a gamble specified by an index 
setGeneric(name = "get_gamble",
	def = function(object, ...)
	{
		standardGeneric("get_gamble")
	}
)

# provide implementation of custom function to return a gamble specified by an index 
setMethod(f = "get_gamble",
	signature = "Gambles",
	definition = function(object, index)
	{	
		return (object@gambles[[index]])
	}
)

########################
# add gamble
########################	

setGeneric(name = "add_gamble",
	def = function(.Object, value)
	{
		standardGeneric("add_gamble")
	}
)


setMethod(f = "add_gamble",
	signature = "Gambles",
	definition = function(.Object, value)
	{	
		gamble_ids <- c()
		
		if (length(.Object@gambles) > 1)
		{
			gamble_ids <- get_gamble_ids(.Object)
		}
		
		new_gamble_id <- get_gamble_id(value)
		if (new_gamble_id %in% gamble_ids)
		{
			cat("Gamble id: ",  new_gamble_id, " already exists.\n")
		}
		else
		{		
			nameObject <- deparse(substitute(.Object))	
			
		
			.Object@gambles <- append(.Object@gambles, value)
			
			assign(nameObject,.Object,envir=parent.frame())
		
			
			return (invisible())			
		}
	}
)

########################
# delete gambles
########################	

setGeneric(name = "delete_gambles",
	def = function(.Object, value)
	{
		standardGeneric("delete_gambles")
	}
)


setMethod(f = "delete_gambles",
	signature = "Gambles",
	definition = function(.Object, value)
	{		
		nameObject <- deparse(substitute(.Object))	


		keep_gambles_vector <- c()
		
		for (n in 1:length(.Object@gambles))
		{
			if ((get_gamble_id(.Object@gambles[[n]]) %in% value) == FALSE)
			{
				keep_gambles_vector <- c(keep_gambles_vector, .Object@gambles[[n]])
			}				
		}		
		
		.Object@gambles <- keep_gambles_vector
		
		assign(nameObject,.Object,envir=parent.frame())
	
		return (invisible())
	}
)

########################
# set_gs_probabilities
########################	

# declare a custom function to set_gs_probabilities	
setGeneric(name = "set_gs_probabilities<-",
	def = function(object, gamble_id, value)
	{
		standardGeneric("set_gs_probabilities<-")
	}
)


setReplaceMethod(f = "set_gs_probabilities",
	signature = "Gambles",
	definition = function(object, gamble_id, value)
	{	
		probability_sum = sum(as.numeric(value))
		
		if (probability_sum < 0 | probability_sum > 1)
		{
			cat(paste("sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));

		}
		else
		{
			for (index in 1:length(object@gambles))
			{
				if (get_gamble_id(object@gambles[[index]]) == gamble_id)
				{
					set_g_probabilities(object@gambles[[index]]) <- value				
					break
				}
	
			}
		}
		return (object)
	}
)


########################
# set_gs_objective_consequence
########################	

# declare a custom function to set_gs_objective_consequence	
setGeneric(name = "set_gs_objective_consequence<-",
	def = function(object, gamble_id, outcome_id, value)
	{
		standardGeneric("set_gs_objective_consequence<-")
	}
)


setReplaceMethod(f = "set_gs_objective_consequence",
	signature = "Gambles",
	definition = function(object, gamble_id, outcome_id, value)
	{
		for (n in 1:length(object@gambles))
		{
			if (get_gamble_id(object@gambles[[n]]) == gamble_id)
			{
				set_g_objective_consequence(object@gambles[[n]], outcome_id) <- value		
				break
			}
		}
		return (object)
	}
)



########################
# computational functions for comparing theories
########################	

########################
# ev related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_expected_value",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_expected_value")
	}
)


setMethod(f = "compare_gambles_under_expected_value",
	signature = "Gambles",
	definition = function(object, digits)
	{	
	
		summary_df <- data.frame()
	
		for (n in 1:length(object@gambles))
		{	
			ev <- compute_expected_value(object@gambles[[n]])
			df <- data.frame(get_gamble_id(object@gambles[[n]]), format(ev, digits=digits, scientific=FALSE))
			summary_df <- rbind(summary_df, df)
		}
		
		colnames(summary_df) <- c("gid", "ev")		

		return (summary_df)

	}
)

########################
# eu related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_expected_utility",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_expected_utility")
	}
)


setMethod(f = "compare_gambles_under_expected_utility",
	signature = "Gambles",
	definition = function(object, utility, digits)
	{	

		
		summary_df <- data.frame()
		
		for (n in 1:length(object@gambles))
		{	
			df <- compute_expected_utility_for_gamble(object@gambles[[n]], utility, digits)
			summary_df <- rbind(summary_df, df)
		}
		
		colnames(summary_df) <- c("gid", "ev", "eu", "euce", "eurp")	
		
		return (summary_df)
	}
)


########################
# pt related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_pt",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_pt")
	}
)


setMethod(f = "compare_gambles_under_pt",
	signature = "Gambles",
	definition = function(object, probability_weighting_specification_for_positive_outcomes, probability_weighting_specification_for_negative_outcomes, utility, digits)
	{	
	
		results_list <- list()
		
		for (n in 1:length(object@gambles))
		{	
			df_list <- compute_prospect(object@gambles[[n]], probability_weighting_specification_for_positive_outcomes, probability_weighting_specification_for_negative_outcomes, utility, digits)
			results_list[[n]] <- df_list
		}
		
		return (results_list)
	}
)


########################
# rdu related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_rdu",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_rdu")
	}
)


setMethod(f = "compare_gambles_under_rdu",
	signature = "Gambles",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	

		results_list <- list()
		
		for (n in 1:length(object@gambles))
		{	

			df_list <- compute_rdu_value_for_gamble(object@gambles[[n]], probability_weighting_specification, utility, digits)
			results_list[[n]] <- df_list
		}

		return (results_list)
	}
)

########################
# ram related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_ram",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_ram")
	}
)


setMethod(f = "compare_gambles_under_ram",
	signature = "Gambles",
	definition = function(object, branch_weighting_vector_list, probability_weighting_specification, utility, digits)
	{	
				
		summary_df <- data.frame()
	
		for (n in 1:length(object@gambles))
		{			
			number_of_outcomes <- get_number_of_outcomes(object@gambles[[n]])
		
			for (m in 1:length(branch_weighting_vector_list))
			{
				if (length(branch_weighting_vector_list[[m]]) == number_of_outcomes)
				{
					branch_weighting_vector <- branch_weighting_vector_list[[m]]
					
					df <- compute_ram_model(object@gambles[[n]], branch_weighting_vector, probability_weighting_specification, utility, digits)
					
					summary_df <- rbind(summary_df, df)
					
					break
				}
			}
	
		}
		
		colnames(summary_df) <- c("gid", "ev", "ramu", "ramuce", "ramurp")		
		
		return (summary_df)

	}
)

########################
# tax related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_tax",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_tax")
	}
)


setMethod(f = "compare_gambles_under_tax",
	signature = "Gambles",
	definition = function(object, probability_weighting_specification, utility, delta, digits)
	{	
	
		summary_df <- data.frame()
	
		for (n in 1:length(object@gambles))
		{	
		
			df <- compute_tax_model(object@gambles[[n]], probability_weighting_specification, utility, delta, digits)

			summary_df <- rbind(summary_df, df)
			
		}
		
		colnames(summary_df) <- c("gid", "ev", "tax", "taxce", "taxrp")		

		return (summary_df)
	}
)

########################
# swu related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_swu",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_swu")
	}
)


setMethod(f = "compare_gambles_under_swu",
	signature = "Gambles",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	
		
		summary_df <- data.frame()
	
		for (n in 1:length(object@gambles))
		{	
		
			df <- compute_swu(object@gambles[[n]], probability_weighting_specification, utility, digits)

			summary_df <- rbind(summary_df, df)
	
		}
		
		colnames(summary_df) <- c("gid", "ev", "swu", "swuce", "swurp")		

		return (summary_df)
	}
)

########################
# swau related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_swau",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_swau")
	}
)


setMethod(f = "compare_gambles_under_swau",
	signature = "Gambles",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	
	
		summary_df <- data.frame()
	
		for (n in 1:length(object@gambles))
		{	
		
			df <- compute_swau(object@gambles[[n]], probability_weighting_specification, utility, digits)

			summary_df <- rbind(summary_df, df)
	
		}
		
		colnames(summary_df) <- c("gid", "ev", "swau", "swauce", "swaurp")		

		return (summary_df)
	}
)


########################
# PRT related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_prt",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_prt")
	}
)


setMethod(f = "compare_gambles_under_prt",
	signature = "Gambles",
	definition = function(object, utility, gamma, digits)
	{	
	
		summary_df <- data.frame()
	
		for (n in 1:length(object@gambles))
		{	
		
			df <- compute_prt(object@gambles[[n]], utility, gamma, digits)

			summary_df <- rbind(summary_df, df)
	
		}
		
		colnames(summary_df) <- c("gid", "ev", "prtu", "prtuce", "prturp")		

		return (summary_df)
	}
)

########################
# gdu related functions
########################	

# declare a custom function to compare gambles
setGeneric(name = "compare_gambles_under_gdu",
	def = function(object, ...)
	{
		standardGeneric("compare_gambles_under_gdu")
	}
)


setMethod(f = "compare_gambles_under_gdu",
	signature = "Gambles",
	definition = function(object, probability_weighting_specification, utility, digits)
	{	
		
		summary_df <- data.frame()
	
		for (n in 1:length(object@gambles))
		{	
		
			df <- compute_gdu(object@gambles[[n]], probability_weighting_specification, utility, digits)

			summary_df <- rbind(summary_df, df)
	
		}
		
		colnames(summary_df) <- c("gid", "ev", "gdu", "gduce", "gdurp")		

		return (summary_df)
	}
)


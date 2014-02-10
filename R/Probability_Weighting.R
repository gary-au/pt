########################
#
# Probability_Weighting, a S4 class
#
########################	


#' The Probability_Weighting class
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{probability_function}:}{Object of class \code{"text"}, containing a text string that specifies the functional form of the probability weighting function.}
#'    \item{\code{parameters}:}{Object of class \code{"vector"}, containing numeric values for the parameter specifications associated with the probability weighting function.}
#'  }
#'
#' @note Probability_Weighting stores both the functional form and parameter specification for a probability weighting function.
#' @name Probability_Weighting 
#' @rdname Probability_Weighting
#' @exportClass Probability_Weighting
#' @aliases Probability_Weighting-class
setClass(
	Class = "Probability_Weighting",
	representation = representation
	(
		probability_function = "character",
		parameters = "vector"
	),
	# check for input consistency when creating new Probability_Weighting objects using "new" constructor
	validity = function(object)
	{
		# run the Probability_Weighting inspector
		if (get_probability_function(object) == "Tversky_Kahneman_1992")
		{
			if (get_number_of_parameters(object) != 1)
			{
				stop(paste(get_probability_function(object), " weighting function requires 1 parameter >= 0.28.\n", sep = ""))
			}
			else if (object@parameters[1] < 0.28)
			{
				stop(paste(get_probability_function(object), " weighting function requires 1 parameter >= 0.28 as the function is not strictly increasing for smaller values.\n", sep = ""))				
			}
		}	
		else if (get_probability_function(object) == "compound_invariance")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}				
		}
		else if (get_probability_function(object) == "linear_in_log_odds")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
		}
		else if (get_probability_function(object) == "neo_additive")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
			else
			{
				a <- object@parameters[1]
				b <- object@parameters[2]
				
				if ((a < 0) | (b < 0) | ((a + b) > 1))
				{
					stop(paste("a >= 0, b >= 0, a + b <= 1.\n", sep = ""))					
				}
			}
		}
		else if (get_probability_function(object) == "exponential_power")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
		}		
		else if (get_probability_function(object) == "power")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
		}
		else if (get_probability_function(object) == "hyperbolic_logarithm")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
		}			
		else if (get_probability_function(object) == "linear")
		{
		}	
		else if (get_probability_function(object) == "constant_relative_sensitivity")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
		}	
		else
		{
			stop(paste(get_probability_function(object), " is an unimplemented probability weighting function\n", sep = ""))			
		}
		

		return (TRUE)
	}
)


#' @name create_probability_weighting
#' @title create_probability_weighting
#' @description Creates an instance of a probability weighting function that can be used as an 
#' input into decision making theories.
#' @details This function creates an instance of a Probability_Weighting class.
#' The following functional forms are currently implemented:
#' 
#' Tversky_Kahneman_1992 (requires 1 parameter > 0.28)
#' 
#' compound_invariance (requires 2 parameters)
#' 
#' linear_in_log_odds (requires 2 parameters)
#' 
#' neo_additive (requires 2 parameters)
#' 
#' exponential_power (requires 2 parameters)
#' 
#' power (requires 2 parameters)
#' 
#' hyperbolic_logarithm (requires 2 parameters)
#' 
#' linear
#' 
#' constant_relative_sensitivity (requires 2 parameters)
#' 
#' @param probability_function text, the probability function string
#' @param parameters vector, a vector of parameters for the probability_function
#' @references
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' Prelec, D. (1998). The probability weighting function. Econometrica, 60(3), 497-528.
#' 
#' Wu, G., & Gonzalez, R. (1996). Curvature of the probability weighting function. Management Science, 42(12), 1676-1690.
#' 
#' Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' 
#' @examples
#' 
#' # This example creates a linear in log odds 
#' # probability weighting function.
#' 
#' linear_in_log_odds_probability_weighting <-
#'	create_probability_weighting(probability_function=
#'	"linear_in_log_odds",
#'	parameters=c(alpha=0.61, beta=0.724))
#' 
#' # These examples create the probability weighting functions
#' # used by Tversky and Kahneman (1992).
#' 
#' tk_1992_positive_probability_weighting <-
#'	create_probability_weighting(probability_function=
#'	"Tversky_Kahneman_1992",
#'	parameters=c(alpha=0.61))
#'	
#'	
#' tk_1992_negative_probability_weighting <-
#'	create_probability_weighting(probability_function=
#'	"Tversky_Kahneman_1992",
#'	parameters=c(alpha=0.69))
#'
#' @export
create_probability_weighting <- function(probability_function, parameters)
{

	if (!missing(parameters))
	{
		new(Class = "Probability_Weighting",
			probability_function = probability_function,
			parameters = parameters
		)		
	}
	else
	{
		new(Class = "Probability_Weighting",
			probability_function = probability_function
		)			
	}

}

# declare a custom function to retrieve value
setGeneric(name = "get_probability_function",
	def = function(object)
	{
		standardGeneric("get_probability_function")
	}
)

# provide implementation of custom function to retrieve value
setMethod(f = "get_probability_function",
	signature = "Probability_Weighting",
	definition = function(object)
	{
		return (object@probability_function)
	}
)

# declare a custom function to retrieve the number of parameters associated with the pwf object
setGeneric(name = "get_number_of_parameters",
	def = function(object)
	{
		standardGeneric("get_number_of_parameters")
	}
)

# provide implementation of custom function
setMethod(f = "get_number_of_parameters",
	signature = "Probability_Weighting",
	definition = function(object)
	{
		return (length(object@parameters))
	}
)

# declare a custom function
setGeneric(name = "compute_probability_weighting",
	def = function(object, ...)
	{
		standardGeneric("compute_probability_weighting")
	}
)

# provide implementation of custom function
setMethod(f = "compute_probability_weighting",
	signature = "Probability_Weighting",
	definition = function(object, probability)
	{
		
		if (get_probability_function(object) == "Tversky_Kahneman_1992")
		{

			weight <- kt_pwf(parameters=object@parameters, probability)
		}
		else if (get_probability_function(object) == "compound_invariance")
		{

			weight <- compound_invariance_pwf(parameters=object@parameters, probability)
		}		
		else if (get_probability_function(object) == "linear_in_log_odds")
		{

			weight <- linear_in_log_odds_pwf(parameters=object@parameters, x=probability)
		}		
		else if (get_probability_function(object) == "neo_additive")
		{	
			weight <- neo_additive_pwf(parameters=object@parameters, x=probability)				
		}
		else if (get_probability_function(object) == "exponential_power")
		{

			weight <- exponential_power_pwf(parameters=object@parameters, x=probability)			
		}
		else if (get_probability_function(object) == "power")
		{

			weight <- power_pwf(parameters=object@parameters, x=probability)			
		}		
		else if (get_probability_function(object) == "hyperbolic_logarithm")
		{

			weight <- hyperbolic_logarithm_pwf(parameters=object@parameters, x=probability)
		}		
		else if (get_probability_function(object) == "linear")
		{

			weight <- linear_pwf(x=probability)
		}
		else if (get_probability_function(object) == "constant_relative_sensitivity")
		{

			weight <- constant_relative_sensitivity_pwf(parameters=object@parameters, x=probability)
		}					
		
	
		return (weight)	
	}
) 
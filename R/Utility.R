########################
#
# Utility, a S4 class
#
########################	

#' The Utility class
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{utility_function}:}{Object of class \code{"text"}, containing a text string that specifies the functional form of the utility function.}
#'    \item{\code{parameters}:}{Object of class \code{"vector"}, containing numeric values for the parameter specifications associated with the utility function.}
#'  }
#'
#' @note Utility stores both the functional form and parameter specification for a utility function.
#' @name Utility 
#' @rdname Utility
#' @exportClass Utility
#' @aliases Utility-class
setClass(
	Class = "Utility",
	representation = representation
	(
		utility_function = "character",
		parameters = "vector"
	),
	# check for input consistency when creating new Utility objects using "new" constructor
	validity = function(object)
	{
		if (get_utility_function(object) == "linear")
		{
			if (get_number_of_utility_parameters(object) != 1)
			{
				stop(paste(get_utility_function(object), " utility function requires 1 parameter.\n", sep = ""))
			}
		}	
		else if (get_utility_function(object) == "exponential")
		{
			if (get_number_of_utility_parameters(object) != 2)
			{
				stop(paste(get_utility_function(object), " utility function requires 2 parameters.\n", sep = ""))
			}				
		}
		else if (get_utility_function(object) == "power")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}
		}
		else if (get_utility_function(object) == "general_power")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}
		}
		else if (get_utility_function(object) == "general_linear")
		{
			if (get_number_of_utility_parameters(object) != 2)
			{
				stop(paste(get_utility_function(object), " utility function requires 2 parameters.\n", sep = ""))
			}
		}		
	
		
		
		return (TRUE)
	}
)

#' @name create_utility
#' @title create_utility
#' @description This function creates an instance of a Utility class.
#' @details 
#' The following functional forms are currently implemented:
#' 
#' linear (requires 1 parameter)
#' 
#' exponential (requires 2 parameters)
#' 
#' power (requires 3 parameters)
#' 
#' general_power (requires 3 parameters)
#' 
#' general_linear (requires 2 parameters)
#' 
#' @param utility_function text, the utility function string
#' @param parameters vector, a vector of parameters for the utility_function
#' @references
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' 
#' Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' @examples
#' 
#' # This example creates a power utility function.
#' 
#' tk_1992_utility <- create_utility(utility_function="power",
#'	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))
#'
#' # This example creates a linear utility function.
#' 
#' my_linear_utility <- create_utility(utility_function="linear",
#' 	parameters=c(lambda=1))
#' 
#' @export
create_utility <- function(utility_function, parameters)
{

	new(Class = "Utility",
		utility_function = utility_function,
		parameters = parameters
	)
}

# declare a custom function to retrieve the number of parameters associated with the utility object
setGeneric(name = "get_number_of_utility_parameters",
	def = function(object)
	{
		standardGeneric("get_number_of_utility_parameters")
	}
)

# provide implementation of custom function
setMethod(f = "get_number_of_utility_parameters",
	signature = "Utility",
	definition = function(object)
	{
		return (length(object@parameters))
	}
)

# declare a custom function to retrieve the utility function character text string
setGeneric(name = "get_utility_function",
	def = function(object)
	{
		standardGeneric("get_utility_function")
	}
)

# provide implementation of custom function
setMethod(f = "get_utility_function",
	signature = "Utility",
	definition = function(object)
	{
		return (object@utility_function)
	}
)

# declare a custom function to compute the utility, given a utility character string and associated parameters
setGeneric(name = "compute_utility",
	def = function(object, ...)
	{
		standardGeneric("compute_utility")
	}
)

# provide implementation of custom function
setMethod(f = "compute_utility",
	signature = "Utility",
	definition = function(object, value)
	{
		
		if (get_utility_function(object) == "linear")
		{
			lambda <- object@parameters[1]			
			
			if (value > 0)
			{		
				utility <- value
			}
			else if (value == 0)
			{
					utility <- 0				
			}			
			else if (value < 0)
			{				
				utility <- -lambda * -value			
			}
		}
		else if (get_utility_function(object) == "exponential")
		{
			# implements Wakker (2010), p.80, Eqn 3.5.4			
			
			alpha <- object@parameters[1]	
			lambda <- object@parameters[2]	
			
			if (value >= 0)
			{
				if (alpha > 0)
				{
					utility <- (1 - exp(-alpha * value))					
				}
				else if (alpha == 0)
				{
					utility <- value
				}
				else if (alpha < 0)
				{
					utility <- (exp(-alpha * value) - 1)						
				}
			}
			else if (value < 0)
			{
				if (alpha > 0)
				{
					utility <- lambda * (1 - exp(-alpha * value))					
				}
				else if (alpha == 0)
				{
					utility <- lambda * value
				}
				else if (alpha < 0)
				{
					utility <- lambda * (exp(-alpha * value) - 1)						
				}				
			}
		
		}
		else if (get_utility_function(object) == "power")
		{
			# implements Wakker (2010), p.78, Eqn 3.5.1
			# and Birnbaum (2008), p.466
			# u(-x) = -lambda * u(x), x >= 0
			
			alpha <- object@parameters[1]
			beta <- object@parameters[2]			
			lambda <- object@parameters[3]
			
		
			if (value > 0)
			{
				if (alpha > 0)
				{
					utility <- value^alpha			
				}
				else if (alpha == 0)
				{
					utility <- log(value)
				}
				else if (alpha < 0)
				{
					utility <- (-value)^alpha
				}				

			}
			else if (value == 0)
			{
					utility <- 0				
			}
			else if (value < 0)
			{
				if (beta > 0)
				{
					utility <- -lambda * (-value)^beta			
				}
				else if (beta == 0)
				{
					utility <- -lambda * log(-value)
				}
				else if (beta < 0)
				{
					utility <- -lambda * (value)^beta
				}				
			}
		}
		else if (get_utility_function(object) == "general_power")
		{
			alpha <- object@parameters[1]				
			beta <- object@parameters[2]
			lambda <- object@parameters[3]			
			
			if (value >= 0)
			{			
				utility <- (beta * value^alpha)
			}
			else if (value < 0)
			{
				utility <- -lambda * (beta * value)^alpha
			}
		}
		else if (get_utility_function(object) == "general_linear")
		{
			
			alpha <- object@parameters[1]			
			lambda <- object@parameters[2]			
			
			if (value >= 0)
			{		
				utility <- alpha * value
			}
			else if (value < 0)
			{				
				utility <- -lambda * alpha * value
			}	
		}		
		
		return (utility)	
	}
)

# declare a custom function
setGeneric(name = "compute_certainty_equivalent",
	def = function(object, ...)
	{
		standardGeneric("compute_certainty_equivalent")
	}
)

# provide implementation of custom function
setMethod(f = "compute_certainty_equivalent",
	signature = "Utility",
	definition = function(object, value)
	{
	
		certainty_equivalent <- 0.0
		
		if (get_utility_function(object) == "linear")
		{
			lambda <- object@parameters[1]			
			
			if (value > 0)
			{			
				certainty_equivalent <- value
			}
			else if (value == 0)
			{
				certainty_equivalent <- 0				
			}			
			else if (value < 0)
			{
				certainty_equivalent <- (-value / -lambda)
			}			
		}
		else if (get_utility_function(object) == "exponential")
		{
			alpha <- object@parameters[1]			
			lambda <- object@parameters[2]	
			
			if (value >= 0)
			{
				if (alpha > 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(1 - value)					
				}
				else if (alpha == 0)
				{
					certainty_equivalent <- value
				}
				else if (alpha < 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(value + 1)					
				}
			}
			else
			{
				if (alpha > 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(1 + value/lambda)					
				}
				else if (alpha == 0)
				{
					certainty_equivalent <- -value/lambda
				}
				else if (alpha < 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(-value/lambda + 1)					
				}
			}
		}		
		else if (get_utility_function(object) == "power")
		{
			alpha <- object@parameters[1]
			beta <- object@parameters[2]			
			lambda <- object@parameters[3]			
			
			if (value > 0)
			{
				if (alpha > 0)
				{
					certainty_equivalent <- value^(1 / alpha)			
				}
				else if (alpha == 0)
				{
					certainty_equivalent <- exp(value)
				}
				else if (alpha < 0)
				{
					certainty_equivalent <- -1 * value^(1 / alpha)
				}					
			}
			else if (value == 0)
			{
				certainty_equivalent <- 0
			}
			else if (value < 0)
			{
				if (beta > 0)
				{
					certainty_equivalent <- -1.0 * (-value / lambda)^(1 / beta)			
				}
				else if (beta == 0)
				{
					certainty_equivalent <- -1.0 * exp(-value / lambda)
				}
				else if (beta < 0)
				{
					certainty_equivalent <- (1 - (value/lambda + 1)^(1 / beta))
				}			
			}
		}	
		else if (get_utility_function(object) == "general_power")
		{
			if (value >= 0)
			{
				alpha <- object@parameters[1]
				beta <- object@parameters[2]
				certainty_equivalent <- (value / beta)^(1.0 / alpha)				
			}
			else if (value < 0)
			{
				alpha <- object@parameters[1]				
				beta <- object@parameters[2]
				lambda <- object@parameters[3]
				certainty_equivalent <- (-value / (beta * lambda))^(1.0 / alpha)		
			}
		}
		else if (get_utility_function(object) == "general_linear")
		{
			alpha <- object@parameters[1]			
			lambda <- object@parameters[2]			
			
			if (value >= 0)
			{		
				certainty_equivalent <- value / alpha
			}
			else if (value < 0)
			{				
				certainty_equivalent <- -value/(lambda * alpha)			
			}			
		}		
	
		
		return (certainty_equivalent)
	}
)
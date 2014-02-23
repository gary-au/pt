########################
#
# routines to compute and plot utility functions and probability weighting functions
#
########################


########################
# probability weighting functions
########################	

#' @name linear_pwf
#' @title The linear probability weighting function.
#' @description Linear probability weighting function.
#' @param x numeric, the x value
#' @export
linear_pwf <- function(x)
{
	return (x)
}

#' @name kt_pwf
#' @title The Tversky and Kahneman (1992) probability weighting function.
#' @description Tversky and Kahneman's (1992) probability weighting function.
#' @references
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' @param par vector, a vector containing parameters for the pwf
#' @param x numeric, the x value
#' @export
kt_pwf <- function(par, x)
{
	if (length(par) == 1)
	{
		alpha <- par[1]
	
		return (x^alpha / ((x^alpha + (1 - x)^alpha)^(1/alpha)))		
	}
	else
	{
		stop("kt_pwf should have one parameter.")
	}
}

#' @name linear_in_log_odds_pwf
#' @title The linear in log odds probability weighting function.
#' @description Linear in log odds probability weighting function.
#' @references
#' Wu, G., & Gonzalez, R. (1996). Curvature of the probability weighting function. Management Science, 42(12), 1676-1690.
#' @param par vector, a vector containing parameters for the pwf
#' @param x numeric, the x value
#' @export
linear_in_log_odds_pwf <- function(par, x)
{
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		return (beta * x^alpha / (beta * x^alpha + (1 - x)^alpha))
	}
	else
	{
		stop("linear_in_log_odds_pwf should have two parameters.")
	}
}

#' @name power_pwf
#' @title The power probability weighting function.
#' @description Power probability weighting function.
#' @references
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' @param par vector, a vector containing parameters for the pwf
#' @param x numeric, the x value
#' @export
power_pwf <- function(par, x)
{
	if (length(par) == 2)
	{
		alpha <- par[1]	
		beta <- par[2]
		
		return (beta * x^alpha)		
	}
	else
	{
		stop("power_pwf should have two parameters.")
	}	
}

#' @name neo_additive_pwf
#' @title The neo-additive probability weighting function.
#' @description Neo-additive probability weighting function.
#' @references
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' @param par vector, a vector containing parameters for the pwf
#' @param x numeric, the x value
#' @export
neo_additive_pwf <- function(par, x)
{	
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		return (beta + alpha*x)
	}
	else
	{
		stop("neo_additive_pwf should have two parameters.")
	}	
}

#' @name exponential_power_pwf
#' @title The exponential power probability weighting function.
#' @description Exponential-power probability weighting function.
#' @references
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' @param par vector, a vector containing parameters for the pwf
#' @param x numeric, the x value
#' @export
exponential_power_pwf <- function(par, x)
{	
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		return (exp(-alpha/beta * (1-x)^beta))
	}
	else
	{
		stop("exponential_power_pwf should have two parameters.")
	}		
}

#' @name hyperbolic_logarithm_pwf
#' @title The hyperbolic logarithm probability weighting function.
#' @description Hyperbolic-logarithm probability weighting function.
#' @references
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' @param par vector, a vector containing parameters for the pwf
#' @param x numeric, the x value
#' @export
hyperbolic_logarithm_pwf <- function(par, x)
{
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		return ((1 - alpha * log(x))^(-beta/alpha))
	}
	else
	{
		stop("hyperbolic_logarithm should have two parameters.")
	}		
}

#' @name compound_invariance_pwf
#' @title The compound invariance probability weighting function.
#' @description Compound invariance probability weighting function.
#' @references
#' Prelec, D. (1998). The probability weighting function. Econometrica, 60(3), 497-528.
#' @param par vector, a vector containing parameters for the pwf
#' @param x numeric, the x value
#' @export
compound_invariance_pwf <- function(par, x)
{

	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		return ((exp(-beta * (-log(x))^alpha)))
	}
	else
	{
		stop("compound_invariance_pwf should have two parameters.")
	}	
}

#' @name constant_relative_sensitivity_pwf
#' @title The constant relative sensitivity probability weighting function.
#' @description Constant relative sensitivity probability weighting function.
#' @references
#' Abdellaoui, M., L'Haridon, O., & Zank, H. (2010). Separating curvature and elevation: A parametric probability weighting function. Journal of Risk and Uncertainty, 41(1), 39-65.
#' @param par vector, parameters for the probability weighting function.
#' @param x numeric, the x value
#' @export
constant_relative_sensitivity_pwf <- function(par, x)
{
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		if (x <= beta)
		{
			return (beta^(1-alpha)*x^alpha)
		}
		else
		{
			return (1 - (1 - beta)^(1-alpha)*(1-x)^alpha)		
		}
	}
	else
	{
		stop("constant_relative_sensitivity_pwf should have two parameters.")
	}	
}


########################
# utility functions
########################	

#' @name linear_uf
#' @title The linear utility function.
#' @description Linear utility function.
#' @param par vector, parameters for the utility function.
#' @param x numeric, the x value
#' @export
linear_uf <- function(par, x)
{	
	if (length(par) == 1)
	{
		lambda <- par[1]
	
		value <- ifelse(x < 0, -lambda * (-x), x)
		
		return (value)		
	}
	else
	{
		stop("linear_uf should have one parameter.")
	}	
}

#' @name power_uf
#' @title The power utility function.
#' @description Power utility function.
#' @param par vector, parameters for the utility function.
#' @param x numeric, the x value 
#' @export
power_uf <- function(par, x)
{
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]
		lambda <- par[3]
		
		value <- ifelse(x < 0, -lambda * (-x)^beta, x^alpha)
		
		return (value)
	}
	else
	{
		stop("power_uf should have three parameters.")
	}
}

#' @name exponential_uf
#' @title The exponential utility function.
#' @description Exponential utility function.
#' @param par vector, parameters for the utility function.
#' @param x numeric, the x value
#' @export
exponential_uf <- function(par, x)
{	
	if (length(par) == 2)
	{
		alpha <- par[1]
		
		value <- ifelse(alpha > 0, (1 - exp(-alpha *x)), ifelse(alpha < 0, exp(-alpha * x) - 1, x))
		
		return (value)
	}
	else
	{
		stop("exponential_uf should have three parameters.")
	}
}

#' @name quadratic_uf
#' @title The quadratic utility function.
#' @description Quadratic utility function.
#' @param par vector, parameters for the utility function.
#' @param x numeric, the x value
#' @export
quadratic_uf <- function(par, x)
{	
	if (length(par) == 2)
	{
		alpha <- par[1]
		lambda <- par[2]	
		
		value <- ifelse(x < 0, -lambda * (alpha * (-x) - (-x)^2), alpha * x - x^2)
		
		return (value)
	}
	else
	{
		stop("quadratic_uf should have two parameters.")
	}	
}

#' @name logarithmic_uf
#' @title The logarithmic utility function.
#' @description Logarithmic utility function.
#' @param par vector, parameters for the utility function.
#' @param x numeric, the x value
#' @export
logarithmic_uf <- function(par, x)
{	
	if (length(par) == 2)
	{
		alpha <- par[1]
		lambda <- par[2]	
		
		value <- ifelse(x < 0, -lambda * (log(alpha + (-x))), log(alpha + x))
		
		return (value)
	}
	else
	{
		stop("logarithmic_uf should have two parameters.")
	}		
}


########################
# plot functions
########################	

#' @name plotProbW
#' @title Plot a probability weighting function.
#' @description Plot a probability weighting function using base graphics.
#' @param my_title text, the title
#' @param my_title_colour text, the title colour
#' @param my_title_font_size numeric, the title font size
#' @param my_x_label text, my_x_label
#' @param my_y_label text, the my_y_label
#' @param pwf function, the pwf
#' @param par vector, the pwf_parameters
#' @param draw_reference_line_flag logical, draw_reference_line_flag
#' @param reference_line_colour text, reference_line_colour
#' @param reference_line_style text, reference_line_style
#' @param my_labels vector, labels
#' @param my_label_positions vector, the coordinates for the labels
#' @param font_scaling numeric, the scaling factor for the labels
#' @param arrow_positions vector, the positions of arrow lines
#' @param window_width numeric, the window_width
#' @param window_height numeric, the window_height
#' @examples
#' 
#' plotProbW(my_title=expression(paste("Kahneman & Tversky (1992), ",
#' 	c==0.61)),
#' 	my_title_colour="black", my_title_font_size=4,
#' 	my_x_label = "p", my_y_label = "w(p)",
#' 	pwf=kt_pwf, par=c(c=0.61),
#' 	draw_reference_line_flag=TRUE, reference_line_colour="red",
#' 	reference_line_style="dotted",
#' 	my_labels=c(expression(paste(w(italic(p)) == frac(italic(p)^c,
#' 	(italic(p)^c + (1-italic(p))^c)^(1/c))))),
#' 	my_label_positions=list(c(0.4,0.8)),
#' 	font_scaling=1.0,
#' 	window_width=7, window_height=7)
#' 	
#' @export
plotProbW <- function(my_title, my_title_colour, my_title_font_size,
	my_x_label, my_y_label, 
	pwf, par,
	draw_reference_line_flag, reference_line_colour, reference_line_style, 
	my_labels, my_label_positions, font_scaling, arrow_positions,	
	window_width, window_height)
{
	if (.Platform$OS.type == "windows")
	{
		options(device=windows)
	}
	else if (.Platform$OS.type == "unix")
	{
		options(device=X11)		
	}
	else if (.Platform$OS.type == "apple")
	{
		options(device=quartz)		
	}	
	
	dev.new(window_width, window_height)
	plot.new()
	
	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
		
	FUN <- match.fun(pwf)

	# plot axes
	plot(x = 0:1,
		y = 0:1,
		lty = 1,
		xaxp = c(0, 1, 10),
		yaxp = c(0, 1, 10),		
		xaxs = "i",	#let the x and y axes intersect at the origin
		yaxs = "i",
		bty = "n",
		xlab = my_x_label, 
		ylab = my_y_label,
		pty = "s")		
	
	curve(FUN(par=par, x),
		from = 0, 
		to = 1, 
		n = 1000,
		xlim = c(0, 1), 
		ylim = c(0, 1),
		xaxs = "i",	# let the x and y axes intersect at the origin
		yaxs = "i",	
		xaxp = c(0, 1, 10),	# specify x axis intervals from 0 to 1 by 0.1
		yaxp = c(0, 1, 10),	# specify y axis intervals from 0 to 1 by 0.1	
		col = "Black",
		add = TRUE,
		asp = 1)	# set 1-1 aspect ratio	
	
	# draw grey axis
	abline(h = 0, v = 0, col = "black")

	# these need to come after the draw curve 
	
	if (draw_reference_line_flag == TRUE)
	{
		# draw a reference line	
		abline(a = 0, b = 1, lty = reference_line_style, col = reference_line_colour)
	}
		
	# draw text labels for selected lines to improve clarity
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]		
			
			text(x=position_x, y=position_y, labels = my_labels[index], col="black", cex = font_scaling)		
		}		
	}
	
	if (!missing(arrow_positions))
	{
		for(index in 1:length(arrow_positions))
		{
			position <- arrow_positions[[index]]
			
			start_position_x <- position[1]
			start_position_y <- position[2]		
			end_position_x <- position[3]
			end_position_y <- position[4]	
			
			grid::grid.move.to(x = start_position_x, y = start_position_y, default.units = "npc", name = NULL, draw = TRUE, vp = NULL)
			grid::grid.line.to(x = end_position_x, y = end_position_y, default.units = "npc",
				arrow=grid::arrow(angle = 30, length = grid::unit(0.01, "npc"),
      			ends = "last", type = "open"), name = NULL,
             		gp = grid::gpar(), draw = TRUE, vp = NULL)			
		}	
	}	

	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}
	
	return (invisible())
}

#' @name plotProbWFam
#' @title Plot a family of two parameter probability weighting functions.
#' @description Plot a family of two parameter probability weighting functions using base graphics.
#' @param my_title text, the title
#' @param my_title_colour text, the title colour
#' @param my_title_font_size numeric, the title font size
#' @param my_x_label text, my_x_label
#' @param my_y_label text, the my_y_label
#' @param pwf function, the pwf
#' @param par vector, the pwf_parameters
#' @param draw_reference_line_flag logical, draw_reference_line_flag
#' @param reference_line_colour text, reference_line_colour
#' @param reference_line_style text, reference_line_style
#' @param my_labels vector, labels
#' @param my_label_positions vector, the coordinates for the labels
#' @param font_scaling numeric, the scaling factor for the labels
#' @param arrow_positions vector, the positions of arrow lines
#' @param window_width numeric, the window_width
#' @param window_height numeric, the window_height
#' @examples
#' 
#' plotProbWFam(my_title=expression(paste("linear in log odds, ",
#' 	gamma == 0.6)),
#' 	my_title_colour="black", my_title_font_size=4,
#' 	my_x_label = "p", my_y_label = "w(p)", pwf=linear_in_log_odds_pwf,
#' 	par=list(a_list=c(0.6), b_list=seq(from=0.2, to=1.8, by=0.06)),
#' 	draw_reference_line_flag=TRUE, reference_line_colour="red",
#' 	reference_line_style="dotted",
#' 	my_labels=c(expression(paste(delta == 0.2)),
#' 	expression(paste(delta == 1.8)),
#' 	expression(paste(w(italic(p)) == frac(delta * italic(p)^gamma,
#' 	delta * italic(p)^gamma + (1-italic(p))^gamma)))),
#' 	my_label_positions=list(c(0.7,0.09),c(0.2,0.6),c(0.42, 0.9)),
#' 	font_scaling=1.0,
#' 	arrow_positions = list(c(0.34,0.59,0.6,0.67),c(0.7,0.23,0.75,0.35)),
#' 	window_width=7, window_height=7)
#' 	
#' @export
plotProbWFam <- function(my_title, my_title_colour, my_title_font_size,
	my_x_label, my_y_label, 
	pwf, par, 
	draw_reference_line_flag, reference_line_colour, reference_line_style, 
	my_labels, my_label_positions, font_scaling, arrow_positions, 
	window_width, window_height)
{
	if (.Platform$OS.type == "windows")
	{
		options(device=windows)
	}
	else if (.Platform$OS.type == "unix")
	{
		options(device=X11)		
	}
	else if (.Platform$OS.type == "apple")
	{
		options(device=quartz)		
	}
	
	dev.new(window_width, window_height)
	plot.new()	
	
	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
	
	FUN <- match.fun(pwf)		
	
	# plot axes
	plot(x = 0:1,
		y = 0:1,
		lty = 1,
		xaxp = c(0, 1, 10),
		yaxp = c(0, 1, 10),		
		xaxs = "i",	#let the x and y axes intersect at the origin
		yaxs = "i",
		bty = "n",
		xlab = my_x_label, 
		ylab = my_y_label,
		pty = "s")
	
	p1_list <- par[[1]]
	p2_list <- par[[2]]
	
	for(p1 in p1_list)
	{

		for(p2 in p2_list)
		{
			params <- c(a=p1, b=p2)
		
			curve(FUN(params, x),
				from = 0, 
				to = 1, 
				n = 1000,
				xlim = c(0, 1), 
				ylim = c(0, 1),
				xaxs = "i",	# let the x and y axes intersect at the origin
				yaxs = "i",	
				xaxp = c(0, 1, 10),	# specify x axis intervals from 0 to 1 by 0.1
				yaxp = c(0, 1, 10),	# specify y axis intervals from 0 to 1 by 0.1	
				col = "Black",
				add = TRUE,
				asp = 1)	# set 1-1 aspect ratio		
		}
	}


	# these need to come after the draw curve 
	
	if (draw_reference_line_flag == TRUE)
	{
		# draw a reference line	
		abline(a = 0, b = 1, lty = reference_line_style, col = reference_line_colour)
	}

	
	# draw text labels for selected lines to improve clarity
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]		
			
			text(x=position_x, y=position_y, labels = my_labels[index], col="black", cex = font_scaling)		
		}		
	}
	
	if (!missing(arrow_positions))
	{
		for(index in 1:length(arrow_positions))
		{
			position <- arrow_positions[[index]]
			
			start_position_x <- position[1]
			start_position_y <- position[2]		
			end_position_x <- position[3]
			end_position_y <- position[4]	
			
			grid::grid.move.to(x = start_position_x, y = start_position_y, default.units = "npc", name = NULL, draw = TRUE, vp = NULL)
			grid::grid.line.to(x = end_position_x, y = end_position_y, default.units = "npc",
				arrow=grid::arrow(angle = 30, length = grid::unit(0.01, "npc"),
      			ends = "last", type = "open"), name = NULL,
             		gp = grid::gpar(), draw = TRUE, vp = NULL)			
		}	
	}
	
	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}

	return (invisible())
}

#' @name plotUtility
#' @title Plot a utility function.
#' @description Plot the utility function.
#' @param my_title text, the title of the chart.
#' @param my_title_colour text, the title colour.
#' @param my_title_font_size numeric, the title font size.
#' @param my_x_label text, the x-axis label.
#' @param xmin numeric, the xmin on the x-axis.
#' @param xmax numeric, the xmax on the x-axis.
#' @param my_y_label text, the y-axis label.
#' @param fun Utility, an instance of the Utility class.
#' @param par vector, the parameters for the utility function.
#' @param fun_colour text, the colour of the utility function line.
#' @param draw_reference_line_flag logical, a boolean flag determining whether or not to draw a y=x reference line.
#' @param reference_line_colour text, the reference line colour.
#' @param reference_line_style numeric, the reference line style.
#' @param my_labels vector, a vector of text labels to draw.
#' @param my_label_positions list, a list of coordinates for the text labels.
#' @param my_label_colours vector, stores the colours for each text label.
#' @param my_label_font_sizes vector, stores the font size of each text label. 
#' @param window_width numeric, the window_width
#' @param window_height numeric, the window_height
#' @examples
#' 
#' plotUtility(my_x_label = "objective consequence",
#' 	my_y_label = "subjective value",
#' 	xmin = -10, xmax = 10, 
#' 	fun=power_uf,
#' 	par=c(alpha = 0.88, beta = 0.88, lambda = 2.25),
#' 	fun_colour = "purple",
#' 	draw_reference_line_flag = TRUE,
#' 	reference_line_colour = "red",
#' 	reference_line_style = 1,
#' 	window_width = 5, window_height = 5)
#' 
#' @export
plotUtility <- function(my_title, my_title_colour, my_title_font_size, 
	my_x_label, xmin, xmax, my_y_label, 
	fun, par, 
	fun_colour, 
	draw_reference_line_flag, reference_line_colour, reference_line_style, 
	my_labels, my_label_positions, my_label_colours, my_label_font_sizes,
	window_width, window_height)
{
	if (.Platform$OS.type == "windows")
	{
		options(device=windows)
	}
	else if (.Platform$OS.type == "unix")
	{
		options(device=X11)		
	}
	else if (.Platform$OS.type == "apple")
	{
		options(device=quartz)		
	}
	
	dev.new(window_width, window_height)
	plot.new()
	
	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
		
	FUN <- match.fun(fun)

	curve(FUN(par, x),
		from = xmin,
		to = xmax,
		add = FALSE,
		col = fun_colour,		
		type = "l",
		xlab = my_x_label,
		ylab = my_y_label,
		xaxs = "i",	# let the x and y axes intersect at the origin
		yaxs = "i",		
		n = 2000)
	
	# draw grey axis
	abline(h = 0, v = 0, col = "black")
	
	if (draw_reference_line_flag == TRUE)
	{
		# draw a reference line	
		abline(a = 0, b = 1, lty = reference_line_style, col = reference_line_colour)
	}
	
	# draw text labels for selected lines to improve clarity
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]		
			
			text(x=position_x, y=position_y, labels = my_labels[index], col=my_label_colours[index], cex = my_label_font_sizes[index])		
		}		
	}	
	
	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}
	
	return (invisible())
}


#' @name plotRP
#' @title Plot the risk premium.
#' @description Plot the risk premium.
#' @param my_title text, the title
#' @param my_title_colour text, the title colour
#' @param my_title_font_size numeric, the title font size
#' @param my_x_label text, my_x_label
#' @param xmin numeric, the xmin
#' @param xmax numeric, the xmax
#' @param my_y_label text, the my_y_label
#' @param my_color text, the line color
#' @param fun function, the utility function
#' @param par vector, the uf_parameters
#' @param ev numeric, the expected value
#' @param eu numeric, the expected utility
#' @param ce numeric, the certainty equivalent
#' @param my_labels vector, text labels
#' @param my_label_colors vector, colors of the text labels
#' @param my_label_positions vector, positions of the text labels
#' @param font_scaling numeric, the scaling of the text labels
#' @param window_width numeric, the window_width
#' @param window_height numeric, the window_height
#' @examples
#' 
#' choice_ids <- c(1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 1, 2, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(4000, 0, 3000,
#' 4000, 0, 3000, 0)
#' 
#' probability_strings <- c("0.8", "0.2", "1.0",
#' "0.2", "0.8", "0.25", "0.75")
#' 
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids,
#' 	outcome_ids=outcome_ids,
#' 	objective_consequences=objective_consequences,
#' 	probability_strings=probability_strings)
#' 
#' my_choices
#' 
#' my_utility <- Utility(fun="power",
#' par=c(alpha=0.88, beta=0.88, lambda=1))
#' eu_df <- compareEU(my_choices, utility=my_utility, digits=4)
#' eu_df
#' 
#' ev <- as.numeric(eu_df$ev[1])
#' eu <- as.numeric(eu_df$eu[1])
#' ce <- as.numeric(eu_df$ce[1])
#' 
#' plotRP(my_title = "risk premium",
#' 	my_title_colour="black", 
#' 	my_title_font_size=4,
#' 	my_x_label = "objective consequence",
#' 	my_y_label = "subjective value", 
#' 	xmin = 2500, xmax = 3500,
#' 	my_color="violet",
#' 	fun=power_uf,
#' 	par=c(alpha=0.88, beta=0.88, lambda=1),
#' 	ev=ev, eu=eu, ce=ce,
#' 	my_labels=c(expression(paste(U(x)==x^alpha, ",
#' 	", x>=0)),
#' 	expression(paste(plain()==-lambda * x^beta, ", ", x<0)),
#' 	"ev","eu","ce","rp"),
#' 	my_label_colors=c("violet","violet","black","red","orange","blue"),
#' 	my_label_positions=list(c(2700,1275),c(2740,1250),c(3250,1075),
#' 	c(2800,1170),c(3050,1075),c(3150,1170)),
#' 	font_scaling=1, 
#' 	window_width = 7, window_height = 7)
#' 
#' @export
plotRP <- function(my_title, my_title_colour, my_title_font_size, 
	my_x_label, xmin, xmax, my_y_label, my_color, 
	fun, par, ev, eu, ce, 
	my_labels, my_label_colors, my_label_positions, font_scaling, 
	window_width, window_height)
{
	if (.Platform$OS.type == "windows")
	{
		options(device=windows)
	}
	else if (.Platform$OS.type == "unix")
	{
		options(device=X11)		
	}
	else if (.Platform$OS.type == "apple")
	{
		options(device=quartz)		
	}
	
	dev.new(window_width, window_height)
	plot.new()
	
	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
		
	FUN <- match.fun(fun)

	curve(FUN(par, x),
		from = xmin,
		to = xmax,
		add = FALSE,
		col = my_color,		
		type = "l",
		xlab = my_x_label,
		ylab = my_y_label,
		xaxs = "i",	# let the x and y axes intersect at the origin
		yaxs = "i",		
		n = 2000)
	
	# draw ev vertical line	
	x0 <- ev
	y0 <- 0
	x1 <- ev	
	y1 <- FUN(par, x0)
	
	segments(x0, y0, x1, y1, lty="dashed")
	
	# draw eu horizontal line	
	x0 <- 0
	y0 <- eu
	x1 <- ce
	y1 <- eu
	
	segments(x0, y0, x1, y1, col="red", lty="dashed")	
	
	# draw ce vertical line	
	x0 <- ce
	y0 <- 0
	x1 <- ce
	y1 <- eu	
	segments(x0, y0, x1, y1, col="orange", lty="dashed")		

	# draw rp horizontal line
	x0 <- ce
	y0 <- eu
	x1 <- ev
	y1 <- eu	
	segments(x0, y0, x1, y1, col = "blue", lwd=2)	
	
	# draw grey axis
	abline(h = 0, v = 0, col = "black")
	
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]
			
			text_color <- my_label_colors[index]
			
			text(x=position_x, y=position_y, labels = my_labels[index], col = text_color, cex = font_scaling)	
			
		}		
	}
	
	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}
	
	return (invisible())
}
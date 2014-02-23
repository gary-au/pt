\name{plotProbWFam}
\alias{plotProbWFam}
\title{Plot a family of two parameter probability weighting functions.}
\usage{
plotProbWFam(my_title, my_title_colour, my_title_font_size, my_x_label,
  my_y_label, pwf, par, draw_reference_line_flag, reference_line_colour,
  reference_line_style, my_labels, my_label_positions, font_scaling,
  arrow_positions, window_width, window_height)
}
\arguments{
  \item{my_title}{text, the title}

  \item{my_title_colour}{text, the title colour}

  \item{my_title_font_size}{numeric, the title font size}

  \item{my_x_label}{text, my_x_label}

  \item{my_y_label}{text, the my_y_label}

  \item{pwf}{function, the pwf}

  \item{par}{vector, the pwf_parameters}

  \item{draw_reference_line_flag}{logical,
  draw_reference_line_flag}

  \item{reference_line_colour}{text, reference_line_colour}

  \item{reference_line_style}{text, reference_line_style}

  \item{my_labels}{vector, labels}

  \item{my_label_positions}{vector, the coordinates for the
  labels}

  \item{font_scaling}{numeric, the scaling factor for the
  labels}

  \item{arrow_positions}{vector, the positions of arrow
  lines}

  \item{window_width}{numeric, the window_width}

  \item{window_height}{numeric, the window_height}
}
\description{
Plot a family of two parameter probability weighting
functions using base graphics.
}
\examples{
plotProbWFam(my_title=expression(paste("linear in log odds, ",
	gamma == 0.6)),
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "p", my_y_label = "w(p)", pwf=linear_in_log_odds_pwf,
	par=list(a_list=c(0.6), b_list=seq(from=0.2, to=1.8, by=0.06)),
	draw_reference_line_flag=TRUE, reference_line_colour="red",
	reference_line_style="dotted",
	my_labels=c(expression(paste(delta == 0.2)),
	expression(paste(delta == 1.8)),
	expression(paste(w(italic(p)) == frac(delta * italic(p)^gamma,
	delta * italic(p)^gamma + (1-italic(p))^gamma)))),
	my_label_positions=list(c(0.7,0.09),c(0.2,0.6),c(0.42, 0.9)),
	font_scaling=1.0,
	arrow_positions = list(c(0.34,0.59,0.6,0.67),c(0.7,0.23,0.75,0.35)),
	window_width=7, window_height=7)
}


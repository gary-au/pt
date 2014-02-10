\name{plot_utility_function}
\alias{plot_utility_function}
\title{plot_utility_function}
\usage{
  plot_utility_function(my_title, my_title_colour,
    my_title_font_size, my_x_label, xmin, xmax, my_y_label,
    utility_function, parameters, utility_function_colour,
    draw_reference_line_flag, reference_line_colour,
    reference_line_style, my_labels, my_label_positions,
    my_label_colours, my_label_font_sizes, window_width,
    window_height)
}
\arguments{
  \item{my_title}{text, the title of the chart.}

  \item{my_title_colour}{text, the title colour.}

  \item{my_title_font_size}{numeric, the title font size.}

  \item{my_x_label}{text, the x-axis label.}

  \item{xmin}{numeric, the xmin on the x-axis.}

  \item{xmax}{numeric, the xmax on the x-axis.}

  \item{my_y_label}{text, the y-axis label.}

  \item{utility_function}{Utility, an instance of the
  Utility class.}

  \item{parameters}{vector, the parameters for the
  utility_function.}

  \item{utility_function_colour}{text, the colour of the
  utility_function.}

  \item{draw_reference_line_flag}{logical, a boolean flag
  determining whether or not to draw a y=x reference line.}

  \item{reference_line_colour}{text, the reference line
  colour.}

  \item{reference_line_style}{numeric, the reference line
  style.}

  \item{my_labels}{vector, a vector of text labels to
  draw.}

  \item{my_label_positions}{list, a list of coordinates for
  the text labels.}

  \item{my_label_colours}{vector, stores the colours for
  each text label.}

  \item{my_label_font_sizes}{vector, stores the font size
  of each text label.}

  \item{window_width}{numeric, the window_width}

  \item{window_height}{numeric, the window_height}
}
\description{
  Plot the utility function.
}
\examples{
plot_utility_function(my_x_label = "objective consequence",
	my_y_label = "subjective value",
	xmin = -10, xmax = 10,
	utility_function=power_uf,
	parameters=c(alpha = 0.88, beta = 0.88, lambda = 2.25),
	utility_function_colour = "purple",
	draw_reference_line_flag = TRUE,
	reference_line_colour = "red",
	reference_line_style = 1,
	window_width = 5, window_height = 5)
}


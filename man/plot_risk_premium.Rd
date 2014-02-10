\name{plot_risk_premium}
\alias{plot_risk_premium}
\title{plot_risk_premium}
\usage{
  plot_risk_premium(my_title, my_title_colour,
    my_title_font_size, my_x_label, xmin, xmax, my_y_label,
    my_color, utility_function, parameters, ev, eu, ce,
    my_labels, my_label_colors, my_label_positions,
    font_scaling, window_width, window_height)
}
\arguments{
  \item{my_title}{text, the title}

  \item{my_title_colour}{text, the title colour}

  \item{my_title_font_size}{numeric, the title font size}

  \item{my_x_label}{text, my_x_label}

  \item{xmin}{numeric, the xmin}

  \item{xmax}{numeric, the xmax}

  \item{my_y_label}{text, the my_y_label}

  \item{my_color}{text, the line color}

  \item{utility_function}{function, the utility function}

  \item{parameters}{vector, the uf_parameters}

  \item{ev}{numeric, the expected value}

  \item{eu}{numeric, the expected utility}

  \item{ce}{numeric, the certainty equivalent}

  \item{my_labels}{vector, text labels}

  \item{my_label_colors}{vector, colors of the text labels}

  \item{my_label_positions}{vector, positions of the text
  labels}

  \item{font_scaling}{numeric, the scaling of the text
  labels}

  \item{window_width}{numeric, the window_width}

  \item{window_height}{numeric, the window_height}
}
\description{
  Plot the risk premium.
}
\examples{
choice_id_vector <- c(1, 1, 1, 2, 2, 2, 2)

gamble_id_vector <- c(1, 1, 2, 1, 1, 2, 2)

outcome_id_vector <- c(1, 1, 2, 1, 2, 1, 2)

objective_consequence_vector <- c(4000, 0, 3000,
4000, 0, 3000, 0)

probability_string_vector <- c("0.8", "0.2", "1.0",
"0.2", "0.8", "0.25", "0.75")

my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector,
	outcome_id_vector=outcome_id_vector,
	objective_consequence_vector=objective_consequence_vector,
	probability_string_vector=probability_string_vector)

my_choices

my_utility <- create_utility(utility_function="power",
parameters=c(alpha=0.88, beta=0.88, lambda=1))
eu_df <- compareEU(my_choices, utility=my_utility, digits=4)
eu_df

ev <- as.numeric(eu_df$ev[1])
eu <- as.numeric(eu_df$eu[1])
ce <- as.numeric(eu_df$euce[1])

plot_risk_premium(my_title = "risk premium",
	my_title_colour="black",
	my_title_font_size=4,
	my_x_label = "objective consequence",
	my_y_label = "subjective value",
	xmin = 2500, xmax = 3500,
	my_color="violet",
	utility_function=power_uf,
	parameters=c(alpha=0.88, beta=0.88, lambda=1),
	ev=ev, eu=eu, ce=ce,
	my_labels=c(expression(paste(U(x)==x^alpha, ",
	", x>=0)),
	expression(paste(plain()==-lambda * x^beta, ", ", x<0)),
	"ev","eu","ce","rp"),
	my_label_colors=c("violet","violet","black","red","orange","blue"),
	my_label_positions=list(c(2700,1275),c(2740,1250),c(3250,1075),
	c(2800,1170),c(3050,1075),c(3150,1170)),
	font_scaling=1,
	window_width = 7, window_height = 7)
}


library("pt")

########################	
# Birnbaum, M. H., & Navarrete, J. B. (1998). Testing descriptive utility theories: Violations of stochastic dominance and cumulative independence. Journal of Risk and Uncertainty, 17(1), 49-79.
#
# Birnbaum, M. H. (1997). Violations of monotonicity in judgment and decision making. In A. A. J. Marley (Ed.), Choice, decision, and measurement: Essays in honor of R. Duncan Luce (pp. 73-100). Mahwah, NJ: Erlbaum.
# on the violation of stochastic dominance recipe
########################

my_list <- create_vsd_choices(x=12, y=96, p="0.1", q="0.9", x_plus=14, y_minus=90, r="0.05")
my_list

########################	
#
# p.58
#
########################	
# G+=(12, 0.05; 14, 0.05; 96, 0.9)
# G-=(12, 0.1; 90, 0.05; 96, 0.85)

#CWU
#CE(G+)=56.99 < CE(G-)=63.23

#PT
#CE(G+)=70.27 > CE(G-)=69.73

choice_id_vector <- c(1, 1, 1, 1, 1, 1)
gamble_id_vector <- c(1, 1, 1, 2, 2, 2)
outcome_id_vector <- c(1, 2, 3, 1, 2, 3)
objective_consequence_vector <- c(12, 14, 96, 12, 90, 96)
probability_string_vector <- 
	c("0.05", "0.05", "0.9", "0.1", "0.05", "0.85")
my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector, 
	outcome_id_vector=outcome_id_vector, 
	objective_consequence_vector=objective_consequence_vector, 
	probability_string_vector=probability_string_vector)
my_choices

my_pwf <- 
	create_probability_weighting(probability_function="power",
		parameters=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=1, beta=1, lambda=1))
compareTAX(my_choices, 
	probability_weighting_specification=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid   ev   tax taxce taxrp
# 1   1   1 87.7 45.77 45.77 41.93
# 2   1   2 87.3  63.1  63.1  24.2

branch_weighting_vector_list <- list(
	c(0.37, 0.63),
	c(0.16, 0.33, 0.51))
my_pwf <- 
	create_probability_weighting(probability_function="power",
		parameters=c(alpha=0.6, beta=1))
my_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=1, beta=1, lambda=1))
compareRAM(my_choices, 
	branch_weighting_vector_list=branch_weighting_vector_list, 
	probability_weighting_specification=my_pwf, 
	utility=my_utility, 
	digits=4)

#   cid gid   ev  ramu ramuce ramurp
# 1   1   1 87.7 55.97  55.97  31.73
# 2   1   2 87.3 62.18  62.18  25.12

tk_1992_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probability_weighting <- 
	create_probability_weighting(probability_function="Tversky_Kahneman_1992", 
		parameters=c(alpha=0.61))
tk_1992_negative_probability_weighting <- 
	create_probability_weighting(probability_function="Tversky_Kahneman_1992", 
		parameters=c(alpha=0.69))
comparePT(my_choices, 
	probability_weighting_specification_for_positive_outcomes=tk_1992_positive_probability_weighting,
	probability_weighting_specification_for_negative_outcomes=tk_1992_negative_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ptce  ptrp
# 1   1   1 87.7 42.18 70.27 17.43
# 2   1   2 87.3  41.9 69.73 17.57


########################	
#
# p.59
#
########################

# S'''=(40, 0.2; 98, 0.8)
# R'''=(10, 0.1; 98, 0.9)
#CWU
# CE(S''')=73.54 > CE(R''')=70.76
#PT
# CE(S''')=74.52 > CE(R''')=70.72

choice_id_vector <- c(1, 1, 1, 1)
gamble_id_vector <- c(1, 1, 2, 2)
outcome_id_vector <- c(1, 2, 1, 2)
objective_consequence_vector <- c(40, 98, 10, 98)
probability_string_vector <- 
	c("0.2", "0.8", "0.1", "0.9")
my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector, 
	outcome_id_vector=outcome_id_vector, 
	objective_consequence_vector=objective_consequence_vector, 
	probability_string_vector=probability_string_vector)
my_choices

branch_weighting_vector_list <- list(
	c(0.37, 0.63))
my_pwf <- 
	create_probability_weighting(probability_function="power",
		parameters=c(alpha=0.6, beta=1))
my_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=1, beta=1, lambda=1))
compareRAM(my_choices, 
	branch_weighting_vector_list=branch_weighting_vector_list, 
	probability_weighting_specification=my_pwf, 
	utility=my_utility, 
	digits=4)


tk_1992_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probability_weighting <- 
	create_probability_weighting(probability_function="Tversky_Kahneman_1992", 
		parameters=c(alpha=0.61))
tk_1992_negative_probability_weighting <- 
	create_probability_weighting(probability_function="Tversky_Kahneman_1992", 
		parameters=c(alpha=0.69))
comparePT(my_choices, 
	probability_weighting_specification_for_positive_outcomes=tk_1992_positive_probability_weighting,
	probability_weighting_specification_for_negative_outcomes=tk_1992_negative_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ptce  ptrp
# 1   1   1 86.4 44.42 74.52 11.88
# 2   1   2 89.2 42.42 70.72 18.48

########################	
#
# p.72
#
########################	
# CWU(12, 0.9; 96, 0.05; 96, 0.05) = 24.18 > CWU(12, 0.9; 96, 0.1) = 23.60
# CWU(12, 0.05; 12, 0.05; 96, 0.9) = 55.6 < CWU(12, 0.1; 96, 0.9) = 70.0
choice_id_vector <- c(1, 1, 1, 1, 1)
gamble_id_vector <- c(1, 1, 1, 2, 2)
outcome_id_vector <- c(1, 2, 3, 1, 2)
objective_consequence_vector <- c(12, 96, 96, 12, 96)
probability_string_vector <- 
	c("0.9", "0.05", "0.05", "0.9", "0.1")
my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector, 
	outcome_id_vector=outcome_id_vector, 
	objective_consequence_vector=objective_consequence_vector, 
	probability_string_vector=probability_string_vector)
my_choices

my_pwf <- 
	create_probability_weighting(probability_function="power",
		parameters=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=1, beta=1, lambda=1))
compareTAX(my_choices, 
	probability_weighting_specification=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid   ev   tax taxce  taxrp
# 1   1   1 11.6 13.32 13.32 -1.716
# 2   1   2  9.6 8.962 8.962 0.6384

branch_weighting_vector_list <- list(
	c(1, 2),
	c(1, 2, 3))
my_pwf <- 
	create_probability_weighting(probability_function="power",
		parameters=c(alpha=0.6, beta=1))
my_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=1, beta=1, lambda=1))
compareRAM(my_choices, 
	branch_weighting_vector_list=branch_weighting_vector_list, 
	probability_weighting_specification=my_pwf, 
	utility=my_utility, 
	digits=4)


tk_1992_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probability_weighting <- 
	create_probability_weighting(probability_function="Tversky_Kahneman_1992", 
		parameters=c(alpha=0.61))
tk_1992_negative_probability_weighting <- 
	create_probability_weighting(probability_function="Tversky_Kahneman_1992", 
		parameters=c(alpha=0.69))
comparePT(my_choices, 
	probability_weighting_specification_for_positive_outcomes=tk_1992_positive_probability_weighting,
	probability_weighting_specification_for_negative_outcomes=tk_1992_negative_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ptce   ptrp
# 1   1   1 11.6 12.03 16.89 -5.287
# 2   1   2  9.6  8.06 10.71 -1.114


choice_id_vector <- c(1, 1, 1, 1, 1)
gamble_id_vector <- c(1, 1, 1, 2, 2)
outcome_id_vector <- c(1, 2, 3, 1, 2)
objective_consequence_vector <- c(12, 12, 96, 12, 96)
probability_string_vector <- 
	c("0.05", "0.05", "0.9", "0.1", "0.9")
my_choices <- create_choices(choice_id_vector=choice_id_vector,
	gamble_id_vector=gamble_id_vector, 
	outcome_id_vector=outcome_id_vector, 
	objective_consequence_vector=objective_consequence_vector, 
	probability_string_vector=probability_string_vector)
my_choices

my_pwf <- 
	create_probability_weighting(probability_function="power",
		parameters=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- create_utility(utility_function="power", 
	parameters=c(alpha=1, beta=1, lambda=1))
compareTAX(my_choices, 
	probability_weighting_specification=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)


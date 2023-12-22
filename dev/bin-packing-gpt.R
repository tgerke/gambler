# can you write r code that drafts a fantasy baseball team that uses bin packing optimization?

player <- c("Mike Trout", "Mookie Betts", "Nolan Arenado", "Max Scherzer", "Gerrit Cole", "Jacob deGrom", "Francisco Lindor", "Ronald Acuna Jr.", "Juan Soto", "Trea Turner", "Jose Ramirez", "Anthony Rendon", "Christian Yelich", "Alex Bregman", "Trevor Story")
position <- c("OF", "OF", "3B", "SP", "SP", "SP", "SS", "OF", "OF", "SS", "3B", "3B", "OF", "3B", "SS")
salary <- c(5000, 4900, 4700, 9300, 8700, 8200, 4500, 5400, 5200, 4900, 4500, 4300, 4800, 4300, 4100)
df <- data.frame(player, position, salary)

library(lpSolve)

# Define the decision variables
num_players <- nrow(df)
num_positions <- length(unique(df$position))
lp_obj <- rep(1, num_players)
lp_dir <- rep("<=", num_positions)
lp_rhs <- rep(1, num_positions)
lp_types <- rep("binary", num_players)
lp_model <- lp("min", lp_obj, t(df), lp_dir, lp_rhs, lp_types)

# Set the budget constraint
budget <- 50000
budget_coeff <- df$salary
lp_dir_budget <- "<="
lp_rhs_budget <- budget
lp_model <- add.constraint(lp_model, budget_coeff, lp_dir_budget, lp_rhs_budget)

# Set the position constraints
positions <- unique(df$position)
for (i in 1:length(positions)) {
  position <- positions[i]
  position_coeff <- as.numeric(df$position == position)
  lp_dir_position <- "=="
  lp_rhs_position <- ifelse(position %in% c("C", "1B", "2B", "SS", "3B"), 1, 0)
  lp_model <- add.constraint(lp_model, position_coeff, lp_dir_position, lp_rhs_position)
}

# Set the number of players constraint
num_players_coeff <- rep(1, num_players)
lp_dir_num_players <- "=="
lp_rhs_num_players <- 10
lp_model <- add.constraint(lp_model, num_players_coeff, lp_dir_num_players, lp_rhs_num_players)

# Solve the optimization problem
lp_solution <- solve(lp_model)

# Print the results
selected_players <- df[lp_solution$solution == 1, ]
total_salary <- sum(selected_players$salary)
cat("Selected Players:\n")
print(selected_players)
cat("Total Salary:", total_salary, "\n")

# can you write code in r that uses bin packing optimization to select the best combination of sports bets given their money lines?

moneyline <- sample(-250:250, size = 100, replace = TRUE)

# calculate implied probabilities for each bet
prob <- ifelse(moneyline > 0, 100 / (moneyline + 100), abs(moneyline) / (abs(moneyline) + 100))

#  goal is to select a combination of bets that maximizes the expected return on investment while staying within a budget of $10,000:
library(lpSolve)

# Set the budget constraint
budget <- 10000

# Define the decision variables
num_bets <- 15
lp_obj <- odds - 1
lp_dir <- "<="
lp_rhs <- budget
lp_types <- "binary"
lp_model <- lp("max", lp_obj, t(df), lp_dir, lp_rhs, lp_types)

# Solve the optimization problem
lp_solution <- solve(lp_model)

# Print the results
selected_bets <- df[lp_solution$solution == 1, ]
total_bet_amount <- sum(selected_bets$bet_amount)
total_payout <- sum(selected_bets$payout)
expected_return_on_investment <- total_payout / total_bet_amount
cat("Selected Bets:\n")
print(selected_bets)
cat("Total Bet Amount:", total_bet_amount, "\n")
cat("Total Payout:", total_payout, "\n")
cat("Expected Return on Investment:", expected_return_on_investment, "\n")

# This code will output the selected bets, their total bet amount, total payout, and expected return on investment, subject to the budget constraint. The odds and bet_amount columns in the data frame represent the odds and bet amounts for each bet, respectively. The payout column represents the payout for each bet, which can be calculated using the following formula:

payout <- ifelse(moneyline > 0, bet_amount * moneyline / 100, bet_amount / abs(moneyline) * 100)

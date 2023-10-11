# real data posterior prediction

library(tidyverse)

source("Functions/abc_posterior.R")
source("Functions/prep_data.R")
source("Functions/transition_contrast_function.R")



reference_data <- read_csv("Data/data_for_analysis.csv")
abc_output <- readRDS("Data/abc_output_1e6.RData")

# extract joint posterior of the same size as nrow(reference_data)
abc_posterior <- create_abc_posterior(abc_output = abc_output[[1]], comb_test = abc_output[[3]], sample_size = nrow(reference_data))

# prep data
reference_data <- prep_data(real = "yes")

N_sim_agents <- nrow(reference_data)

# initialize sim data
sim_data <- data.frame(agent_ind = 1:N_sim_agents)
sim_data$time_in_ub <- reference_data$time_in_ub
sim_data$h_state <- NA
sim_data$initial_house_state <- NA
sim_data$h_state_change_t <- NA
sim_data$type_trans <- NA
sim_data$s_state <- NA
sim_data$l_state <- NA
sim_data$f_state <- NA



for(i in 1:N_sim_agents){
  
  final_payoffs <- final_payoff_func(scenario = abc_posterior$payoff_prior[i])
  
  optimal_strategy_output <- UB_optimal(states_h = 2, states_s = 2, states_l = 2, states_f = 3, final_payoffs = final_payoffs, maxt = 40, 
                                        p_s_save = abc_posterior$p_s_prior[i], 
                                        p_l_move = abc_posterior$p_l_prior[i], 
                                        p_h_build = abc_posterior$p_h_prior[i], 
                                        build_condition = abc_posterior$build_cond_prior[i])
  names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
  
  t <- sim_data$time_in_ub[i]
  
  sim_output <- sim_strat(N = 1, maxt = t , agent_init = "sample", optimal_strategy_output = optimal_strategy_output)
  
  # capture states
  sim_data$h_state[i] <- sim_output[1, "h_state",t]
  sim_data$initial_house_state[i] <- sim_output[1, "h_state",1]
  sim_data$s_state[i] <- sim_output[1, "s_state",t]
  sim_data$l_state[i] <- sim_output[1, "l_state",t]
  sim_data$f_state[i] <- sim_output[1, "f_state",t]
  
  # # if a change occured, when
  change_indx <- c(1,1+which(diff(as.numeric(sim_output[1, "h_state",]))!=0))
  sim_data$h_state_change_t[i] <- change_indx[length(change_indx)]
  # 
  # # if a change didn't occur, t+1
  if(sim_data$h_state_change_t[i] == 1){
    sim_data$h_state_change_t[i] <- t+1
  }
  
}

# add category of transition
sim_data$type_trans <- paste(sim_data$initial_house_state, sim_data$h_state, sep = "-")

reference_data <- sim_data

reference_data <- rename(reference_data, house_state = h_state)




# on generated data
png(filename = "Figures/ub_posterior_pred_plot.png", width = 15, height = 10, units = "cm", res = 1000)

alf <- 0.6
cols <- c(adjustcolor( "#994048", alpha.f = alf), 
          adjustcolor( "#232160", alpha.f = alf), 
          adjustcolor( "#76A2D0", alpha.f = alf),
          adjustcolor( "#C4687C", alpha.f = alf))

lwd <- 1

par(mfrow = c(1,1), 
    mar = c(4,4,4,1),
    family = "serif")

plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "House state",
     xlab = "Time in Ulaanbaatar",
     main = "Posterior prediction of dwelling transitions",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("ger", "bashin"))

transition_contrast(reference_data = reference_data)

dev.off()


source("Functions/sdp_model_functions.R")

library(tidyverse)

#' prep_data
#' 
#' this function prepares the reference data to be used in the abc analysis
#'
#' @param real yes/no parameter defining whether real data from UB is to be used, or whether simulated data should be used
#' @param N_sim_agents number of agents to simulate
#' @param p_s_save input for simulated data, probability of saving when behavior is to save, between 0 and 1
#' @param p_l_move input for simulated data, probability of gaining tenure when behavior is to move, between 0 and 1
#' @param p_h_build input for simulated data, probability of gaining house when behavior is to build, between 0 and 1
#' @param scenario input for simulated data, which final payoff scenario to use, 1, 3, 4 or 6
#' @param build_condition input for simulated data, do you need savings to build? yes = 1, no = 0
#'
#' @return a dataframe called reference data


prep_data <- function(real = "yes",
                      N_sim_agents = N_sim_agents,
                      p_s_save = 0.3,   
                      p_l_move = 0.5,
                      p_h_build = 0.9,
                      scenario = 4,
                      build_condition = 1){
  
  if(real == "yes"){
    
    # load in cleaned data for both survey waves
    reference_data <- read_csv("Data/data_for_analysis_anonym.csv")

    
  }else{
    
    final_payoffs <- final_payoff_func(scenario = scenario)
    
    # optimal strategy is calculated for adult life - 40 years
    optimal_strategy_output <- UB_optimal(final_payoffs = final_payoffs, 
                                          maxt = 40, 
                                          p_s_save = p_s_save, 
                                          p_l_move = p_l_move, 
                                          p_h_build = p_h_build, 
                                          build_condition = build_condition)
    names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
    
    # initialize sim_d dataframe
    reference_data <- data.frame(agent_ind = 1:N_sim_agents)
    reference_data$time_in_ub <- sample(1:40, N_sim_agents, replace = TRUE)
    reference_data$h_state <- NA
    reference_data$initial_house_state <- NA
    reference_data$h_state_change_t <- NA
    reference_data$type_trans <- NA
    reference_data$s_state <- NA
    reference_data$l_state <- NA
    reference_data$f_state <- NA
    
    
    for(i in 1:N_sim_agents){
      
      t <- reference_data$time_in_ub[i]
      
      sim_output <- sim_strat(N = 1, maxt = t , agent_init = "sample", optimal_strategy_output = optimal_strategy_output)
      
      # capture states
      reference_data$h_state[i] <- sim_output[1, "h_state",t]
      reference_data$initial_house_state[i] <- sim_output[1, "h_state",1]
      reference_data$s_state[i] <- sim_output[1, "s_state",t]
      reference_data$l_state[i] <- sim_output[1, "l_state",t]
      reference_data$f_state[i] <- sim_output[1, "f_state",t]
      
      # # if a change occured, when
      change_indx <- c(1,1+which(diff(as.numeric(sim_output[1, "h_state",]))!=0))
      reference_data$h_state_change_t[i] <- change_indx[length(change_indx)]
      # 
      # # if a change didn't occur, t+1
      if(reference_data$h_state_change_t[i] == 1){
        reference_data$h_state_change_t[i] <- t+1
      }
    }
    
    # add category of transition
    reference_data$type_trans <- paste(reference_data$initial_house_state, reference_data$h_state, sep = "-")
    
  }
  
  return(reference_data)
}


library(tidyverse)

source("Functions/sdp_model_functions.R")

#' abc_loop
#' 
#' this function carries out the abc comparison between reference data and data generated from the parameter sweep
#' it is a simple implementation that depends on having a large enough parameter space
#'
#' @param p_s_save input parameter for simulating data, taken from comb_test, probability to save given behavior is saving, between 0 and 1
#' @param p_l_move input parameter for simulating data, taken from comb_test, probability to gain tenure given behavior is moving, between 0 and 1
#' @param p_h_build input parameter for simulating data, taken from comb_test, probability to gain house given behavior is building, between 0 and 1
#' @param scenario input parameter for simulating data, taken from comb_test, final payoff scenario, 2,3,4, or 6
#' @param build_condition input parameter for simulating data, taken from comb_test, do you need savings to build? yes = 1, no = 0
#' @param N_agent number of agents in the data
#' @param reference_data the reference data set to be used 
#'
#' @return output, a single number representing the abc result - a combination of the differences in each variable of interest between reference data and generated data

abc_loop <- function(    p_s_save = 0.6,   
                         p_l_move = 0.2,
                         p_h_build = 0.5,
                         scenario = 4,
                         build_condition = 1,
                         N_agent = 10,
                         reference_data){
  
  ## step 1
  # calculate optimal strategy for given param values
  final_payoffs <- final_payoff_func(scenario = scenario)
  
  # optimal strategy is calculated for adult life - 40 years (maxt = 41, because we start at year 1)
  # note, if wanting to run the optimal strategy for a different number of years, here it needs to be total_years + 1
  optimal_strategy_output <- UB_optimal(states_h = 2, states_s = 2, states_l = 2, states_f = 3, final_payoffs = final_payoffs, maxt = 41, p_s_save = p_s_save, p_l_move = p_l_move, p_h_build = p_h_build, build_condition = build_condition)
  names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
  
  ## step 2
  # run generative forward simulation for each agent, for length of reference time 
  
  # initialize sim_d dataframe
  sim_data <- data.frame(agent_ind = 1:nrow(reference_data))
  sim_data$time_in_env <- reference_data$time_in_ub
  sim_data$h_state_change_t <- NA
  sim_data$house_state <- NA
  sim_data$initial_house_state <- NA
  sim_data$type_trans <- NA
  sim_data$s_state <- NA
  sim_data$l_state <- NA
  sim_data$f_state <- NA
  
  
  for(i in 1:nrow(reference_data)){
    
    t <- sim_data$time_in_env[i]
    
    sim_output <- sim_strat(N = 1, maxt = t , agent_init = "sample", optimal_strategy_output = optimal_strategy_output)
    
    # extract states
    sim_data$house_state[i] <- sim_output[1, "h_state",t]
    sim_data$initial_house_state[i] <- sim_output[1, "h_state",1]
    sim_data$s_state[i] <- sim_output[1, "s_state",t]
    sim_data$l_state[i] <- sim_output[1, "l_state",t]
    sim_data$f_state[i] <- sim_output[1, "f_state",t]
    
    # if a change occured, when
    change_indx <- c(1,1+which(diff(as.numeric(sim_output[1, "h_state",]))!=0))
    sim_data$h_state_change_t[i] <- change_indx[length(change_indx)]
    
    # if a change didn't occur, t+1
    if(sim_data$h_state_change_t[i] == 1){
      sim_data$h_state_change_t[i] <- t+1
    }
  }
  
  # add category
  sim_data$type_trans <- paste(sim_data$initial_house_state, sim_data$house_state, sep = "-")
  
  
  ## step 3: epsilons
  # store difference between each set of points from sim_data and reference_data
  sim_tab <- as.data.frame(table(sim_data$type_trans))
  ref_tab <- as.data.frame(table(reference_data$type_trans))
  
  if(nrow(sim_tab) < 4){
    cat_vec <- c("1-1", "1-2", "2-2", "2-1")
    sim_tab$Var1 <- as.character(sim_tab$Var1)
    for(i in 1:4){
      if(length(which(sim_tab$Var1 == cat_vec[i])) == 0){
        add <- as.character(cat_vec[i])
        sim_tab <- rbind(sim_tab, c(add, 0))
      }
    }
    sim_tab$Var1 <- as.factor(sim_tab$Var1)
  }
  
  if(nrow(ref_tab) < 4){
    cat_vec <- c("1-1", "1-2", "2-2", "2-1")
    ref_tab$Var1 <- as.character(ref_tab$Var1)
    for(i in 1:4){
      if(length(which(ref_tab$Var1 == cat_vec[i])) == 0){
        add <- as.character(cat_vec[i])
        ref_tab <- rbind(ref_tab, c(add, 0))
      }
    }
    ref_tab$Var1 <- as.factor(ref_tab$Var1)
  }
  
  sim_tab$Freq <- as.numeric(sim_tab$Freq)
  ref_tab$Freq <- as.numeric(ref_tab$Freq)
  
  diff_1_1 <- sim_tab$Freq[which(sim_tab$Var1 == "1-1")] - ref_tab$Freq[which(ref_tab$Var1 == "1-1")]
  diff_1_2 <- sim_tab$Freq[which(sim_tab$Var1 == "1-2")] - ref_tab$Freq[which(ref_tab$Var1 == "1-2")]
  diff_2_2 <- sim_tab$Freq[which(sim_tab$Var1 == "2-2")] - ref_tab$Freq[which(ref_tab$Var1 == "2-2")]
  diff_2_1 <- sim_tab$Freq[which(sim_tab$Var1 == "2-1")] - ref_tab$Freq[which(ref_tab$Var1 == "2-1")]
  
  diff_s1 <- nrow(sim_data[which(sim_data$s_state == 1),]) - nrow(reference_data[which(reference_data$s_state == 1),])
  diff_s2 <- nrow(sim_data[which(sim_data$s_state == 2),]) - nrow(reference_data[which(reference_data$s_state == 2),])
  diff_l1 <- nrow(sim_data[which(sim_data$l_state == 1),]) - nrow(reference_data[which(reference_data$l_state == 1),])
  diff_l2 <- nrow(sim_data[which(sim_data$l_state == 2),]) - nrow(reference_data[which(reference_data$l_state == 2),])
  diff_f1 <- nrow(sim_data[which(sim_data$f_state == 1),]) - nrow(reference_data[which(reference_data$f_state == 1),])
  diff_f2 <- nrow(sim_data[which(sim_data$f_state == 2),]) - nrow(reference_data[which(reference_data$f_state == 2),])
  diff_f3 <- nrow(sim_data[which(sim_data$f_state == 3),]) - nrow(reference_data[which(reference_data$f_state == 3),])
  
  output <- abs(diff_1_1) + 
    abs(diff_1_2) + 
    abs(diff_2_2) + 
    abs(diff_2_1) +
    abs(diff_s1) +
    abs(diff_s2) +
    abs(diff_l1) +
    abs(diff_l2) +
    abs(diff_f1) +
    abs(diff_f2) +
    abs(diff_f3)
  
  return(
    output
  )
  
}



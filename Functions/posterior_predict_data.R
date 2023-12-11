
source("Functions/prep_data.R")

#' posterior_predict_data
#'
#' @param abc_posterior posterior output from abc analysis to use for generating data 
#'
#' @return reference_set, a data frame containing the different differences between reference and generated data


posterior_predict_data <- function(abc_posterior = abc_posterior){
  
  set_of_gen_data <- list()
  
  for(i in 1:nrow(abc_posterior)){
    
    generated_data <- prep_data(real = "no", 
                                N_sim_agents = abc_posterior$N_agents[i], 
                                p_s_save = abc_posterior$p_s_prior[i], 
                                p_l_move = abc_posterior$p_l_prior[i], 
                                p_h_build = abc_posterior$p_h_prior[i], 
                                build_condition = abc_posterior$build_cond_prior[i], 
                                scenario = abc_posterior$payoff_prior[i])
    
    set_of_gen_data[[i]] <- generated_data
  }
  return(set_of_gen_data)
}











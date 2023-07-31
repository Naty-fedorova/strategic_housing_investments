source("Functions/sdp_model_functions.R")

# the generate_combinations functions creates the parameters space for testing for the abc analysis

#' generate_combinations
#' 
#' this function creates the parameter space that is that sweeped over in the abc analysis
#' it does so in 3 version:
#' fix_param = no, each parameter is sampled from its prior
#' fix_param = yes, all parameters except payoff prior are kept at one value
#' fix_param = validate, each parameter is sweeped over in turn, while others are held constant
#'
#' @param n_comb the number of combinations to generate, this translates to rows of output
#' @param N_agents a value needed for the abc analysis - externally defined
#' @param fix_param parameter switching between generation scenarios
#'
#' @return comb_test, a dataframe where each row is a parameter combination for the abc analysis


generate_combinations <- function(n_comb = 100, 
                                  N_agents = 10,
                                  fix_param = "no"
                                  ){
  

  if(fix_param == "no"){
    
    # for probability of saving given saving behavior, I've decided to keep it quite low, peak at 0.2
    # this is because the population of the ger districts is generally very poor
    p_s_prior <- rbeta(n = n_comb, 2, 5)  # 0.2
    
    # for the probability of acquiring land when moving, I've decided to make it higher than savings
    # but I'm still thinking here, because I think people generally move about 3 times before settling (source:IOM mongolia)
    p_l_prior <- rbeta(n = n_comb, 5, 5)  # 0.5
    
    # for the probability of having a house when building, I think it makes sense to keep this high, but maybe not 1
    p_h_prior <- rbeta(n = n_comb, 8, 2) # 0.9
    
    # final payoffs
    # 2 is baseline: savings and tenure matter
    # 3 is only family matters
    # 4 is additive
    # 6 house priority
    payoff_prior <- sample(c(2, 3, 4, 6), n_comb, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))
    
    # savings needed for building?
    # prior is on yes
    build_cond_prior <- sample(c(0,1), n_comb, replace = TRUE, prob = c(0.3, 0.7))
    
    # combine into test set
    comb_test <- data.frame(p_s_prior, p_l_prior, p_h_prior, payoff_prior, build_cond_prior, N_agents)
    
    
  }
  
  if(fix_param == "yes"){
    
    # fixed
    p_s_prior <- rep(0.3, n_comb)  # 0.3
    
    # fixed
    p_l_prior <- rep(0.5, n_comb)  # 0.5
    
    # fixed
    p_h_prior <- rep(0.9, n_comb) # 0.9
    
    # fixed
    payoff_prior <- sample(c(2, 3, 4, 6), n_comb, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))
    
    # fixed 
    build_cond_prior <- rep(1, n_comb) # 1
    
    comb_test <- data.frame(p_s_prior, p_l_prior, p_h_prior, payoff_prior, build_cond_prior, N_agents)
    
    
  }
  
  if(fix_param == "validate"){
    
    comb_test <- data.frame(matrix(ncol = 6, nrow = ((n_comb/5) *3)))
    
    colnames(comb_test) <- c("p_s_prior", "p_l_prior", "p_h_prior", "payoff_prior", "build_cond_prior", "N_agents")
    
    step_comb <- n_comb/5
    
    test_seq <- seq(from = 0, to = 1, length.out = step_comb)
    
    for(i in 1:3){
      
      if( i == 1){
        # first param fluctuates, rest are constant
        comb_test$p_s_prior[1:step_comb] <- test_seq
        
        comb_test$p_l_prior[1:step_comb] <- rep(0.5, step_comb)  # 0.5
        comb_test$p_h_prior[1:step_comb] <- rep(0.9, step_comb) # 0.9
        comb_test$payoff_prior[1:step_comb] <- rep(6, step_comb)
        comb_test$build_cond_prior[1:step_comb] <- rep(1, step_comb) # 1
      }
      
      if( i == 2){
        
        comb_test$p_s_prior[(step_comb + 1): (step_comb * 2)] <- rep(0.2, step_comb)  # 0.5
        
        comb_test$p_l_prior[(step_comb + 1): (step_comb * 2)] <- test_seq
        
        comb_test$p_h_prior[(step_comb + 1): (step_comb * 2)]  <- rep(0.9, step_comb) # 0.9
        comb_test$payoff_prior[(step_comb + 1): (step_comb * 2)]  <- rep(6, step_comb)
        comb_test$build_cond_prior[(step_comb + 1): (step_comb * 2)] <- rep(1, step_comb) # 1
      }
      
      if( i == 3){
        
        comb_test$p_s_prior[((step_comb * 2) + 1): (step_comb * 3)] <- rep(0.2, step_comb)  # 0.5
        comb_test$p_l_prior[((step_comb * 2) + 1): (step_comb * 3)] <- rep(0.5, step_comb)  # 0.5
        
        comb_test$p_h_prior[((step_comb * 2) + 1): (step_comb * 3)]  <- test_seq
        
        comb_test$payoff_prior[((step_comb * 2) + 1): (step_comb * 3)] <- rep(6, step_comb)
        comb_test$build_cond_prior[((step_comb * 2) + 1): (step_comb * 3)] <- rep(1, step_comb) # 1
      }
    } # end combos for probs
    
    comb_test$N_agents <- N_agents
    
    # add combinations for payoffs
    p_s_prior <- rep(0.3, step_comb)  # 0.3
    p_l_prior <- rep(0.5, step_comb)  # 0.5
    p_h_prior <- rep(0.9, step_comb) # 0.9
    build_cond_prior <- rep(1, step_comb) # 1
    
    payoff_vec <- c(2,3,4,6)
    rep_times <- step_comb/4
    payoff_prior <- rep(payoff_vec, rep_times)
    
    payoff_mat <- data.frame(p_s_prior, p_l_prior, p_h_prior, payoff_prior, build_cond_prior, N_agents)
    
    comb_test <- rbind(comb_test, payoff_mat)
    
    
    # add combinations for bc
    p_s_prior <- rep(0.3, step_comb)  # 0.3
    p_l_prior <- rep(0.5, step_comb)  # 0.5
    p_h_prior <- rep(0.9, step_comb) # 0.9
    payoff_prior <- rep(6, step_comb) # 6
    
    bc_vec <- c(0,1)
    rep_bc_times <- step_comb/2
    
    build_cond_prior <- rep(bc_vec, rep_bc_times)
    
    payoff_mat_bc <- data.frame(p_s_prior, p_l_prior, p_h_prior, payoff_prior, build_cond_prior, N_agents)

    comb_test <- rbind(comb_test, payoff_mat_bc)
    
  }
  
  return(comb_test)
}

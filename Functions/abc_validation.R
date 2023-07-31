library(parallel)

source("Functions/generate_combinations.R")
source("Functions/prep_data.R")
source("Functions/abc_loop.R")

#' abc_validate
#' 
#' this function runs a validation of the abc analysis by running through a parameter sweep where always only one parameter is left to vary, with a known generated set
#' this allows us to identify the accuracy of the abc in terms of reproducing the posterior
#'
#' @param n_comb_gen the number of combinations to use for generating datasets
#' @param N number of agents
#' @param n_comb_test the number of combinations to run in the abc analysis - higher is better, but longer
#' @param cores number of cores
#'
#' @return abcs to val, a list of ordered abc results, test_set which is the parameter values used to generate simulated data, and set of ref data, the actual generated data sets


abc_validate <- function(n_comb_gen,
                         N = 1000,
                         n_comb_test = 10, 
                         cores = 2){
  
  
  # generate test scenarios for reference data
  test_set <- generate_combinations(n_comb = n_comb_gen, N_agents = N, fix_param = "validate")
  
  # initialize list to store abc_output stuff
  abcs_to_val <- list() 
  set_of_ref_data <- list()
  
  for(i in 1:nrow(test_set)){
    
    print("##################")
    print(i)
    
    # generate combinations
    comb_test <- generate_combinations(n_comb = n_comb_test, N_agents = N, fix_param = "no")
    
    # formulate combinations runs
    run_sim <- n_comb_test
    jobs_list <- list(1:run_sim)  
    temp <- unname(comb_test)
    rownames(temp) <- NULL
    ll <- 1:nrow(comb_test)
    jobs_list <- as.list(ll)
    jobs_list[1:run_sim] <- as.list( as.data.frame(t(temp)) )
    
    
    reference_data <- prep_data(real = "no", 
                                N_sim_agents = N, 
                                p_s_save = test_set$p_s_prior[i], 
                                p_l_move = test_set$p_l_prior[i], 
                                p_h_build = test_set$p_h_prior[i], 
                                build_condition = test_set$build_cond_prior[i], 
                                scenario = test_set$payoff_prior[i])
    
    
    # argumentize abc function
    abc_arg_list <- function(arg_list) {
      return(abc_loop(arg_list[1], arg_list[2], arg_list[3], arg_list[4], arg_list[5], arg_list[6], reference_data))
    }
    
    abc_output <- mclapply( jobs_list, abc_arg_list, mc.cores = cores)
    
    comb_test$abc_output <- unlist(abc_output[1:nrow(comb_test)])
    comb_test <- comb_test[order(comb_test$abc_output, decreasing = FALSE),]
    
    abcs_to_val[[i]] <- comb_test
    set_of_ref_data[[i]] <- reference_data 

    
  }
  
  return(list(abcs_to_val,
              test_set,
              set_of_ref_data
              ))
}


library(parallel)

source("Functions/generate_combinations.R")
source("Functions/prep_data.R")
source("Functions/abc_loop.R")


#' abc_runs
#' 
#' this function carries out the abc analysis, looping over the parameter space encoded in comb_test and comparing each generated dataset to the reference data set
#' the analysis is parallelyzed and so can run on multiple cores
#'
#' @param n_comb input for generate_combinations, number of parameter combinations to test, this translates as rows in comb_test 
#' @param N number of agents
#' @param fix_param input for generate_combinations, which of the 3 generation scenarios to run, fixed param yes, or no, or validate
#' @param real_data input for prep_data, whether real data is to be used, or simulated data is required, yes no
#' @param p_s_save input for simulated data, probability of saving when behavior is to save, between 0 and 1
#' @param p_l_move input for simulated data, probability of gaining tenure when behavior is to move, between 0 and 1
#' @param p_h_build input for simulated data, probability of gaining house when behavior is to build, between 0 and 1
#' @param scenario input for simulated data, which final payoff scenario to use, 1, 3, 4 or 6
#' @param build_condition input for simulated data, do you need savings to build? yes = 1, no = 0
#' @param cores number of cores to run the analysis on 
#'
#' @return abc_output, a list where each entry is the abc result for each parameter combination in comb_test compared to reference data
#' @return reference data, dataframe of reference data that was tested
#' @return comb_test, dataframe of parameter combinations that was tested



abc_runs <- function(n_comb = 100,
                     N = 825,
                     fix_param = "no",
                     real_data = "no",
                     p_s_save = 0.3,   
                     p_l_move = 0.5,
                     p_h_build = 0.9,
                     scenario = 4,
                     build_condition = 1,
                     cores = 2){
  
  
  # generate combinations
  jobs <- generate_combinations(n_comb = n_comb, N_agents = N, fix_param = fix_param)
  
  reference_data <- prep_data(real = real_data, N_sim_agents = N, p_s_save = p_s_save, p_l_move = p_l_move, p_h_build = p_h_build, build_condition = build_condition, scenario = scenario)
  
  
  abc_sum_diff <- mclapply(1:n_comb,
                         function(i) {
                           if (i %% 100 == 0) print(i)
                           out <- abc_loop(
                             p_s_save = jobs$p_s_prior[i],
                             p_l_move = jobs$p_l_prior[i],
                             p_h_build = jobs$p_h_prior[i],
                             scenario = jobs$payoff_prior[i],
                             build_condition = jobs$build_cond_prior[i],
                             N_agent = jobs$N_agents[i], # abc_loop never uses this
                             reference_data = reference_data
                           )
                         },
                         mc.cores = cores
  )
  
  return(list(abc_sum_diff,
              reference_data,
              jobs))
  
}











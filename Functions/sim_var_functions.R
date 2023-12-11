# assessing variance in simulation runs

source("Functions/sdp_model_functions.R")


#' run_sim_sweep
#'
#' @param sim_runs: number of times to run each param combinations, 1 if just doing a single sweep 
#' @param maxt: number of time steps to run
#' @param N: how many agents to run forward simulation for 
#' @param p_s_save: probability of gaining saving state when savings  
#' @param p_l_move: probability of gaining tenure when moving  
#' @param p_h_build: probability of gaining house state when building  
#' @param p_s_loss: probability of stochastic loss to savings  
#' @param p_force_move: probability of stochastic need to move 
#' @param build_condition: binary param, 0 = no savings needed to build, 1 = savings needed to build 
#' @param scenario: final payoffs scenario  
#'
#' @return list of forward simulation outputs - each has maxt slices of output

run_sim_sweep <- function(sim_runs = sim_runs,
                          maxt = 40,
                          N = 100,
                          
                          p_s_save = 0.6,   
                          p_l_move = 0.2,
                          p_h_build = 0.5,
                          p_s_loss = 0.2,
                          p_force_move = 0.3,
                          build_condition = 1,
                          scenario = 4
){
  
  #if(sim_runs > 1){
    sim_output_runs <- vector("list", length = sim_runs)
    
    for(i in 1:sim_runs){
      
      final_payoffs <- final_payoff_func(scenario = scenario)
      optimal_strategy_output <- UB_optimal(states_h = 2, states_s = 2, states_l = 2, states_f = 3, 
                                            final_payoffs = final_payoffs, 
                                            maxt = maxt, 
                                            p_s_save = p_s_save, 
                                            p_l_move = p_l_move, 
                                            p_h_build = p_h_build,
                                            p_s_loss = p_s_loss,
                                            p_force_move = p_force_move,
                                            build_condition = build_condition)
      names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
      
      
      sim_output_runs[[i]] <- sim_strat(N = N, maxt = maxt, agent_init = "sample", optimal_strategy_output = optimal_strategy_output)
      
    #}
  #}else{
  #   final_payoffs <- final_payoff_func(scenario = scenario)
  #   optimal_strategy_output <- UB_optimal(states_h = 2, states_s = 2, states_l = 2, states_f = 3, 
  #                                         final_payoffs = final_payoffs, 
  #                                         maxt = maxt, 
  #                                         p_s_save = p_s_save, 
  #                                         p_l_move = p_l_move, 
  #                                         p_h_build = p_h_build,
  #                                         p_s_loss = p_s_loss,
  #                                         p_force_move = p_force_move,
  #                                         build_condition = build_condition)
  #   names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
  #   
  #   sim_output_runs <- sim_strat(N = N, maxt = maxt, agent_init = "sample", optimal_strategy_output = optimal_strategy_output)
  #   
   }
  
  return(sim_output_runs)
}



# generating beh_frq_sweep 

#' beh_frq
#'
#' @param maxt: time optimality model is run to
#' @param sim_output: array output of optimality model, each slice is a year, and contains information about optimal behavior for each simulated household
#' @param sim_runs: number of times a singular parameter combination is run, that is what is being summarized here
#'
#' @return beh_frq_sweep, summarized simulation variance runs

beh_frq <- function(maxt = 40,
                    sim_output = sim_output,
                    sim_runs = 10
){
  
  
  beh_frq_sweep <- list()
  
  # here I average over the sim_runs 
  
  for(i in 1:length(sim_output)){
    
    beh_frq_run <- matrix(data = 0, nrow = maxt, ncol = 3)
    colnames(beh_frq_run) <- c("build", "save", "move")
    
    for(j in 1:sim_runs){
      
      beh_freq <- matrix(data = 0, nrow = maxt, ncol = 3)
      colnames(beh_freq) <- c("build", "save", "move")
      
      for(t in 1:maxt){
        sim_output_t <- table(sim_output[[i]][[j]][ ,"beh", t])
        
        beh_freq[t,"build"] <- sim_output_t["build"]
        beh_freq[t,"save"] <- sim_output_t["save"]
        beh_freq[t,"move"] <- sim_output_t["move"]
      }
      beh_freq[is.na(beh_freq)] <- 0
      
      beh_frq_run <- beh_frq_run + beh_freq
      
    }
    
    beh_frq_sweep[[i]] <- beh_frq_run/sim_runs  
    
  }
  
  return(beh_frq_sweep)
}



# function for plotting outputs

#' sweep_lines
#'
#' @param selected_set: subset of jobs being selected from parameter sweep 
#' @param beh_frq_sweep: summarized sim outputs, produced by function beh_frq
#'
#' @return lines to plot representing optimal strategy over time

sweep_lines <- function(selected_set = selected_set,
                        beh_frq_sweep = beh_frq_sweep){
  
  for(i in 1:length(selected_set)){
    j <- selected_set[i]
    selected_run <- beh_frq_sweep[[j]]
    
    for (e in 1:ncol(selected_run)) {
      xs <- c()
      ys <- c()
      for(t in 1:maxt){
        xs <- c(xs, t)
        y <- selected_run[t,e]
        ys <- c(ys, y)
      }
      lines(xs, ys, col=colors[e], lwd = 1, lty = 1)
    }
  }
}





source("Functions/sdp_model_functions.R")

#' run_sdp
#'
#' @param scenario: final payoff scenario, 2 = material baseline, 3 = family baseline, 4 = additive, 6 = house priority 
#' @param maxt: time to run the optimal model to 
#' @param p_s_save: probability of increasing in saving state if behavior is saving
#' @param p_l_move: probability of increasing land state if behavior is move
#' @param p_h_build: probability of increasing house state if behavior is move  
#' @param p_s_loss: probability of losing saving state in any t
#' @param p_force_move: probability of being forced to move in any t 
#' @param build_condition: are savings required for building? 0 = no, 1 = yes
#' @param N: number of households to generate in forward simulation
#'
#' @return sim_output
#' 
run_sdp <- function(

           scenario = 6,

           maxt = 41,
           p_s_save = 0.5,
           p_l_move = 0.5,
           p_h_build = 0.5,
           p_s_loss = 0.5,
           p_force_move = 0.5,
           build_condition = 1,

           N = 1000){
  
  final_payoffs <- final_payoff_func(scenario = scenario)
  optimal_strategy_output <- UB_optimal(final_payoffs = final_payoffs, 
                                        maxt = maxt, 
                                        p_s_save = p_s_save, 
                                        p_l_move = p_l_move, 
                                        p_h_build = p_h_build,
                                        p_s_loss = p_s_loss,
                                        p_force_move = p_force_move,
                                        build_condition = build_condition)
  names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
  
  sim_output <- sim_strat(N = N, maxt = maxt, agent_init = "sample", optimal_strategy_output = optimal_strategy_output)

}


#' generate_reference_data
#'
#' @param scenario: final payoff scenario, 2 = material baseline, 3 = family baseline, 4 = additive, 6 = house priority 
#' @param maxt: time to run the optimal model to 
#' @param p_s_save: probability of increasing in saving state if behavior is saving
#' @param p_l_move: probability of increasing land state if behavior is move
#' @param p_h_build: probability of increasing house state if behavior is move  
#' @param p_s_loss: probability of losing saving state in any t
#' @param p_force_move: probability of being forced to move in any t 
#' @param build_condition: are savings required for building? 0 = no, 1 = yes
#' @param N: number of households to generate in forward simulation
#'
#' @return reference_data
#' 
generate_reference_data <- function(

           scenario = 6,

           maxt = 41,
           p_s_save = 0.5,
           p_l_move = 0.5,
           p_h_build = 0.5,
           p_s_loss = 0.5,
           p_force_move = 0.5,
           build_condition = 1,

           N = 1000){


  final_payoffs <- final_payoff_func(scenario = scenario)

  # optimal strategy is calculated for adult life - 30 years
  optimal_strategy_output <- UB_optimal(states_h = 2, states_s = 2, states_l = 2, states_f = 3, final_payoffs = final_payoffs,
                                        maxt = maxt,
                                        p_s_save = p_s_save,
                                        p_l_move = p_l_move,
                                        p_h_build = p_h_build,
                                        p_s_loss = p_s_loss,
                                        p_force_move = p_force_move,
                                        build_condition = build_condition)
  names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")

  # initialize sim_d dataframe
  reference_data <- data.frame(agent_ind = 1:N)
  reference_data$time_in_ub <- sample(1:(maxt - 1), N, replace = TRUE)
  reference_data$s_state <- NA
  reference_data$l_state <- NA
  reference_data$f_state <- NA
  reference_data$house_state <- NA
  reference_data$initial_house_state <- NA
  reference_data$type_trans <- NA
  reference_data$h_state_change_t <- NA



  for(i in 1:N){

    t <- reference_data$time_in_ub[i]

    sim_output <- sim_strat(N = 1, maxt = t , agent_init = "sample", optimal_strategy_output = optimal_strategy_output)

    # house state captures the type of dwelling they are in at end of observation
    reference_data$house_state[i] <- sim_output[1, "h_state",t]

    # what is the initial house state
    reference_data$initial_house_state[i] <- sim_output[1, "h_state",1]

    reference_data$s_state[i] <- sim_output[1, "s_state",t]

    reference_data$l_state[i] <- sim_output[1, "l_state",t]

    reference_data$f_state[i] <- sim_output[1, "f_state",t]
    
    # if a change occured, when
    change_indx <- c(1,1+which(diff(as.numeric(sim_output[1, "h_state",]))!=0))
    reference_data$h_state_change_t[i] <- change_indx[length(change_indx)]
    
    # if a change didn't occur, t+1
    if(reference_data$h_state_change_t[i] == 1){
      reference_data$h_state_change_t[i] <- t+1
    }
  }

  # add dwelling transition type category
  reference_data$type_trans <- paste(reference_data$initial_house_state, reference_data$house_state, sep = "-")


  return(reference_data)

}

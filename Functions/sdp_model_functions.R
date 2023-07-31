### Functions needed to run SDP model


#' final_payoff_func
#'
#' @param scenario: optimizing scenario considered: 2 = baseline, 3 = family baseline, 4 = additive, 6 = house priority
#'
#' @return: final payoffs - a 2x2x2x3 array filling in the final payoff to each state space combination
#' 
final_payoff_func <- function(scenario = "baseline") {
  
  ## states
  states_h <- 2            # number of states for house (ger, bashin)
  states_s <- 2            # number of states for savings (no have savings, have savings) 
  states_l <- 2            # number of states for land (no land, own land)
  states_f <- 3            # number of states for family composition (single, pair, with dependents)
  
  ## initialise payoff array
  init_payoffs_array <- function(values, dim, dimnames) {
    
    data <- c()
    for ( i in 1:length(values) ) { 
      data <- c(data, t(array(values[[i]], c(dim[1], dim[2]))))
    }
    
    dimnames_expanded <- list()
    for ( i in 1:length(dim) )
    { dimnames_expanded[[i]] <- sprintf("%s=%d", dimnames[i], 1:dim[i]) }
    
    
    return(array(
      data=data,
      dim=dim,
      dimnames=dimnames_expanded
    ))
  }
  
  # different payoff scenarios:
  # NOTE: rows of mini arrays are house state, columns are saving state
  
  # baseline 
  # savings are good
  # tenure is good
  # no effect of fam, or house
  if(scenario == 2){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 1,  2,
           1,  2),
        #l=2, f=1
        c( 3,  4,
           3,  4),
        
        #l=1, f=2
        c( 1,  2,
           1,  2),
        #l=2, f=2
        c( 3,  4,
           3,  4),
        
        #l=1, f=3
        c( 1,  2,
           1,  2),
        #l=2, f=3
        c( 3,  4,
           3,  4)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
  }
  
  # baseline where only fam matters
  if(scenario == 3){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 1,  1,
           1,  1),
        #l=2, f=1
        c( 1,  1,
           1,  1),
        
        #l=1, f=2
        c( 2,  2,
           2,  2),
        #l=2, f=2
        c( 2,  2,
           2,  2),
        
        #l=1, f=3
        c( 3,  3,
           3,  3),
        #l=2, f=3
        c( 3,  3,
           3,  3)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
  }
  
  ## additive, each increase in state has payoff gains, saving more valuable when no tenure, building more valuable when have tenure
  if(scenario == 4){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 1,  3,
           2,  4),
        #l=2, f=1
        c( 4,  5,
           6,  7),
        
        #l=1, f=2
        c( 6,  8,
           7,  9),
        #l=2, f=2
        c( 9,  10,
           11, 12),
        
        #l=1, f=3
        c( 12,  14,
           13,  15),
        #l=2, f=3
        c( 15,  16,
           17,  18)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
  }
  
  
  # house priority
  # having a house is more valuable than the other conditions
  # having a house better with tenure though, risk averse
  # all the other conditions treated equally
  
  if(scenario == 6){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 1,  2,
           1,  2),
        #l=2, f=1
        c( 3,  3,
           4,  5),
        
        #l=1, f=2
        c( 1,  2,
           1,  2),
        #l=2, f=2
        c( 3,  3,
           4,  5),
        
        #l=1, f=3
        c( 1,  2,
           1,  2),
        #l=2, f=3
        c( 3,  3,
           4,  5)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
  }
  
  return(final_payoffs)
}




#' get_fam_probs
#'
#' @param house: house state value (1 or 2)
#' @param savings: saving state value (1 or 2) 
#' @param land: land state value (1 or 2) 
#' @param p_f_down_nothing: prob of new person leaving fam if fam has nothing (all states at 1)
#' @param p_f_down_everything: prob of new person leaving fam if fam has everything (all states maxed out)
#' @param p_f_up_nothing: prob of new person joining fam if fam has nothing 
#' @param p_f_up_everything: prob of new person joining fam if fam has everything 
#'
#' @return probability of decreasing family state, and probability of increasing family state

get_fam_probs <- function(
  house,
  savings,
  land,
  p_f_down_nothing = 0.02,     
  p_f_down_everything = 0.01,  
  p_f_up_nothing = 0.1,           
  p_f_up_everything = 0.2         

) {
  
  # positive relationship with house and land
  # negative relationship with savings
  # normalized between 0 and 1
  p_base <- (house - savings + land) / 3
  
  if( p_base > 1 | p_base < 0 ) stop('p_f_down+p_f_up > 1')
  
  # prob of new person joining the family (marriage or birth)
  # inbetween it's linear
  p_f_up <- p_base * p_f_up_everything + (1-p_base) * p_f_up_nothing
  
  # prob of someone dying
  # inbetween it's linear
  p_f_down <- p_base * p_f_down_everything + (1-p_base) * p_f_down_nothing
  
  
  return(c(p_f_down, p_f_up))
}





#' propensity_to_save
#'
#' @param fam_state: family state value 
#'
#' @return value between 0 and 1, to be multiplied with probability to save when saving. Output of this function changes it in respect to family state

propensity_to_save <- function(fam_state) {
  if(fam_state ==1){
    return(0.5)
  }
  if(fam_state ==2){
    return(1)
  }
  if(fam_state ==3){
    return(0.5)
  }
}



#' UB_optimal
#'
#' @param states_h: number of states for house (ger, bashin) 
#' @param states_s: number of states for savings (no have savings, have savings)  
#' @param states_l: number of states for land (no land, own land) 
#' @param states_f: number of states for family (single, pair, with dependents) 
#' @param final_payoffs: payoffs in final time period  
#' @param maxt: number of time step 
#' @param p_s_save: probability of gaining saving state when saving 
#' @param p_h_build: prob gain house when building 
#' @param p_l_move: prob gain tenure when moving (essentially finding free land)
#' @param build_condition: do you need savings to build? (1 = yes, 0 = no) 
#' @param p_s_loss: prob of losing savings at each t
#' @param p_force_move: prob of having to move from land, if you don't have tenure 
#'
#' @return list containing strat and parameter values. strat is a 2x2x2x3xt array containing a the optimal behavior for combinations in the state space for each t

UB_optimal <- function( 
  states_h = 2,           
  states_s = 2,            
  states_l = 2,            
  states_f = 3,            
  final_payoffs,       
  maxt = 3,            
  p_s_save = 0.7,      
  p_h_build = 1.0,     
  p_l_move = 0.7,      
  build_condition = 1, 
  p_s_loss = 0.1,      
  p_force_move = 0.1   
) {
  
  if( p_s_save < 0 | p_s_save > 1) stop(c('p_s_save is not a probability: ', p_s_save))
  if( p_h_build < 0 | p_h_build > 1) stop(c('p_h_build is not a probability: ', p_h_build))
  if( p_l_move < 0 | p_l_move > 1) stop(c('p_l_move is not a probability: ', p_l_move))
  if( p_s_loss < 0 | p_s_loss > 1) stop(c('p_s_loss is not a probability: ', p_s_loss))
  if( p_force_move < 0 | p_force_move > 1) stop(c('p_force_move is not a probability: ', p_force_move))
  
  if( any(dim(final_payoffs) != c(states_h, states_s, states_l, states_f)) ) stop(c('final_payoffs dimensions do not correspond to states: ', dim(final_payoffs)))
  
  if( !(build_condition %in% c(0, 1)) )  stop(c('build_condition is invalid: ', build_condition))
  
  # array for solution
  dim <- c(states_h, states_s, states_l, states_f, maxt)
  dimnames <- c("h", "s", "l", "f", "t")
  dimnames_expanded <- list()
  for ( i in 1:length(dim) )
  { dimnames_expanded[[i]] <- sprintf("%s=%d", dimnames[i], 1:dim[i]) }
  
  strat <- array(
    data = NA, 
    dim = dim,
    dimnames = dimnames_expanded
  ) 
  
  # init payoffs in each state
  payoff <- final_payoffs
  new_payoff <- payoff # used for updating
  
  ## loop backwards through time
  for ( t in maxt:1 ) {
    new_payoff[] <- -1
    #print(payoff)
    for ( h in 1:(states_h) ) {
      
      ## define payoffs when we move states
      # must bound maximum and minimum payoff
      h_up <- h+1
      if(h_up > states_h) h_up <- states_h
      h_down <- h-1
      if(h_down < 1) h_down <- 1
      
      for ( s in 1:(states_s)) { 
        
        ## define payoffs when we move states
        # must bound maximum and minimum payoff
        s_down <- s-1
        if(s_down < 1) s_down <- 1
        s_up <- s+1
        if(s_up > states_s) s_up <- states_s
        
        if(build_condition > 0){  
          s_down_build <- s_down
        } else{
          s_down_build <- s
        }
        
        for ( l in 1:(states_l)) { 
          
          ## define payoffs when we move states
          # must bound maximum and minimum payoff
          l_up <- l+1
          if(l_up > states_l) l_up <- states_l
          
          # condition for stochastic forced moves
          # can only happen to agents with no tenure
          if(l == 1){
            effective_p_force_move <- p_force_move
          }else(
            effective_p_force_move <- 0
          )
          
          for( f in 1:(states_f)) {
            
            ## define payoffs when we move states
            # must bound maximum and minimum payoff
            f_down <- f-1
            if(f_down < 1) f_down <- 1
            f_up <- f+1
            if(f_up > states_f) f_up <- states_f
            
            ## calculate expected payoffs
            
            fam_probs <- get_fam_probs(house = h, savings = s, land = l)
            p_f_down <- fam_probs[1]
            p_f_up <- fam_probs[2]
            
            if( p_f_down+p_f_up > 1 ) stop('p_f_down+p_f_up > 1')
            
            p_f_static <- 1 - p_f_down - p_f_up
            
            payoff_build <- (
              (1-effective_p_force_move) * (
                (1-p_s_loss) * (
                  p_f_down * (p_h_build * payoff[h_up, s_down_build, l, f_down] + (1-p_h_build) * payoff[h, s, l, f_down]) +
                    p_f_static * (p_h_build * payoff[h_up, s_down_build, l, f] + (1-p_h_build) * payoff[h, s, l, f]) +
                    p_f_up * (p_h_build * payoff[h_up, s_down_build, l, f_up] + (1-p_h_build) * payoff[h, s, l, f_up]))
                +
                  (p_s_loss) * (
                    # NOTE: if you build, you already lose savings, so you can't really re-lose them
                    p_f_down * (p_h_build * payoff[h_up, s_down, l, f_down] + (1-p_h_build) * payoff[h, s_down, l, f_down]) +
                      p_f_static * (p_h_build * payoff[h_up, s_down, l, f] + (1-p_h_build) * payoff[h, s_down, l, f]) +
                      p_f_up * (p_h_build * payoff[h_up, s_down, l, f_up] + (1-p_h_build) * payoff[h, s_down, l, f_up])))
              + 
                (effective_p_force_move) * (
                  # when forced to move, payoff is payoff to move
                  (1-p_s_loss) * (
                    p_f_down * (p_l_move * payoff[h_down, s, l_up, f_down] + (1-p_l_move) * payoff[h, s, l, f_down]) +
                      p_f_static * (p_l_move * payoff[h_down, s, l_up, f] + (1-p_l_move) * payoff[h, s, l, f]) +
                      p_f_up * (p_l_move * payoff[h_down, s, l_up, f_up] + (1-p_l_move) * payoff[h, s, l, f_up]))
                  +
                    (p_s_loss) * (
                      p_f_down * (p_l_move * payoff[h_down, s_down, l_up, f_down] + (1-p_l_move) * payoff[h, s_down, l, f_down]) +
                        p_f_static * (p_l_move * payoff[h_down, s_down, l_up, f] + (1-p_l_move) * payoff[h, s_down, l, f]) +
                        p_f_up * (p_l_move * payoff[h_down, s_down, l_up, f_up] + (1-p_l_move) * payoff[h, s_down, l, f_up]))
                )
            )
            
            
            effective_p_s_save <- propensity_to_save(f) * p_s_save
            payoff_save <- (
              (1 - effective_p_force_move) * (
                (1-p_s_loss) * (
                  p_f_down * (effective_p_s_save * payoff[h, s_up, l, f_down] + (1-effective_p_s_save) * payoff[h, s, l, f_down]) +
                    p_f_static * (effective_p_s_save * payoff[h, s_up, l, f] + (1-effective_p_s_save) * payoff[h, s, l, f]) +
                    p_f_up * (effective_p_s_save * payoff[h, s_up, l, f_up] + (1-effective_p_s_save) * payoff[h, s, l, f_up]))
                +
                  (p_s_loss) * (
                    p_f_down * (effective_p_s_save * payoff[h, s, l, f_down] + (1-effective_p_s_save) * payoff[h, s_down, l, f_down]) +
                      p_f_static * (effective_p_s_save * payoff[h, s, l, f] + (1-effective_p_s_save) * payoff[h, s_down, l, f]) +
                      p_f_up * (effective_p_s_save * payoff[h, s, l, f_up] + (1-effective_p_s_save) * payoff[h, s_down, l, f_up])))
              +
                (effective_p_force_move) * (
                  # when forced to move, payoff is payoff to move
                  (1-p_s_loss) * (
                    p_f_down * (p_l_move * payoff[h_down, s, l_up, f_down] + (1-p_l_move) * payoff[h, s, l, f_down]) +
                      p_f_static * (p_l_move * payoff[h_down, s, l_up, f] + (1-p_l_move) * payoff[h, s, l, f]) +
                      p_f_up * (p_l_move * payoff[h_down, s, l_up, f_up] + (1-p_l_move) * payoff[h, s, l, f_up]))
                  +
                    (p_s_loss) * (
                      p_f_down * (p_l_move * payoff[h_down, s_down, l_up, f_down] + (1-p_l_move) * payoff[h, s_down, l, f_down]) +
                        p_f_static * (p_l_move * payoff[h_down, s_down, l_up, f] + (1-p_l_move) * payoff[h, s_down, l, f]) +
                        p_f_up * (p_l_move * payoff[h_down, s_down, l_up, f_up] + (1-p_l_move) * payoff[h, s_down, l, f_up]))
                )
            )
            
            payoff_move <- (
              (1-p_s_loss) * (
                p_f_down * (p_l_move * payoff[h_down, s, l_up, f_down] + (1-p_l_move) * payoff[h, s, l, f_down]) +
                  p_f_static * (p_l_move * payoff[h_down, s, l_up, f] + (1-p_l_move) * payoff[h, s, l, f]) +
                  p_f_up * (p_l_move * payoff[h_down, s, l_up, f_up] + (1-p_l_move) * payoff[h, s, l, f_up]))
              +
                (p_s_loss) * (
                  p_f_down * (p_l_move * payoff[h_down, s_down, l_up, f_down] + (1-p_l_move) * payoff[h, s_down, l, f_down]) +
                    p_f_static * (p_l_move * payoff[h_down, s_down, l_up, f] + (1-p_l_move) * payoff[h, s_down, l, f]) +
                    p_f_up * (p_l_move * payoff[h_down, s_down, l_up, f_up] + (1-p_l_move) * payoff[h, s_down, l, f_up]))
            )
            
            
            # check payoffs are between min and max of final_payoffs
            if( min(final_payoffs) > payoff_build | payoff_build > max(final_payoffs) ) stop(c('payoff_build outside of bounds ', payoff_build))
            if( min(final_payoffs) > payoff_save | payoff_save > max(final_payoffs) ) stop(c('payoff_save outside of bounds ', payoff_save))
            if( min(final_payoffs) > payoff_move | payoff_move > max(final_payoffs) ) stop(c('payoff_move outside of bounds ', payoff_move))
            
            
            # condition: can only build if you have enough savings
            if(s > build_condition){   # if you have enough savings to build
              
              # which is better?
              # record optimal behavior 
              
              possible_current_payoffs <- c(payoff_save, payoff_move, payoff_build) # there is order of preference here, if payoffs end up being equal
              # make note if payoffs are equal 
              if((length(unique(possible_current_payoffs))== 3) == FALSE) print(c(possible_current_payoffs, h = h, s = s, l = l, f = f, t = t ))
              best_payoff_ind <- which(possible_current_payoffs==max(possible_current_payoffs))[1]
              
              if ( best_payoff_ind == 3 ) { # payoff_build
                strat[h, s, l, f, t] <- "build"
                new_payoff[h, s, l, f] <- payoff_build
              }
              if ( best_payoff_ind == 1 ) { # payoff_save
                strat[h, s, l, f, t] <- "save"
                new_payoff[h, s, l, f] <- payoff_save
              }
              if ( best_payoff_ind == 2 ) { # payoff_move
                strat[h, s, l, f, t] <- "move"
                new_payoff[h, s, l, f] <- payoff_move
              }
            }else{  # can only save or move because you don't have enough savings to build 
              
              # which is better?
              # record optimal behavior 
              # and store expected payoff when behaving optimally
              
              possible_current_payoffs <- c(payoff_save, payoff_move) # there is order of preference here, if payoffs end up being equal
              if((length(unique(possible_current_payoffs))== 2) == FALSE) print(c(possible_current_payoffs, h = h, s = s, l = l, f = f, t = t ))
              best_payoff_ind <- which(possible_current_payoffs==max(possible_current_payoffs))[1]
              
              if ( best_payoff_ind == 1 ) { # payoff_save
                strat[h, s, l, f, t] <- "save"
                new_payoff[h, s, l, f] <- payoff_save
              }
              if ( best_payoff_ind == 2 ) { # payoff_move
                strat[h, s, l, f, t] <- "move"
                new_payoff[h, s, l, f] <- payoff_move
              }
            } # end of ifelse s>1
          }#f
        }#l
      }#s
    }#h
    
    #check that all new_payoffs have been assigned a value in expected range
    for ( h in 1:(states_h) ) {
      for ( s in 1:(states_s)) {
        for ( l in 1:(states_l)) {
          for( f in 1:(states_f)) {
            if( min(final_payoffs) > new_payoff[h, s, l, f] | new_payoff[h, s, l, f] > max(final_payoffs) ) stop(c('new_payoff outside of bounds ', new_payoff[h, s, l, f]))
          }
        }
      }
    }
    
    # update payoffs to expected payoffs under optimal strategy
    payoff <- new_payoff
    
  }#t
  
  return( list(strat, c(maxt, states_h, states_s, states_l, states_f, p_h_build, p_s_save, p_l_move, build_condition, p_s_loss, p_force_move )))
}





#' sim_strat
#'
#' @param N: number of agents 
#' @param maxt: number of time steps 
#' @param optimal_strategy_output: optimal strategy array (need output from UB_optimal here) 
#' @param agent_init: "sample" or "start", sample means agents have sample of state values, start means agents all begin with lowest of states 
#'
#' @return agent_prop: array that contains slices for each t, each slice containing state space info for each agent as well as behavior taken that t step

sim_strat <- function(
    N = 50,                                                                                  
    maxt = 50,
    optimal_strategy_output = optimal_strategy_output,           
    agent_init = "sample"                                         
){
  
  # initialize info from optimal strategy run
  optimal_strat <- optimal_strategy_output[[1]]                # optimal strategy
  #maxt  <- optimal_strategy_output[[2]]["maxt"]               # timescale
  
  states_h  <- optimal_strategy_output[[2]]["states_h"]       # states of house (ger, bashin)
  states_s  <- optimal_strategy_output[[2]]["states_s"]        # states of savings (savings, no savings)
  states_l  <- optimal_strategy_output[[2]]["states_l"]        # states of land (tenure, no tenure)
  states_f  <- optimal_strategy_output[[2]]["states_f"]        # states of family (single, pair, with dependents)
  
  p_h_build  <- optimal_strategy_output[[2]]["p_h_build"]      # prob of gaining house state when building
  p_s_save  <- optimal_strategy_output[[2]]["p_s_save"]       # prob of gaining save state when saving
  p_l_move  <- optimal_strategy_output[[2]]["p_l_move"]       # prob of gaining tenure when moving
  
  build_condition  <- optimal_strategy_output[[2]]["build_condition"]  # do you need savings to build?
  
  p_s_loss  <- optimal_strategy_output[[2]]["p_s_loss"]        # prob of losing savings each t
  p_force_move  <- optimal_strategy_output[[2]]["p_force_move"]# prob of having to move each t
  
  # initialize array for agents * time
  c_names <- list(1:N , c("agent", "h_state", "s_state", "l_state","f_state", "beh"), rep("t", maxt))
  agent_prop <- array(NA, c(N, length(c_names[[2]]), maxt), dimnames = c_names )
  
  # initialize states level
  states_of_h <- 1:states_h
  states_of_s <- 1:states_s
  states_of_l <- 1:states_l
  states_of_f <- 1:states_f
  
  # intialize agents in first time step
  # atm just random sample from statespace
  if(agent_init == "sample"){
    agent_prop[ , "agent", ] <- c(1:N)
    agent_prop[ , "h_state", 1] <- sample(states_of_h, N, replace = TRUE) 
    agent_prop[ , "s_state", 1] <- sample(states_of_s, N, replace = TRUE)
    agent_prop[ , "l_state", 1] <- sample(states_of_l, N, replace = TRUE)
    agent_prop[ , "f_state", 1] <- sample(states_of_f, N, replace = TRUE)
  }
  
  if(agent_init == "start"){
    agent_prop[ , "agent", ] <- c(1:N)
    agent_prop[ , "h_state", 1] <- rep(1, N)
    agent_prop[ , "s_state", 1] <- rep(1, N)
    agent_prop[ , "l_state", 1] <- rep(1, N)
    agent_prop[ , "f_state", 1] <- rep(1, N)
  }
  
  for(t in 1: maxt){
    
    ## stochastic changes to states
    for(agent in 1:N){
      
      ## stochastic change to fam state
      fam_probs <- get_fam_probs(
        as.numeric(agent_prop[agent ,"h_state", t]), 
        as.numeric(agent_prop[agent ,"s_state", t]), 
        as.numeric(agent_prop[agent ,"l_state", t]))
      
      p_f_down <- fam_probs[1]
      p_f_up <- fam_probs[2]
      
      if( p_f_down+p_f_up >= 1 ) stop('p_f_down+p_f_up > 1')
      
      p_f_static <- 1 - p_f_down - p_f_up
      
      fam_change <- sample(c(-1,0,1), 1, prob = c(p_f_down, p_f_static, p_f_up))
      
      f <- as.numeric(agent_prop[agent ,"f_state", t])
      f_down <- f-1
      if(f_down < 1) f_down <- 1
      f_up <- f+1
      if(f_up > states_f) f_up <- states_f
      
      if(fam_change == -1){
        # fam state decrease
        agent_prop[agent ,"f_state", t] <- f_down
      }
      if(fam_change == 1){
        # fam state increase
        agent_prop[agent ,"f_state", t] <- f_up
      }
      
      # otherwise it stays the same 
      
      #-----------------------------------------
      
      ## stochastic change to savings 
      
      if(rbinom(1,1, prob = p_s_loss) == 1){
        # saving state decreases
        s <- as.numeric(agent_prop[agent ,"s_state", t])
        s_down <- s-1
        if(s_down < 1) s_down <- 1
        
        agent_prop[agent ,"s_state", t] <- s_down
      }
      
      #-----------------------------------------
      
      ## decide on behavior
      agent_prop[agent ,"beh", t] <- optimal_strat[
        as.numeric(agent_prop[agent ,"h_state", t]), 
        as.numeric(agent_prop[agent ,"s_state", t]), 
        as.numeric(agent_prop[agent ,"l_state", t]), 
        as.numeric(agent_prop[agent ,"f_state", t]),
        t]
      
      #-----------------------------------------
      
      ## if agent doesn't have tenure, they might be forced to move
      
      if(agent_prop[agent ,"l_state", t] == 1){
        if(rbinom(1,1, prob = p_force_move) == 1){
          # forced to move
          agent_prop[agent ,"beh", t] <- "move"
        }
      }
    }
    
    
    ## materialize state change | intialize states for t+1
    
    if(t < maxt){
      for(agent in 1:N){
        
        # define movements in state space
        # house state
        h <- as.numeric(agent_prop[agent, "h_state", t])
        h_up <- h+1
        if(h_up > states_h) h_up <- states_h
        h_down <- h-1
        if(h_down < 1) h_down <- 1
        
        # save state
        s <- as.numeric(agent_prop[agent, "s_state", t])
        s_up <- s+1
        if(s_up > states_s) s_up <- states_s
        s_down <- s-1
        if(s_down < 1) s_down <- 1
        
        # land state
        l <- as.numeric(agent_prop[agent, "l_state", t])
        l_up <- l+1
        if(l_up > states_l) l_up <- states_l
        l_down <- l-1
        if(l_down < 1) l_down <- 1
        
        # fam state
        f <- as.numeric(agent_prop[agent, "f_state", t])
        f_up <- f+1
        if(f_up > states_f) f_up <- states_f
        f_down <- f-1
        if(f_down < 1) f_down <- 1
        
        # if agent builds
        if(agent_prop[agent, "beh", t] == "build"){
          if(rbinom(1, 1, prob = p_h_build) == 1){
            
            # state changes if agent builds "successfully"
            # increase in house state
            # decrease in saving state
            
            agent_prop[agent, "h_state", t+1] <- h_up
            agent_prop[agent, "s_state", t+1] <- s_down
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
            
          }else{
            
            # state changes if agent builds "unsuccessfully"
            # everything stays the same
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
          }
        }#
        
        # if agent saves
        if(agent_prop[agent, "beh", t] == "save"){
          if(rbinom(1, 1, prob = p_s_save) == 1){
            
            # state changes if agent saves "successfully
            # increase in save state
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s_up
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
            
          }else{
            
            # state changes if agent saves "unsuccessfully"
            # everything stays the same
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
          }
        }#
        
        # if agent moves
        if(agent_prop[agent, "beh", t] == "move"){
          if(rbinom(1, 1, prob = p_l_move) == 1){
            
            # state changes if agent moves "successfully"
            # increase in land state
            agent_prop[agent, "h_state", t+1] <- h_down
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l_up
            agent_prop[agent, "f_state", t+1] <- f
            
          }else{
            
            # state changes if agent moves "unsuccessfully"
            # everything stays the same
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
            
          }
        }#
      }
    }
  }#t
  
  return(agent_prop)
  
}#function











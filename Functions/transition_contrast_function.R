
#' transition_contrast
#'
#' @param reference_data: dataframe containing intial and current house state information 
#'
#' @return plots transition lines in a plot window

#' 
transition_contrast <- function(reference_data = reference_data){
  
  for(i in 1:nrow(reference_data)){
    
    # define dwelling sequence based on whether a transition happened or not
    if(reference_data$initial_house_state[i] == reference_data$house_state[i]){
      
      time_seq <- rep(as.numeric(reference_data$initial_house_state[i]), reference_data$time_in_ub[i])
      
      #print(c(time_seq, length(time_seq), i))
      
    }else{
      h_1 <- 1:reference_data$h_state_change_t[i]
      h_2 <- (reference_data$h_state_change_t[i] + 1):reference_data$time_in_ub[i]
      
      hh_1 <- rep(as.numeric(reference_data$initial_house_state[i]), length(h_1))
      hh_2 <- rep(as.numeric(reference_data$house_state[i]), length(h_2))
      
      time_seq <- c(hh_1, hh_2)
      
      #print(c(time_seq, length(time_seq), i))
    }
    
    #print(c(i, length(time_seq)))
    
    transition <- unique(time_seq)
    
    #print(transition)
    
    if(length(transition) == 1){
      if(unique(transition) == 1){
        # these are hh that stay in ger
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, col = cols[1], lwd = lwd)
      }
      
      if(unique(time_seq) == 2){
        # these are hh that stay in bashin
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, col = cols[2], lwd = lwd)
      }
    }else{
      
      # hh that transition from ger to bashin 
      if(sum(transition == c(1,2)) == 2){
        #print(c(i, time_seq))
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, lwd = lwd, col = cols[3])
      }
      
      # these are hh that transition from bashin to ger
      if(sum(transition == c(2,1)) == 2){
        #print(c(i, time_seq))
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, lwd = lwd, col = cols[4])
      } 
    }
    
  }
}

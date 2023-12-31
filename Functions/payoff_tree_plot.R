
#' payoff_tree
#'
#' @param final_payoffs: array of final payoffs  
#' @param scenario: final payoff scenario, either 2, 3, 4, or 6 
#' @param title: title to put in the plot 
#'
#' @return tree plot showing final payoff structure in relation to the model state space 

payoff_tree <- function(final_payoffs = final_payoffs, 
                        scenario = "default",
                        title = "type title",
                        labs = "yes",
                        labs_leg = "yes"){
  
  states_h <- 2
  states_s <- 2
  states_l <- 2
  states_f <- 3
  
  ## payoffs in correct order
  dend_data <- data.frame(payoff_index = 1:24, payoff = 0, h = 0, s = 0, l = 0, f = 0)
  
  dend_index <- 1
  for(h in states_h:1){
    for(s in states_s:1){
      for(l in states_l:1){
        for(f in states_f:1){
          dend_data[dend_index, 2:6 ] <- c(final_payoffs[h, s, l, f], h, s, l, f)
          dend_index <- dend_index + 1
        }
      }
    }
  }
  
  # scale payoffs so they are between 0 and 10
  a <- 0
  b <-  1
  scaled_payoffs <- (b - a)* ((dend_data$payoff - min(dend_data$payoff))/(max(dend_data$payoff) - min(dend_data$payoff))) + a
  
  
  height <- 28
  dims <- dim(final_payoffs)
  cols <- c("#24652D", "#5F4596", "#FF2E00", "#FEA82F")
  
  plot(10,1, 
       main = title,
       xlim = c(-1, (length(dims) + max(scaled_payoffs) + 1 )), 
       ylim = c(0, height),
       xaxt = "n",
       xlab = "payoffs",
       yaxt = "n",
       ylab = "possible state combinations",
       bty = "n",
       type = "n"
  )
  axis(1, at = 4:(4 + max(scaled_payoffs)), label = a:b)
  
  prev_pos <- 1
  for(x in 1:length(dims)){
    d <- dims[x]
    n_pos <- prev_pos * d
    prev_pos <- n_pos
    
    gap <- height / n_pos
    offset <- (gap - 1) / 2
    #print("#########")
    #print(c(x, d, n_pos, gap, offset))
    for(point in 0:(n_pos-1)){
      y <- offset + point * gap
      points(x, y, pch = 20)
      
      state <- (point %% d)
      
      if(x == 4){
        if(state == 0) {lty = 3} 
        if(state == 1) {lty = 4}
        if(state == 2) {lty = 5}
      } else{
        if(state == 0) {lty = 1} 
        if(state == 1) {lty = 2}
      }
      
      
      dim_adjustment <- 4 - d
      twig_direction <- state * dim_adjustment - 1
      twig_angle <- gap / dim_adjustment
      y_diff <- y - twig_direction * twig_angle
      lines(c(x-1,x), c(y_diff,y), type = "l", lty = lty, lwd = 1.5, col=cols[x])
      points(x, y, pch = 20)
    }
  }
  
  for(i in 1:24){
    gap <- height / n_pos
    lines(x = c(4, 4 + scaled_payoffs[i]), y = c(i*gap - 1, i*gap - 1),  )
  }
  
  
  if(labs == "yes"){
  cex <- 1.4
  # states are house, savings, tenure, family
  text(x = 0.7, y = 18, substitute(bold("House")), cex = cex , pos = 2, col = cols[1])
  text(x = 1.5, y = 22, substitute(bold("Savings")), cex = cex , pos = 2, col = cols[2])
  text(x = 2.3, y = 25, substitute(bold("Tenure")), cex = cex , pos = 2, col = cols[3])
  text(x = 3.3, y = 27, substitute(bold("Family")), cex = cex , pos = 2, col = cols[4])
  }
 
  if(labs_leg == "yes"){
  cex <- 1.3
  # legend binary states
  arrows(x0 = -1, y0=5, x1=0, y1=6, length = 0.0, lty = 2)
  arrows(-1, 5, 0, 4, length = 0.0, lty = 1)
  
  points(x = -1, y = 5, pch = 16)
  
  text(x = 0, y = 6, "no", cex = cex , pos = 4)
  text(x = 0, y = 4, "yes", cex = cex , pos = 4)
  
  # legend fam states
  arrows(x0 = -1, y0 = 1, x1 = 0, y1 = 2.5, length = 0.0, lty = 5)
  arrows(x0 = -1, y0 = 1, x1 = 0, y1 = 1, length = 0.0, lty = 4)
  arrows(x0 = -1, y0 = 1, x1 = 0, y1 = -0.5, length = 0.0, lty = 3)
  
  points(x = -1, y = 1, pch = 16)
  
  text(x = 0, y = 2.5, "single", cex = cex , pos = 4)
  text(x = 0, y = 1, "pair", cex = cex , pos = 4)
  text(x = 0, y = -0.5, "family", cex = cex , pos = 4)
  }
  
}




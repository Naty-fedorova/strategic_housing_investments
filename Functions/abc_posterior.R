
#' create_abc_posterior
#' 
#' this function takes the output of abc_runs and creates a joint posterior 
#'
#' @param abc_output the raw abc result from abc_runs
#' @param comb_test the dataframe of parameter combinations that were an input to the abc
#' @param sample_size how many samples to draw from the joint posterior
#'
#' @return abc_posterior, a dataframe on nrow sample_size which is a subset of comb_test with the abc_result attached selected based on the joint posterior probability


create_abc_posterior <- function(abc_output = abc_output,
                                 comb_test = comb_test,
                                 sample_size = 100){
  
  comb_test$abc_output <- unlist(abc_output[1:nrow(comb_test)])
  plot(density(comb_test$abc_output), lwd = 3)
  
  # divide by something (this needs to be figured out) in order to make values smaller and prevent whole posterior of being 0
  comb_test$abc_output <- comb_test$abc_output / 11
  
  # exponentiate negative to get it between 0 and 1
  comb_test$abc_output_p <- exp(-comb_test$abc_output)
  
  # normalize to make it a probability distribution, i.e. for probabilities to sum to 1
  comb_test$abc_output_p <- comb_test$abc_output_p/sum(comb_test$abc_output_p)
  
  abc_output_sample <- sample(comb_test$abc_output, size = sample_size, prob = comb_test$abc_output_p)
  lines(density(abc_output_sample), col = "purple", lty = 3, lwd = 3)
  
  # add index to sample joint posterior
  comb_test$index <- 1:nrow(comb_test)
  
  abc_output_sample <- sample(comb_test$index, size = sample_size, prob = comb_test$abc_output_p)
  
  abc_posterior <- comb_test[abc_output_sample,]
  
  return(abc_posterior)
  
}


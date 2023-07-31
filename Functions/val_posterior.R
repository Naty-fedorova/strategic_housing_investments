#' val_posterior
#'
#' @param comb_test: a dataframe of parameter combinations the abc ran through, as well as the raw abc output
#'
#' @return abc_posterior - the posterior distribution from a particular abc run 
val_posterior <- function(comb_test = comb_test, sample_size = 1000){
  comb_test$abc_output <- comb_test$abc_output / 11
  comb_test$abc_output_p <- exp(-comb_test$abc_output)
  comb_test$abc_output_p <- comb_test$abc_output_p/sum(comb_test$abc_output_p)
  abc_output_sample <- sample(comb_test$abc_output, size = sample_size, prob = comb_test$abc_output_p)
  comb_test$index <- 1:nrow(comb_test)
  abc_output_sample <- sample(comb_test$index, size = sample_size, prob = comb_test$abc_output_p)
  abc_posterior <- comb_test[abc_output_sample,]
}
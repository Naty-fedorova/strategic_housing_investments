
source("Functions/prep_data.R")

#' posterior_predictive_check
#'
#' @param reference_data reference data to compare generated data to
#' @param abc_posterior posterior output from abc analysis to use for generating data 
#'
#' @return diff_df, a data frame containing the different differences between reference and generated data
#' note: differences are always generated - reference, they are not absolute so we can see directionality

posterior_predictive_check <- function( reference_data = reference_data,
                                        abc_posterior = abc_posterior
){
  
  
  #  create data frame to store differences between reference and generated data
  diff_df <- data.frame(posterior_run = 1:nrow(abc_posterior),
                        diff_1_1 = rep(NA, nrow(abc_posterior)),
                        diff_1_2 = rep(NA, nrow(abc_posterior)),
                        diff_2_2 = rep(NA, nrow(abc_posterior)),
                        diff_2_1 = rep(NA, nrow(abc_posterior)),
                        
                        diff_s1 = rep(NA, nrow(abc_posterior)),
                        diff_s2 = rep(NA, nrow(abc_posterior)),
                        diff_l1 = rep(NA, nrow(abc_posterior)),
                        diff_l2 = rep(NA, nrow(abc_posterior)),
                        diff_f1 = rep(NA, nrow(abc_posterior)),
                        diff_f2 = rep(NA, nrow(abc_posterior)),
                        diff_f3 = rep(NA, nrow(abc_posterior)),
                        output = rep(NA, nrow(abc_posterior))
  )
  
  for(i in 1:nrow(abc_posterior)){
    
    generated_data <- prep_data(real = "no", 
                                N_sim_agents = abc_posterior$N_agents[i], 
                                p_s_save = abc_posterior$p_s_prior[i], 
                                p_l_move = abc_posterior$p_l_prior[i], 
                                p_h_build = abc_posterior$p_h_prior[i], 
                                build_condition = abc_posterior$build_cond_prior[i], 
                                scenario = abc_posterior$payoff_prior[i])
    
    
    # store difference between each set of points from sim_data and reference_data
    sim_tab <- as.data.frame(table(generated_data$type_trans))
    ref_tab <- as.data.frame(table(reference_data$type_trans))
    
    if(nrow(sim_tab) < 4){
      cat_vec <- c("1-1", "1-2", "2-2", "2-1")
      sim_tab$Var1 <- as.character(sim_tab$Var1)
      for(j in 1:4){
        if(length(which(sim_tab$Var1 == cat_vec[j])) == 0){
          add <- as.character(cat_vec[j])
          sim_tab <- rbind(sim_tab, c(add, 0))
        }
      }
      sim_tab$Var1 <- as.factor(sim_tab$Var1)
    }
    
    if(nrow(ref_tab) < 4){
      cat_vec <- c("1-1", "1-2", "2-2", "2-1")
      ref_tab$Var1 <- as.character(ref_tab$Var1)
      for(k in 1:4){
        if(length(which(ref_tab$Var1 == cat_vec[k])) == 0){
          add <- as.character(cat_vec[k])
          ref_tab <- rbind(ref_tab, c(add, 0))
        }
      }
      ref_tab$Var1 <- as.factor(ref_tab$Var1)
    }
    
    sim_tab$Freq <- as.numeric(sim_tab$Freq)
    ref_tab$Freq <- as.numeric(ref_tab$Freq)
    
    diff_df$diff_1_1[i] <- sim_tab$Freq[which(sim_tab$Var1 == "1-1")] - ref_tab$Freq[which(ref_tab$Var1 == "1-1")]
    diff_df$diff_1_2[i] <- sim_tab$Freq[which(sim_tab$Var1 == "1-2")] - ref_tab$Freq[which(ref_tab$Var1 == "1-2")]
    diff_df$diff_2_2[i] <- sim_tab$Freq[which(sim_tab$Var1 == "2-2")] - ref_tab$Freq[which(ref_tab$Var1 == "2-2")]
    diff_df$diff_2_1[i] <- sim_tab$Freq[which(sim_tab$Var1 == "2-1")] - ref_tab$Freq[which(ref_tab$Var1 == "2-1")]
    
    diff_df$diff_s1[i] <- nrow(generated_data[which(generated_data$s_state == 1),]) - nrow(reference_data[which(reference_data$s_state == 1),])
    diff_df$diff_s2[i] <- nrow(generated_data[which(generated_data$s_state == 2),]) - nrow(reference_data[which(reference_data$s_state == 2),])
    diff_df$diff_l1[i] <- nrow(generated_data[which(generated_data$l_state == 1),]) - nrow(reference_data[which(reference_data$l_state == 1),])
    diff_df$diff_l2[i] <- nrow(generated_data[which(generated_data$l_state == 2),]) - nrow(reference_data[which(reference_data$l_state == 2),])
    diff_df$diff_f1[i] <- nrow(generated_data[which(generated_data$f_state == 1),]) - nrow(reference_data[which(reference_data$f_state == 1),])
    diff_df$diff_f2[i] <- nrow(generated_data[which(generated_data$f_state == 2),]) - nrow(reference_data[which(reference_data$f_state == 2),])
    diff_df$diff_f3[i] <- nrow(generated_data[which(generated_data$f_state == 3),]) - nrow(reference_data[which(reference_data$f_state == 3),])
    
    diff_df$output[i] <- abs(diff_df$diff_1_1[i]) + 
      abs(diff_df$diff_1_2[i]) + 
      abs(diff_df$diff_2_2[i]) + 
      abs(diff_df$diff_2_1[i]) +
      abs(diff_df$diff_s1[i]) +
      abs(diff_df$diff_s2[i]) +
      abs(diff_df$diff_l1[i]) +
      abs(diff_df$diff_l2[i]) +
      abs(diff_df$diff_f1[i]) +
      abs(diff_df$diff_f2[i]) +
      abs(diff_df$diff_f3[i])
    
  }
  
  return(diff_df)
}










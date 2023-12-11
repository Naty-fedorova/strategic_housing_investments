# this script runs the validation of the abc analysis and plots the results
library(rethinking)

source("Functions/abc_validation.R")
source("Functions/val_posterior.R")
source("Functions/posterior_predictive_check.R")

posterior_pred_output <- readRDS("Data/post_pred_output_bc_fix.RDS")
abc_validated <- readRDS("Data/validation_output_1e5_bc_fix.RDS")

# abc validation 
# n_comb_gen needs to be a factor of 4 ideally
# min 20

abc_val_runs <- abc_validate(n_comb_gen = 40, n_comb_test = 100000, cores = 80)

saveRDS(abc_val_runs, file="validation_output_1e5.RDS")

abc_validated <- abc_val_runs


# posterior prediction

sample_size <- 1000

posterior_pred_output <- vector("list", length = length(abc_validated[[3]]) )

# loop through all the generated reference_data in abc_val_runs
for(i in 1:1){  # length(abc_validated[[3]])
  
  abc_slice <- as.data.frame(abc_validated[[1]][[i]])
  ref_slice <- as.data.frame(abc_validated[[3]][[i]])
  
  abc_slice$abc_output <- abc_slice$abc_output/ 11
  abc_slice$abc_output_p <- exp(-abc_slice$abc_output)
  abc_slice$abc_output_p <- abc_slice$abc_output_p/sum(abc_slice$abc_output_p)
  abc_output_sample <- sample(abc_slice$abc_output, size = sample_size, prob = abc_slice$abc_output_p)
  abc_slice$index <- 1:nrow(abc_slice)
  abc_output_sample <- sample(abc_slice$index, size = sample_size, prob = abc_slice$abc_output_p)
  
  abc_posterior <- abc_slice[abc_output_sample,]
  
  print(i)
  posterior_pred_output[[i]] <- posterior_predictive_check(reference_data = ref_slice, abc_posterior = abc_posterior)
  
}

saveRDS(posterior_pred_output, file="post_pred_output_bc_fix.RDS")


# posterior_pred_output now has a list, each containing the sequences of differences calculated between ref data and generated data
# the total differences are n_agents * 11 (the number of differences compared)
# 1000*11 = 11000

n_diff <- 11
N <- 1000

post_pred_diff <- as.data.frame(matrix(data = NA, nrow = length(posterior_pred_output), ncol = 1,
                                       dimnames = list(1:length(posterior_pred_output), "diff")))

for(i in 1:length(posterior_pred_output)){
  
  post_pred_diff[i,] <- mean(posterior_pred_output[[i]][["output"]]/(n_diff*N))
  
}

comb_test_all <- abc_validated[[1]]
test_set <- abc_validated[[2]]

total_tests <- nrow(test_set)
segment <- total_tests/5

# p_s_save, 1:segment
p_s_save <- test_set[1:segment,]
p_s_save_res <- list()

for(i in 1:nrow(p_s_save)){
  
  comb_test <- comb_test_all[[i]]
  abc_posterior <- val_posterior(comb_test = comb_test, sample_size = 1000)
  p_s_save_res[[i]] <- abc_posterior
  
}

# p_l_move
p_l_move <- test_set[(segment+1):(segment*2),]
p_l_move_res <- list()

for(i in 1:segment){
  j <- i + segment
  comb_test <- comb_test_all[[j]]
  abc_posterior <- val_posterior(comb_test = comb_test, sample_size = 1000)
  
  p_l_move_res[[i]] <- abc_posterior
}

# p_h_build
p_h_build <- test_set[(segment*2 + 1):(segment*3),]
p_h_build_res <- list()

for(i in 1:segment){
  j <- i + (segment*2)
  comb_test <- comb_test_all[[j]]
  abc_posterior <- val_posterior(comb_test = comb_test, sample_size = 1000)
  
  p_h_build_res[[i]] <- abc_posterior
}

# payoffs
p_payoff <- test_set[(segment*3 +1):(segment*4),]
p_payoff_res <- list()

for(i in 1:segment){
  j <- i + (segment*3)
  comb_test <- comb_test_all[[j]]
  abc_posterior <- val_posterior(comb_test = comb_test)
  
  p_payoff_res[[i]] <- abc_posterior
}

# bc
p_bc <- test_set[(segment*4 +1):(segment*5),]
p_bc_res <- list()

for(i in 1:segment){
  j <- i + (segment*4)
  comb_test <- comb_test_all[[j]]
  abc_posterior <- val_posterior(comb_test = comb_test)
  
  p_bc_res[[i]] <- abc_posterior
}


input_col <- "#A1E44D"
output_col <- "#793e6e"




png(filename = "Figures/val.png", width = 15, height = 12, units = "cm", res = 500)

par(mfrow = c(2,3), 
    mar = c(3,4,3,2),
    oma = c(1,1,4,1),
    family = "serif")

nf <- layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
layout.show(nf)

circle_size <- 1.5

plot(1,1, type = "n",
     xlim = c(1,segment),
     ylim = c(-0.1,1),
     bty = "n",
     xlab = "runs",
     ylab = "value",
     axes = FALSE,
     main = "p of gaining savings 
     when saving")
axis(1, at = 1:segment, labels = FALSE)
axis(2, at = seq(0, 1, length.out = 5))
points(x = 1:segment, y = p_s_save[,"p_s_prior"], col = input_col, pch = 19, cex = circle_size)
for(i in 1:segment){
  points(x = i, y = median(p_s_save_res[[i]][["p_s_prior"]]), col = output_col, cex = circle_size)
  #segments(x0 = i,
           # y0 = HPDI(p_s_save_res[[i]][["p_s_prior"]], prob = 0.89)[1],
           # x1 = i ,
           # y1 = HPDI(p_s_save_res[[i]][["p_s_prior"]], prob = 0.89)[2],
           # col = output_col)
  
  #j <- i 
  #points(x = i, y = post_pred_diff[j,], pch = "-", col = "coral")
}

mtext("model run", side = 1, line = 1, outer = FALSE, cex = 0.7)

mtext("Validation and posterior prediction results", side = 3, line = 1, outer = TRUE)

# p_l_move
plot(1,1, type = "n",
     xlim = c(1,segment),
     ylim = c(-0.1,1),
     bty = "n",
     xlab = "",
     ylab = "",
     axes = FALSE,
     main = "p of gaining tenure 
     when moving")
axis(1, at = 1:segment, labels = FALSE)
axis(2, at = seq(0, 1, length.out = 5))
points(x = 1:segment, y = p_l_move[,"p_l_prior"], col = input_col, pch = 19, cex = circle_size)
for(i in 1:segment){
  points(x = i, y = median(p_l_move_res[[i]][["p_l_prior"]]), col = output_col, cex = circle_size)
  #segments(x0 = i,
           # y0 = HPDI(p_l_move_res[[i]][["p_l_prior"]], prob = 0.89)[1],
           # x1 = i ,
           # y1 = HPDI(p_l_move_res[[i]][["p_l_prior"]], prob = 0.89)[2],
           # col = output_col)
           # 
  #j <- i + segment
  #points(x = i, y = post_pred_diff[j,], pch = "-", col = "coral")
}

# p_h_build
plot(1,1, type = "n",
     xlim = c(1,segment),
     ylim = c(-0.1, 1),
     bty = "n",
     xlab = "",
     ylab = "",
     axes = FALSE,
     main = "p of gaining house 
     when building")
axis(1, at = 1:segment, labels = FALSE)
axis(2, at = seq(0, 1, length.out = 5))
points(x = 1:segment, y = p_h_build[,"p_h_prior"], col = input_col, pch = 19, cex = circle_size)
for(i in 1:segment){
  points(x = i, y = median(p_h_build_res[[i]][["p_h_prior"]]), col = output_col, cex = circle_size)
  # segments(x0 = i,
  #          y0 = HPDI(p_h_build_res[[i]][["p_h_prior"]], prob = 0.89)[1],
  #          x1 = i ,
  #          y1 = HPDI(p_h_build_res[[i]][["p_h_prior"]], prob = 0.89)[2],
  #          col = output_col)
  
  #j <- i + (segment*2)
  #points(x = i, y = post_pred_diff[j,], pch = "-", col = "coral")
}


# payoff
plot(1,1, type = "n",
     xlim = c(1,segment),
     ylim = c(0,7.5),
     bty = "n",
     xlab = "",
     ylab = "",
     axes = FALSE,
     main = "Final payoffs")
axis(1, at = 1:segment, labels = FALSE)
axis(2, at = c(0,2,3,4,6), labels = c("", "bs", "f_p", "add", "h_p"), las = 1)
points(x = 1:segment, y = p_payoff[,"payoff_prior"], col = input_col, pch = 19, cex = circle_size)
for(i in 1:segment){
  points(x = i, y = median(p_payoff_res[[i]][["payoff_prior"]]), col = output_col, cex = circle_size)
  
  #j <- i + (segment*3)
  #points(x = i, y = post_pred_diff[j,], pch = "-", col = "coral")
}



# bc
plot(1,1, type = "n",
     xlim = c(1,segment),
     ylim = c(-0.1,1.2),
     bty = "n",
     xlab = "",
     ylab = "",
     axes = FALSE,
     main = "Savings needed/not needed 
     for building")
axis(1, at = 1:segment, labels = FALSE)
axis(2, at = 0:1)
points(x = 1:segment, y = p_bc[,"build_cond_prior"], col = input_col, pch = 19, cex = circle_size)

for(i in 1:segment){
  points(x = i, y = median(p_bc_res[[i]][["build_cond_prior"]]), col = output_col, cex = circle_size)
  
  #j <- i + (segment*4)
  #points(x = i, y = post_pred_diff[j,], pch = "-", col = "coral")
}

dev.off()



# TODO
# if we just compare generated data sets, how different are they from each other?


# generated data sets 
gen_data_all <- abc_validated[[3]]
count <- 10



#  create data frame to store differences between reference and generated data
diff_df <- data.frame(posterior_run = 1:count,
                      diff_1_1 = rep(NA, count),
                      diff_1_2 = rep(NA, count),
                      diff_2_2 = rep(NA, count),
                      diff_2_1 = rep(NA, count),
                      
                      diff_s1 = rep(NA, count),
                      diff_s2 = rep(NA, count),
                      diff_l1 = rep(NA, count),
                      diff_l2 = rep(NA, count),
                      diff_f1 = rep(NA, count),
                      diff_f2 = rep(NA, count),
                      diff_f3 = rep(NA, count),
                      output = rep(NA, count)
)


for(i in 1:10){
  j <- i + count
  
  generated_data <- as.data.frame(gen_data_all[i])
  reference_data <- as.data.frame(gen_data_all[j])
  
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




source("Functions/abc_posterior.R")
source("Functions/posterior_predict_data.R")

abc_output <- readRDS("Data/abc_output_1e4_bc_fix.RDS") 
reference_data <- read_csv("Data/data_for_analysis.csv")

# order is 1-1 1-2 2-1 2-2
trans_freq_ref <- as.numeric(table(reference_data$type_trans))
ger_ger <- trans_freq_ref[1]
ger_bash <- trans_freq_ref[2]
bash_ger <- trans_freq_ref[3]
bash_bash <- trans_freq_ref[4]


abc_posterior <- create_abc_posterior(abc_output = abc_output[[1]], comb_test = abc_output[[3]], sample_size = 1000)

sim_set <- posterior_predict_data(abc_posterior = abc_posterior)

#save sim set
saveRDS(sim_set, file = "Data/sim_set_from_posterior.RDS")


ger_ger_sim <- rep(NA, length(sim_set))
ger_bash_sim <- rep(NA, length(sim_set))
bash_ger_sim <- rep(NA, length(sim_set))
bash_bash_sim <- rep(NA, length(sim_set))



for(i in 1:length(sim_set)){
  
  one_sim <- as.data.frame(sim_set[i])
  
  ger_ger_sim[i] <- sum(one_sim$type_trans == "1-1")
  ger_bash_sim[i] <- sum(one_sim$type_trans == "1-2")
  bash_ger_sim[i] <- sum(one_sim$type_trans == "2-1")
  bash_bash_sim[i] <- sum(one_sim$type_trans == "2-2")
  
}



lwd = 1.5

alf <- 1
cols <- c(adjustcolor( "#994048", alpha.f = alf), 
          adjustcolor( "#232160", alpha.f = alf), 
          adjustcolor( "#76A2D0", alpha.f = alf),
          adjustcolor( "#C4687C", alpha.f = alf))



png(filename = "Figures/direct_comparison.png", width = 18, height = 10, units = "cm", res = 500)

par(mfrow = c(1,4), 
    mar = c(3,3,3,2),
    oma = c(1,1,4,1),
    family = "serif")


plot(density(ger_ger_sim),
             ylim = c(0,0.03),
             xlim = c(0,500),
             col = cols[1],
             main = "ger to ger transitions", 
             lwd = 3,
             xaxt = "n",
             xlab = "",
             yaxt = "n",
             bty = "n")
axis(1, at = c(0,100,200,300,400,500))
axis(2, at = c(0, 0.01, 0.03))
abline(v = ger_ger, col = cols[1], lty = 2, lwd = lwd)

plot(density(ger_bash_sim),
             ylim = c(0,0.03),
             xlim = c(0,500),
             col = cols[3],
             main = "ger to bashin transitions", 
             lwd = 3,
             xaxt = "n",
             xlab = "",
             yaxt = "n",
             bty = "n")
axis(1, at = c(0,100,200,300,400,500))
axis(2, at = c(0, 0.01, 0.03))
abline(v = ger_bash, col = cols[3], lty = 2, lwd = lwd) 

plot(density(bash_ger_sim),
             ylim = c(0,0.03),
             xlim = c(0,500),
             col = cols[4],
             main = "bashin to ger transitions", 
             lwd = 3,
             xaxt = "n",
             xlab = "",
             yaxt = "n",
             bty = "n")
axis(1, at = c(0,100,200,300,400,500))
axis(2, at = c(0, 0.01, 0.03))
abline(v = bash_ger, col = cols[4], lty = 2, lwd = lwd)

plot(density(bash_bash_sim),
             ylim = c(0,0.03),
             xlim = c(0,500),
             col = cols[2],
             main = "bashin to bashin transitions", 
             lwd = 3,
             xaxt = "n",
             xlab = "",
             yaxt = "n",
             bty = "n")
axis(1, at = c(0,100,200,300,400,500))
axis(2, at = c(0, 0.01, 0.03))
abline(v = bash_bash, col = cols[2], lty = 2, lwd = lwd)

# overall title
mtext("Comparison of transition types between Ulaanbaatar and data simulated from posterior", side = 3, line = 1, outer = TRUE)

dev.off()


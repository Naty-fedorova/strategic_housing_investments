abc_res <- readRDS("Data/abc_output_1e6_bc_fix.RDS")
abc_output <- abc_res[[1]]
comb_test <- abc_res[[3]]

# abc_output is difference result of particular run
# comb_test defines the particular run in terms of parameter values compared to data


comb_test$abc_output <- unlist(abc_output[1:nrow(comb_test)])

abc_ordered <- comb_test[order(comb_test$abc_output),]

abc_top <- abc_ordered[1:1000,]

plot(density(abc_top$p_s_prior))
plot(density(abc_top$p_l_prior))
plot(density(abc_top$p_h_prior))
plot(table(abc_top$payoff_prior))
plot(table(abc_top$build_cond_prior))

prior_col <- "#6e793e"
posterior_col <- "#793e6e"

png(filename = "Figures/posteriors_1e6_rejection_bc_fix.png", width = 15, height = 10, units = "cm", res = 500)
par(mfrow = c(2,3), 
    mar = c(3,3,3,2),
    oma = c(1,1,4,1))

nf <- layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
layout.show(nf)

plot(density(comb_test$p_s_prior), ylim = c(0,10), 
     col = prior_col,
     main = "p to save when saving", 
     lwd = 3,
     xaxt = "n",
     xlab = "",
     yaxt = "n",
     bty = "n")
lines(density(abc_top$p_s_prior), col = posterior_col, lwd = 3)
axis(1, at = c(0,0.25,0.5,0.75,1))
axis(2, at = c(0, 5, 10))
mtext("probability value", side = 1, line = 2, cex = 0.75)
mtext("density", side = 2, line = 2, cex = 0.75)
text(x = 0.34, y = 8, "posterior", col = posterior_col)
text(x = 0.5, y = 2.8, "prior", col = prior_col)

plot(density(comb_test$p_l_prior), ylim = c(0,10), 
     col = prior_col, 
     main = "p of tenure when moving", 
     lwd = 3,
     xaxt = "n",
     ylab = "",
     xlab = "",
     yaxt = "n",
     bty = "n")
lines(density(abc_top$p_l_prior), col = posterior_col, lwd = 3)
axis(1, at = c(0,0.25,0.5,0.75,1))
axis(2, at = c(0, 5, 10))

plot(density(comb_test$p_h_prior), ylim = c(0,10), 
     col = prior_col, 
     main = "p of house when building", 
     lwd = 3,
     xaxt = "n",
     xlab = "",
     ylab = "",
     yaxt = "n",
     bty = "n")
lines(density(abc_top$p_h_prior), col = posterior_col, lwd = 3)
axis(1, at = c(0,0.25,0.5,0.75,1))
axis(2, at = c(0, 5, 10))

payoff_prior <- table(sample(comb_test$payoff_prior, size = 1000))
payoff_posterior <- table(abc_top$payoff_prior)
payoff_prior_post <- rbind(payoff_prior, payoff_posterior)

barplot(payoff_prior_post,beside=T, 
        col = c(prior_col, posterior_col),
        main = "payoff scenario",
        xaxt = "n",
        yaxt = "n",
        ylim = c(0,600),
        xlim = c(0,14),
        ylab = "",
        xlab = "",
        border = NA)
axis(1, at = c(0,2,5,8,11,14), labels = c("","mb", "fp", "add","h_p", ""))
axis(2, at = c(0,300,600))
mtext("payoff value", side = 1, line = 2)
mtext("frequency", side = 2, line = 2)

bc_prior <- table(sample(comb_test$build_cond_prior, size = 1000))
bc_posterior <- table(abc_top$build_cond_prior)

bc_prior_post <- rbind(bc_prior, bc_posterior)

barplot(bc_prior_post,beside=T, 
        col = c(prior_col, posterior_col),
        main = "Are savings needed to \n build?",
        xaxt = "n",
        yaxt = "n",
        ylim = c(0,1000),
        xlim = c(0,8),
        ylab = "",
        xlab = "",
        border = NA)
axis(1, at = c(0,2,5,8), labels = c("","no", "yes",""))
axis(2, at = c(0,500,1000))

# overall title
mtext("Prior and posterior distributions of model parameters", side = 3, line = 1, outer = TRUE)
dev.off()
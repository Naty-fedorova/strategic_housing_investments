# plot with payoff tree, bc contrast, for scenario 3,4,6

source("Functions/sdp_model_functions.R")
source("Functions/payoff_tree_plot.R")
source("Functions/sim_var_functions.R")


sim_output <- readRDS("Data/sdp_model_param_sweep_bc_fix.RDS")

beh_frq_sweep <- beh_frq(sim_output = sim_output)

jobs <- expand.grid(p_s_save = c(0.25, .5, .75),   
                    p_l_move = c(0.25, .5, .75),
                    p_h_build = c(0.25, .5, .75),
                    p_s_loss = c(0.25, .5, .75),
                    p_force_move = c(0.25, .5, .75),
                    build_condition = c(0,1),
                    scenario = c(2, 3, 4, 6))

maxt <- 40 # change according to sim_output if necessary

tr <- 0.1 # transparency
colors <- c("#FF5733", "#7DCFF7","#772854") # orange is build, blue is save, prune is move
colors <- adjustcolor(colors, tr)



png(filename = "Figures/payoff_trees.png", width = 17, height = 9, units = "cm", res = 500)
par(mfrow = c(1, 3),
    mar = c(3,1,3,0),
    oma = c(0,0,4,0),
    family = "serif")

scs <- c(3,4,6)
texts <- c("Family Priority", "Additive", "House Priority")
labs <- c("I.", "II.", "III.")

for (i in 1:3) {
  sc <- scs[i]
  final_payoffs <- final_payoff_func(scenario = sc)
  if(i == 1){
    payoff_tree(final_payoffs = final_payoffs, title = "", labs = "yes", labs_leg = "no")
  } else{
    payoff_tree(final_payoffs = final_payoffs, title = "", labs = "no", labs_leg = "no")
  }
  mtext(texts[i], side = 3)
  mtext(labs[i], side = 3, line = 0, at = -1)
}

mtext("Final payoffs for 3 optimisation scenarios", side = 3, line = 0, outer = TRUE, cex = 1.5)
dev.off()


png(filename = "Figures/payoff_trees_legend.png", width = 10, height = 5, units = "cm", res = 500)
par(mfrow = c(1, 1),
    mar = c(1,1,1,1),
    oma = c(0,0,1,0),
    family = "serif")

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', xlim = c(-1,0.3), ylim = c(-1,6.2))

cex <- 1.5
# legend binary states
arrows(x0 = -1, y0 = 5, x1 = 0, y1 = 6, length = 0.0, lty = 2)
arrows(x0 = -1, y0 = 5, x1 = 0, y1 = 4, length = 0.0, lty = 1)
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
dev.off()


png(filename = "Figures/key_contrast_plot_bc_fix.png", width = 17, height = 12, units = "cm", res = 500)

par(mfrow = c(2, 3),
    mar = c(3,2,3,1),
    oma = c(0,4,4,2),
    family = "serif")


## build condition 0, sc 3
selected_set <- which(jobs$build_condition == 0 & jobs$scenario == 3)

plot(x = 1, y = 1, 
     main = "",
     xlab = "", 
     ylab = "",
     type = "n", 
     xlim = c(0, maxt), 
     ylim = c(0, 110), # plotting will fail under different values of N - change to 1.1 * N
     xaxt = "n",
     yaxt = "n",
     bty = "n")
axis(2, at = c(0,50, 100), labels = TRUE)
axis(1, labels = TRUE)
axis(2, at = c(0,50, 100), labels = TRUE)
mtext("Behavioral frequency", side = 2, line = 2)
mtext("Time", side = 1, line = 3)
mtext("IV.", side = 3, line = 0, at = -1)
mtext("Family priority", side = 3, line = 0, at = 20)

mtext(substitute(bold("Build condition = 0")), side = 2, line = 4)

sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)


## build condition 0, sc 4
selected_set <- which(jobs$build_condition == 0 & jobs$scenario == 4)

plot(x = 1, y = 1, 
     xlab = "", 
     ylab = "",
     type = "n", 
     xlim = c(0, maxt), 
     ylim = c(0, 110),
     xaxt = "n",
     yaxt = "n",
     bty = "n")
axis(1, labels = FALSE)
axis(2, at = c(0,50, 100), labels = FALSE)
mtext("V.", side = 3, line = 0, at = -1)
mtext("Additive", side = 3, line = 0, at = 20)

sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)

# build condition 0, sc 6
selected_set <- which(jobs$build_condition == 0 & jobs$scenario == 6)

plot(x = 1, y = 1, 
     xlab = "", 
     ylab = "",
     type = "n", 
     xlim = c(0, maxt), 
     ylim = c(0, 110),
     xaxt = "n",
     yaxt = "n",
     bty = "n")
mtext("VI.", side = 3, line = 0, at = -1)
axis(1, labels = FALSE)
axis(2, at = c(0,50, 100), labels = FALSE)
mtext("House Priority", side = 3, line = 0, at = 20)

sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)


#####


# build condition 1, sc 3
selected_set <- which(jobs$build_condition == 1 & jobs$scenario == 3)
plot(x = 1, y = 1,
     main = "",
     xlab = "", 
     ylab = "",
     type = "n", 
     xlim = c(0, maxt), 
     ylim = c(0, 110),
     xaxt = "n",
     yaxt = "n",
     bty = "n")
axis(1, labels = FALSE)
axis(2, at = c(0,50, 100), labels = FALSE)
mtext("VII.", side = 3, line = 0, at = -1)

mtext(substitute(bold("Build condition = 1")), side = 2, line = 4)

sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)


# build condition 1, sc 4
selected_set <- which(jobs$build_condition == 1 & jobs$scenario == 4)
plot(x = 1, y = 1, 
     xlab = "", 
     ylab = "",
     type = "n", 
     xlim = c(0, maxt), 
     ylim = c(0, 110),
     xaxt = "n",
     yaxt = "n",
     bty = "n")
axis(1, labels = FALSE)
axis(2, at = c(0,50, 100), labels = FALSE)
mtext("VIII.", side = 3, line = 0, at = -1)

sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)


## build condition 1, sc 6
selected_set <- which(jobs$build_condition == 1 & jobs$scenario == 6)
plot(x = 1, y = 1, 
     xlab = "", 
     ylab = "",
     type = "n", 
     xlim = c(0, maxt), 
     ylim = c(0, 110),
     xaxt = "n",
     yaxt = "n",
     bty = "n")
axis(1, labels = FALSE)
axis(2, at = c(0,50, 100), labels = FALSE)
mtext("IX.", side = 3, line = 0, at = -1)

sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)

mtext("Build condition contrast plots for 3 optimisation scenarios", side = 3, line = 0, outer = TRUE, cex = 1.3)

dev.off()




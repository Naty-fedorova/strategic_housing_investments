
source("Functions/sim_var_functions.R")
source("Functions/abc_posterior.R")


abc_output <- readRDS("Data/abc_output_1e6_bc_fix.RDS")

# extract joint posterior
abc_posterior <- create_abc_posterior(abc_output = abc_output[[1]], comb_test = abc_output[[3]], sample_size = 1000)

# construct sweep from posterior
sample_size <- 100

p_s_save <- sample(abc_posterior$p_s_prior, sample_size <- 100, replace = FALSE)  
p_l_move <- sample(abc_posterior$p_l_prior, sample_size <- 100, replace = FALSE)
p_h_build <- sample(abc_posterior$p_h_prior, sample_size <- 100, replace = FALSE)
build_condition <- sample(abc_posterior$build_cond_prior, sample_size <- 100, replace = FALSE)
scenario <- sample(abc_posterior$payoff_prior, sample_size <- 100, replace = FALSE)

# not in posterior, just from prior valies 
p_s_loss <-  rbeta(n = sample_size, 2, 5)
p_force_move <- rbeta(n = sample_size, 2, 5)

jobs <- data.frame(p_s_save, p_l_move, p_h_build, p_s_loss, p_force_move, build_condition, scenario)

sim_output <- vector("list", length = nrow(jobs))

for(i in 1:nrow(jobs)){
  
  sim_output[[i]] <- run_sim_sweep(sim_runs = 1,
                                   maxt = 40,
                                   N = 1000,
                                   p_s_save = jobs$p_s_save[i],   
                                   p_l_move = jobs$p_l_move[i],
                                   p_h_build = jobs$p_h_build[i],
                                   p_s_loss = jobs$p_s_loss[i],
                                   p_force_move = jobs$p_force_move[i],
                                   build_condition = jobs$build_condition[i],
                                   scenario = jobs$scenario[i])
  
}


png(filename = "Figures/inferred_optimal_strategy_bc_fix.png", width = 15, height = 10, units = "cm", res = 1000)

par(family = "serif")

tr <- 0.6
colors <- c("#FF5733", "#7DCFF7","#772854") # orange is build, blue is save, prune is move (order is orange, blue, prune )
colors <- adjustcolor(colors, tr)

plot(x = 1, y = 1, 
     main = "Inferred optimal strategy",
     xlab = "Time", 
     ylab = "Behavioral frequency",
     type = "n", 
     xlim = c(0, 40), 
     ylim = c(0,1000),
     yaxt = "n",
     bty = "n")
axis(2, at = c(0,500,1000), label = c(0,500,1000))

# all
for(i in 1:nrow(jobs)){
  
  selected_run <- sim_output[[i]]
  x <- 1:40
  
  selected_t <- selected_run[ ,"beh", 1]
  ys <- length(selected_t[which(selected_t == "save")])
  ym <- length(selected_t[which(selected_t == "move")])
  yb <- length(selected_t[which(selected_t == "build")])
  
  for(t in 2:40){
    selected_t <- selected_run[ ,"beh", t]
    ys <- c(ys, length(selected_t[which(selected_t == "save")]))
    ym <- c(ym, length(selected_t[which(selected_t == "move")]))
    yb <- c(yb, length(selected_t[which(selected_t == "build")]))
  }
  
  lines(x, ys, col = colors[2])
  lines(x, ym, col = colors[3])
  lines(x, yb, col = colors[1])
 
}


dev.off()




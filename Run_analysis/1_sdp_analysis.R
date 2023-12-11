source("Functions/sdp_model_functions.R")
source("Functions/sim_var_functions.R")


# run parameter sweep

# number of runs for each parameter combination
sim_runs <- 1

# generate jobs to sweep through
jobs <- expand.grid(p_s_save = c(0.25, .5, .75),   
                    p_l_move = c(0.25, .5, .75),
                    p_h_build = c(0.25, .5, .75),
                    p_s_loss = c(0.25, .5, .75),
                    p_force_move = c(0.25, .5, .75),
                    build_condition = c(0,1),
                    scenario = c(2, 3, 4, 6))


sim_output <- list()

for(i in 1:nrow(jobs)){
  
  sim_output[[i]] <- run_sim_sweep(sim_runs = sim_runs,
                                   maxt = 40,
                                   N = 100,
                                   p_s_save = jobs$p_s_save[i],   
                                   p_l_move = jobs$p_l_move[i],
                                   p_h_build = jobs$p_h_build[i],
                                   p_s_loss = jobs$p_s_loss[i],
                                   p_force_move = jobs$p_force_move[i],
                                   build_condition = jobs$build_condition[i],
                                   scenario = jobs$scenario[i])
  
}


saveRDS(sim_output, file = "Data/sdp_model_param_sweep.RData")


sim_output <- readRDS("Data/sdp_model_param_sweep_bc_fix.RDS")

# parameter sweep plot for each payoff scenario
source("Scripts/sdp_param_sweep_plot.R")

# key contrast plot
source("Scripts/key_contrast_plot.R")











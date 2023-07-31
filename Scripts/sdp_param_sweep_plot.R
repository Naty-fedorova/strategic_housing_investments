source("Functions/sim_var_functions.R")

sim_output <- readRDS("Data/sdp_model_param_sweep.RData")

# generate jobs to sweep through
jobs <- expand.grid(p_s_save = c(0.25, .5, .75),   
                    p_l_move = c(0.25, .5, .75),
                    p_h_build = c(0.25, .5, .75),
                    p_s_loss = c(0.25, .5, .75),
                    p_force_move = c(0.25, .5, .75),
                    build_condition = c(0,1),
                    scenario = c(2, 3, 4, 6))


maxt <- 40
sim_runs <- length(sim_output[[1]])
beh_frq_sweep <- beh_frq(sim_output = sim_output, sim_runs = sim_runs, maxt = maxt)


tr <- 0.1
colors <- c("#FF5733", "#7DCFF7","#772854") # orange is build, blue is save, prune is move
colors <- adjustcolor(colors, tr)

all_sc <- c(2, 3, 4, 6)
sc_names <- c("Baseline", "Family baseline", "Additive", "House priority")
at_l <- -5


make_filename = function(label, number){
  # these can be easily turned into parameters
  dir <- "Figures"
  
  filename <- paste(label, number, sep="_")
  filename <- paste0(filename, ".png")
  filename <- file.path(dir, filename)
  
}

for( i in all_sc ){
  sc <- i
  j <- which(all_sc == sc)
  filename <- make_filename(label = "sdp_param_sweep_sc", number = i)
  
  png(filename = filename, width = 15, height = 30, units = "cm", res = 300)
  par(mfrow = c(6, 3),
      mar = c(2,4,2,0),
      oma = c(4,4,4,4))
  
  # p_s_save
  selected_set <- which(jobs$p_s_save == 0.25 & jobs$scenario == sc)
  
  plot(x = 1, y = 1, 
       xlab = "", 
       ylab = "",
       type = "n", 
       xlim = c(0, maxt), 
       ylim = c(0, 110),
       xaxt = "n",
       yaxt = "n",
       bty = "n")
  axis(1)
  axis(2, at = c(0,50, 100), labels = TRUE)
  mtext("param = 0.25", side = 3, line = 1)
  mtext("A.", side = 3, line = 0, at = at_l)
  mtext("Behavioral frequency", side = 2, line = 2, at = 50, cex = 0.5)
  mtext("Time", side = 1, line = 2, at = 20, cex = 0.5)
  mtext("p_s_save", side = 2, line = 3, adj = 0)
  mtext(paste("Parameter sweep of behavioral frequencies \n scenario = ", sc_names[j]), side = 3, line = 1, outer = TRUE )
  mtext("Model parameters", side = 2, line = 1, outer = TRUE)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_s_save == 0.5 & jobs$scenario == sc)
  
  plot(x = 1, y = 1, 
       xlab = "", 
       ylab = "",
       axes = FALSE,
       type = "n", 
       xlim = c(0, maxt), 
       ylim = c(0, 110),
       xaxt = "n",
       yaxt = "n",
       bty = "n")
  axis(1, labels = FALSE)
  axis(2, at = c(0,50, 100), labels = FALSE)
  mtext("param = 0.5", side = 3, line = 1)
  mtext("B.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_s_save == 0.75 & jobs$scenario == sc)
  
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
  mtext("param = 0.75", side = 3, line = 1)
  mtext("C.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  #
  # p_l_move
  selected_set <- which(jobs$p_l_move == 0.25 & jobs$scenario == sc)
  
  plot(x = 1, y = 1, 
       xlab = "", 
       ylab = "",
       axes = FALSE,
       type = "n", 
       xlim = c(0, maxt), 
       ylim = c(0, 110),
       xaxt = "n",
       yaxt = "n",
       bty = "n")
  axis(1, labels = FALSE)
  axis(2, at = c(0,50, 100), labels = TRUE)
  mtext("p_l_move", side = 2, line = 3, adj = 0)
  mtext("D.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_l_move == 0.5 & jobs$scenario == sc)
  
  plot(x = 1, y = 1, 
       xlab = "", 
       ylab = "",
       axes = FALSE,
       type = "n", 
       xlim = c(0, maxt), 
       ylim = c(0, 110),
       xaxt = "n",
       yaxt = "n",
       bty = "n")
  axis(1, labels = FALSE)
  axis(2, at = c(0,50, 100), labels = FALSE)
  mtext("E.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_l_move == 0.75 & jobs$scenario == sc)
  
  plot(x = 1, y = 1, 
       xlab = "", 
       ylab = "",
       axes = FALSE,
       type = "n", 
       xlim = c(0, maxt), 
       ylim = c(0, 110),
       xaxt = "n",
       yaxt = "n",
       bty = "n")
  axis(1, labels = FALSE)
  axis(2, at = c(0,50, 100), labels = FALSE)
  mtext("F.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  # p_h_build
  selected_set <- which(jobs$p_h_build == 0.25 & jobs$scenario == sc)
  
  plot(x = 1, y = 1, 
       xlab = "", 
       ylab = "",
       axes = FALSE,
       type = "n", 
       xlim = c(0, maxt), 
       ylim = c(0, 110),
       xaxt = "n",
       yaxt = "n",
       bty = "n")
  axis(1, labels = FALSE)
  axis(2, at = c(0,50, 100), labels = TRUE)
  mtext("p_h_build", side = 2, line = 3, adj = 0)
  mtext("H.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_h_build == 0.5 & jobs$scenario == sc)
  
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
  mtext("I.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_h_build == 0.75 & jobs$scenario == sc)
  
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
  mtext("J.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  #
  # p_s_loss
  selected_set <- which(jobs$p_s_loss == 0.25 & jobs$scenario == sc)
  
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
  axis(2, at = c(0,50, 100), labels = TRUE)
  mtext("p_s_loss", side = 2, line = 3, adj = 0)
  mtext("K.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_s_loss == 0.5 & jobs$scenario == sc)
  
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
  mtext("L.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_s_loss == 0.75 & jobs$scenario == sc)
  
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
  mtext("M.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  # p_force_move
  selected_set <- which(jobs$p_force_move == 0.25 & jobs$scenario == sc)
  
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
  axis(2, at = c(0,50, 100), labels = TRUE)
  mtext("p_force_move", side = 2, line = 3, adj = 0)
  mtext("N.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_force_move == 0.5 & jobs$scenario == sc)
  
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
  mtext("O.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$p_force_move == 0.75 & jobs$scenario == sc)
  
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
  mtext("P.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  # build condition
  selected_set <- which(jobs$build_condition == 0 & jobs$scenario == sc)
  
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
  axis(2, at = c(0,50, 100), labels = TRUE)
  mtext("b_c = 0", side = 3, line = 0)
  mtext("build_condition", side = 2, line = 3, adj = 0)
  mtext("Q.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  selected_set <- which(jobs$build_condition == 1 & jobs$scenario == sc)
  
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
  mtext("b_c = 1", side = 3, line = 0)
  mtext("R.", side = 3, line = 0, at = at_l)
  
  sweep_lines(selected_set = selected_set, beh_frq_sweep = beh_frq_sweep)
  
  dev.off()
  
}



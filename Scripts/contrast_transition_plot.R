library("tidyverse")

source("Functions/prep_data.R")
source("Functions/transition_contrast_function.R")
# real data
source("Functions/generate_build_table.R")


alf <- 0.6
cols <- c(adjustcolor( "#994048", alpha.f = alf), 
          adjustcolor( "#232160", alpha.f = alf), 
          adjustcolor( "#76A2D0", alpha.f = alf),
          adjustcolor( "#C4687C", alpha.f = alf))

alf <- 1
legend_cols <- c(adjustcolor( "#994048", alpha.f = alf), 
                 adjustcolor( "#232160", alpha.f = alf), 
                 adjustcolor( "#76A2D0", alpha.f = alf),
                 adjustcolor( "#C4687C", alpha.f = alf))

lwd <- 1

# generate reference data
set.seed(100)
reference_data_3_0 <- prep_data(real = "no", N_sim_agents = 1000, p_s_save = 0.3,
                                p_l_move = 0.6,
                                p_h_build = 0.9,
                                scenario = 3,
                                build_condition = 0)

names(reference_data_3_0)[names(reference_data_3_0) == "h_state"] <- "house_state"

reference_data_3_1 <- prep_data(real = "no", N_sim_agents = 1000, p_s_save = 0.3,
                                p_l_move = 0.6,
                                p_h_build = 0.9,
                                scenario = 3,
                                build_condition = 1)

names(reference_data_3_1)[names(reference_data_3_1) == "h_state"] <- "house_state"

reference_data_4_0 <- prep_data(real = "no", N_sim_agents = 1000, p_s_save = 0.3,
                                p_l_move = 0.6,
                                p_h_build = 0.9,
                                scenario = 4,
                                build_condition = 0)

names(reference_data_4_0)[names(reference_data_4_0) == "h_state"] <- "house_state"

reference_data_4_1 <- prep_data(real = "no", N_sim_agents = 1000, p_s_save = 0.3,
                                p_l_move = 0.6,
                                p_h_build = 0.9,
                                scenario = 4,
                                build_condition = 1)

names(reference_data_4_1)[names(reference_data_4_1) == "h_state"] <- "house_state"

reference_data_6_0 <- prep_data(real = "no", N_sim_agents = 1000, p_s_save = 0.3,
                                p_l_move = 0.6,
                                p_h_build = 0.9,
                                scenario = 6,
                                build_condition = 0)

names(reference_data_6_0)[names(reference_data_6_0) == "h_state"] <- "house_state"

reference_data_6_1 <- prep_data(real = "no", N_sim_agents = 1000, p_s_save = 0.3,
                                p_l_move = 0.6,
                                p_h_build = 0.9,
                                scenario = 6,
                                build_condition = 1)

names(reference_data_6_1)[names(reference_data_6_1) == "h_state"] <- "house_state"



png(filename = "Figures/contrast_transition_plot_n.png", width = 25 , height = 15, units = "cm", res = 500)

par(mfrow = c(2,3), 
    mar = c(4,4,4,0),
    oma = c(2,2,2,0))

# nf <- layout(matrix(c(1,2,3,4,5,6,7,7), nrow = 4, ncol = 2, byrow = TRUE))
# layout.show(nf)

# A
# fam baseline, bc = 0
plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "House state",
     xlab = "Time in Ulaanbaatar",
     main = "",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("mobile", "fixed"))

mtext("Building condition contrast for dwelling transitions", side = 3, outer = TRUE)

mtext("I.", side = 3, line = 1, at = -5)
mtext("Family baseline", side = 3, line = 1, outer = FALSE)
mtext("Build condition = 0", side = 2, line = 4)

transition_contrast(reference_data = reference_data_3_0)

# C
# additive bc = 0
plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "House state",
     xlab = "",
     main = "",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("mobile", "fixed"))
mtext("II.", side = 3, line = 1, at = -5)
mtext("Additive", side = 3, line = 1, outer = FALSE)

transition_contrast(reference_data = reference_data_4_0)

# E
# house bc = 0
plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "House state",
     xlab = "",
     main = "",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("mobile", "fixed"))
mtext("III.", side = 3, line = 1, at = -5)
mtext("House priority", side = 3, line = 1, outer = FALSE)

transition_contrast(reference_data = reference_data_6_0)




# B
# fam baseline bc = 1
plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "",
     xlab = "",
     main = "",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("mobile", "fixed"))
mtext("IV.", side = 3, line = 1, at = -5)
mtext("Build condition = 1", side = 2, line = 4)

transition_contrast(reference_data = reference_data_3_1)



# D
# additive bc = 1
plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "",
     xlab = "",
     main = "",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("mobile", "fixed"))
mtext("V.", side = 3, line = 1, at = -5)

transition_contrast(reference_data = reference_data_4_1)



# F
# house bc = 1
plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "",
     xlab = "",
     main = "",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("mobile", "fixed"))
mtext("VI.", side = 3, line = 1, at = -5)

transition_contrast(reference_data = reference_data_6_1)

# legend
# plot(x = 1, xlim = c(0,45), ylim = c(0.7,2.3), type = "n",
#      yaxt = "n",
#      xaxt = "n",
#      ylab = "",
#      xlab = "",
#      main = "Legend",
#      bty = "n"
# )
#axis(2, at = c(1,2), labels = c("ger", "bashin"))
# legend(x = "center", y = 1.7, legend = c("Ger", "Bashin", "G.to B.", "B.to G."),
#        col = legend_cols,
#        lwd = 4,
#        bty = "n",
#        y.intersp = 0.75,
#        cex = 2)
#mtext("F.", side = 3, line = 1, at = -20)


dev.off()



# plot for real data

# # streamline build table to just take everything from 1980
# build_table_s <- build_table[, -c(2:39)] 

build_table_s <- build_table

png(filename = "contrast_transition_plot_ub.png", width = 15 , height = 10, units = "cm", res = 500)
plot(x = 1, xlim = c(0,90), ylim = c(0.7,2.3), type = "n",
     yaxt = "n",
     ylab = "House state",
     xlab = "Time in Ulaanbaatar",
     main = "Dwelling transitions in UB data",
     bty = "n"
)
axis(2, at = c(1,2), labels = c("ger", "bashin"))
for(i in 1:nrow(build_table_s)){
  
  time_seq <- build_table_s[i,2:ncol(build_table_s)]
  
  if(sum(time_seq == 0) < (ncol(build_table_s) - 1)){
    
    # index of first non-zero value
    indx <- which(time_seq != 0)
    
    # remove everything before that
    time_seq <- as.numeric(time_seq[indx[1]:(ncol(build_table_s) - 1)])
    
    transition <- unique(time_seq)
    
    print(transition)
    
    if(length(transition) == 1){
      if(unique(transition) == 1){
        # add some "jitter"
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, col = cols[1], lwd = lwd)
      }
      
      if(unique(time_seq) == 2){
        # add some "jitter"
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, col = cols[2], lwd = lwd)
      }
    }else{
      # hh that transition from ger to bashin 
      if(sum(transition == c(1,2)) == 2){
        #print(c(i, time_seq))
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, lwd = lwd, col = cols[3])
      }
      
      # these are hh that transition from bashin to ger
      if(sum(transition == c(2,1)) == 2){
        #print(c(i, time_seq))
        time_seq <- time_seq + rnorm(1, mean = 0, sd = 0.05)
        lines(x = 1:length(time_seq), y = time_seq, lwd = lwd, col = cols[4])
      } 
    }
  }
}
dev.off()





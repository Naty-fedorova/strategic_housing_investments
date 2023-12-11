source("Functions/abc_runs.R")
source("Functions/abc_posterior.R")

# run abc
# if wanting to run with synthetic data, real_data = "no"
# look at documentation for abc_runs in Functions folder for details on input parameters
abc_output <- abc_runs(n_comb = 1e6, N = 825, real_data = "yes") # n_comb for analysis in manuscript is 1e6

saveRDS(abc_output, file="Data/abc_output_1e6.RDS")


# Where to from now?
# run scripts to generate plots

# plot showing abc posteriors against priors
source("Scripts/prior_posterior_plot.R")
        
# posterior prediction for dwelling transitions (takes abc posterior and uses SDP model to simulate data, from which dwelling transitions are plotted)
source("Scripts/ub_posterior_pred_plot.R")

# inferred optimal strategy (takes abc posterior as input into SDP model, plots implied optimal strategy in the form of behavioral frequencies over time)
source("Scripts/optimal_strat_from_abc_posterior.R")


















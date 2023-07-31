# Strategic housing investments: A stochastic dynamic programming model and empirical application in Ulaanbaatar, Mongolia

[![License: CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

How do households decide when to build a fixed, higher-investment dwelling? We construct a stochastic dynamic programming model that explores the trade-offs between building, moving, and saving that households must balance over time. We then test this model with data from the ger districts of Ulaanbaatar, Mongolia

This repository contains all the code necessary for the manuscript "Strategic housing investments and the evolution of urban settlements: Optimality modeling and empirical application in central Asia" by Natalia Fedorova & Richard McElreath



## Dependencies

- R (3.3.6 or greater) https://cran.r-project.org/
- tidyverse package https://tidyverse.tidyverse.org/
- rethinking package (v1.59 or greater), http://xcelab.net/rm/software/


## Instructions:

In R, set the working directory to that containing this readme file. For example, on a Mac or Linux machine, you might type into the command prompt

```
  setwd('~/Desktop/strategic_housing_investments')
```

Running the analysis in the manuscript requires running the full SDP parameter sweep for the optimality model, and then the ABC analysis to explore the best-fit parameter values in relation to data from Ulaanbaatar.

Scripts to do so are in the Run_analysis folder 

In the same folder, sdp_explore.RMD contains a lighter script which will allow you to explore how the optimal strategy responds to changes in the parameter space and payoff scenario, without running the entire parameter sweep.



## Authors & License
The project is maintained by Natalia Fedorova in a Github repository at https://github.com/Naty-fedorova/Dutch-historical-mobility and licensed under Creative Commons [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). See LICENSE.md for details.




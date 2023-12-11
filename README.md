# Strategic housing investments: A stochastic dynamic programming model and empirical application in Ulaanbaatar, Mongolia

[![License: CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

How do households decide when to build a fixed, higher-investment dwelling? We construct a stochastic dynamic programming model that explores the trade-offs between building, moving, and saving that households must balance over time. We then test this model with data from the ger districts of Ulaanbaatar, Mongolia

This repository contains all the code necessary for the manuscript "Strategic housing investments and the evolution of urban settlements: Optimality modeling and empirical application in central Asia" by Natalia Fedorova, Anne Kandler & Richard McElreath



## Dependencies

- R (3.3.6 or greater) https://cran.r-project.org/
- tidyverse package https://tidyverse.tidyverse.org/
- rethinking package (v1.59 or greater), http://xcelab.net/rm/software/

## Data
- The analysis presented here is run on a dataset constructed from household surveys conducted with households residing in Ulaanbaatar, Mongolia. More information on the sort of data available is present in the manuscript
- Data needed for the ABC analysis and for most of the plots is available here: (to be updated as part of publication process)
- Data needed for dwelling transitions trajectories is not available due to privacy concerns


## Instructions:


In R, set the working directory to that containing this readme file. For example, on a Mac or Linux machine, you might type into the command prompt

```
setwd('~/Desktop/strategic_housing_investments')
```

The project contains 3 subfolders:
Functions
Scripts
Run_analysis

The Functions folder holds functions that are called in Scripts and in Run_analysis, Scripts focus mainly on generating plots 

You will also need to create sub-folders to hold figures and data

```
dir.create("Figures")
dir.create("Data")
```

Running the analysis in the manuscript requires running the full SDP parameter sweep for the optimality model, and then the ABC analysis to explore the best-fit parameter values in relation to data from Ulaanbaatar.

Scripts to do so are in the Run_analysis folder 

In the same folder, sdp_explore.RMD contains a lighter script which will allow you to explore how the optimal strategy responds to changes in the parameter space and payoff scenario, without running the entire parameter sweep.

## Notes on run time

The analysis documented here is computationally intensive, and has been constructed with access to a cluster in mind. Depending on specifications, the SDP parameter sweep will take several hours, while the full ABC (1e6 parameter combinations) should really not be attempted on a singular machine. However, fewer parameter combinations will still be informative, while not providing a fully resolved posterior. 


## Authors & License
The project is maintained by Natalia Fedorova in a Github repository at https://github.com/Naty-fedorova/strategic_housing_investments and licensed under Creative Commons [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). See LICENSE.md for details.




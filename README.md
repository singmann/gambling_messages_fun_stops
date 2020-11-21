
Each folder contains data anad analysis for one study.

### Roulette Study

Contains all data and script for the roulette experiment investigating the effect of gambling messages.

The main file is `roulette_analysis.Rmd` which creates the output document `roulette_analysis.html`.

The folder in the project are:

- `data`: all data as well as two text files describing the columns in the data.
- `model_fits`: binary files, each containing a fitted Bayesian model. These files are created in case they are not existing (not the folder though). Thus, deleting the files in `model_fits` and rerunning (i.e., knitting) `roulette_analysis.Rmd` will refit all model files and populate the folder again. Note that refitting requires a working `C++` toolchain for `rstan` as described here: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
- `figures`: figure files, some of which are used in the manuscript and created when running `roulette_analysis.Rmd`.

### Football Study

Contains all data and script for the football betting experiment investigating the effect of gambling messages.

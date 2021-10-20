library("checkpoint")
#create_checkpoint("2021-02-18", project_dir="e3_football_study/")
use_checkpoint("2021-02-18")

library("tidyverse")
#library("tidylog")
theme_set(theme_bw() + 
            theme(panel.grid.major.x = element_blank(), 
                  panel.grid.minor.x = element_blank()))
library("brms")
library("tidybayes")
library("BayesFactor")
library("binom")
par_labels <- c("Gamble at all?", 
                "Gamble everything?", 
                "Proportion bet?")
cond_labels <- c("None", "Yellow")
ylabel <- "Gambling message"
library("cowplot")

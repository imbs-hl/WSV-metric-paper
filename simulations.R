##' With this script the simulations from Laabs et al. "Identification of 
##' representative trees in random forests based on a new tree-based distance 
##' measure" can be reproduced. 
##' Please note, that the simulations in the paper were performed using 
##' batchtools on  a high throughout batch system. This script will implement 
##' the same calculations on your local system, which may lead to a high 
##' computation time. Comments will show you where you can save time or 
##' incorporate your own batch system. 

## Define directories
## Please define your main directory here. 
## This should be the directory you cloned the git repository into.
main_dir <- "~/"
## Create and define registry directory
dir.create(file.path(main_dir, "registires"), showWarnings = FALSE)
reg_dir <- file.path(main_dir, "registires")
## Create and define functions directory
dir.create(file.path(main_dir, "functions"), showWarnings = FALSE)
fun_dir <- file.path(main_dir, "functions")
## Create and define proc directory
dir.create(file.path(main_dir, "proc"), showWarnings = FALSE)
proc_dir <- file.path(main_dir, "proc")

## Load libraries
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(batchtools)
pacman::p_load(ranger)
pacman::p_load(devtools)
pacman::p_load(rpart)

if("timbR" %in% installed.packages()){
  library(timbR)
} else {
  devtools::install_github("imbs-hl/timbR", "master")
  library(timbR)
}

# --------------------------------------------------- #
#                  Data Simulation                    #
# --------------------------------------------------- #
## In this part the data will be simulated and saved 
## for later use

## Define constants ----
n <- 1000 ## Number of samples in random forest training data sets. You can save time by reducing this.

## Load functions ----
source(file.path(fun_dir, "simulate_rf_setting_1.R"))
source(file.path(fun_dir, "simulate_rf_setting_2.R"))
source(file.path(fun_dir, "simulate_rf_setting_3.R"))
source(file.path(fun_dir, "simulate_rf_setting_4.R"))
source(file.path(fun_dir, "simulate_rf_setting_5.R"))
source(file.path(fun_dir, "calculate_decision_tree.R"))
source(file.path(fun_dir, "calculate_distances.R"))

## Create registry ----
reg_name <- "simulate_metrics"

reg <- batchtools::makeExperimentRegistry(
    file.dir = file.path(reg_dir, reg_name),
    work.dir = main_dir,
    conf.file = NA, ## If you have a batch system, please enter conf file here,
    packages = c("ranger", "timbR", "rpart") ## Define which packages to use in your simulations
  )

## Add problems ----
## There is a separate function for generating each of the settings from the paper.
## You can save time, excluding settings you are not interested in. 
batchtools::addProblem(name = "simulate_setting_1",
                       reg = reg, 
                       fun = simulate_rf_setting_1,
                       data = n,
                       seed = 12345)
# batchtools::addProblem(name = "simulate_setting_2",
#                        reg = reg,
#                        fun = simulate_rf_setting_2,
#                        data = n,
#                        seed = 12345)
# batchtools::addProblem(name = "simulate_setting_3",
#                        reg = reg,
#                        fun = simulate_rf_setting_3,
#                        data = n,
#                        seed = 12345)
# batchtools::addProblem(name = "simulate_setting_4",
#                        reg = reg,
#                        fun = simulate_rf_setting_4,
#                        data = n,
#                        seed = 12345)
# batchtools::addProblem(name = "simulate_setting_5",
#                        reg = reg,
#                        fun = simulate_rf_setting_5,
#                        data = n,
#                        seed = 12345)

## Add algorithms to solve the problem ----
batchtools::addAlgorithm(reg = reg,
                         name = "distances",
                         fun = calculate_distances
)
batchtools::addAlgorithm(reg = reg,
                         name = "decision_tree",
                         fun = calculate_decision_tree
)

## define problem and algorithm designs ----
n_test    <- 100     ## Number of samples in test data set
n_val     <- 1000    ## Number of samples in validation data set
p         <- 100     ## Number of variables 
num_trees <- 500     ## Number of trees in random forest
eps       <- 1       ## Simulated noise in data set
mtry      <- sqrt(p) ## Mtry for random forest


prob.designs <- list(
  simulate_setting_1 = data.frame(p_eff       = 5, ## Number of true effect variables
                                  beta_eff    = 2, ## Effect size of true effect variables
                                  n_test      = n_test,
                                  n_val       = n_val, 
                                  p           = p,
                                  num.trees   = num_trees, 
                                  eps         = 1,  
                                  mtry        = mtry,
                                  min_node_size = c(10, 50, 100, 200), ## Minimal node  sizes in random forest
                                  stringsAsFactors = FALSE
  )#,
  # simulate_setting_2 = data.frame(p_eff       = 50,
  #                                 beta_eff    = 0.2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = c(10, 50, 100, 200),
  #                                 stringsAsFactors = FALSE
  # ),
  # simulate_setting_3 = data.frame(p_eff       = 5,
  #                                 beta_eff    = 2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 p_corr      = 5,
  #                                 n_blocks    = 5,
  #                                 cor         = 0.3,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = c(10, 50, 100, 200),
  #                                 stringsAsFactors = FALSE
  # ),
  # simulate_setting_4 = data.frame(p_eff       = 5,
  #                                 beta_eff    = 2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 p_int       = 5,
  #                                 beta_int    = 2,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = c(10, 50, 100, 200),
  #                                 stringsAsFactors = FALSE
  # ),
  # simulate_setting_5 = data.frame(p_eff_bin   = 5,
  #                                 p_eff_con   = 5,
  #                                 beta_eff    = 2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = c(10, 50, 100, 200),
  #                                 stringsAsFactors = FALSE
  # )
)

##' For the algorithms you can only define, which metrics to use. We recommend
##' to exclude "terminal nodes" to save time. 
algo.designs <- list(
  distances = data.frame(metric = c("splitting variables", "weighted splitting variables", "prediction"),
                         stringsAsFactors = FALSE),
  decision_tree = data.frame()
)
# algo.designs <- list(
#   distances = data.frame(metric = c("splitting variables", "weighted splitting variables", "prediction", "terminal nodes"), 
#                          stringsAsFactors = FALSE),
#   decision_tree = data.frame()
# )

## Add experiments ----
ids = batchtools::addExperiments(reg = reg,
                                 prob.designs = prob.designs,
                                 algo.designs = algo.designs,
                                 repls = 10 ## Number of times each experiment is repeated. You can save time here
                                 )

summarizeExperiments(reg = reg)

## Submit jobs ----
ids <- findNotDone()
ids[, chunk := 1]

## Test jobs before submission
# testJob(id = 1, reg = reg)

## Please change this if you have a batch system. 

submitJobs(ids = ids, reg = reg)

##' With pre selected parameters it will take around 10 min to complete.
##' The different metrics will have the following computation time for one replication of setting 1.
##' splitting variables          - 20s
##' weighted splitting variables - 21.6s
##' prediction                   - 24.4s
##' terminal nodes               - 2103s
##' decision tree                - 4.7s
##' Please note, the run times for the other setting could differ. 
##' Anyway simulating data for the figures in the paper will probably run for >100 days on you computer. 

getStatus()

## Collect and save results ----
results <- reduceResultsList(reg = reg, missing.val = 0)
## Save results
saveRDS(results, file = file.path(proc_dir, "results.Rds"))



##' With this script figure 3 from Laabs et al. "Identification of 
##' representative trees in random forests based on a new tree-based distance 
##' measure" can be reproduced. Given a simulated data set. 
##' Run simulations.R to get such a data set. 

## Define directories
## Please define your main directory here. 
## This should be the directory you cloned the git repository into.
main_dir <- "~/Documents/Eigene_Forschung/2022_WSVmetric/WSV-metric-paper/"

## Create and define proc directory
dir.create(file.path(main_dir, "proc"), showWarnings = FALSE)
proc_dir <- file.path(main_dir, "proc")
## Create and define output directory
dir.create(file.path(main_dir, "output"), showWarnings = FALSE)
out_dir <- file.path(main_dir, "output")

## Load libraries
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}
if (!"ggplot2" %in% installed.packages()){
  install.packages("ggplot2")
}

pacman::p_load(ggplot2)
pacman::p_load(ggpubr)

## Load and prepare data ----
results <- readRDS(file.path(proc_dir, "results.Rds"))
## Load results from paper
results <- readRDS(file.path(proc_dir, "results_paper.Rds"))

## Transform results from list to data.table
plot_data <- as.data.frame(data.table::rbindlist(results))

plot_data$Metric <- factor(plot_data$metric, 
                           levels = c("prediction", "terminal nodes", "splitting variables", "weighted splitting variables", "decision tree"),
                           labels = c("Prediction", "Clustering", "Splitting variables", "Weighted splitting variables", "Decision tree"))

## Build figure ----
## Split results into experiments
plot_data_1 <- plot_data[plot_data$setting == "Setting 1",]
plot_data_2 <- plot_data[plot_data$setting == "Setting 2",]
plot_data_3 <- plot_data[plot_data$setting == "Setting 3",]
plot_data_4 <- plot_data[plot_data$setting == "Setting 4",]
plot_data_5 <- plot_data[plot_data$setting == "Setting 5",]

plot_data_cov_1 <- aggregate(x = plot_data_1$covered_effect_vars, 
                             by = list(plot_data_1$Metric,
                                       plot_data_1$min_node_size), 
                             FUN = mean)
names(plot_data_cov_1) <- c("Metric", "min_node_size", "covered_effect_vars")

plot_cov_1 <- ggplot(data = plot_data_cov_1, aes(y = covered_effect_vars, color = Metric, x = as.factor(min_node_size), group = Metric)) + geom_point() + geom_line() +
  ylab("Fraction of covered effect variables") + xlab("Minimal node size") + ggtitle("Scenario 1") + 
  theme(legend.position = "right") + scale_y_continuous(limits = c(0,1)) + scale_color_discrete(name = "Method") + xlab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_data_cov_2 <- aggregate(x = plot_data_2$covered_effect_vars, 
                             by = list(plot_data_2$Metric,
                                       plot_data_2$min_node_size), 
                             FUN = mean)
names(plot_data_cov_2) <- c("Metric", "min_node_size", "covered_effect_vars")

plot_cov_2 <- ggplot(data = plot_data_cov_2, aes(y = covered_effect_vars, color = Metric, x = as.factor(min_node_size), group = Metric)) + geom_point() + geom_line() +
  ylab("Fraction of covered effect variables") + xlab("Minimal node size") + ggtitle("Scenario 2") + 
  theme(legend.position = "bottom") + scale_y_continuous(limits = c(0,1)) + scale_color_discrete(name = "Method") + ylab("") + xlab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_data_cov_3 <- aggregate(x = plot_data_3$covered_effect_vars, 
                             by = list(plot_data_3$Metric,
                                       plot_data_3$min_node_size), 
                             FUN = mean)
names(plot_data_cov_3) <- c("Metric", "min_node_size", "covered_effect_vars")

plot_cov_3 <- ggplot(data = plot_data_cov_3, aes(y = covered_effect_vars, color = Metric, x = as.factor(min_node_size), group = Metric)) + geom_point() + geom_line() +
  ylab("Fraction of covered effect variables") + xlab("Minimal node size") + ggtitle("Scenario 3") + 
  theme(legend.position = "bottom") + scale_y_continuous(limits = c(0,1)) + scale_color_discrete(name = "Method") + ylab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_data_cov_4 <- aggregate(x = plot_data_4$covered_effect_vars, 
                             by = list(plot_data_4$Metric,
                                       plot_data_4$min_node_size), 
                             FUN = mean)
names(plot_data_cov_4) <- c("Metric", "min_node_size", "covered_effect_vars")

plot_cov_4 <- ggplot(data = plot_data_cov_4, aes(y = covered_effect_vars, color = Metric, x = as.factor(min_node_size), group = Metric)) + geom_point() + geom_line() +
  ylab("Fraction of covered effect variables") + xlab("Minimal node size") + ggtitle("Scenario 4") + 
  theme(legend.position = "bottom") + scale_y_continuous(limits = c(0,1)) + scale_color_discrete(name = "Method") + ylab("") + xlab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_data_cov_5 <- aggregate(x = plot_data_5$covered_effect_vars, 
                             by = list(plot_data_5$Metric,
                                       plot_data_5$min_node_size), 
                             FUN = mean)
names(plot_data_cov_5) <- c("Metric", "min_node_size", "covered_effect_vars")

plot_cov_5 <- ggplot(data = plot_data_cov_5, aes(y = covered_effect_vars, color = Metric, x = as.factor(min_node_size), group = Metric)) + geom_point() + geom_line() +
  ylab("Fraction of covered effect variables") + xlab("Minimal node size") + ggtitle("Scenario 5") + 
  theme(legend.position = "bottom") + scale_y_continuous(limits = c(0,1)) + scale_color_discrete(name = "Method") + ylab("") + xlab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_cov <- ggarrange(plot_cov_1, plot_cov_2, plot_cov_3, plot_cov_4, plot_cov_5, 
                      ncol = 5, common.legend = TRUE, legend = "bottom",
                      label.y = 0)
plot_cov

ggsave(filename = "plot_cov_vars.pdf",
       plot     = plot_cov,
       device   = "pdf",
       path     = out_dir,
       width    = 36,
       height   = 10,
       units    = "cm"
)
##' With this script figure 2 from Laabs et al. "Identification of 
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

pacman::p_load(ggplot2)
pacman::p_load(gridExtra)

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

## Only needed for common legend
plot_mse_0 <- ggplot(data = plot_data_1, aes(y = mse_rf_pred, fill = Metric, x = as.factor(min_node_size))) + geom_boxplot() +
  ylab("Deviation from original random forest") + xlab("Minimal node size") + ggtitle("Scenario 1: Few large main effects") +
  scale_fill_discrete(name = "Method") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_mse_1 <- ggplot(data = plot_data_1, aes(y = mse_rf_pred, fill = Metric, x = as.factor(min_node_size))) + geom_boxplot() + 
  ylab("") + xlab("") + ggtitle("Scenario 1: Few large main effects") + 
  theme(legend.position = "right") + scale_fill_discrete(name = "Method") +
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_mse_2 <- ggplot(data = plot_data_2, aes(y = mse_rf_pred, fill = Metric, x = as.factor(min_node_size))) + geom_boxplot() + 
  ylab("Deviation from original random forest") + xlab("Minimal node size") + ggtitle("Scenario 2: Many small main effects") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Method") + xlab("") + ylab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_mse_3 <- ggplot(data = plot_data_3, aes(y = mse_rf_pred, fill = Metric, x = as.factor(min_node_size))) + geom_boxplot() + 
  ylab("Deviation from original random forest") + xlab("Minimal node size") + ggtitle("Scenario 3: Correlated variables") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Method") + xlab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_mse_4 <- ggplot(data = plot_data_4, aes(y = mse_rf_pred, fill = Metric, x = as.factor(min_node_size))) + geom_boxplot() + 
  ylab("Deviation from original random forest") + xlab("Minimal node size") + ggtitle("Scenario 4: Interaction effects") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Method") + ylab("") + 
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

plot_mse_5 <- ggplot(data = plot_data_5, aes(y = mse_rf_pred, fill = Metric, x = as.factor(min_node_size))) + geom_boxplot() + 
  ylab("Deviation from original random forest") + xlab("Minimal node size") + ggtitle("Scenario 5: Binary and continuous variables") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Method") + ylab("") +
  theme(legend.text = element_text(size=14), 
        legend.title = element_text(size=16),
        title = element_text(size=16))

## Function to define common legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot_mse_0)


plot_mse_rf_pred <- grid.arrange(plot_mse_1 + theme(legend.position="none"),
                                 plot_mse_2 + theme(legend.position="none"),
                                 plot_mse_3 + theme(legend.position="none"),
                                 plot_mse_4 + theme(legend.position="none"),
                                 plot_mse_5 + theme(legend.position="none"),
                                 mylegend, nrow = 3, ncol = 2)

ggsave(filename = "plot_mse_rf_pred.pdf",
       plot     = plot_mse_rf_pred,
       device   = "pdf",
       path     = out_dir,
       width    = 36,
       height   = 30,
       units    = "cm"
)

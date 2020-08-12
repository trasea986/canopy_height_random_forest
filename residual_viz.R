#load packages

library(tidyverse)
library(cowplot)
library(MASS)

###### read in data and remove NAs induced by using Excel workbooks
data <- read.csv("Rsd_RF_GRF.csv")
data <- data[complete.cases(data), ]

data_sum <- summary(data)
data_sum

##### calculate quantiles
data_quant_RF <- quantile(data$rsd_RF, c(.90, .95, .99))
data_quant_GRF <- quantile(data$rsd_GRF, c(.90, .95, .99))
data_quant_RF
data_quant_GRF


##### quantile calc

#Get quantiles, add factor
quants_rf <- quantile(data$rsd_RF, c(0.95, 0.99))
data$quant_rf  <- with(data, factor(ifelse(abs(rsd_RF) < quants_rf[1], 0, 
                                        ifelse(abs(rsd_RF) < quants_rf[2], 1, 2))))

quants_grf <- quantile(data$rsd_GRF, c(0.95, 0.99))
data$quant_grf  <- with(data, factor(ifelse(abs(rsd_GRF) < quants_grf[1], 0, 
                                           ifelse(abs(rsd_GRF) < quants_grf[2], 1, 2))))

quants_th <- quantile(data$true_height, c(0.95, 0.99))
data$quant_th  <- with(data, factor(ifelse(abs(true_height) < quants_th[1], 0, 
                                            ifelse(abs(true_height) < quants_grf[2], 1, 2))))

##### histograms

#true height histogram
th_hist <- ggplot(data, aes(true_height, fill = quant_th)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_fill_manual(values = c("white", "blue", "red"), 
                    labels = c("0-95", "95-99", "99-100")) + 
  xlab("True Canopy Height") +
  ylab("Height Count") +
  labs(fill = "Quantile") +
  #scale_y_continuous(limits = c(0, 1200)) +
  theme_bw(base_size = 14)

ggsave("Histogram_true_height.png", plot = th_hist, width = 8, height = 6)

#make the two plots
rf_hist <- ggplot(data, aes(abs(rsd_RF), fill = quant_rf)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_fill_manual(values = c("white", "blue", "red"), 
                      labels = c("0-95", "95-99", "99-100")) + 
  xlab("Random Forest") +
  ylab("Residual Count") +
  labs(fill = "Quantile") +
  scale_y_continuous(limits = c(0, 1200)) +
  theme_bw(base_size = 14)

grf_hist <-  ggplot(data, aes(abs(rsd_GRF), fill = quant_grf)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_fill_manual(values = c("white", "blue", "red"), 
                    labels = c("0-95", "95-99", "99-100")) + 
  xlab("Geographically Weighted Random Forest") +
  ylab("Residual Count") +
  labs(fill = "Quantile") +
  scale_y_continuous(limits = c(0, 1200)) +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_hist)
second_row = plot_grid(grf_hist)
hist_all = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
hist_all

ggsave("Histogram_RFvGRF.png", plot = hist_all, width = 8, height = 6)

##### checking height and residual relationship

## note that by request added absolute value

#make the two plots
rf_reg <- ggplot(data, aes(abs(rsd_RF), true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_rf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "black")+
  scale_color_manual(values = c("black", "blue", "red"), 
                    labels = c("0-95", "95-99", "99-100")) + 
  xlab("Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

grf_reg <- ggplot(data, aes(abs(rsd_GRF), true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_grf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "black")+
  scale_color_manual(values = c("black", "blue", "red"), 
                     labels = c("0-95", "95-99", "99-100")) + 
  xlab("Geograhically Weighted Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg)
second_row = plot_grid(grf_reg)
reg_all = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_all

ggsave("ResidualvTrueHeight.png", plot = reg_all, width = 8, height = 6)

##### checking height if we only look at the 95th or greater quantile

#subset data to drop everything < 95
data_rf_big <- subset(data, quant_rf != '0')
data_grf_big <- subset(data, quant_grf != '0')

#make the two plots
rf_reg_big <- ggplot(data_rf_big, aes(abs(rsd_RF), true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_rf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "black")+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("95-99", "99-100")) + 
  xlab("Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

grf_reg_big <- ggplot(data_grf_big, aes(abs(rsd_GRF), true_height)) +
  geom_point(size=.5, alpha = 0.5, aes(color = quant_grf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "black")+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("95-99", "99-100")) + 
  xlab("Geograhically Weighted Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg_big)
second_row = plot_grid(grf_reg_big)
reg_big = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_big

ggsave("ResidualvTrueHeight_BigQuant.png", plot = reg_big, width = 8, height = 6)

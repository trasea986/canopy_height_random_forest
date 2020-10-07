#load packages

library(tidyverse)
library(cowplot)
library(MASS)

###### read in data and remove NAs induced by using Excel workbooks
data <- read.csv("Rsd_RF_GRF.csv")
data <- data[complete.cases(data), ]

data_sum <- summary(data)
data_sum

# remove 1 and lower
data_tall <- subset(data, data$true_height > 1)
data_quant_tall <- quantile(data_tall$true_height, c(.95, .99), type = 8)
data_quant_tall


##### quantile calc

#Get quantiles, add factor
quants_rf <- quantile(data$rsd_RF, c(0.01, 0.05, 0.95, 0.99))
data$quant_rf  <- with(data, factor(ifelse(rsd_RF < quants_rf[1], 0,
                                          ifelse(rsd_RF < quants_rf[2] & rsd_RF > quants_rf[1], 1,
                                          ifelse(rsd_RF > quants_rf[3] & rsd_RF < quants_rf[4], 3,       
                                          ifelse(rsd_RF > quants_rf[4] & rsd_RF > quants_rf[3], 4, 2))))))
                       
quants_grf <- quantile(data$rsd_GRF, c(0.01, 0.05, 0.95, 0.99), type = 8)
data$quant_grf  <- with(data, factor(ifelse(rsd_GRF < quants_grf[1], 0,
                                          ifelse(rsd_GRF < quants_grf[2] & rsd_GRF > quants_grf[1], 1,
                                          ifelse(rsd_GRF > quants_grf[3] & rsd_GRF < quants_grf[4], 3,       
                                          ifelse(rsd_GRF > quants_grf[4] & rsd_GRF > quants_grf[3], 4, 2))))))                       
#all canopy height is positive, so it is a bit simpler here. note I do both sides later at William's request
quants_th <- quantile(data_tall$true_height, c(0.95, 0.99))
data_tall$quant_th  <- with(data_tall, factor(ifelse(abs(true_height) < quants_th[1], 0, 
                                            ifelse(abs(true_height) < quants_th[2], 1, 2))))

write.csv(quants_rf, "quantiles_rf.csv")
write.csv(quants_grf, "quantiles_grf.csv")


##### histograms

#true height histogram
th_hist <- ggplot(data_tall, aes(true_height, fill = quant_th)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_fill_manual(values = c("white", "blue", "red"), 
                    labels = c("0-95", "95-99", "99-100")) + 
  xlab("True Canopy Height (when > 1)") +
  ylab("Height Count") +
  labs(fill = "Quantile") +
  #scale_y_continuous(limits = c(0, 1200)) +
  theme_bw(base_size = 14)

ggsave("Histogram_true_height_tall.png", plot = th_hist, width = 8, height = 6)

#make the two plots
rf_hist <- ggplot(data, aes(rsd_RF, fill = quant_rf)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_fill_manual(values = c("red", "blue", "white", "blue", "red"), 
                    labels = c("00-01", "01-05", "05-95", "95-99", "99-100")) + 
  xlab("Random Forest") +
  ylab("Residual Count") +
  labs(fill = "Quantile") +
  scale_y_continuous(limits = c(0, 1200)) +
  theme_bw(base_size = 14)

grf_hist <-  ggplot(data, aes(rsd_GRF, fill = quant_grf)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_fill_manual(values = c("red", "blue", "white", "blue", "red"), 
                    labels = c("00-01", "01-05", "05-95", "95-99", "99-100")) + 
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
rf_reg <- ggplot(data, aes(rsd_RF, true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_rf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("red", "blue", "black", "blue", "red"), 
                    labels = c("00-01", "01-05", "05-95", "95-99", "99-100")) +
  geom_vline(xintercept=quants_rf[1], color = "red") +
  geom_vline(xintercept=quants_rf[2], color = "blue") +
  geom_vline(xintercept=quants_rf[3], color = "blue") +
  geom_vline(xintercept=quants_rf[4], color = "red") +
  xlab("Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

grf_reg <- ggplot(data, aes(rsd_GRF, true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_grf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("red", "blue", "black", "blue", "red"), 
                    labels = c("00-01", "01-05", "05-95", "95-99", "99-100")) +
  geom_vline(xintercept=quants_grf[1], color = "red") +
  geom_vline(xintercept=quants_grf[2], color = "blue") +
  geom_vline(xintercept=quants_grf[3], color = "blue") +
  geom_vline(xintercept=quants_grf[4], color = "red") +
  xlab("Geograhically Weighted Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg)
second_row = plot_grid(grf_reg)
reg_all = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_all

ggsave("ResidualvTrueHeight.png", plot = reg_all, width = 8, height = 6)



##### checking height if we only look at the 95th or greater quantile

#subset data to drop everything < 95
data_rf_big <- subset(data, quant_rf != '2')
data_grf_big <- subset(data, quant_grf != '2')

#make the two plots
rf_reg_big <- ggplot(data_rf_big, aes(rsd_RF, true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_rf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("red", "blue", "blue", "red"), 
                     labels = c("00-01", "01-05", "95-99", "99-100")) +
  geom_vline(xintercept=quants_rf[1], color = "red") +
  geom_vline(xintercept=quants_rf[2], color = "blue") +
  geom_vline(xintercept=quants_rf[3], color = "blue") +
  geom_vline(xintercept=quants_rf[4], color = "red") +
  xlab("Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

grf_reg_big <- ggplot(data_grf_big, aes(rsd_GRF, true_height)) +
  geom_point(size=.5, alpha = 0.5, aes(color = quant_grf)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("red", "blue", "blue", "red"), 
                     labels = c("00-01", "01-05", "95-99", "99-100")) +
  geom_vline(xintercept=quants_rf[1], color = "red") +
  geom_vline(xintercept=quants_rf[2], color = "blue") +
  geom_vline(xintercept=quants_rf[3], color = "blue") +
  geom_vline(xintercept=quants_rf[4], color = "red") +
  xlab("Geograhically Weighted Random Forest Residual") +
  ylab("True Canopy Height") +
  labs(color = "Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg_big)
second_row = plot_grid(grf_reg_big)
reg_big = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_big

ggsave("ResidualvTrueHeight_BigQuant.png", plot = reg_big, width = 8, height = 6)




#### switch x and y and color code by residual change dash line color

##### quantile calc

#Get quantiles, add factor... tall was already calculated above... right now not used
#quants_rf <- quantile(data_tall$rsd_RF, c(0.01, 0.05, 0.95, 0.99))
#data_tall$quant_rf  <- with(data_tall, factor(ifelse(rsd_RF < quants_rf[1], 0,
#                                           ifelse(rsd_RF < quants_rf[2] & rsd_RF > quants_rf[1], 1,
#                                                  ifelse(rsd_RF > quants_rf[3] & rsd_RF < quants_rf[4], 3,       
#                                                         ifelse(rsd_RF > quants_rf[4] & rsd_RF > quants_rf[3], 4, 2####))))))

#quants_grf <- quantile(data_tall$rsd_GRF, c(0.01, 0.05, 0.95, 0.99))
#data_tall$quant_grf  <- with(data_tall, factor(ifelse(rsd_GRF < quants_grf[1], 0,
#                                            ifelse(rsd_GRF < quants_grf[2] & rsd_GRF > quants_grf[1], 1,
#                                                   ifelse(rsd_GRF > quants_grf[3] & rsd_GRF < quants_grf[4], 3,       
#                                                          ifelse(rsd_GRF > quants_grf[4] & rsd_GRF > quants_grf[3], 4, #2)))))) 




##### checking height and residual relationship

## note that by request added absolute value

#make the two plots
rf_reg_tall <- ggplot(data_tall, aes(true_height, rsd_RF)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("black", "blue", "red"), 
                     labels = c("0-95", "95-99", "99-100")) + 
  geom_vline(xintercept=quants_th[1], color = "blue") +
  geom_vline(xintercept=quants_th[2], color = "red") +
  geom_vline(xintercept=median(data_tall$true_height), color = "black") +
  xlab("True Canopy Height") +
  ylab("Random Forest Residual") +
  labs(color = "True Height Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

grf_reg_tall <- ggplot(data_tall, aes(true_height, rsd_GRF)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("black", "blue", "red"), 
                     labels = c("0-95", "95-99", "99-100")) +
  geom_vline(xintercept=quants_th[1], color = "blue") +
  geom_vline(xintercept=quants_th[2], color = "red") +
  geom_vline(xintercept=median(data_tall$true_height), color = "black") +
  xlab("True Canopy Height") +
  ylab("Geograhically Weighted \n Random Forest Residual") +
  labs(color = "True Height Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg_tall)
second_row = plot_grid(grf_reg_tall)
reg_all = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_all

ggsave("ResidualvTrueHeight_v2.png", plot = reg_all, width = 10, height = 8)






##### checking height if we only look at the 95th or greater quantile

#subset data to drop everything < 95
data_rf_tall_bigQ <- subset(data_tall, quant_th != '0')


#make the two plots
rf_reg_big <- ggplot(data_rf_tall_bigQ, aes(true_height, rsd_RF)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("95-99", "99-100")) +
  geom_vline(xintercept=quants_th[1], color = "blue") +
  geom_vline(xintercept=quants_th[2], color = "red") +
  xlab("True Canopy Height") +
  ylab("Random Forest Residual") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

grf_reg_big <- ggplot(data_rf_tall_bigQ, aes(true_height, rsd_GRF)) +
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("95-99", "99-100")) +
  geom_vline(xintercept=quants_th[1], color = "blue") +
  geom_vline(xintercept=quants_th[2], color = "red") +
  xlab("True Canopy Height") +
  ylab("Geograhically Weighted \n Random Forest Residual") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg_big)
second_row = plot_grid(grf_reg_big)
reg_big = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_big

ggsave("ResidualvTrueHeight_BigQuantOnly_tall.png", plot = reg_big, width = 10, height = 8)




##### checking height if we only look at the 5th or greater quantile

#subset data to drop everything < 95 was done intiailly, but William wanted median as the threshold
quants_th <- quantile(data_tall$true_height, c(.01,.05,.95, .99), type = 8)
data_tall$quant_th  <- with(data_tall, factor(ifelse(true_height < quants_th[1], 0,
                                                    ifelse(true_height < quants_th[2] & true_height > quants_th[1], 1,
                                                    ifelse(true_height > quants_th[3] & true_height < quants_th[4], 3,       
                                                    ifelse(true_height > quants_th[4] & true_height > quants_th[3], 4, 2))))))


write.csv(quants_th, "quantiles_th_greater_than_1.csv")
data_rf_tall_smallQ <- subset(data_tall, true_height < median(true_height))


#make the two plots
rf_reg_small <- ggplot(data_rf_tall_smallQ, aes(true_height, rsd_RF)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("red", "blue", "black"), 
                     labels = c("00-01", "01-05", "05-95","95-99","99-100")) +
  geom_vline(xintercept=quants_th[1], color = "red") +
  geom_vline(xintercept=quants_th[2], color = "blue") +
  geom_vline(xintercept=median(data_tall$true_height), color = "black") +
  xlab("True Canopy Height") +
  ylab("Random Forest Residual") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

grf_reg_small <- ggplot(data_rf_tall_smallQ, aes(true_height, rsd_GRF)) +
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("red", "blue", "black"), 
                     labels = c("00-01", "01-05", "05-95","95-99","99-100")) +
  geom_vline(xintercept=quants_th[1], color = "red") +
  geom_vline(xintercept=quants_th[2], color = "blue") +
  geom_vline(xintercept=median(data_tall$true_height), color = "black") +
  xlab("True Canopy Height") +
  ylab("Geograhically Weighted \n Random Forest Residual") +
  labs(color = "Quantile") +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg_small)
second_row = plot_grid(grf_reg_small)
reg_small = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_small

ggsave("ResidualvTrueHeight_ShortQuantOnly_tall.png", plot = reg_small, width = 10, height = 8)



#mean residual divided by true canopy height, with some binning
quants_th <- quantile(data_tall$true_height, c(0.95, 0.99))
data_tall$quant_th  <- with(data_tall, factor(ifelse(abs(true_height) < quants_th[1], 0, 
                                                     ifelse(abs(true_height) < quants_th[2], 1, 2))))
rf_reg_tall <- ggplot(data_tall, aes(true_height, rsd_RF/true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("black", "blue", "red"), 
                     labels = c("0-95", "95-99", "99-100")) + 
  geom_vline(xintercept=quants_th[1], color = "blue") +
  geom_vline(xintercept=quants_th[2], color = "red") +
  geom_vline(xintercept=median(data_tall$true_height), color = "black") +
  xlab("True Canopy Height") +
  ylab("Random Forest Residual \n / True Canopy Height") +
  labs(color = "True Height Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

grf_reg_tall <- ggplot(data_tall, aes(true_height, rsd_GRF/true_height)) + 
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(linetype="dashed", alpha=0.2, method="loess", color = "purple")+
  scale_color_manual(values = c("black", "blue", "red"), 
                     labels = c("0-95", "95-99", "99-100")) +
  geom_vline(xintercept=quants_th[1], color = "blue") +
  geom_vline(xintercept=quants_th[2], color = "red") +
  geom_vline(xintercept=median(data_tall$true_height), color = "black") +
  xlab("True Canopy Height") +
  ylab("Geograhically Weighted \n Random Forest Residual \n / True Canopy Height") +
  labs(color = "True Height Quantile") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw(base_size = 14)

#combine the two plots
first_row = plot_grid(rf_reg_tall)
second_row = plot_grid(grf_reg_tall)
reg_all = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
reg_all

ggsave("ResidualvTrueHeightStandardRes.png", plot = reg_all, width = 10, height = 8)


#both standardized residual line with loess line from the data... 
#4 lines the standardized and the loess lines (GRF and RF)
data_tall$RF_stand <- data_tall$rsd_RF/data_tall$true_height
data_tall$GRF_stand <- data_tall$rsd_GRF/data_tall$true_height

data_tall_lines <- data_tall[ , c("rsd_RF", "rsd_GRF", "RF_stand", "GRF_stand", "true_height")] 

data_tall_lines <- data_tall_lines %>% gather(key = "Residual_Type", value = "value", -true_height)
data_tall_lines$Residual_Type <- as.factor(data_tall_lines$Residual_Type)

line_compare <- ggplot(data_tall_lines, aes(true_height, value, color = Residual_Type, linetype = Residual_Type)) + 
  geom_smooth(method="loess")+
  scale_linetype_manual(name= "Type",
                        values=c("dashed", "dashed", "solid", "solid"),
                        labels = c("GRF/TCH", "RF/TCH", "GRF", "RF"))+
  scale_color_manual(name="Type",
                      values = c("black", "blue", "black", "blue"),
                     labels = c("GRF/TCH", "RF/TCH", "GRF", "RF")) +
  xlab("True Canopy Height") +
  ylab("Residual or \n Residual / True Canopy Height") +
  labs(color = "") +
  theme_bw(base_size = 14)

ggsave("ResidualLines.png", plot = line_compare, width = 10, height = 8)


#plot 5 grf resid - rf residual scatterplot, add line 0

data_tall$r_diff <- data_tall$rsd_GRF - data_tall$rsd_RF
point_compare <- ggplot(data_tall, aes(true_height, r_diff))+
  geom_point(size=.5, alpha = 0.5, aes(color = quant_th)) + 
  geom_smooth(method="loess", color = "purple", linetype = "dashed") +
  scale_color_manual(values = c("black", "blue", "red"), 
                     labels = c("0-95", "95-99", "99-100")) +
  geom_vline(xintercept=quants_th[1], color = "blue") +
  geom_vline(xintercept=quants_th[2], color = "red") +
  geom_vline(xintercept=median(data_tall$true_height), color = "black") +
  xlab("True Canopy Height") +
  ylab("GRF Residual - RF Residual") +
  theme_bw(base_size = 14)

ggsave("ResidualChange.png", plot = point_compare, width = 10, height = 8)

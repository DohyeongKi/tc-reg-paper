rm(list = ls())

########################################################################
# Source and Library ###################################################
library(tidyverse)
library(regmdc)
library(wooldridge)  # the k401ksubs data set
data(hprice2)
########################################################################

########################################################################
# Load Results #########################################################
load("../../results/hprice/hprice_overfitting.Rda")
our2_model <- results$our2_model
interaction3_model <- results$interaction3_model
########################################################################

hprice2 <- hprice2 %>% 
  mutate(
    log_price = log(price),
    log_nox = log(nox), 
    log_dist = log(dist)
  ) %>% 
  select(log_price, log_nox, log_dist, crime, rooms, stratio)

m1 <- 51L
m2 <- 51L

log_nox <- median(hprice2$log_nox)
log_dist <- median(hprice2$log_dist)
crime <- seq(min(hprice2$crime), max(hprice2$crime), length.out = m1)
rooms <- seq(min(hprice2$rooms), max(hprice2$rooms), length.out = m2)
stratio <- median(hprice2$stratio)

X_plot <- expand.grid(
  list(log_nox, log_dist, crime, rooms, stratio)
)
colnames(X_plot) <- c("log_nox", "log_dist", "crime", "rooms", "stratio")

our2_fit_plot <- predict_regmdc(our2_model, X_plot)
interaction3_fit_plot <- predict(interaction3_model, X_plot)

our2_fit_outer <- matrix(nrow = m1, ncol = m2)
interaction3_fit_outer <- matrix(nrow = m1, ncol = m2)

for (i in (1L:m1)) {
  for (j in (1L:m2)) {
    our2_fit_outer[i, j] <- our2_fit_plot[i + m1 * (j - 1L)]
    interaction3_fit_outer[i, j] <- interaction3_fit_plot[i + m1 * (j - 1L)]
  }
}

pdf(
  "../../plots/hprice/fig4_hprice_overfitting_instance.pdf",
  width = 5.8, height = 2.7
)

par(
  mfrow = c(1, 2),
  mar = c(0.8, 0.6, 0.9, 0.3)
)
persp(
  crime, rooms, our2_fit_outer,
  xlab = "per Captia Crime Rate", 
  ylab = "Avg. Number of Rooms", 
  zlab = "Log of Median Housing Price (dollars)",
  cex.axis = 0.6, cex.lab = 0.7,
  theta = -60, phi = 10,
  shade = 0.3, lwd = 0.6,
  ticktype = "detailed"
)
mtext("A", adj = 0, cex = 0.7)

persp(
  crime, rooms, interaction3_fit_outer,
  xlab = "per Captia Crime Rate", 
  ylab = "Avg. Number of Rooms", 
  zlab = "Log of Median Housing Price (dollars)",
  cex.axis = 0.6, cex.lab = 0.7,
  theta = -60, phi = 10,
  shade = 0.3, lwd = 0.6,
  ticktype = "detailed"
)
mtext("B", adj = 0, cex = 0.7)

dev.off()
gc()

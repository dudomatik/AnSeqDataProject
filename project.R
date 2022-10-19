install.packages("fpp2")
install.packages("Mcomp")
install.packages("plotly")
install.packages("cowplot")
library(Mcomp)
library(fpp2)
library(cowplot)  
#library(plotly)

monthly_ts_list <- subset(M3,"monthly")
N1876 <- monthly_ts_list[[475]]

N1876tr <- monthly_ts_list[[475]]$x
N1876te <- monthly_ts_list[[475]]$xx

autoplot(N1876, facets = FALSE) + ggtitle("N1876") + xlab("")
autoplot(N1876tr) + ggtitle("N1876 Training Data")
ggAcf(N1876tr)
ggseasonplot(N1876tr, year.labels.left = TRUE,   # Add labels
             year.labels = TRUE)
ggseasonplot(N1876tr, polar=TRUE)
ggsubseriesplot(N1876tr)
diffts = diff(N1876tr) #difference between t and t-1
autoplot(diffts) 
ggAcf(diffts) #autocorrelation of diff. time series

show_fc <- function(fc) {
  dev.new()
  fc_data = fc
  fitted <- fitted(fc_data)
  res <- residuals(fc_data)
  
  b1 <- autoplot(fc_data)
  b2 <- autoplot(N1876tr, series="Data") +
    autolayer(fitted, series="Fitted")
  
  
  plot_grid(b1, b2,  ncol = 2)
  dev.new()
  checkresiduals(fc_data)
  accuracy(fc_data)

  
}

#forecasts
fc_mean <- meanf(N1876tr) #mean
fc_naive <- naive(N1876tr, h=20) #naive
fc_seas_naive <- snaive(N1876tr, h=20) # seasonal naive
fc_drift <- rwf(N1876tr, drift = TRUE) #drift

show_fc(fc = fc_mean)
show_fc(fc = fc_naive)
show_fc(fc = fc_seas_naive)
show_fc(fc = fc_drift)


e <- tsCV(N1876tr, rwf, drift=TRUE,h=1)
RSE <- sqrt(mean(e^2, na.rm=TRUE))
RMSE <- sqrt(mean(residuals(rwf(N1876tr, drift=TRUE))^2, na.rm=TRUE))

e <- tsCV(N1876tr, snaive, drift=TRUE,h=1)
RSE <- sqrt(mean(e^2, na.rm=TRUE))
RMSE <- sqrt(mean(residuals(snaive(N1876tr, drift=TRUE))^2, na.rm=TRUE))

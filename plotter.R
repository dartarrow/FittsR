plotter <- function(xval, yval, colour, dataname, 
                    ylimit=10000, level=FALSE, quantileReg=FALSE, 
                    xlabel="Effective Index of Difficulty",
                    superimposeDetails=TRUE) {
  g_xlim = c(0,5.6)
  g_ylim = c(0, ylimit)
  g_ylab = "Absolute Error"
  g_xlab = xlabel
  
  #quantileReg=FALSE #override all
  
  plot(xval, yval, pch="+", col=colour, xlim=g_xlim, ylim=g_ylim, xlab=g_xlab, main=dataname, ylab=g_ylab, yaxs="i", xaxs="i")
  linearmodel <- lm(yval~xval)
  abline(linearmodel, col="black", lwd=2)
  abline(0,1)
  if(level) { # confidence levels
    #polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
    newx <- data.frame(x=seq(min(xval), max(xval),length=length(xval)))
    preds <- predict(linearmodel, newdata = newx, interval = 'confidence', level=level)
    matlines(xval, preds[,c("lwr","upr")], col=2, lty=1)
  }
  
  if(quantileReg){
    library("quantreg")
      quartReg = rq(yval ~ xval, tau=0.95)
      abline(quartReg, col="#AA2222", lty=2)
      quartReg = rq(yval ~ xval, tau=0.75)
      abline(quartReg, col="#AA2222", lty=6)
      quartReg = rq(yval ~ xval, tau=0.5)
      abline(quartReg, col="#AA2222", lty=1)
      quartReg = rq(yval ~ xval, tau=0.25)
      abline(quartReg, col="#AA2222", lty=6)
      quartReg = rq(yval ~ xval, tau=0.05)
      abline(quartReg, col="#AA2222", lty=2)
    }
  
  
  
  intercept = round(linearmodel$coefficients[1], 2)
  slope = round(linearmodel$coefficients[2], 2)
  rSquared = round(summary(linearmodel)$r.squared, 2)
  pearsonsr = round(cor(xval, yval), 2)
  rmse = round(sqrt(mean(resid(linearmodel)^2)), 2)
  mae = round(mean(abs(resid(linearmodel))), 2)
  
  if(superimposeDetails){
    text(0.5, g_ylim[2]*0.95, paste("RMSE: ", rmse), pos=4)
    text(0.5, g_ylim[2]*0.90, paste("MAE: ", mae), pos=4)
    text(0.5, g_ylim[2]*0.85, paste("Slope: ", slope), pos=4)
    text(0.5, g_ylim[2]*0.80, paste("Intercept:", intercept), pos=4)
    text(0.5, g_ylim[2]*0.75, bquote(paste(R^2, ': ', .(rSquared))), pos=4)
    text(0.5, g_ylim[2]*0.70, paste("r: ", pearsonsr), pos=4)
  }
}
# CONFIGS:
colGrayWithAlpha = rgb(155, 155, 155, 100, maxColorValue = 255)
colGrapNoAlpha = "#AAAAAA"
colLightGray = "#cccccc"

setwd("/Users/dartarrow/Projects/spheredata/newdata/")

planarFiles1 = list.files(pattern="D3.*[:print:].csv")

if(exists("gestureDataset")) rm(gestureDataset)

#import files, into 
source("../scripts/importer.R")
gestureDataset = importer(planarFiles1)
# remove the trial datapoints
gestureDataset = gestureDataset[gestureDataset$Trial!=1,]
# removing one of the Widths, because it allows for distinct Index of Difficulties
#gestureDataset = gestureDataset[gestureDataset$W!=128,]

#Plot Stuff
source("../scripts/plotter.R")
par(mfrow=c(2,3))

# PLOT:
# 1. The most basic: ID vs MTe 
plotter(gestureDataset$ID, gestureDataset$MTe, colGrayWithAlpha, "a: MT per trial", 6200, xlabel="Index of Difficulty")
# 2. Similar to (1), but mean per user
MTe = aggregate(gestureDataset$MTe, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
IDe = aggregate(gestureDataset$ID, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
plotter(IDe, MTe, colGrapNoAlpha, "b: Means per user", 2600, xlabel="Index of Difficulty")
# 3. Regression, Fitts, means of ID
g = aggregate(gestureDataset$MeanMTe~gestureDataset$ID, FUN=mean)
plotter(g[,1], g[,2], "#AA2222", "c: Means of means", 2600, quantileReg=FALSE, xlabel="Index of Difficulty")
# ?. Boxplot over all points
# d = data.frame(gestureDataset$ID, gestureDataset$MTe)
# boxplot(gestureDataset$MTe~gestureDataset$ID, d)

# == Now we start talking about IDe == #
# 4. Scatter: Fitts over all points. with IDe
plotter(gestureDataset$IDe.1d, gestureDataset$MTe, colGrayWithAlpha, "d: MT per trial", 6200)
# 5. Scatter: Fitts over means of MTe per person. 
MTe = aggregate(gestureDataset$MeanMTe, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
IDe = aggregate(gestureDataset$IDe.1d., by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
plotter(IDe, MTe, colGrapNoAlpha, "e: Means per user", 2600)
# 6. Boxplot over Means MTe
g = aggregate(gestureDataset$MeanMTe~gestureDataset$ID, FUN=mean)
plotter(g[,1], g[,2], colGrapNoAlpha, "f: Means of means", 2600, quantileReg=FALSE, xlabel="Index of Difficulty")
ID = aggregate(gestureDataset$ID, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
d = data.frame(ID, MTe)
boxplot(MTe~ID, d, medcol="#AA2222", at=sort(unique(ID)), boxwex=0.2, boxlwd=2, add=TRUE, names=c("","","","","","","",""))
linearmodel <- lm(g[,2]~g[,1])
abline(linearmodel, col="black", lwd=1)

#Uniqueness
unique(gestureDataset$A)
unique(gestureDataset$W)
unique(gestureDataset$Subject)
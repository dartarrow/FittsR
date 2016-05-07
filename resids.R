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
par(mfrow=c(1,3))

# == Now we start talking about IDe == #
# 1. Scatter: Fitts over all points. with IDe
g.linear = lm(gestureDataset$MTe ~ gestureDataset$IDe.1d)
squaredErrors = abs(resid(g.linear))
graphMax = max(squaredErrors)*1.1
plotter(gestureDataset$IDe.1d, squaredErrors, colGrayWithAlpha, "a: MT per trial", graphMax)

# 2. Scatter: Fitts over means of MTe per person. 
MTe = aggregate(gestureDataset$MeanMTe, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
IDe = aggregate(gestureDataset$IDe.1d., by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
g.linear = lm(MTe ~ IDe)
squaredErrors = abs(resid(g.linear))
plotter(IDe, squaredErrors, colGrapNoAlpha, "b: Means per user", graphMax)

# 3. Regression, Fitts, means of ID
g = aggregate(gestureDataset$MeanMTe~gestureDataset$ID, FUN=mean)
xval = g[,1]
yval = g[,2]
g.linear = lm(yval ~ xval)
squaredErrors = abs(resid(g.linear))
plotter(xval, squaredErrors, "#AA2222", "c: Means of means", graphMax, xlabel="Index of Difficulty")

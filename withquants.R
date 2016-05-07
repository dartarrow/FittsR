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
par(mfrow=c(3,1))

# == Now we start talking about IDe == #
# 1. Scatter: Fitts over all points. with IDe
plotter(gestureDataset$IDe.1d, gestureDataset$MTe, colGrayWithAlpha, "a: MT per trial", 6300, superimposeDetails=FALSE, quantileReg=TRUE)

# 2. Scatter: Fitts over means of MTe per person. 
MTe = aggregate(gestureDataset$MTe, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
IDe = aggregate(gestureDataset$IDe.1d., by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
plotter(IDe, MTe, colGrapNoAlpha, "b: Means per user", 2600, superimposeDetails=FALSE, quantileReg=TRUE)
# 3. Similar to (1), but mean per user
MTe = aggregate(gestureDataset$MTe, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
IDe = aggregate(gestureDataset$ID, by=list(gestureDataset$Subject,gestureDataset$A,gestureDataset$W), FUN=mean)[,4]
plotter(IDe, MTe, colGrapNoAlpha, "c: Means per user", 2600, xlabel="Index of Difficulty", superimposeDetails=FALSE, quantileReg=TRUE)


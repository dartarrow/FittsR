importer <- function(inputFiles){
  
  for (inputFile in inputFiles) {
    # if the merged mydataset doesn't exist, create it
    if (!exists("mydataset")){
      mydataset = read.table(inputFile, header=TRUE, sep=",")
    } else {
      # else, the merged dataset does exist, append to it
      temp_dataset = read.table(inputFile, header=TRUE, sep=",")
      mydataset = rbind(mydataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  
  return(mydataset)
}

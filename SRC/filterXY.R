# Removes probes associated to allosomes (chr X and Y)
# 
# Args: methylumi object
#       chr_remove which chromosomes to remove. Default - X, Y
#
# Returns: methylumi object

filterXY <- function(methLumi_data, chr_remove=c("X", "Y")){
  probeXY <- vector(mode = "character")
  for (i in chr_remove) {
    probeXY <- c(probeXY, as.character(fData(methLumi_data)$TargetID[ which(fData(methLumi_data)$CHR == i) ]))
  }
	
	indexXY <- which(is.element(featureNames(methLumi_data), probeXY))
	rm(probeXY)
	if(length(indexXY) > 0) methLumi_data <- methLumi_data[-indexXY, ]
	
	print(paste("Number of probes on chromosomes ", paste(chr_remove, collapse = " "), " to remove: ", length(indexXY), sep=""))
	
	return(methLumi_data)
}

# Removes probes not associated to allosomes (chr X and Y)
# 
# Args: methylumi object
#
# Returns: methylumi object

filterNoneXY <- function(methLumi_data){
  
  probeXY <- fData(methLumi_data)$TargetID[ which(fData(methLumi_data)$CHR=="Y" | fData(methLumi_data)$CHR=="X") ]
  indexXY <- which(is.element(featureNames(methLumi_data), probeXY))
  len <- nrow(methLumi_data)
  rm(probeXY)
  if(length(indexXY) > 0) methLumi_data <- methLumi_data[indexXY,]
  len <- len - length(indexXY)
  print(paste("Nb probes on X & Y chr to remove: ", len, sep=""))
  
  return(methLumi_data)
}

#determine if single IP address is within a given range
#adds column of yes or no
singleAddress <- function(x){
  #change find$V1 for different vectors of ranges
  ifelse(sum(ip_in_range(as.vector(find$V1), as.vector(x[1]))) > 0, "Yes", "No")
}
ranges <- cbind(ranges, apply(ranges, MARGIN=1, singleAddress))


#find range within a range
rangeAddress <- function(x){
  temp <- range_generate(x[2])
  for(i in ranges$cidr){
    #if whole range is in at least one range of 'ranges' data frame
    if(sum(ip_in_range(as.vector(temp), as.vector(i))) == length(temp)){
      return("Yes")
    }
  }
  return("No")
}
wireless <- cbind(wireless, apply(wireless, MARGIN=1, rangeAddress))


#given x = 198.211.135.0-198.211.135.255
#convert ip range to cidr
ipRange <- function(x){
  whole <- unlist(strsplit(x, "-", fixed = TRUE))
  start <- unlist(strsplit(whole[1], ".", fixed = TRUE))
  end <- unlist(strsplit(whole[2], ".", fixed = TRUE))
  
  s <- as.character((as.numeric(end[1]) - as.numeric(start[1])) + (as.numeric(end[2]) - as.numeric(start[2])) + (as.numeric(end[3]) - as.numeric(start[3])) + (as.numeric(end[4]) - as.numeric(start[4])))
  if(s == "3") return(paste(whole[1], "/30", sep=""))
  else if(s == "7") return(paste(whole[1], "/29", sep=""))
  else if(s == "15") return(paste(whole[1], "/28", sep=""))
  else if(s == "31") return(paste(whole[1] , "/27", sep=""))
  else if(s == "63") return(paste(whole[1], "/26", sep=""))
  else if(s == "127") return(paste(whole[1], "/25", sep= ""))
  else if(s == "255") return(paste(whole[1], "/24", sep=""))
  else if(s == "256") return(paste(whole[1], "/23", sep=""))
  else if(s == "258") return(paste(whole[1] , "/22", sep=""))
  else if(s == "262") return(paste(whole[1] , "/21", sep=""))
  else if(s == "270") return(paste(whole[1], "/20", sep=""))
  else if(s == "286") return(paste(whole[1] , "/19", sep=""))
  else if(s == "318") return(paste(whole[1], "/18", sep=""))
  else if(s == "382") return(paste(whole[1] , "/17", sep=""))
  else if(s == "510") return(paste(whole[1], "/16", sep= ""))
  else if(s == "511") return(paste(whole[1], "/15", sep=""))
  else if(s == "513") return(paste(whole[1] , "/14", sep=""))
  else if(s == "517") return(paste(whole[1] , "/13", sep=""))
  else if(s == "525") return(paste(whole[1] , "/12", sep=""))
  else if(s == "541") return(paste(whole[1] , "/11", sep=""))
  else return("0")
}
wireless <- cbind(wireless, apply(wireless, MARGIN=1, ipRange))


#compare range within a range of IPs
#return all matching IP ranges, concatenated
returnMatches <- function(x){
  allMatches <- NULL
  #expand a range into list of IP addresses
  full <- range_generate(x[2])
  for(i in externalRanges$V1){
    #if even a partial match, add to final output
    if(sum(ip_in_range(as.vector(full), as.vector(i))) >=1) allMatches <- c(allMatches, i)
  }
  return(paste(allMatches, collapse= ", "))
}
ranges <- cbind(ranges, apply(ranges, MARGIN=1, returnMatches))

#line for writing a data frame to csv
write.table(ranges, file="ranges.csv", row.names= FALSE, col.names= TRUE, sep=",")

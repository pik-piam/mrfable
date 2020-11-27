#' @title correctIndiaAPY
#' @description Correct IndiaAPY data
#' @param x magpie object provided by the read function
#' @return magpie objects with corrected IndiaAPY data
#' @author Anastasis Giannousakis
#' @examples
#'
#' \dontrun{
#'   readSource("IndiaAPY", convert="onlycorrect")
#' }
#' 
#' @importFrom magclass as.magpie as.data.frame getCells getCells<- dimSums
#' @importFrom utils read.csv
#' @importFrom madrat toolCountryFill
#' @importFrom dplyr  %>% select

correctIndiaAPY <- function(x){

  x <- as.data.frame(x) %>% select(-"Cell")
  x <- x[which(!is.na(x[, "Value"])),]
  
  x[,"Region"] <- sub("Chattisgarh|Chhatisgarh", "Chhattisgarh", x[,"Region"])
  x[,"Region"] <- sub("UttaraKhand", "Uttarakhand", x[,"Region"])
  x[,"Region"] <- sub("Dadra Nagar Haveli", "D & N Haveli", x[,"Region"])
  x[,"Region"] <- sub("Panjab", "Punjab", x[,"Region"])
  x[,"Region"] <- sub("Pondicherry", "Puducherry", x[,"Region"])
  x[,"Region"] <- sub("Orissa", "Odisha", x[,"Region"])
  x[,"Region"] <- sub("A & N Islands", "Andaman and Nicobar Islands", x[,"Region"])
  x[,"Data4"] <- sub("NA", "total", x[,"Data4"])
  x[,"Data4"] <- sub("total kharif", "kharif total", x[,"Data4"])
  x[,"Data4"] <- sub("kharif \\(autumn\\)", "autumn", x[,"Data4"])
  x[,"Data4"] <- sub("kharif \\(winter\\)", "winter", x[,"Data4"])
  x[,"Data4"] <- sub("summer/rabi|total rabi/ summer", "rabi/summer", x[,"Data4"])
  
  colnames(x)<-c("state", "year", "crop", "variable", "unit", "season", "Value")
  
  # for (i in unique(x[,"state"])) {
  #   # find rows of x$state that are duplicated
  #   tmp1 <- filter(x,state==i) %>% duplicated() %>% which()
  #   tmp <- filter(x,state==i)
  #   print(tmp1[tmp,])
  # }

  x <- as.magpie(x, spatial="state")
  
#  diff <- dimSums(dimSums(x["All India",,invert=T],dim = "year",na.rm = T),dim="state",na.rm=T)-
#          dimSums(x["All India",,],dim = "year",na.rm = T)
  
  x["D & N Haveli",,] <- x["D & N Haveli",,] + x["Daman & Diu",,]
  tmp <- x["All India",,]
  getCells(x) <- sub("D & N Haveli", "Dadra and Nagar Haveli and Daman and Diu", getCells(x))
  mapping <- read.csv(system.file("extdata", "regional/mappingIndiaAPY.csv", package = "mrfable"))
  x<-toolCountryFill(x,countrylist = as.vector(mapping[,"State"]),no_remove_warning = c("All India","Daman & Diu"))
  x <- mbind(tmp,x)

  return(x)
}

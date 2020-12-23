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
#' @importFrom magclass as.magpie as.data.frame getCells getCells<- dimSums mbind
#' @importFrom utils read.csv
#' @importFrom madrat toolCountryFill
#' @importFrom dplyr  %>% select

correctIndiaAPY <- function(x){
  
  # if for a croptype a season has been found and the rest are NA, use this one for all years and regions
  # for (i in getItems(x,split = TRUE)[[3]][["crop"]]) {
  #   if ("NA" %in% getItems(x[,,i],split = TRUE)[[3]][["season"]] & length(getItems(x[,,i],split = TRUE)[[3]][["season"]])==2) {
  #     season <- grep("NA",getItems(x[,,i],split = TRUE)[[3]][["season"]],invert = TRUE,value = TRUE)
  #     if (all(getItems(x[,1966:2013,i],split = TRUE)[[3]][["season"]]=="NA")) {
  #       x[,1966:2013,i][,,season]<-setNames(mselect(x,crop=i,year=paste0("y",c(1966:2013)),season="NA"),getNames(x[,1966:2013,i][,,season]))
  #       x <- mbind(x[,,i,invert=TRUE],x[,,i][,,season])
  #     }
  #   }
  # }

  x <- as.data.frame(x) %>% select(-"Cell")
  x <- x[which(!is.na(x[, "Value"])),]
  
  # Remove one trailing space from variable names
#  x[,"Data2"] <- gsub(" $", "", x[,"Data2"])
  
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
  x[,"Data4"] <- sub("summer/rabi", "rabi/summer", x[,"Data4"])
  
  colnames(x)<-c("state", "year", "crop", "variable", "unit", "season", "Value")
  
  season <- NULL
  x <- filter(x,season!="total rabi/ summer")
  # for (i in unique(x[,"state"])) {
  #   # find rows of x$state that are duplicated
  #   tmp1 <- filter(x,state==i) %>% duplicated() %>% which()
  #   tmp <- filter(x,state==i)
  #   print(tmp1[tmp,])
  # }

  x <- as.magpie(x, spatial="state")
  
#  diff <- dimSums(dimSums(x["All India",,invert=T],dim = "year",na.rm = T),dim="state",na.rm=T)-
#          dimSums(x["All India",,],dim = "year",na.rm = T)
  x["D & N Haveli",,] <- dimSums(x[c("D & N Haveli","Daman & Diu"),,],dim = 1,na.rm = TRUE)
  #  tmp <- x["All India",,]
  getCells(x) <- sub("D & N Haveli", "Dadra and Nagar Haveli and Daman and Diu", getCells(x))
  mapping <- read.csv(system.file("extdata", "regional/mappingIndiaAPY.csv", package = "mrfable"))
  x<-toolCountryFill(x,countrylist = as.vector(mapping[,"state"]),no_remove_warning = c("All India","Daman & Diu"))
#  x <- mbind(tmp,x)

  # add total value where it is missing
  getNames(x)<-sub("NA$","total",getNames(x))

  tmp <- dimSums(x[,,c("winter","autumn","kharif")],dim = "season",na.rm = TRUE)
  x[,,"kharif total"][is.na(x[,,"kharif total"])] <- tmp[is.na(x[,,"kharif total"])]

  tmp <- dimSums(x[,,c("total","kharif total"),invert=TRUE],dim = "season",na.rm = TRUE)
  x[,,"total"][is.na(x[,,"total"])] <- tmp[is.na(x[,,"total"])]

  return(x)
}

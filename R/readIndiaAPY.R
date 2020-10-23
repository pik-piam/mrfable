#' @title readIndiaAPY
#' Contains foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @param subtype Area, Yield, or Production
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#' @importFrom dplyr filter %>%
#' @importFrom magclass as.magpie getRegions<-
#' @examples
#' \dontrun{ a <- readSource(type="IndiaAPY",subtype="Rice") }
#' @return magpie object containing Area, Yield, or Production data. 


readIndiaAPY <- function(subtype){
  
  # helper function to convert data from wide to long format and specify variable and unit
  .gather <- function(x, varunit, season=FALSE){
    fct<- 1
    if(season) fct<-2
    d <- gather(x, "year", "value", colnames(x)[-(1:fct)], factor_key = T) # convert to long format
#    d <- d[-c(grep("State", d[,1]), which(is.na(d[,1]))),] # remove irrelevant rows 
    varname <- strsplit(varunit, " \\(")[[1]][[1]] # split variable-unit
    unit <- gsub(" |\\)|\\'", "", strsplit(varunit, " \\(")[[1]][[2]])
    unit <- sub("000", "k", unit)
    out <- cbind("variable"=varname, "unit"=unit, "season"=NA, d)
    if (season) out <- cbind("variable"=varname, "unit"=unit, d)
    return(out)
  }
  
  # helper function to prepare data from Wheat excel files for conversion to data.frame
  .fixdataWheat <- function(a){
    colnames(a)<-sub("-.*","",a[3, ]) # use years as column names
    a <- a[-which(is.na(a[, 1])),] # remove rows that start with NA
    colnames(a)[1] <- "region"
    ind <- length(which(grepl("[0-9]",colnames(a))))/3 # find length of each data table
    out <- rbind(
      .gather(cbind(a[-1,1], a[-1,(2+0*ind):(1+1*ind)]), a[[1,0*ind+2]]),
      .gather(cbind(a[-1,1], a[-1,(2+1*ind):(1+2*ind)]), a[[1,1*ind+4]]),
      .gather(cbind(a[-1,1], a[-1,(2+2*ind):(1+3*ind)]), a[[1,2*ind+4]])
      )
    return(out)
  }
  
  # helper function to prepare data from Rice excel files for conversion to data.frame
  .fixdataRice <- function(a){
    colnames(a)<-sub("-.*","",a[2,]) # use years as column names
    a<-a[-which(is.na(a[, 2])),] # remove rows that start with NA
    colnames(a)[1:2]<-c("region", "season")
    # in the first column, complete the missing region names
    for (i in 1:length(t(a[, 1]))){
      if(is.na(a[[i, 1]])) a[[i, 1]]<-a[[i-1, 1]]
    }
    if (grepl("1",a[,1])) a <- a[-which(a[, 1]==1),] # remove remaining rows that do not contain data
    ind <- length(which(grepl("[0-9]", colnames(a)))) / 3 # find length of each data table
    a <- as.data.frame(a)
    a[,"season"] <- tolower(a[,"season"])
    out<-rbind(
      .gather(cbind(a[-1,1:2], a[-1,(3+0*ind):(2+1*ind)]), a[[1,0*ind+3]], TRUE),
      .gather(cbind(a[-1,1:2], a[-1,(3+1*ind):(2+2*ind)]), a[[1,1*ind+3]], TRUE),
      .gather(cbind(a[-1,1:2], a[-1,(3+2*ind):(2+3*ind)]), a[[1,2*ind+3]], TRUE)
    )
    return(out)
  }
  
  crop <- NULL
  excelfiles <- dir()[grep("xls", dir())] # read-in only excel files
  out <- NULL
  for (i in grep("Rice", excelfiles, value = TRUE)) {
    a <- read_excel(i)
    tmp <- cbind("crop" = "Rice", .fixdataRice(a))
    out <- rbind(out, tmp)
  }

  for (i in grep("Wheat", excelfiles, value = TRUE)) {
    a <- read_excel(i)
    tmp <- cbind("crop" = "Wheat", .fixdataWheat(a))
    out <- rbind(out, tmp)
  }
  
  out[["value"]]<-as.numeric(out[["value"]])

  out <- filter(out,`crop`==subtype) %>% as.magpie()
  getRegions(out) <- "IND"

  return(out)

}

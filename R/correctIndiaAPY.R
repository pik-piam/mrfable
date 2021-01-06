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
#' @importFrom magclass as.magpie as.data.frame getCells getCells<- dimSums mbind getNames<-
#' @importFrom utils read.csv
#' @importFrom madrat toolCountryFill
#' @importFrom dplyr  %>% select anti_join filter mutate ungroup group_by summarise bind_rows distinct

correctIndiaAPY <- function(x){
  
  .rmvdplseas <- function(xx,seas1,seas2) {
    tmp<-select(filter(xx,`season`%in%c(seas1,seas2)),-"season")
    tmp<-tmp[which(duplicated(tmp)),]
    if (nrow(tmp)>0) xx <- anti_join(xx,mutate(tmp,"season"=seas2),
        by = c("state", "year", "crop", "variable", "unit", "season", "Value"))
    return(xx)
  }

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
  x[,"Data4"] <- sub("/^kharif/", "kharif total", x[,"Data4"])
  x[,"Data4"] <- sub("kharif \\(autumn\\)", "kharif total", x[,"Data4"])
  x[,"Data4"] <- sub("kharif \\(winter\\)", "kharif total", x[,"Data4"])
  x[,"Data4"] <- sub("summer/rabi", "rabi/summer", x[,"Data4"])
  
  colnames(x)<-c("state", "year", "crop", "variable", "unit", "season", "Value")
  
  season <- NULL
  x <- filter(x,season!="total rabi/ summer")
  
  # remove mistakes in excel files leading to double counting (e.g. Rice,2015,Jharkhand)
  x <- .rmvdplseas(x,"autumn","kharif")
  x <- .rmvdplseas(x,"winter","kharif")
  
  # add "kharif total" only where it is missing. 
  # (because some regions/years/crops have it as a new data point
  # whereas most of them calculate it in the excel files)
  x %>% filter(season%in%c("kharif","autumn","winter")) %>% 
    group_by(`season`,state,crop,year,variable,unit) %>% 
    summarise("Value"=sum(Value,na.rm = TRUE)) %>% 
    mutate(season="kharif total") %>% 
    ungroup()-> tmp
  tmp1 <- bind_rows(filter(x,season=="kharif total"),tmp)
  tmp1 %>% distinct() %>%  distinct(state,year,crop,unit,season,.keep_all = T) -> tmp
  x <- bind_rows(tmp,filter(x,season!="kharif total"))
  
  # add "total" where it is missing
  x %>% filter(!season%in%c("kharif total","total")) %>% 
    group_by(`season`,state,crop,year,variable,unit) %>% 
    summarise("Value"=sum(Value,na.rm = TRUE)) %>% 
    mutate(season="total") %>% 
    ungroup()-> tmp
  tmp1 <- bind_rows(filter(x,season=="total"),tmp)
  tmp1 %>% distinct() %>%  distinct(state,year,crop,unit,season,.keep_all = T) -> tmp
  x <- bind_rows(tmp,filter(x,season!="total"))
  
  # add "total" if there is only "kharif total" as season
  x %>% filter(season%in%c("kharif total")) -> tmp
  tmp[,"season"] <- "total"
  tmp1 <- bind_rows(x,tmp)
  tmp1 %>% distinct() %>%  distinct(state,year,crop,unit,season,.keep_all = T) -> x

  x <- as.magpie(x, spatial="state")

  x["D & N Haveli",,] <- dimSums(x[c("D & N Haveli","Daman & Diu"),,],dim = 1,na.rm = TRUE)

  getCells(x) <- sub("D & N Haveli", "Dadra and Nagar Haveli and Daman and Diu", getCells(x))
  mapping <- read.csv(system.file("extdata", "regional/mappingIndiaAPY.csv", package = "mrfable"))
  x<-toolCountryFill(x,countrylist = as.vector(mapping[,"state"]),no_remove_warning = c("All India","Daman & Diu"))

  return(x)
}

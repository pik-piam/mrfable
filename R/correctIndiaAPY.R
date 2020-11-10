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
#' @importFrom magclass getNames<- getNames getCells getCells<-

correctIndiaAPY <- function(x){
  getCells(x)<-sub("Chattisgarh|Chhatisgarh","Chhattisgarh",getCells(x))
  getCells(x)<-sub("UttaraKhand","Uttarakhand",getCells(x))
  
  getNames(x)<-sub("summer/rabi","rabi/summer",getNames(x)) 
  getNames(x)<-sub("kharif total","total kharif",getNames(x))
  getNames(x)<-sub("NA$","total",getNames(x))
  
      
     
  return(x)
}

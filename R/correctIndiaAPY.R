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
#' @importFrom magclass getNames<- getNames 

correctIndiaAPY <- function(x){
  getNames(x)<-sub("Chattisgarh|Chhatisgarh","Chhattisgarh",getNames(x))
  getNames(x)<-sub("UttaraKhand","Uttarakhand",getNames(x))
  
  getNames(x)<-sub("summer/rabi","rabi/summer",getNames(x)) 
  getNames(x)<-sub("kharif total","total kharif",getNames(x))
  getNames(x)<-sub("NA$","total",getNames(x))
  
      
     
  return(x)
}

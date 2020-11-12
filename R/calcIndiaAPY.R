#' @title calcIndiaAPY
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @param subtype Area, Yield, or Production
#' @importFrom madrat readSource toolAggregate
#' @importFrom magclass getNames
#' @importFrom utils read.csv
#' @examples
#' \dontrun{ a <- madrat::calcOutput("IndiaAPY",subtype="Area") }
#' @return magpie object containing Area, Yield, and Production data. 


calcIndiaAPY <- function(subtype=NULL){
  
  x <- readSource("IndiaAPY", convert = "onlycorrect")
  mapping <- read.csv(system.file("extdata", "mapping.csv", package = "mrfable"))
  x <- toolAggregate(x["All India",,invert=TRUE], mapping, from = 1)
  return(list(x=x[,,subtype],weight=NULL,unit=getNames(x,fulldim = TRUE)$unit[which(getNames(x,fulldim = TRUE)$variable==subtype)],
                description=paste0("IndiaAPY ",subtype," data from: https://eands.dacnet.nic.in/APY_96_To_07.htm")))

}

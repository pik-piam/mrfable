#' @title fullDATAINDIA
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @param subtype Area, Yield, or Production
#' @importFrom madrat readSource toolAggregate
#' @importFrom magclass getNames
#' @importFrom utils read.csv
#' @examples
#' \dontrun{ a <- madrat::fullDATAINDIA("Foodcrop",subtype="Area") }
#' @return magpie object containing Area, Yield, and Production data. 


fullDATAINDIA <- function(){
  
  setConfig(extramappings = system.file("extdata", "mappingIndiaAPY.csv", package = "mrfable"))
  
#  x <- toolAggregate(x["All India",,invert=TRUE], mapping, from = 1)
  
  calcOutput("Foodcrop", subtype = "Rice", file = "test.mz", aggregate = "State")
  calcOutput("Foodcrop", subtype = "Rice", file = "test.mz")
  
  
  
}

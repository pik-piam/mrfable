#' @title fullDATAINDIA
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @importFrom madrat calcOutput setConfig
#' @examples
#' \dontrun{ a <- madrat::fullDATAINDIA() }
#' @return Foodcrop India data 


fullDATAINDIA <- function(){
  
  setConfig(extramappings = system.file("extdata", "mappingIndiaAPY.csv", package = "mrfable"))
  
#  x <- toolAggregate(x["All India",,invert=TRUE], mapping, from = 1)

  calcOutput("Foodcrop", subtype = "Rice", file = "test.mz", aggregate = "State")
  calcOutput("Foodcrop", subtype = "Rice", file = "test.mz")
  
  
  
}

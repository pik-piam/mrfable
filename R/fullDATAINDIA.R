#' @title fullDATAINDIA
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @importFrom madrat calcOutput setConfig
#' @examples
#' \dontrun{ a <- madrat::fullDATAINDIA() }
#' @return Foodcrop India data 


fullDATAINDIA <- function(){
  
  setConfig(extramappings = "mappingIndiaAPY.csv")

  calcOutput("Foodcrop", subtype = "Rice", aggregate = "Country", file = "test.mz")

}

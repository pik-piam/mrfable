#' @title fullDATAINDIA
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @importFrom madrat calcOutput setConfig
#' @examples
#' \dontrun{ a <- retrieveData("DATAINDIA) }
#' @return Foodcrop India data 


fullDATAINDIA <- function(){
  
  setConfig(extramappings = "mappingIndiaAPY.csv")
  crops <- readLines(paste0(getConfig("sourcefolder") ,"/IndiaAPY/crops.txt"))

  for (i in crops)   calcOutput("Foodcrop", subtype = i, aggregate = "Country", file = paste0(i,".mz"))

  
}

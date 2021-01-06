#' @title fullDATAINDIA
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @importFrom madrat calcOutput setConfig getConfig
#' @examples
#' \dontrun{ a <- retrieveData("DATAINDIA") }
#' @return Foodcrop India data 


fullDATAINDIA <- function(){
  
  setConfig(extramappings = "mappingIndiaAPY.csv")

  for (i in c("Area","Production")) calcOutput("IndiaFoodcrop",
                                                       subtype = i, 
                                                       aggregate = "Zonal.Council",
                                                       file = paste0(i,"ZonalCouncil",".mz"))

  for (i in c("Area","Production")) calcOutput("IndiaFoodcrop",
                                                       subtype = i, 
                                                       aggregate = "Country",
                                                       file = paste0(i,"Country",".mz"))
  
  
}

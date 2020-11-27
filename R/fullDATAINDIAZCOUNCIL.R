#' @title fullDATAINDIAZCOUNCIL
#' Calculates foodcrops data for India Zonal Councils taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @importFrom madrat calcOutput setConfig getConfig
#' @examples
#' \dontrun{ a <- retrieveData("DATAINDIAZCOUNCIL") }
#' @return Foodcrop India data for India Zonal Councils 


fullDATAINDIAZCOUNCIL <- function(){
  
  setConfig(extramappings = "mappingIndiaAPY.csv")

  for (i in c("Area","Production","Yield")) calcOutput("Foodcrop",
                                                       subtype = i, 
                                                       aggregate = "Zonal.Council",
                                                       file = paste0(i,".mz"))

  
}

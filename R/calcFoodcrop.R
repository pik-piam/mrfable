#' @title calcFoodcrop
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @param subtype Area, Yield, or Production
#' @importFrom madrat readSource
#' @importFrom magclass getNames
#' @examples
#' \dontrun{ a <- madrat::calcOutput("Foodcrop",subtype="Area") }
#' @return magpie object containing Area, Yield, and Production data. 


calcFoodcrop <- function(subtype="Area"){
  
  x <- readSource("IndiaAPY", convert = "onlycorrect")
  
  weight <- NULL
  if (subtype=="Yield") {
    weight <- x[,,subtype]
    weight[,,] <- 1
  }

  return(list(x=x[,,subtype],
              weight=weight,
              unit=getNames(x,fulldim = TRUE)$unit[which(getNames(x,fulldim = TRUE)$variable==subtype)],
              isocountries = FALSE,
              description=paste0("IndiaAPY Foocrop ",subtype," data from: https://eands.dacnet.nic.in/APY_96_To_07.htm")))

}

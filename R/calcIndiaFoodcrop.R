#' @title calcIndiaFoodcrop
#' Calculates foodcrops data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm
#' @author Anastasis Giannousakis
#' @param subtype Area, Yield, or Production
#' @importFrom madrat readSource
#' @importFrom magclass getNames setNames collapseNames
#' @examples
#' \dontrun{
#' a <- madrat::calcOutput("IndiaFoodcrop", subtype = "Area")
#' }
#' @return magpie object containing Area, Yield, and Production data.


calcIndiaFoodcrop <- function(subtype = "Area") {

  if (!subtype %in% c("Area", "Yield", "Production")) stop("choose a valid subtype")
  x <- readSource("IndiaAPY", convert = "onlycorrect")

  weight <- NULL
  if (subtype == "Yield") {
    weight <- x[, , "Area"]
    x[, , "Production"][x[, , "Area"] == 0] <- 0
    x[, , "Area"][x[, , "Area"] == 0] <- 10^-10
    x <- setNames(collapseNames(1000 * x[, , "Production"] / x[, , "Area"], preservedim = c(2, 3)),
                  sub("Area", "Yield", getNames(x[, , "Area"])))
    getNames(x) <- sub("kHectares", "Kg/Hectare", getNames(x))
    weight[is.na(weight)] <- 0
  }
  x[is.na(x)] <- 0


  return(list(x = x[, , subtype],
              weight = weight,
              mixed_aggregation = TRUE,
              min = 0,
              max = 999999,
              unit = getNames(x, fulldim = TRUE)$unit[which(getNames(x, fulldim = TRUE)$variable == subtype)],
              isocountries = FALSE,
              description = paste0("IndiaAPY Foodcrop ", subtype,
                                   " data from: https://eands.dacnet.nic.in/APY_96_To_07.htm")))

}

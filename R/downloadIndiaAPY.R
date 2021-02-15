#' downloadIndiaAPY
#'
#' Function to download data from https://eands.dacnet.nic.in/APY_96_To_07.htm.
#' Best used in combination with the madrat package.
#' 
#' @importFrom utils download.file
#' 
#' @author Anastasis Giannousakis
#' @examples
#' \dontrun{ madrat::downloadSource(type="IndiaAPY") }

downloadIndiaAPY <- function() {

  durl <- "http://eands.dacnet.nic.in/"
  years <- c("APY_state_data/Apy-1966-76/Foodgrains/","Archive/Year76-86/","10-Year-1985-96/")
  b<-suppressWarnings(readLines("http://eands.dacnet.nic.in/StateData_66-76Year.htm"))
  crops <- gsub("\\..*.","",gsub(".*.Foodgrains/","",grep("APY.*.Food.*.xls",b,value=TRUE))) # extract filenames
  crops <- grep(" |Summary",crops,value=TRUE,invert = T) # keep only single names (the rest are not relevant)
  for (i in crops) {
    for (j in years) {
      for (k in c(".xls", ".xlsx")) {
        if (Sys.info()[["sysname"]]=="Windows") {
          suppressWarnings(try(download.file(paste0(durl, j, i, k),mode = "wb", destfile = paste0(gsub("/|[a-z,A-Z]", "", j), i, k)),silent = T))
        } else {
          suppressWarnings(try(download.file(paste0(durl, j, i, k), destfile = paste0(gsub("/|[a-z,A-Z]", "", j), i, k)),silent = T))
        }
      }
    }
  }
  suppressWarnings(try(download.file("http://eands.dacnet.nic.in/PDF/foodgrain-5_years.xls",mode = "wb",destfile = "allfood1996-2013.xls")))
  suppressWarnings(try(download.file("http://eands.dacnet.nic.in/PDF/5-Year%20Foodgrain%202018-19.xls",mode = "wb",destfile = "allfood2014-2018.xls")))
  writeLines(crops,con = "crops.txt")
  meta <- list(url=durl,
               title="Data on Area, Production and Yield of Major Crops")
  return(list(url           = meta$url,
              title         = meta$title,
              author        = "Directorate of Economics and Statistics, Ministry of Agriculture and Farmers Welfare, Govt. of India",
              license       = "unknown"))

}
 
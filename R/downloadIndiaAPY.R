#' downloadIndiaAPY
#'
#' Function to download data from https://eands.dacnet.nic.in/APY_96_To_07.htm
#' 
#' @importFrom utils download.file
#' 
#' @author Anastasis Giannousakis

downloadIndiaAPY <- function() {

  url <- "https://eands.dacnet.nic.in/"
  crops <- c("Rice","Wheat","Maize")
  years <- c("APY_state_data/Apy-1966-76/Foodgrains/","Archive/Year76-86/","10-Year-1985-96/")
  for (i in crops) {
    for (j in years) {
      for (k in c(".xls", ".xlsx")) {
        suppressWarnings(try(download.file(paste0(url, j, i, k), destfile = paste0(gsub("/|[a-z,A-Z]", "", j), i, k)),silent = T))
      }
    }
  }
  suppressWarnings(try(download.file("https://eands.dacnet.nic.in/PDF/foodgrain-5_years.xls",destfile = "allfood1996-2013.xls")))
  suppressWarnings(try(download.file("https://eands.dacnet.nic.in/PDF/5-Year%20Foodgrain%202018-19.xls",destfile = "allfood2014-2018.xls")))
  
}
 
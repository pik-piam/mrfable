#' downloadIndiaAPY
#'
#' Function to download data from https://eands.dacnet.nic.in/APY_96_To_07.htm
#' 
#' @importFrom utils download.file
#' 
#' @author Anastasis Giannousakis

downloadIndiaAPY <- function() {

  url <- "https://eands.dacnet.nic.in/"
  crops <- c("Rice","Wheat")
  years <- c("APY_state_data/Apy-1966-76/Foodgrains/","10-Year-1996-12/","Archive/Year76-86/","10-Year-1985-96/")
  for (i in crops) {
    for (j in years) {
      for (k in c(".xls", ".xlsx")) {
        try(download.file(paste0(url, j, i, k), destfile = paste0(gsub("/|[a-z,A-Z]", "", j), i, k)))
      }
    }
  }
}
 
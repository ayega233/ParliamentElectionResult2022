#' @import httr
#' @import shiny
#' @import ggplot2
#' @import maps



#' @export election_summary
election_summary <- function (){
  election_result <- GET("https://resultat.val.se/assets/valdata/val2022/RD_00_S_mandat.json")
  stop_for_status(election_result,"Summary fetching is not available")
  dat <- content(election_result)
  result <-dat$rosterPaverkaMandat$partiroster
  return(result)
}

#' @export election_result
election_result <- function (county_code){
  stopifnot("county_code must not empty"= county_code != '')
  url <-paste0("https://resultat.val.se/assets/valdata/val2022/RD_",county_code,"_S_mandat.json")
  election_result <- GET(url)
  stop_for_status(election_result,"Fetch data please check the county code")
  dat <- content(election_result)
  result <-dat$rosterPaverkaMandat$partiroster

  return(result)
}

#' @export showElectionData
showElectionData <- function(){
  shiny::runApp("R")
}


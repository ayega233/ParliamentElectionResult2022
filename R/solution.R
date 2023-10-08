#' @import httr

#' @export election_summary
election_summary <- function (){
  election_result <- GET("https://resultat.val.se/assets/valdata/val2022/RD_00_S_mandat.json")
  dat <- content(election_result)
  v <-dat$rosterPaverkaMandat$partiroster

  return(v)
}

#' @export election_result
election_result <- function (county_code){
  url <-paste0("https://resultat.val.se/assets/valdata/val2022/RD_",county_code,"_S_mandat.json")
  election_result <- GET(url)
  dat <- content(election_result)
  v <-dat$rosterPaverkaMandat$partiroster

  return(v)
}

#' @export showElectionData
showElectionData <- function(){
shiny::runApp("app")
}


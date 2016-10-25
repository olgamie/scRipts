get_past_events <- function(main, group, apki_key){
  url <- paste0(main, group, "/events")
  url %>%
    httr::GET(query = list(status = 'past', key = api_key)) %>%
    httr::content()
}

get_meetup_attendes <- function(main, group, id, api_key){
  url_attendes <- paste0(main, group, "/events/", id, "/attendance")
  url_attendes %>%
    httr::GET(query = list(key = api_key)) %>%
    httr::content()
}

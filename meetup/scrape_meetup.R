library(magrittr)
library(genderizeR)
library(ggplot2)
library(viridis)

source('meetup_functions.R')
source('api_keys.R')

group <- 'Spotkania-Entuzjastow-R-Warsaw-R-Users-Group-Meetup'
main <- 'https://api.meetup.com/'

past_events <- get_past_events(main, group, api_key)

past_ids <- sapply(past_events, function(x) x$id)
past_names <- sapply(past_events, function(x) x$name)

past <- data.frame(past_ids, past_names)

meetup_attendes <- sapply(past_ids, function(x) {
  rsvp <- get_meetup_attendes(main, group, x, api_key)
  sapply(rsvp, function(x) x$member$name)
})
  
name_sex <- lapply(1:length(meetup_attendes),
                   function(x){ 
                     data.frame(findGivenNames(meetup_attendes[[x]],
                                               progress = FALSE, apikey = genderize_key))
                     })

df_names_sex <- do.call(rbind, name_sex)

df_names_sex %>%
  dplyr::group_by(id_meetup, gender) %>%
  dplyr::summarise(count = n()) ->
  count_sex_meetups

df_names_sex <- dplyr::left_join(count_sex_meetups, past, by = c('id_meetup' = 'past_ids'))

df_names_sex$past_names <- factor(df_names_sex$past_names,
                                   levels = df_names_sex$past_names[order(df_names_sex2id_meetup)])

ggplot2::ggplot(df_names_sex, aes(past_names, count, fill = gender)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60)) +
  labs(y = "Number of participants", x = "Meetups 2014-2016") +
  coord_flip()

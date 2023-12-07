setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Weekstaten ophalen
library(dplyr)
library(tidyr)  
library(lubridate)
weekstaat_url = "https://www.rivm.nl/sites/default/files/2019-01/RecenteVirUitslagen27w.csv"

download.file(weekstaat_url, destfile = "weekstaten_recent.csv")

weekstaat_recent <- read.csv("weekstaten_recent.csv", sep = ";")

#names toewijzen
names(weekstaat_recent) <- c("scenario", paste0("week_",as.character(weekstaat_recent[3,])[-1]))

#opschonen
weekstaat_recent <- weekstaat_recent %>% 
  #Lege / betekenisloze rijen verwijderen
  filter(!is.na(scenario),
         !scenario %in% c("Datum","Aantal Labs","")) %>%
  #Aantallen per week naar numiric
  mutate_at(vars(matches("week")),as.numeric) %>%
  #Van breed naar lang
  pivot_longer(cols = matches("week"), names_to = "week", values_to = "aantal") %>%
  #datumvariabelen maken
  mutate(weeknummer = substring(week,10,12) %>% as.numeric(),
         jaar = substring(week,6,9),
         datum = ymd(parse_date_time(paste(jaar, weeknummer, 1, sep="/"),'Y/W/w')),
         start_maand = floor_date(datum,'month'),
         start_week = floor_date(datum, 'week',week_start = 1)
  )


#Als er nog geen historisch weekstaatbestand bestaat met onderstaande regel recent opslaan als historisch.
#  write.csv(weekstaat_recent, "weekstaat_historisch.csv", row.names = F)


weekstaat_historisch <- read.csv("weekstaat_historisch.csv") %>%
  mutate_at(c("datum","start_maand","start_week"), ymd)

#Van weekstaat historisch alleen overhouden wat niet in recent zit
#Bestaat een kans dat cijfers nog worden bijgewerkt dus weekstaat recent > weekstaat historisch 
weekstaat_historisch <- weekstaat_historisch %>% filter(start_week < min(weekstaat_recent$start_week))

if(nrow(weekstaat_historisch) > 0){
  weekstaat_totaal <- rbind(weekstaat_historisch, weekstaat_recent)
}else{
  weekstaat_totaal <- weekstaat_recent
}

 
write.csv(weekstaat_totaal, "weekstaat_historisch.csv", row.names = F)

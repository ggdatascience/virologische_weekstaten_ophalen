#draai eerst script "virologische_weekstaten_ophalen"
library(ggplot2)

#Bekijken alle scenarios
unique(weekstaat_totaal$scenario)

#berekenen toename van gemelde ziekteverwerkkers
weekstaat_wider <- weekstaat_totaal %>% select(c(scenario, aantal,week))
weekstaat_wider <- pivot_wider(weekstaat_wider, names_from = week, values_from = aantal)
weekstaat_wider$toename1wk <- weekstaat_wider[,2] - weekstaat_wider[,3]
weekstaat_wider$toename4wk <- weekstaat_wider[,2] - weekstaat_wider[,6]
Toename1wk <- weekstaat_wider %>%  filter(toename1wk > 0)
Toename4wk <- weekstaat_wider %>%  filter(toename4wk > 0)

#ziekteverwekkers die toenamen in zowel de afgelopen week als de afgelopen 4 weken:
common_stijging <- Toename1wk %>%  filter(scenario %in% Toename4wk$scenario)
unique(common_stijging$scenario)

#nu een graph maken voor ieder van de gestegen ziekteverwekkers
stijging_wide <- common_stijging %>% select(-c(toename1wk,toename4wk))
stijging_long <- pivot_longer(stijging_wide, cols = !scenario, names_to = "week", values_to = "aantal")
stijging_long$week <- as.numeric(sub("week_2023", "", stijging_long$week))

plot.stijgingen <- stijging_long %>% 
  ggplot(aes(x = week, y = aantal)) +
  geom_line(aes(group = scenario),linewidth =1) +
  geom_point(size=1.5)+
  facet_wrap(~scenario, scales = "free") +
  scale_x_continuous(breaks = seq(min(stijging_long$week), max(stijging_long$week), by = 2)) +
  theme_light()+
  labs(x = "Weeknummer", 
       y = "Aantal gemeld door labs",
       title = "Virussen/scenarios met stijging in afgelopen week én afgelopen maand",
       caption = "Bron: Virologische weekstaat RIVM") +
  theme(plot.title = element_text(face = 'bold'))
plot.stijgingen

#plot voor alle ziekteverwekkers waarbij de stijgers extra opvallen
weekstaat.totaalincr <- weekstaat_totaal
weekstaat.totaalincr$toename <- 0
weekstaat.totaalincr$toename[weekstaat.totaalincr$scenario %in% stijging_long$scenario] <- 1

plot.alles <- weekstaat.totaalincr %>% 
  ggplot(aes(x = weeknummer, y = aantal)) +
  geom_rect(aes(fill = ifelse(toename == 1, rgb(1, 0.8, 0.8), NA)),
            alpha = 0.5,
            xmin = -Inf,
            xmax = Inf,
            ymin = -Inf,
            ymax = Inf,
            color = NA
  ) +
  geom_line(aes(group = scenario),linewidth =1) +
  geom_point(size=1.5)+
  facet_wrap(~scenario, scales = "free") +
  scale_x_continuous(breaks = seq(min(stijging_long$week), max(stijging_long$week), by = 2)) +
  theme_light()+
  labs(x = "Weeknummer", 
       y = "Aantal gemeld door labs",
       title = "Aantal meldingen labs per week per ziekteverwekker",
       subtitle = "Roodgekleurd ziekteverwekkers met stijging in de afgelopen maand en afgelopen week",
       caption = "Bron: Virologische weekstaat RIVM") +
  theme(plot.title = element_text(face = 'bold')) +
  theme(legend.position="none")
plot.alles

ggsave(paste0("Plot virologische weekstaat alles ",Sys.Date(),".png"), plot.alles, width = 20, height = 12)

#een losse aandoening plotten
data.RS <- weekstaat_totaal %>% 
  filter(scenario == 'RS-virus' ) 

plot.RS <- data.RS %>% 
  ggplot(aes(x = weeknummer, y = aantal)) +
  geom_line(linewidth =1) +
  geom_point(size=1.5)+
  theme_light()+
  labs(x = "Weeknummer", 
       y = "Aantal gemeld door labs",
       title = "Aantal keer RS-virus gemeld",
       caption = "Bron: Virologische weekstaat RIVM") +
  theme(plot.title = element_text(face = 'bold'))

plot.RS
ggsave(paste0("Plot virologische weekstaat RSV ",Sys.Date(),".png"), plot.RS)





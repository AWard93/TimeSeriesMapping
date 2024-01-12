#Goals:

#Clearing environment before we begin
rm(list = ls())

'%!in%' <- function(x,y)!('%in%'(x,y))

#install.packages("hexmapmaker")
library(RColorBrewer)
library(rayshader)
library(ggplot2)
library(dplyr)
library(rvest)
library(polite)
library(lubridate)
library(janitor)
library(httr)
library(gganimate)
library(gifski)
library(tweenr)
library(maps)
library(mapdata)
library(rnaturalearth)
library(rnaturalearthhires)
library(cartogram)
library(broom)
library(stringr)
library(sf)
library(viridis)
library(hexbin)
library(showtext)
library(ggtext)
library(ggstream)
library(ggforce)

#Loading county gis data. 
#directories
gis_dir <- "C:/Users/alanw/OneDrive/Documents/data/gis"
dir(gis_dir)

county_sf<- sf::st_read(paste0(gis_dir, "/admin_counties"))%>%
  select("COUNTY")

#Need to union the 4 dublin council regions to generate the historical county
county_sf$geometry[county_sf$COUNTY == "DUBLIN"]<- county_sf%>%
  dplyr::filter(COUNTY == "DUBLIN")%>%
  sf::st_union()

county_sf$geometry[county_sf$COUNTY == "GALWAY"]<- county_sf%>%
  dplyr::filter(COUNTY == "GALWAY")%>%
  sf::st_union()

county_sf$geometry[county_sf$COUNTY == "CORK"]<- county_sf%>%
  dplyr::filter(COUNTY == "CORK")%>%
  sf::st_union()

county_sf<- county_sf%>%
  as.data.frame()%>%
  distinct()%>%
  st_as_sf()

#Polygons for the 26 historical counties
plot(county_sf)

#Scraping time series population data from wikipedia: 
#https://en.wikipedia.org/wiki/Historical_population_of_Ireland
url <- "https://en.wikipedia.org/wiki/Historical_population_of_Ireland"
browseURL(url)
url_bow<- polite::bow(url)

ind_html <- polite::scrape(url_bow)%>%
  rvest::html_nodes("table.wikitable")%>%
  rvest::html_table(fill = TRUE)

titles <- polite::scrape(url_bow)%>%
  rvest::html_nodes("h3")%>%
  html_text()%>%
  substr(., 1, nchar(.)-6)%>%
  str_to_upper()

titles <- titles[-c(1:4)]

#Processing scraped data ready to mapping
counties_historical <- ind_html[c(7:38)]

for(i in 1:32){
  counties_historical[[i]] <- counties_historical[[i]]%>%
    t()%>%
    as.data.frame()%>%
    dplyr::select(1)%>%
    dplyr::rename("population" = "V1")%>%
    slice(-1)%>%
    mutate(county = titles[i],
           year = row.names(.))%>%
    select(county, year, population)%>%
    mutate(population = str_remove(population, ","))
}

counties <- counties_historical%>%
  bind_rows()%>%
  filter(!is.na(population))%>%
  mutate(year = as.numeric(year),
         population = as.numeric(population))%>%
  filter(county %in% county_sf$COUNTY)%>%
  dplyr::rename("COUNTY"= "county")%>%
  left_join(., county_sf, by = "COUNTY")%>%
  st_as_sf()%>%
  st_simplify(., dTolerance = 100)

counties$year[counties$year == 1928] <- 1926
counties$year[counties$year == 1931] <- 1936

########################################################################

#Plotting graphs #stacked line chart

########################################################################





#Generating colour scheme
v_pal = viridis(11)

#Generating ordered vector of the top 10 most populous counties in 2022
order <- counties%>%
  as.data.frame()%>%
  filter(year == 2022)%>%
  arrange(desc(population))%>%
  select(COUNTY)%>%
  pull()%>%
  as.character()

pop22<-counties%>%
  as.data.frame()%>%
  filter(year == "2022")%>%
  select(COUNTY, population)%>%
  arrange(desc(population))
  
chart<- 
  counties%>%
  mutate(county = case_when(COUNTY %in% order[1:10] ~ COUNTY, 
                           COUNTY %!in% order[1:10] ~ "OtherCounties"))%>%
  group_by(county, year)%>%
  summarise(population = sum(population))%>%
  as.data.frame()%>%
  arrange(population)%>%
  mutate(county = factor(county, levels = c(order[1:10], "OtherCounties")))%>%
  ggplot(aes(x = year, y = population))+
  geom_area(position = "stack",
            aes(fill = county,
                lwd = NA))+
  scale_fill_manual(values = rev(v_pal))+
  annotate("text", x = 2030, y = 4200, 
           label = paste0("Dublin: ", pop22$population[1]*1000),
           size = 2)+
  annotate("text", x = 2030, y = 3300, 
           label = paste0("Cork ", pop22$population[2]*1000),
            size = 2)+
    annotate("text", x = 2030, y = 2900, 
             label = paste0("Galway: ", pop22$population[3]*1000),
             size = 2)+
    annotate("text", x = 2030, y = 2700, 
             label = paste0("Kildare: ", pop22$population[4]*1000),
             size = 2)+
    annotate("text", x = 2030, y = 2500, 
             label = paste0("Meath: ", pop22$population[5]*1000),
             size = 2)+
    annotate("text", x = 2030, y = 2300, 
             label = paste0("Limerick: ", pop22$population[6]*1000),
             size = 2)+
    annotate("text", x = 2030, y = 2100, 
             label = paste0("Tipperary: ", pop22$population[7]*1000),
             size = 2)+
    annotate("text", x = 2030, y = 1900, 
             label = paste0("Donegal: ", pop22$population[8]*1000),
             size = 2)+
    annotate("text", x = 2030, y = 1750, 
             label = paste0("Wexford: ", pop22$population[9]*1000),
             size = 2)+
    annotate("text", x = 2030, y = 1550, 
             label = paste0("Kerry: ", pop22$population[10]*1000),
             size = 2)+
    annotate("text", x = 2031, y = 1000, 
             label = paste0("Remaining counties: \n", sum(pop22$population[11:26]*1000)),
             size = 2)+
  theme_classic()+

#Add lines/ boxes to shade historical dates e.g. great famine, indpendence,
#joining EU.
  
  geom_segment(aes(x = 1845, xend = 1845, yend = 6000))+
  geom_point(aes(x = 1845, y = 6000))+
  annotate("text",x = 1849, y = 6100, 
           label = "The great famine \n1845-52",
           size = 3, hjust = 0.2)+
  geom_segment(aes(x = 1879, xend = 1879, yend = 4000))+
  geom_point(aes(x = 1879, y = 4000))+
  annotate("text",x = 1879, y = 4150, 
           label = "1879 famine",
           size = 3, hjust = 0.2)+
  geom_segment(aes(x = 1922, xend = 1922, yend = 3000))+
  geom_point(aes(x = 1922, y = 3000))+
  annotate("text",x = 1922, y = 3300, 
           label = "Independence \n1922",
           size = 3, hjust = 0.5)+
  geom_segment(aes(x = 1973, xend = 1973, yend = 3100))+
  geom_point(aes(x = 1973, y = 3100))+
  annotate("text",x = 1973, y = 3450, 
           label = "Joining EU \n1973",
           size = 3, hjust = 0.7)+
  geom_segment(aes(x = 1985, xend = 1985, yend = 3550))+
  geom_point(aes(x = 1985, y = 3550))+
  annotate("text",x = 1985, y = 3850, 
           label = "Recession \n1980s",
           size = 3, hjust = 0.7)+
  geom_segment(aes(x = 2008, xend = 2008, yend = 4500))+
  geom_point(aes(x = 2008, y = 4500))+
  annotate("text",x = 2008, y = 4800, 
           label = "Financial \ncrisis 2008",
           size = 3, hjust = 0.8)+
  labs(
     title = "Ireland's changing population 1841-2022.",
     caption = str_glue("**Data:** wikipedia.org/wiki/Historical_population_of_Ireland<br>",
                        "**Design:** Alan Ward<br>"))+
  theme(
    plot.title = element_text(face="bold", size=25),
    axis.title = element_blank(),
    legend.position = "none",
    plot.caption = element_markdown(hjust=0, margin=margin(10,0,0,0), size=8, lineheight = 1.2),
  )

ggsave(filename = "StackedAreaAnnotated.png",
       plot = chart,
       height = 15,
       width = 20,
       units = "cm")

browseURL("StackedAreaAnnotated.png")

################################################################

#Doughnut plot

################################################################

counties%>%
  mutate(county = case_when(COUNTY %in% order[1:10] ~ COUNTY, 
                            COUNTY %!in% order[1:10] ~ "OtherCounties"))%>%
  group_by(county, year)%>%
  summarise(population = sum(population))%>%
  as.data.frame()%>%
  filter(year == 2022)%>%
  mutate(fraction = population/sum(population),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax+ymin)/2,
         label = paste0(county, "\n", population))%>%
  ggplot(.,
         aes(ymax=ymin, ymin=ymax,
             xmax=2, xmin=1, 
             fill=county)) +
  geom_rect() +
  geom_text( x= 2.2, 
             aes(y=labelPosition, 
                 label=label),
             size= 2, 
             color = "black",
             fontface = "bold")+
  coord_polar(theta="y",
              start = -pi/2)+
  scale_fill_manual(values = rev(v_pal))+
  xlim(c(0, 2))+
  ylim(c(0,2))+
  theme_void()+
  theme(
    legend.position = "none"
  )

##################################################################

#Coxcomb plot

##################################################################

data<-counties%>%
  mutate(county = case_when(COUNTY %in% order[1:10] ~ COUNTY, 
                            COUNTY %!in% order[1:10] ~ "OtherCounties"))%>%
  group_by(county, year)%>%
  summarise(population = sum(population))%>%
  as.data.frame()%>%
  filter(year == 2022)%>%
  mutate(fraction = population/sum(population),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax+ymin)/2,
         label = paste0(county, "\n", population))


  ggplot(data = data,
         aes(x = county, y = population,fill = population),
         color = population
         )+
  geom_bar(stat = "identity")+
  coord_polar()+
  scale_fill_viridis(option = "A")+
   theme_void()

##################################################################

#Pie chart

##################################################################




##################################################################

#Windrose diagram

##################################################################




##################################################################

#Bullseye

##################################################################






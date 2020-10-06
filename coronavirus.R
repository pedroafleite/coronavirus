install.packages("maps")
install.packages("mapdata")
install.packages("sidrar")
install.packages("brazilmaps")
install.packages("coronabr")
install.packages("tmap")
install.packages("leaflet")
library(maps)
#US maps
mn.map <- map(database="county", region="new york") #w/ county boundaries
mn.map <- map("state", "new york") #w/o county boundaries
mn.map <- map(database="world", region="brazil")
# for more go to: help(package='maps'), or:
library(mapdata)

#for brazil maps:
library(geobr)
library(brazilmaps) #NOT WORKING
library(coronabr) #Neither this one


#Also:
library(ggplot2)
library(sf)
library(dplyr)
library(knitr)
library(rio)
install_formats()
library(sidrar)

#Municipality of São José dos Campos
muni <- read_municipality(code_muni=3549904, year=2010 )
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

ggplot() +
  geom_sf(data=muni, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="São José dos Campos", size=8) +
  theme_minimal() +
  no_axis

#Import shapefiles from ArcGis
library(maptools)

#State of Sâo Paulo
sp <- read_municipality(code_muni="SP", year=2010)
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

ggplot(data=sp) +
  geom_sf(aes(fill="red"), size=.15, show.legend = FALSE) +
  labs(subtitle="São Paulo", size=8) +
  theme_minimal() +
  no_axis

#Neightboorhoods in São José dos Campos
sp_census <- read_census_tract(code_tract="SP", year=2010)
sjc_census <- sp_census[which(sp_census$name_muni=='São José Dos Campos'),]

ggplot() +
  geom_sf(data=sjc_census, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="São José dos Campos", size=8) +
  theme_minimal() +
  no_axis

#Import coronavirus data
coronadata <- read.csv2("INFLUD-07-09-2020.csv")

#Import population data
pop <- read.csv2("estimativa_dou_2015_20150915.csv")

#New dataframe with only SP data
levels(coronadata$SG_UF_NOT)
spcorona <- coronadata[which(coronadata$SG_UF_NOT=='SP'),]

#Only geometry and code_muni
spcorona_geom <- sp[,c("code_muni","geom")]

#merge coronavirus data with sp map
names(sp)
names(spcorona)
names(spcorona)[names(spcorona) == "CO_MUN_NOT"] <- "code_muni"
sp_corona <- merge(sp, spcorona, by="code_muni", all=TRUE)

#Edit code_muni in sp to display only 6 digits (instead of 7)
sp$code_muni <- as.numeric(substr(sp$code_muni, 1, 6))

#How many contaminations for each city?
contam_sp <- spcorona %>% 
  group_by(code_muni) %>%
  summarise(no_rows = length(code_muni))

#merge coronavirus data with number of contaminations per municipality NOT GOOD
sp_corona <- merge(spcorona, contam_sp, by="code_muni", all=TRUE)

#Insert it in SP state map GOOD ONE
sp_coronamap <- merge(sp,contam_sp, by="code_muni", all=TRUE)

#merge pop data with other data GOOD ONE
sp_coronamap_pop <- merge(sp_coronamap,pop, by="name_muni", all=TRUE)

#Plot the data in a visually appealling way
library(RColorBrewer)
library(classInt)
library(tibble)
nclr <- 7
plotclr <- brewer.pal(nclr,"Reds")
sp_coronamap_pop[["no_rows"]][is.na(sp_coronamap_pop[["no_rows"]])] <- 0
str(sp_coronamap_pop)
sp_coronamap_pop[["pop"]][is.na(sp_coronamap_pop[["pop"]])] <- 0
CASES <- sp_coronamap_pop$no_rows
POP2015 <- sp_coronamap_pop$pop
sp_coronamap_pop$var <- ((CASES/POP2015)*2000)
class <- classIntervals(var, nclr, style="sd", dataPrecision=4)
colcode <- findColours(class, plotclr)
colcode

ggplot(data=sp_coronamap_pop) +
  geom_sf(size=.15, color=NA) + geom_sf(aes(fill=var)) +
  labs(subtitle="São Paulo", size=8, fill="Imunidade de rebanho (%)") +
  theme_void() +
  no_axis

#How many corona deaths for each city?
spcorona_deaths <- spcorona[spcorona$EVOLUCAO %in% c("2"),]

deaths_sp <- spcorona_deaths %>% 
  group_by(code_muni) %>%
  summarise(deaths = length(code_muni))

#Insert it in SP state map GOOD ONE
sp_coronamap <- merge(sp,deaths_sp, by="code_muni", all=TRUE)

#merge pop data with other data GOOD ONE
sp_coronamap_pop <- merge(sp_coronamap_pop,deaths_sp, by="code_muni", all=TRUE)

sp_coronamap_pop[["deaths"]][is.na(sp_coronamap_pop[["deaths"]])] <- 0

class <- classIntervals(sp_coronamap_pop$deaths, nclr, style="equal", dataPrecision=10)
colcode <- findColours(class, plotclr)
colcode

DEATHS <- sp_coronamap_pop$deaths
POP2015 <- sp_coronamap_pop$pop
sp_coronamap_pop$var_d <- ((DEATHS/POP2015))*100

ggplot(data=sp_coronamap_pop) +
  geom_sf(size=.15, color=NA) + geom_sf(aes(fill=var_d)) +
  labs(subtitle="São Paulo", size=8) +
  theme_void() +
  no_axis

#Estimating which cities are being less tested in the the general population 
#OR in which there are copntributing factors for a higher death percentage
sp_coronamap_pop$var2d <- (sp_coronamap_pop$no_rows)/(sp_coronamap_pop$deaths)
#OR herd imunity directly from deaths:
sp_coronamap_pop$var3d <- (sp_coronamap_pop$var_d)*60

ggplot(data=sp_coronamap_pop) +
  geom_sf(size=.15, color=NA) + geom_sf(aes(fill=var3d)) +
  labs(subtitle="São Paulo", size=8) +
  theme_void() +
  no_axis
sp_coronamap_pop + tm_basemap(server = "OpenTopoMap")

#In the map above, cities with brighter blue have more accessible tests (or have lesser deaths)
#Grey blocks have no deaths

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps

#either ggplot2 or tmap
#testing tmap

# Add fill layer to sp_coronamap_pop shape
tm_shape(sp_coronamap_pop) +
  tm_fill() 
# Add border layer to sp_coronamap_pop shape
tm_shape(sp_coronamap_pop) +
  tm_borders() 
# Add fill and border layers to sp_coronamap_pop shape
tm_shape(sp_coronamap_pop) +
  tm_fill() +
  tm_borders() 

#Map of SP in tm
#tm_polygons()=tm_fill() + tm_borders()
map_sp <- tm_shape(sp) + tm_polygons()  
map_sp

#Learning the syntax here...
tm_shape(sp_coronamap_pop) + tm_polygons(col = "var3d", id="name_muni", palette="BuGn")


#Rio Claro
#Municipality of Rio Claro
muni_rc <- read_municipality(code_muni=3543907, year=2010)
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

ggplot() +
  geom_sf(data=muni_rc, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Rio Claro", size=8) +
  theme_minimal() +
  no_axis

#Neightboorhoods in Rio Claro
rc_census <- sp_census[which(sp_census$name_muni=='Rio Claro'),]

ggplot() +
  geom_sf(data=rc_census, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Rio Claro", size=8) +
  theme_minimal() +
  no_axis

#How many corona deaths for each neightborhood in Rio Claro?
#1) New dataframe with only Rio Claro data
#New dataframe with only SP data
rc_corona <- spcorona[which(spcorona$ID_MN_RESI=='RIO CLARO'),]

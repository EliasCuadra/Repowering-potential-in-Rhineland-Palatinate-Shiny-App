###############################################################################
#Preparation of the WEPP dataset msb19_new.csv for furhter use in a Leaflet  #
#map and Shiny application                                                    #
###############################################################################
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, leaflet, sp, raster, rgdal, ggmap, htmltools, 
               htmlwidgets, tmaptools)
###############################################################################
#Import data                                                                  #
###############################################################################
msb19_new <- read.csv("msb19_new.csv", header=TRUE)
msb19_before2005 <- subset(msb19_new, indatum_s < "2005-12-31")
attach(msb19_before2005)
###############################################################################
#create adress collumn                                                        #
###############################################################################
sum(is.na(msb19_before2005$ort))
sum(is.na(msb19_new$l_wgs84))
adress <- mutate(msb19_before2005, adress = paste(ort, plz, "Germany"))
unique_ads <- unique(adress$adress)
unique_ads
###############################################################################
#geocoding with geocode_OSM                                                   #
###############################################################################
geocodes <- geocode_OSM(unique_ads)
geo <- geocodes[,c(1,2,3)]
###############################################################################
#combine data frame with adresses                                             #
###############################################################################
geo <- geo %>% 
  rename(adress = query)
adress_coords <- left_join(adress, geo, by = "adress")
###############################################################################
#find adresses that have not been found before                                #
###############################################################################
missing_adresses <- adress_coords %>% 
  filter(is.na(lat) == TRUE)
###############################################################################
#create a new adress collumn for the missing WEPP with the county and         #
#municipality name and extract unique adresses                                #
###############################################################################
missing_lk_vg <- mutate(missing_adresses, missing_lk_vg = paste(vg_name, 
                                                                lk_name, 
                                                                "Germany"))
unique_ads2 <- unique(missing_lk_vg$missing_lk_vg)
###############################################################################
#geocoding of missing adresses with geocode_OSM                               #
###############################################################################
geocodes2 <- geocode_OSM(unique_ads2)
geo2 <- geocodes2[,c(1,2,3)]
###############################################################################
#combine two data frames with geocodes                                        #
###############################################################################
geo2 <- geo2 %>% 
  rename(adress = query)
geo_combined <- rbind(geo, geo2)
###############################################################################
#replace the adresses were there was no location found                        #
###############################################################################
replaced_adresses <- adress %>% 
  mutate(adress = str_replace_all(string = adress, 
                                  pattern = "Kirburg 57629 Germany", 
                                  replacement =  "Bad Marienberg Westerwald 
                                  Germany")) %>% 
  mutate(adress = str_replace_all(string = adress, 
                                  pattern = "Habscheid 54597 Germany", 
                                  replacement = "Prüm Eifelkreis Bitburg-Prüm 
                                  Germany")) %>% 
  mutate(adress = str_replace_all(string = adress, 
                                  pattern = "Boxberg 54552 Germany", 
                                  replacement = "Kelberg Vulkaneifel Germany"))
###############################################################################
#merge complete geocodes with all correct adresses                            #
###############################################################################
adress_merge <- merge(replaced_adresses, geo_combined, by = "adress", 
                      all.x = TRUE)
###############################################################################
#replace the NA value in the initial coordinate collums with new gernerated   #
#coordinates and create new collumn                                           #
###############################################################################
geocodes_complete <- adress_merge %>% 
  mutate(lat_wgs84 = ifelse(is.na(b_wgs84), lat, b_wgs84)) %>%
  mutate(lon_wgs84 = ifelse(is.na(l_wgs84), lon, l_wgs84)) 
###############################################################################
#add white noise to the geocodes to avoid multiple data points in one location#
###############################################################################
geocodes_complete$lat_wgs84 <- jitter(geocodes_complete$lat_wgs84, factor = 1)
geocodes_complete$lon_wgs84 <- jitter(geocodes_complete$lon_wgs84, factor = 1)
###############################################################################
#create subset with desired variables                                         #
###############################################################################
adress_final <- geocodes_complete[,-c(2:3,30:31)]
###############################################################################
#clean environment                                                            #
###############################################################################
rm(adress, adress_complete, adress_coords, adress_merge, geo, geo2, geocodes, 
   geocodes2, geocodes_complete, geo_combined, missing_adresses, missing_lk_vg, 
   replaced, replaced_adresses, unique_ads, unique_ads2)
###############################################################################
#check in map                                                                 #
###############################################################################
leaflet(data = adress_final) %>%
  addTiles() %>%
  addMarkers(~lon_wgs84, ~lat_wgs84, popup = ~paste(eeg_nr), 
             label = ~as.character(vg_name))
###############################################################################
#---------> some data points lay outside of Rhineland-Palatinate              #
###############################################################################

###############################################################################
#find Wind turbines outside of Rhineland-Palatinate with EEG system key       #
###############################################################################
outlier <- adress_final %>% 
  filter(msb19_before2005$eeg_nr == "E31238010000020170000009043119003" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401810" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401809" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797405750" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401812" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401811" |
           msb19_before2005$eeg_nr == "E31238010000010170000001432618844" |
           msb19_before2005$eeg_nr == "E31238010000010170000001432618846"|
           msb19_before2005$eeg_nr == "E31238010000000000000045704818817")
###############################################################################
#geocode again for new adresses                                               #
###############################################################################
adress <- mutate(outlier, adress = paste(vg_name, plz, lk_name, "Germany"))
###############################################################################
#find unique adresses among outliers                                          #
###############################################################################
unique_ads <- unique(adress$adress)
unique_ads
###############################################################################
#geocoding for outlier adresses                                               #
###############################################################################
geocodes <- geocode_OSM(unique_ads)
geo <- geocodes[,c(1,2,3)]
###############################################################################
#check location in map                                                        #
###############################################################################
leaflet(data = geo) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~paste(eeg_nr), 
             label = ~as.character(vg_name))
###############################################################################
#combine geocodes from outlier with the adress data table                     #
###############################################################################
geo <- geo %>% 
  rename(adress = query)
adress <- adress[,-c(28:29)]
outliers_new <- left_join(adress, geo, by = "adress")
outliers_new <- outliers_new %>% 
  rename(lat_wgs84 = lat) %>% 
  rename(lon_wgs84 = lon)
###############################################################################
#add white noise to data points with same location                            #
###############################################################################
outliers_new$lat_wgs84 <- jitter(outliers_new$lat_wgs84, factor = 2)
outliers_new$lon_wgs84 <- jitter(outliers_new$lon_wgs84, factor = 2)
###############################################################################
#clean environment                                                            #
###############################################################################
rm(outlier, unique_ads, adress, geocodes, geo)
###############################################################################
#take the wind turbines with wrong coordinates out of original data set       #
###############################################################################
adress_final_sub <- subset(adress_final, 
                           eeg_nr != "E31238010000020170000009043119003" &
                           eeg_nr != "E30254010000000000005230797401810" &
                           eeg_nr != "E30254010000000000005230797401809" &
                           eeg_nr != "E30254010000000000005230797405750" &
                           eeg_nr != "E30254010000000000005230797401812" &
                           eeg_nr != "E30254010000000000005230797401811" &
                           eeg_nr != "E31238010000010170000001432618844" &
                           eeg_nr != "E31238010000010170000001432618846" &
                           eeg_nr != "E31238010000000000000045704818817")
###############################################################################
#combine adress_final_sub and the outliers                                    #
###############################################################################
msb19_before2005_geocoded <- rbind(adress_final_sub, outliers_new)
###############################################################################
#check location in map                                                        #
###############################################################################
leaflet(data = msb19_before2005_geocoded) %>%
  addTiles() %>%
  addMarkers(~lon_wgs84, ~lat_wgs84, popup = ~paste(eeg_nr), 
             label = ~as.character(vg_name))
###############################################################################
#clean environment                                                            #
###############################################################################
rm(adress_final, adress_final_sub, outliers_new)
###############################################################################
#save file to folder                                                          #
###############################################################################
write.csv(msb19_before2005_geocoded,"msb19_before2005_geocoded.csv")
sum(is.na(msb19_before2005_geocoded$lat_wgs84))
###############################################################################
#-->still 7 wind turbines without georeference that will be omitted in the map#
###############################################################################
###############################################################################
#load data again                                                              #
###############################################################################
msb19_before2005_geocoded <- read.csv("msb19_before2005_geocoded.csv", 
                                      header=TRUE)
###############################################################################
#change leistung_s to numeric and indatum_s to date                           #
###############################################################################
msb19_before2005_geocoded$leistung_s <- as.numeric(msb19_before2005_geocoded$
                                                     leistung_s)
msb19_before2005_geocoded$indatum_s <- as.Date(msb19_before2005_geocoded$
                                                 indatum_s, "%Y-%m-%d")
###############################################################################
#create leaflet map                                                           #
###############################################################################
###############################################################################
#load borders of Rhineland-Palatinate as shape files                          #
###############################################################################
land <- readOGR("Grenzen/Landesgrenze_RLP.shp")
landkreise <- readOGR("Grenzen/Landkreise_RLP.shp")
gemeinden <- readOGR("Grenzen/Verbandsgemeinde_RLP.shp")
###############################################################################
#create map                                                                   #
###############################################################################
m <- leaflet(data = msb19_before2005_geocoded) %>%
  addTiles() %>%
  
  addPolygons(data = land,
              color = "#5DADE2",
              weight = 2,
              opacity = 0.6,
              fillColor = "#5DADE200",
              highlight = highlightOptions(weight = 7,
                                           color = "#5DADE2",
                                           fillColor = "#5DADE2",
                                           fillOpacity = 0.3,
                                           bringToFront = TRUE),
              label = "Rheinland-Pfalz",
              group = "Rheinland-Pfalz") %>% 
  
  addPolygons(data = landkreise,
              color = "#000fff",
              weight = 2,
              opacity = 0.6,
              fillColor = "#000fff00",
              highlight = highlightOptions(weight = 7,
                                           color = "#000fff",
                                           fillColor = "#000fff",
                                           fillOpacity = 0.3,
                                           bringToFront = TRUE),
              label = landkreise$ldkreis,
              group = "Landkreise") %>%
  
  addPolygons(data = gemeinden,
              color = "#D93F0D",
              weight = 1,
              opacity = 0.6,
              fillColor = "#D93F0D00",
              highlight = highlightOptions(weight = 7,
                                           color = "#D93F0D",
                                           fillColor = "#D93F0D",
                                           fillOpacity = 0.3,
                                           bringToFront = TRUE),
              label = gemeinden$vgname,
              group = "Verbandsgemeinden") %>% 
  addLayersControl(overlayGroups = c("Rheinland-Pfalz", "Landkreise", "Verbandsgemeinden"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(lng = msb19_before2005_geocoded$lon_wgs84,
             lat = msb19_before2005_geocoded$lat_wgs84,
             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10),
             popup = ~paste("<h3> Daten der Windkraftanlage</h3>",
                            "<b>Landkreis (LK):</b>",lk_name, "<br>",
                            "<b>Verbandsgemeinde:</b>", vg_name, "<br>",
                            "<b>EEG-Nr.:</b>", eeg_nr,"<br>",
                            "<b>Leistung [kW]:</b>", leistung_s, "<br>",
                            "<b>Nabenhoehe [m]:</b>", nabe, "<br>",
                            "<b>Rotordurchmesser [m]:</b>", rotor, "<br>",
                            "<b>Stromertrag 2019 [MWh]:</b>", Ertrag2019_MWh, "<br>",
                            "<b>Volllaststunden im LK 2019 [h]:</b>", round(menge_kwh/leistung_s), "<br>",
                            "<b style ='color: red'>Prognose Volllaststunden [h]:</b>", lk_volllast, "<br>",
                            "<b style ='color: red'>Prognose Stromertrag nach <br>Repowering [MWh]:</b>", ErtragRepowert, "<br>",
                            "<b style ='color: red'>Prognose Ertragssteigerung [%]:</b>", Ertragssteigerung, "<br>",
                            "<b style ='color: red'>Emissionsminderung nach Repowering bei Bundesstrommix 2019 [t/a]:</b>", round(Emissionsminderung), "<br>"
             ),
             label = ~as.character(vg_name))
###############################################################################
#print map                                                                    #
###############################################################################
m
###############################################################################
#save map as html file                                                        #
###############################################################################
saveWidget(m, file="repowering_map.html")























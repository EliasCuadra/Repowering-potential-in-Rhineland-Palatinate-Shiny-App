pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, leaflet, sp, raster, rgdal, htmltools, htmlwidgets,
               shiny, shinydashboard, DT, lubridate, readr)
# import and formatting
msb19_before2005 <- read.csv("data//msb19_before2005_geocoded.csv", header=TRUE)
msb19_before2005 <- msb19_before2005[,-1]
msb19_before2005$indatum_s <- as.Date(msb19_before2005$indatum_s, "%Y-%m-%d")
msb19_before2005 <- msb19_before2005 %>% 
    mutate(year = as.numeric(format(msb19_before2005$indatum_s, "%Y")))
    
# load shapes
land <- readOGR("Grenzen/Landesgrenze_RLP.shp")
landkreise <- readOGR("Grenzen/Landkreise_RLP.shp")
gemeinden <- readOGR("Grenzen/Verbandsgemeinde_RLP.shp")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Repowerin in RLP"),
    dashboardSidebar(
        sliderInput(inputId = "daterange",
                    label = "Date Range",
                    min = min(msb19_before2005$year),
                    max = max(msb19_before2005$year),
                    value = c(min(msb19_before2005$year), max(msb19_before2005$year)),
                    sep = "",
                    step = 1
        )
    ),
    
    dashboardBody(
        fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
        fluidRow(box(width = 12, dataTableOutput(outputId = "subdata")))
    )
    
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data_input <- reactive({
        
        msb19_before2005 %>% 
            filter(year >= input$daterange[1]) %>% 
            filter(year <= input$daterange[2])
    })
    
    output$mymap <- renderLeaflet(
        leaflet(data = data_input()) %>%
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
            
            addMarkers(lng = data_input() %>% pull(lon_wgs84),
                       lat = data_input() %>% pull(lat_wgs84),
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10),
                       popup = ~paste("<h3> Daten der Windkraftanlage</h3>",
                                      "<b>Landkreis (LK):</b>", lk_name, "<br>",
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
                                      "<b style ='color: red'>Emissionsminderung nach Repowering bei Bundesstrommix 2019 [t/a]:</b>", round(Emissionsminderung), "<br>"),
                       label = ~as.character(vg_name)
                       )
    )
    
    output$subdata <- renderDataTable(data_input(),
                                      options = list(scrollX = TRUE))
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)





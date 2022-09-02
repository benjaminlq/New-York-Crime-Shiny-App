#Library
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(readr)
library(dplyr)
library(plyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(tidyverse)
library(data.table)
library(zoo)
library(ggmosaic)
library(corrplot)
library(ggwordcloud)
library(wordcloud2)
#library(matrixStats)
library(ggpubr)
library(MASS)
library(pROC)
library(caret)
library(car)
#library(HH)
library(highcharter)
library(fresh)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#13435E"
  ),
  adminlte_sidebar(
    dark_bg = "#FAC56A",
    dark_hover_bg = "#2D75A3",
    dark_color = "#2E3440"
  )
)


## Import Dataz
#("C:/Users/User/Desktop/ASAR Project/ASAR-Project")
data <- read_csv("arrest_2016.csv")
for (i in 2017:2021){
  data <- rbind(data,read_csv(paste("arrest_",as.character(i),".csv",sep = "")))
}
arrest = subset(data, select = c('ARREST_DATE','OFNS_DESC','OFFENSE_TYPE', 'LAW_CAT_CD', 'ARREST_BORO',
                                 'ARREST_PRECINCT', 'AGE_GROUP','PERP_SEX','PERP_RACE','Latitude','Longitude'))
# precinct_info <- read_csv("Precinct_Population.csv")


## Recode Data
summary(arrest)
arrest$ARREST_DATE <- as.Date(arrest$ARREST_DATE,format = "%m/%d/%Y")
arrest$WEEKDAY <- weekdays(arrest$ARREST_DATE)
arrest$WEEKDAY <- factor(arrest$WEEKDAY, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
arrest$ARREST_PRECINCT <- as.character(arrest$ARREST_PRECINCT)
arrest$ARREST_BORO <- mapvalues(arrest$ARREST_BORO,
                                from = c('B','K','M','Q','S'),
                                to = c('Bronx','Brooklyn','Manhattan','Queens','Staten Island'))
arrest$LAW_CAT_CD <- mapvalues(arrest$LAW_CAT_CD,
                               from = c('F','I','M','V'),
                               to = c('Felony','Infraction','Misdemeanor','Violation'))
arrest$PERP_SEX <- mapvalues(arrest$PERP_SEX,
                             from = c('F','M'),
                             to = c('Female','Male'))

arrest$F_SEVEN <-ifelse(arrest$LAW_CAT_CD=='Felony',1,0)

## Exclude Mising Data
arrest = filter(arrest, LAW_CAT_CD!='')

## Create new variables YEAR, MONTH, MONTHYEAR:
arrest$ARREST_YEAR <- format(as.Date(arrest$ARREST_DATE, format="%d/%m/%Y"),"%Y")
arrest$ARREST_MONTH <- format(as.Date(arrest$ARREST_DATE, format="%d/%m/%Y"),"%m")
arrest$MONTHYEAR <- strftime(arrest$ARREST_DATE,format = "%m-%Y")
arrest$WEEK <- strftime(arrest$ARREST_DATE, format = "%V")
arrest$WEEKYEAR <- strftime(arrest$ARREST_DATE, format = "%V-%Y")
#arrest$LAW_CAT_CD [arrest$LAW_CAT_CD == 'Violation'] <- 'Violation and Infraction'
#arrest$LAW_CAT_CD [arrest$LAW_CAT_CD == 'Infraction'] <- 'Violation and Infraction'

## Map Data
boro_precinct = read.csv("boro_precinct.csv")
geo_boro <- rgdal::readOGR("Data/geo_boro/geo_export_fff598f0-6f34-4f6a-8c7c-8f23570c22c2.shp")
geo_precinct <- rgdal::readOGR("Data/geo_precinct1/geo_export_8c139c99-f563-418f-8a0e-ec536cb2ecc2.shp")
geo_precinct@data <- left_join(geo_precinct@data,boro_precinct[,c('ARREST_BORO','ARREST_PRECINCT')], by=c('precinct'='ARREST_PRECINCT')) 

## Calculate crime rate by population and area sqm2
# nyarea <- 783830000 # m2
# 
# precinct_info$Aream2 <- geo_precinct$shape_area/sum(geo_precinct$shape_area) * nyarea
# precinct_info$ARREST_PRECINCT <- as.character(precinct_info$ARREST_PRECINCT)

## Group by Precinct
# byprecinct <- left_join(arrest %>%filter(ARREST_YEAR != '2021') %>%group_by(ARREST_PRECINCT, ARREST_YEAR) %>%
#               dplyr::summarise(TOTAL = n()), precinct_info)
# byprecinct$crimeperpop <- byprecinct$TOTAL / byprecinct$Population * 1000  ## Per 1000 population
# byprecinct$crimeperarea <- byprecinct$TOTAL / byprecinct$Aream2 * 1000 ## Per 1000m2

arrest2021 <- arrest %>% filter(ARREST_YEAR == '2021')

####UI####
#Define UI for application that draws a histogram
ui <- dashboardPage(
                    dashboardHeader(title = 'New York Crime Rate 2016-2021', titleWidth = 350),
                    dashboardSidebar(width = 350,
                                     sidebarMenu(id = 'ibm',
                                                 menuItem('Exploratory Map', tabName = 'Map', icon = NULL),
                                                 menuItem('Descriptive', tabName = 'Descriptive', icon = NULL),
                                                 menuItem('Inferential', tabName = 'Inferential', icon = NULL)
                                                 # menuItem('CI', tabName = 'CI', icon = NULL)
                                     )
                    ),
                    dashboardBody(use_theme(mytheme),
                      tabItems(
                        
                        #########################MAP#######################
                        
                        tabItem(tabName = 'Map',
                                fluidPage(

                                  # Application title
                                  titlePanel("NYC Crime Map in 2021"),
                                  box(width = 8,
                                      selectInput("boro", "Borough:",
                                                  choices = c('All','Bronx','Brooklyn','Manhattan','Queens','Staten Island'))
                                      , 
                                      leafletOutput("map_boro", height = 500)
                                    )
                                  )
                                ),
                      
                      ########################DESCRIPTIVE#######################  
                      
                        
                      tabItem(tabName = 'Descriptive',
                               fluidPage(
                                 titlePanel("Descriptive"),
                                 
                                 ####WORD_CLOUD####         
                                 fluidRow(
                                   column(width = 12, tabsetPanel(
                                     tabPanel("WordCloud",
                                              box(width = 6,
                                                radioButtons('wcloud',
                                                             label = tags$strong('By: '),
                                                             choices = c('Crime Description' = 'OFNS_DESC',
                                                                         'Arrest Precincts' = 'ARREST_PRECINCT'),
                                                             inline = TRUE)
                                              ),
                                              
                                              box(
                                                width = 6,
                                                height = 80,
                                                solidHeader = TRUE,
                                                collapsible = FALSE,
                                                collapsed = FALSE,
                                                selectInput(
                                                  inputId = "YearWC", 
                                                  label = "Select years:", 
                                                  choices = as.character(2016:2021))
                                                  ),
                                              
                                              fluidRow(
                                                column(12, align="center",
                                                       wordcloud2Output(outputId  = "word1", width = 1200 , height = 600)
                                                )
                                                )
                                              ),
                                     
                                     ####EDA####
                                     tabPanel("Total Crime Comparison",
                                              box(
                                                width = 12,
                                                height = 80,
                                                solidHeader = TRUE,
                                                collapsible = FALSE,
                                                collapsed = FALSE,
                                                selectInput(
                                                  inputId = "Year", 
                                                  label = "Select years:", 
                                                  choices = as.character(2016:2021))
                                              ),
                                              box(width = 6,
                                                  plotOutput(outputId = "bar1",
                                                             height = "500")
                                              ),
                                              box(width = 6,
                                                  plotOutput(outputId = "bar2",
                                                             height = "500")
                                              ),
                                              box(
                                                radioButtons('xcol',
                                                             label = tags$strong('Crime Comparison by:'),
                                                             choices = c('Severity' = 'LAW_CAT_CD',
                                                                         'Boroughs' = 'ARREST_BORO',
                                                                         'Age Group' = 'AGE_GROUP',
                                                                         'Sex' = 'PERP_SEX',
                                                                         'Race' = 'PERP_RACE'),
                                                                         inline = TRUE)
                                              ),
                                              
                                              fluidRow(
                                                column(12, align = "center",
                                                       box(
                                                         width = 6,
                                                         height = 450,
                                                         solidHeader = TRUE,
                                                         collapsible = FALSE,
                                                         collapsed = FALSE,
                                                         plotOutput('EDA_bar', height = 450)
                                                       ),
                                                       box(
                                                         width = 6,
                                                         height = 450,
                                                         plotOutput('pie1', height = 450)
                                                       )
                                                       )
                                              ),
                                              box(width = 6,
                                                radioButtons('crimerate',
                                                             label = tags$strong('Crime Rate by:'),
                                                             choices = c('Months' = 'months',
                                                                         'Weeks' = 'weeks'),
                                                             inline = TRUE)
                                              ),
                                              
                                              fluidRow(
                                                column(12, align = "center",
                                                       box(width = 12,
                                                           plotOutput(outputId = "boxplot1", height = 600)
                                                       )
                                                  
                                                )
                                              )
                                              ),

                                     ####MOSAIC PLOTS####
                                     tabPanel("Mosaic Plots",
                                              box(
                                                width = 6,
                                                height = 80,
                                                solidHeader = TRUE,
                                                collapsible = FALSE,
                                                collapsed = FALSE,
                                                selectInput(
                                                  inputId = "dep_var", 
                                                  label = "Select Dependent Variable:", 
                                                  choices = c('LAW_CAT_CD','ARREST_BORO','AGE_GROUP','PERP_SEX','PERP_RACE','WEEKDAY'))
                                              ),
                                              
                                              box(
                                                width = 6,
                                                height = 80,
                                                solidHeader = TRUE,
                                                collapsible = FALSE,
                                                collapsed = FALSE,
                                                selectInput(
                                                  inputId = "ind_var",
                                                  label = "Select Independent Variable:",
                                                  choices = c('AGE_GROUP','LAW_CAT_CD','ARREST_BORO','PERP_SEX','PERP_RACE','WEEKDAY'))
                                              ),
                                              
                                              box(width = 6,
                                                  plotOutput(outputId = "mosaic")
                                              ),
                                              
                                              box(
                                                width = 6,
                                                selectInput(
                                                  inputId = "cont1",
                                                  label = "Total Count/Percentage",
                                                  choices = c('Total Count','Percentage')
                                                  )
                                              ),
                                              
                                              box(
                                                width = 6,
                                                tableOutput("contingency1")
                                              )
                                              ),
                                     
                                     ####TIME_SERIES####
                                     tabPanel("Time Series",
                                              box(width = 10, highchartOutput("crimets")),
                                              box(width = 2, title = "Filter",
                                                  checkboxGroupInput("tscrime", label = "Crime Type: ",
                                                                     choices = c("Felony", "Misdemeanor", "Violation", 
                                                                                 "Infraction"),
                                                                     selected = c("Felony", "Misdemeanor", "Violation", 
                                                                                  "Infraction")
                                                                     ) 
                                                  ), 
                                              
                                              box(width = 12, highchartOutput("highheatmap")),
                                              fluidRow(
                                                  column(12, align = "center",
                                                         selectInput(
                                                             inputId = "tsbox", 
                                                             label = "Select Borough:", 
                                                             choices = c('All','Bronx','Brooklyn','Manhattan','Queens','Staten Island')
                                                         ),
                                                         
                                                         box(width = 12,
                                                             plotOutput(outputId = "tsboxplot", height = 600)
                                                         )
                                                         
                                                  )
                                              )
                                              )
                                     

                                     
                                     
                                   )
                                 )
                               )
                        
                      )
                    ),
                    
                    ########################INFERENTIAL#######################  
                    
                    tabItem(tabName = "Inferential",
                            fluidRow(
                              column(width = 12, tabsetPanel(
                                
                                tabPanel("One Way ANOVA",
                                         box(width = 6,
                                             height = 80,
                                             radioButtons('anova1rate',
                                                          label = tags$strong('Crime Rate by:'),
                                                          choices = c('Month' = 'months',
                                                                      'Week' = 'weeks'),
                                                          inline = TRUE)
                                         ),
                                         
                                         box(
                                           width = 6,
                                           height = 80,
                                           solidHeader = TRUE,
                                           collapsible = FALSE,
                                           collapsed = FALSE,
                                           selectInput(
                                             inputId = "anova1",
                                             label = "Select Category:",
                                             choices = c('AGE_GROUP','LAW_CAT_CD','ARREST_BORO','PERP_SEX','PERP_RACE','WEEKDAY'))
                                         ),
                                         
                                         
                                         box(width = 6,
                                             plotOutput(outputId = "anovaboxplot1", height = 300
                                                        )
                                         ),
                                         
                                         box(width = 6,
                                             sliderInput("conflvl1", "Confidence Level",
                                                     min = 80, max = 99, value = 95
                                             ),
                                             plotOutput(outputId = "fstatplot", height = 300
                                                        )
                                             ),
                                         
                                         box(
                                           verbatimTextOutput("anova_oneway")
                                         ),
                                         
                                         box("Key stats",
                                           verbatimTextOutput("keystatsanova1")
                                         )
                                ),
                                
                                tabPanel("TWO WAY ANOVA",
                                         fluidRow(
                                           column(2, selectInput("variable_y", "Dependent Variable:",
                                                                        c("Monthly Crime Rate" = "TOTAL_ARRESTS"))),
                                           column(2, selectInput("variable_x1", "Factor A:",
                                                                 c("Arrest Borough" = "ARREST_BORO",
                                                                   "Severity of Offense" = "LAW_CAT_CD",
                                                                   "Day of the Week" = "WEEKDAY",
                                                                   "Age" = "AGE_GROUP",
                                                                   "Gender" = "PERP_SEX",
                                                                   "Race" = "PERP_RACE"))),
                                           column(2, selectInput("variable_x2", "Factor B:",
                                                                 c("COVID-19" = "COVID_19")))
                                          ),
                                          box(
                                            plotOutput("twowayinteractionplot")
                                            ),
                                          box(
                                            plotOutput("twowayboxplot")
                                          ),
                                          box(
                                            verbatimTextOutput("twowayanovatable")
                                          )
                                ),
                                
                                tabPanel("CHI-SQUARE TEST",
                                         box(
                                           width = 6,
                                           height = 80,
                                           selectInput(
                                             inputId = "dep_var2", 
                                             label = "Dependent Variable:", 
                                             choices = c('LAW_CAT_CD'))
                                         ),
                                         
                                         box(
                                           width = 6,
                                           height = 80,
                                           solidHeader = TRUE,
                                           collapsible = FALSE,
                                           collapsed = FALSE,
                                           selectInput(
                                             inputId = "ind_var2",
                                             label = "Select Independent Variable:",
                                             choices = c('AGE_GROUP','ARREST_BORO','PERP_SEX','PERP_RACE','WEEKDAY'))
                                         ),
                                         
                                         fluidRow(
                                           column(width = 12,
                                                  box(
                                                    width = 6,
                                                    box(
                                                      width = 6,
                                                      selectInput(
                                                        inputId = "cont2",
                                                        label = "Contingency Table by:",
                                                        choices = c('Total Count','Percentage')
                                                      )
                                                    ),
                                                    tableOutput("contingency2")
                                                  ),
                                                  box(
                                                    width = 6,
                                                    plotOutput("residualplot")
                                                  )
                                                  )
                                         ),
                                         
                                         fluidRow(
                                           column(width = 12,
                                                  box(width = 6,
                                                      sliderInput("conflvl2", "Confidence Level",
                                                                  min = 80, max = 99, value = 95
                                                      ),
                                                      plotOutput(outputId = "chisqstatplot", height = 450
                                                      )
                                                  ),
                                                  
                                                  box(
                                                    verbatimTextOutput("chisqtest")
                                                  )
                                                  )
                                         )
                                         
                                         
                                ),
                                
                                tabPanel("LOGISTIC REGRESSION",
                                         fluidRow(
                                           column(2,
                                                  selectInput("sex", "Gender:",
                                                              c("Male" = 1,
                                                                "Female" = 0)),
                                                  selectInput("PERP_RACE", "Race:",
                                                              c("White"=4,
                                                                "Black"=0,
                                                                "Asian"=1,
                                                                "White Hispanic"=5,
                                                                "Black Hispanic"=2,
                                                                "American Native"=0,
                                                                "Unknown"=3
                                                              )),
                                                  radioButtons("AGEGROUP", "Age:",
                                                               c("< 18"=0,
                                                                 "18 to 24"=1,
                                                                 "25 to 44"=2,
                                                                 "45 to 64"=3,
                                                                 "> 65"=4
                                                               ))
                                                  
                                                  
                                           ),
                                           column(2,
                                                  selectInput("BORO", "Borough:",
                                                              c("Brooklyn"=1,
                                                                "Manhattan"=2,
                                                                "Queens"=3,
                                                                "Staten Island"=4,
                                                                "Bronx"=0
                                                              )),
                                                  airDatepickerInput("YEARMONTH",
                                                                     label = "Year and Month:",
                                                                     value = "2021-10-01",
                                                                     maxDate = "2030-12-31",
                                                                     minDate = "2015-08-01",
                                                                     view = "months", 
                                                                     minView = "months",
                                                                     dateFormat = "yyyy-mm"
                                                  ),
                                                  selectInput("WEEKDAY", "Day of the week:",
                                                              c("Monday"=0,
                                                                "Tuesday"=5,
                                                                "Wednesday"=6,
                                                                "Thursday"=4,
                                                                "Friday"=1,
                                                                "Saturday"=2,
                                                                "Sunday"=3
                                                              )),
                                           ),
                                           column(7,
                                                  box(width = 8,
                                                      plotOutput(outputId = "prediction")
                                                  )
                                           )
                                           
                                         )
                                )
                              )
                              )
)
)
)
)
)

server <- function(input, output) {

  ################MAP######################
  output$map_boro <- renderLeaflet({
    if (input$boro == 'Bronx') {
      pal <- colorFactor(palette = "red", levels = 'Bronx')
    } else if (input$boro == 'Brooklyn') {
      pal <- colorFactor(palette = "green", levels = 'Brooklyn')
    } else if (input$boro == 'Manhattan') {
      pal <- colorFactor(palette = "blue", levels = 'Manhattan')
    } else if (input$boro == 'Queens') {
      pal <- colorFactor(palette = "orange", levels = 'Queens')
    } else if (input$boro == 'Staten Island') {
      pal <- colorFactor(palette = "purple", levels = 'Staten Island')
    } else {
      pal <- colorFactor(palette = c("red", "green", "blue", "orange", "purple"),
                         levels = c('Bronx','Brooklyn','Manhattan','Queens','Staten Island'))
    }
    
    #arrboro <- arrest %>% filter(ARREST_YEAR == input$boro_yr)
    
    geo_precinct %>%
      leaflet() %>%
      setView(lng = -73.8, lat = 40.7, zoom = 10) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolygons(
        fillColor = ~pal(ARREST_BORO),
        label = ~ precinct,
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 0.9, fillOpacity = 0.3) %>%
      addMarkers(~arrest2021$Longitude, ~arrest2021$Latitude, 
                 clusterOptions = markerClusterOptions(showCoverageOnHover = F)
      )
  })
  

  ################DESCRIPTIVE######################
  output$word1 <- renderWordcloud2({
    word1 <- arrest %>%
      group_by_(.dots = input$wcloud) %>%
      filter(ARREST_YEAR == input$YearWC) %>%
      dplyr::summarise(TOTAL = n())
    
    p <- wordcloud2(word1, color='random-light', backgroundColor="black")
    
    return(p)
    
  })    
  
  output$bar1 <- renderPlot({
    word <- arrest %>%
      group_by(OFFENSE_TYPE) %>%
      filter(ARREST_YEAR == input$Year) %>%
      dplyr::summarise(TOTAL = n())
    
    top10 <- top_n(word, 10, TOTAL)
    
    p <- ggplot(top10, aes(x = reorder(OFFENSE_TYPE,TOTAL),y = TOTAL)) +
      geom_bar(aes(fill = OFFENSE_TYPE),stat='identity') + coord_flip() +
      labs(title = 'Total Crimes by OFFENSE_TYPE', subtitle = 'OFFENSE_TYPE') + 
      xlab('OFFENSE_TYPE') +
      theme(axis.text.y = element_text(size = 10,angle=35),
            title = element_text(size = 20),
            legend.position = "None")
    
    return(p)
    
  })
  
  output$bar2 <- renderPlot({
    word <- arrest %>%
      group_by(ARREST_PRECINCT) %>%
      filter(ARREST_YEAR == input$Year) %>%
      dplyr::summarise(TOTAL = n())
    
    top10 <- top_n(word, 10, TOTAL)
    p <- ggplot(top10, aes(x = reorder(ARREST_PRECINCT,TOTAL),y = TOTAL)) +
      geom_bar(aes(fill = ARREST_PRECINCT),stat='identity') + coord_flip() +
      labs(title = 'Total Crimes by ARREST_PRECINCT',subtitle = 'PRECINCTS') + 
      xlab('ARREST_PRECINCT') +
      theme(axis.text.y = element_text(size = 10,angle=35),
            title = element_text(size = 20),
            legend.position = "None")
    
    return(p)
    
  })
  
  output$EDA_bar <- renderPlot({
    analysis <- arrest %>%
      group_by_(.dots = input$xcol) %>%
      filter(ARREST_YEAR == input$Year) %>%
      dplyr::summarise(TOTAL = n())
    
    p <- ggplot(analysis, aes_string(y = 'TOTAL', x = input$xcol)) +
      geom_bar(aes_string(fill = input$xcol), stat = 'identity') +
      labs(title = 'Total Crimes', subtitle = paste('by', input$xcol), 
           fill = input$xcol) + 
      xlab('Total Crimes by Year') +
      theme(axis.title = element_text(size = 16), title = element_text(size = 20),
            axis.text.x = element_text(size = 10,angle=90))
    
    return(p)
    
  })
  
  output$pie1 <- renderPlot({
    analysis <- arrest %>%
      group_by_(.dots = input$xcol) %>%
      filter(ARREST_YEAR == input$Year) %>%
      dplyr::summarise(TOTAL = n())
    
    if (input$xcol == 'LAW_CAT_CD'){
      p <- ggplot(analysis, aes (x ="", y= TOTAL, fill=LAW_CAT_CD)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + theme_void() +
        labs(title = 'Crime breakdown by Severity') +
        theme(title = element_text(size = 20))
    } else if (input$xcol == 'ARREST_BORO') {
      p <- ggplot(analysis, aes (x ="", y= TOTAL, fill=ARREST_BORO)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + theme_void() +
        labs(title = 'Crime breakdown by ARREST_BORO') +
        theme(title = element_text(size = 20))
    } else if (input$xcol == 'AGE_GROUP') {
      p <- ggplot(analysis, aes (x ="", y= TOTAL, fill=AGE_GROUP)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + theme_void() +
        labs(title = 'Crime breakdown by AGE_GROUP') +
        theme(title = element_text(size = 20))
    } else if (input$xcol == 'PERP_SEX') {
      p <- ggplot(analysis, aes (x ="", y= TOTAL, fill=PERP_SEX)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + theme_void() +
        labs(title = 'Crime breakdown by PERP_SEX') +
        theme(title = element_text(size = 20))
    } else {
      p <- ggplot(analysis, aes (x ="", y= TOTAL, fill=PERP_RACE)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + theme_void() +
        labs(title = 'Crime breakdown by PERP_RACE') +
        theme(title = element_text(size = 20))
    }
    
    return(p)
  })  
  
  output$boxplot1 <- renderPlot({
    
    if (input$crimerate == "months"){
      crimerate <- arrest %>%
        group_by(.dots = input$xcol, MONTHYEAR) %>%
        dplyr::summarise(TOTAL = n())
    } else {
      crimerate <- arrest %>%
        group_by(.dots = input$xcol, WEEKYEAR) %>%
        dplyr::summarise(TOTAL = n())
    }

    p <- ggplot(crimerate, aes_string(y = 'TOTAL', x = input$xcol)) +
      geom_boxplot(aes_string(fill = input$xcol)) +
      labs(title = 'Crime Rate Comparison', subtitle = paste('by', input$xcol)) + 
      xlab(paste('Category:',input$xcol)) +
      ylab(paste(input$crimerate,"rate")) +
      theme(axis.title = element_text(size = 16), title = element_text(size = 20),
            axis.text.x = element_text(size = 10,angle=90))
    
    return(p)
  })
  
  output$mosaic <- renderPlot({
    unloadNamespace("HH")
    unloadNamespace("matrixStats")
    #library(dplyr)
    if (input$ind_var == 'LAW_CAT_CD') {
      p <- ggplot(data = arrest) +
        geom_mosaic(aes_string(x = product(LAW_CAT_CD), fill = input$dep_var)) +
        labs(title = paste(input$dep_var, 'against LAW_CAT_CD')) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              axis.text.y = element_text(angle = 45),
              legend.position = "None")
    } else if (input$ind_var == 'AGE_GROUP') {
      p <- ggplot(data = arrest) +
        geom_mosaic(aes_string(x = product(AGE_GROUP), fill = input$dep_var)) +
        labs(title = paste(input$dep_var, 'against AGE_GROUP')) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              axis.text.y = element_text(angle = 45),
              legend.position = "None")
    } else if (input$ind_var == 'PERP_SEX') {
      p <- ggplot(data = arrest) +
        geom_mosaic(aes_string(x = product(PERP_SEX), fill = input$dep_var)) +
        labs(title = paste(input$dep_var, 'against PERP_SEX')) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              axis.text.y = element_text(angle = 45),
              legend.position = "None")
    } else if (input$ind_var == 'PERP_RACE') {
      p <- ggplot(data = arrest) +
        geom_mosaic(aes_string(x = product(PERP_RACE), fill = input$dep_var)) +
        labs(title = paste(input$dep_var, 'against PERP_RACE')) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              axis.text.y = element_text(angle = 45),
              legend.position = "None")
    } else if (input$ind_var == 'WEEKDAY') {
      p <- ggplot(data = arrest) +
        geom_mosaic(aes_string(x = product(WEEKDAY), fill = input$dep_var)) +
        labs(title = paste(input$dep_var, 'against WEEKDAY')) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              axis.text.y = element_text(angle = 45),
              legend.position = "None")
    } else {
      p <- ggplot(data = arrest) +
        geom_mosaic(aes_string(x = product(ARREST_BORO), fill = input$dep_var)) +
        labs(title = paste(input$dep_var, 'against ARREST_BORO')) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              axis.text.y = element_text(angle = 45),
              legend.position = "None")
    }
    return(p)
    
  }) 
  
  output$contingency1 <- renderTable({
    cont1 <- arrest%>%
      group_by_(.dots = input$ind_var, input$dep_var)%>%
      dplyr::summarise(n=n())%>%
      spread(input$dep_var, n)   
    
    cont1pct <- cont1
    sumcont1 <- sum(colSums(cont1pct[,-1]))
    cont1pct[-1] = cont1pct[-1]/sumcont1*100
    
    if (input$cont1 == 'Total Count'){
    cont1
    } else {
    cont1pct
    }
  })  
  
  ##########TS Chart###########
  output$crimets <- renderHighchart({
      library(xts)
      arrestts <- arrest %>% group_by(ARREST_DATE,LAW_CAT_CD) %>%
          dplyr::summarise(n=n()) %>%
          spread(LAW_CAT_CD, n)
      arrestts <- as.data.frame(arrestts)
      rownames(arrestts) <- seq.Date(as.Date("2016-01-01"), as.Date("2021-06-30"), "days")
      arrestts <- arrestts[,-1]
      arrestts.xts <- as.xts(arrestts)
      arrestts.xts[is.na(arrestts.xts)] <- 0
      filtered_arrestts <- arrestts.xts[,input$tscrime]

      p <- highchart(type = 'stock') %>%
          hc_title(text = "Time Series By Crime Severity")

      for(i in 1: ncol(filtered_arrestts)) {
            p <- p %>%
            hc_add_series(filtered_arrestts[,i], name = input$tscrime[i])
          }
      p
  })
  
  output$highheatmap <- renderHighchart({
      library(xts)
      arresthm <- arrest %>% filter(arrest$ARREST_YEAR != 2021) %>% 
          group_by(ARREST_DATE,LAW_CAT_CD) %>% dplyr::summarise(n=n()) %>%
          spread(LAW_CAT_CD, n)
      arresthm <- as.data.frame(arresthm)
      rownames(arresthm) <- seq.Date(as.Date("2016-01-01"), as.Date("2020-12-31"), "days")
      arresthm <- arresthm[,-1]
      arresthm.xts <- as.xts(arresthm)
      data.mon <- apply.monthly(arresthm.xts, mean)
      data.mon[is.na(data.mon)] <- 0
      data.mon.sum <- apply(data.mon, 1, sum)
      dsheatmap <- tibble::as_tibble((expand.grid(seq(12) - 1, seq(5) - 1))) %>% 
          mutate(value = data.mon.sum) %>% 
          list_parse2()
      
      stops <- data.frame(q = 0:4/4,
                          c = rev(substring(heat.colors(4 + 1), 0, 7)),
                          stringsAsFactors = FALSE)
      stops <- list_parse2(stops)
      
      highchart() %>% 
          hc_title(text = "Monthly Total Crime Number") %>%
          hc_chart(type = "heatmap") %>% 
          hc_xAxis(categories = month.abb) %>% 
          hc_yAxis(categories = seq(2016, 2020, by = 1)) %>% 
          hc_add_series(name = "Crime", data = dsheatmap) %>% 
          hc_colorAxis(stops = stops, min = 200, max = 1100) 
      
  })
  
  output$tsboxplot <- renderPlot({
      if (input$tsbox == 'All'){
          crimerate <- arrest %>%
              group_by(WEEKDAY) %>%
              dplyr::summarise(TOTAL = n())
      } else {
          crimerate <- arrest %>%
              filter(ARREST_BORO == input$tsbox) %>%
              group_by(WEEKDAY) %>%
              dplyr::summarise(TOTAL = n())
      }

      p <- ggplot(crimerate, aes(y = TOTAL, x = WEEKDAY)) +
          geom_bar(aes(fill = WEEKDAY),stat = 'identity') +
          labs(title = 'Crime Rate Comparison by Weekday', subtitle = paste('by', input$tsbox)) + 
          xlab('Weekday') +
          ylab('Crime rate') +
          theme(axis.title = element_text(size = 16), title = element_text(size = 20),
                axis.text.x = element_text(size = 10,angle=90))
      
      return(p)
  })
  
  ################INFERENTIAL######################
  
  ################ONE-WAY ANOVA#################
  output$anovaboxplot1 <- renderPlot({
    
    if (input$anova1rate == "months"){
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, MONTHYEAR) %>%
        dplyr::summarise(TOTAL = n())
    } else {
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, WEEKYEAR) %>%
        dplyr::summarise(TOTAL = n())
    }

    p <- ggplot(crimerate, aes_string(y = 'TOTAL', x = input$anova1)) +
      geom_boxplot(aes_string(fill = input$anova1)) +
      labs(title = 'Crime Rate Comparison', subtitle = paste('by', input$anova1)) +
      xlab(paste('Category:',input$anova1)) +
      ylab(paste("Crime Rate")) +
      theme(axis.title = element_text(size = 16), title = element_text(size = 16),
            axis.text.x = element_text(size = 10,angle=75,vjust = 0.5),
            legend.position = "None")
    return(p)
    
  }) 
  
  output$fstatplot <- renderPlot({
    library(HH)
    if (input$anova1rate == "months"){
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, MONTHYEAR) %>%
        dplyr::summarise(TOTAL = n())
    } else {
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, WEEKYEAR) %>%
        dplyr::summarise(TOTAL = n())
    }
    
    if (input$anova1 == 'LAW_CAT_CD'){
      res.aov <- aov(TOTAL ~ LAW_CAT_CD, data = crimerate)
    } else if (input$anova1 == 'AGE_GROUP') {
      res.aov <- aov(TOTAL ~ AGE_GROUP, data = crimerate)
    } else if (input$anova1 == 'ARREST_BORO') {
      res.aov <- aov(TOTAL ~ ARREST_BORO, data = crimerate)
    } else if (input$anova1 == 'PERP_SEX'){
      res.aov <- aov(TOTAL ~ PERP_SEX, data = crimerate)
    } else if (input$anova1 == 'PERP_RACE'){
      res.aov <- aov(TOTAL ~ PERP_RACE, data = crimerate)
    } else {
      res.aov <- aov(TOTAL ~ WEEKDAY, data = crimerate)
    }
    
    fstat <- summary(res.aov)
    df1 = fstat[[1]][['Df']][[1]]
    df2 = length(crimerate$TOTAL) + 1
    old.omd <- par(omd=c(.05,.88, .05,1))
    F.setup(df1=df1, df2=df2)
    F.curve(df1=df1, df2=df2, col='blue', alpha = 1 - input$conflvl1/100)
    par(old.omd)
  }) 
  
  output$anova_oneway <- renderPrint({
    
    if (input$anova1rate == "months"){
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, MONTHYEAR) %>%
        dplyr::summarise(TOTAL = n())
    } else {
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, WEEKYEAR) %>%
        dplyr::summarise(TOTAL = n())
    }
    
    if (input$anova1 == 'LAW_CAT_CD'){
      res.aov <- aov(TOTAL ~ LAW_CAT_CD, data = crimerate)
    } else if (input$anova1 == 'AGE_GROUP') {
      res.aov <- aov(TOTAL ~ AGE_GROUP, data = crimerate)
    } else if (input$anova1 == 'ARREST_BORO') {
      res.aov <- aov(TOTAL ~ ARREST_BORO, data = crimerate)
    } else if (input$anova1 == 'PERP_SEX'){
      res.aov <- aov(TOTAL ~ PERP_SEX, data = crimerate)
    } else if (input$anova1 == 'PERP_RACE'){
      res.aov <- aov(TOTAL ~ PERP_RACE, data = crimerate)
    } else {
      res.aov <- aov(TOTAL ~ WEEKDAY, data = crimerate)
    }
    print(summary(res.aov))
    print(TukeyHSD(res.aov))
  }) 
  
  output$keystatsanova1 <- renderPrint({
    
    if (input$anova1rate == "months"){
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, MONTHYEAR) %>%
        dplyr::summarise(TOTAL = n())
    } else {
      crimerate <- arrest %>%
        group_by(.dots = input$anova1, WEEKYEAR) %>%
        dplyr::summarise(TOTAL = n())
    }
    
    if (input$anova1 == 'LAW_CAT_CD'){
      res.aov <- aov(TOTAL ~ LAW_CAT_CD, data = crimerate)
    } else if (input$anova1 == 'AGE_GROUP') {
      res.aov <- aov(TOTAL ~ AGE_GROUP, data = crimerate)
    } else if (input$anova1 == 'ARREST_BORO') {
      res.aov <- aov(TOTAL ~ ARREST_BORO, data = crimerate)
    } else if (input$anova1 == 'PERP_SEX'){
      res.aov <- aov(TOTAL ~ PERP_SEX, data = crimerate)
    } else if (input$anova1 == 'PERP_RACE'){
      res.aov <- aov(TOTAL ~ PERP_RACE, data = crimerate)
    } else {
      res.aov <- aov(TOTAL ~ WEEKDAY, data = crimerate)
    }
    
    fstat <- summary(res.aov)
    
    print(paste("DF1:", fstat[[1]][['Df']][[1]]))
    print(paste("DF2:", fstat[[1]][['Df']][[2]]))
    print(paste("F-Value",fstat[[1]][['F value']][[1]]))
    print(paste("p-Value",fstat[[1]][['Pr(>F)']][[1]]))
  }) 
  
  #################TWO-WAY ANOVA#################
  output$twowayboxplot <- renderPlot({
    
    crime_rate <- arrest %>% dplyr::group_by(.dots = input$variable_x1, ARREST_YEAR, ARREST_MONTH, MONTHYEAR) %>%
      dplyr::summarise(TOTAL_ARRESTS=n()) %>%
      dplyr::mutate(COVID_19 = case_when(ARREST_YEAR < "2020" ~ "Before COVID-19", ARREST_YEAR == "2020" & ARREST_MONTH < "03" ~ "Before COVID-19",
                                  ARREST_YEAR == "2020" & ARREST_MONTH >= "03" ~ "COVID-19", ARREST_YEAR == "2021" ~ "COVID-19"))
    p <- ggboxplot(crime_rate, 
              x = input$variable_x1,
              y = input$variable_y,
              color = input$variable_x2,
              title = paste("Crime Rate by", input$variable_x1, "and", input$variable_x2))
    
    ggpar(p, legend = "bottom",
              color = input$variable_x2, font.main = 16) +
      theme(axis.text.x = element_text(size = 8,angle=90,vjust = 0.5))
  })
  
  output$twowayinteractionplot <- renderPlot({
    
    crime_rate <- arrest %>% dplyr::group_by(.dots = input$variable_x1, ARREST_YEAR, ARREST_MONTH, MONTHYEAR) %>%
      dplyr::summarise(TOTAL_ARRESTS=n()) %>%
      dplyr::mutate(COVID_19 = case_when(ARREST_YEAR < "2020" ~ "Before COVID-19", ARREST_YEAR == "2020" & ARREST_MONTH < "03" ~ "Before COVID-19",
                                         ARREST_YEAR == "2020" & ARREST_MONTH >= "03" ~ "COVID-19", ARREST_YEAR == "2021" ~ "COVID-19"))
    
    p <- ggline(crime_rate,
           x = input$variable_x1,
           y = input$variable_y,
           color = input$variable_x2,
           add = c("mean_se", "dotplot"),
           title = paste("Two-way Interaction Plot of", input$variable_x1, "and", input$variable_x2))
    ggpar(p, legend = "bottom", font.main = 16) +
           theme(axis.text.x = element_text(size = 8,angle=90,vjust = 0.5))
  
  })
  
  output$twowayanovatable <- renderPrint({
    
    crime_rate <- arrest %>% dplyr::group_by(.dots = input$variable_x1, ARREST_YEAR, ARREST_MONTH, MONTHYEAR) %>%
      dplyr::summarise(TOTAL_ARRESTS=n()) %>%
      dplyr::mutate(COVID_19 = case_when(ARREST_YEAR < "2020" ~ "Before COVID-19", ARREST_YEAR == "2020" & ARREST_MONTH < "03" ~ "Before COVID-19",
                                         ARREST_YEAR == "2020" & ARREST_MONTH >= "03" ~ "COVID-19", ARREST_YEAR == "2021" ~ "COVID-19"))
    
    crime_rate[["selected_x"]] = crime_rate[[input$variable_x1]]
    crime_rate[["selected_y"]] = crime_rate[[input$variable_y]]
    aov.model <- aov(
      as.formula(
        paste(input$variable_y, '~', input$variable_x1, '*', input$variable_x2)),
              data=crime_rate)
        
    aov.model.unbalanced <- Anova(aov.model, type = "III")
    cat('---------------------------- ANOVA ----------------------------\n')
    print(aov.model.unbalanced)
    
    cat("\n")
    cat('-------------------- TUKEY HSD for Factor A --------------------\n')
    tukey <- TukeyHSD(aov.model, which = input$variable_x1)
    print(tukey)
    
    cat("\n")
    cat('-------------------- TUKEY HSD for Factor B --------------------\n')
    tukey <- TukeyHSD(aov.model, which = input$variable_x2)
    
  })
  #################CHI-SQUARE TEST#################
  output$contingency2 <- renderTable({
    cont2 <- arrest%>%
      group_by_(.dots = input$ind_var2, input$dep_var2)%>%
      dplyr::summarise(n=n())%>%
      spread(input$dep_var2, n)   
    
    cont2pct <- cont2
    sumcont2 <- sum(colSums(cont2pct[,-1]))
    cont2pct[-1] = cont2pct[-1]/sumcont2*100
    
    if (input$cont2 == 'Total Count'){
      cont2
    } else {
      cont2pct
    }
  })  
  
  output$residualplot <- renderPlot({
    if (input$ind_var2 == 'AGE_GROUP'){
      conttable <- xtabs(~AGE_GROUP + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'ARREST_BORO'){
      conttable <- xtabs(~ARREST_BORO + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'PERP_SEX'){
      conttable <- xtabs(~PERP_SEX + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'PERP_RACE'){
      conttable <- xtabs(~PERP_RACE + LAW_CAT_CD, data = arrest)
    } else {
      conttable <- xtabs(~WEEKDAY + LAW_CAT_CD, data = arrest)
    }
    
    chisqtest <- chisq.test(conttable)
    corrplot(chisqtest$residuals, is.cor = FALSE)
  })
  
  output$chisqtest <- renderPrint ({
    if (input$ind_var2 == 'AGE_GROUP'){
      conttable <- xtabs(~AGE_GROUP + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'ARREST_BORO'){
      conttable <- xtabs(~ARREST_BORO + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'PERP_SEX'){
      conttable <- xtabs(~PERP_SEX + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'PERP_RACE'){
      conttable <- xtabs(~PERP_RACE + LAW_CAT_CD, data = arrest)
    } else {
      conttable <- xtabs(~WEEKDAY + LAW_CAT_CD, data = arrest)
    }
    
    chisqtest <- chisq.test(conttable)
    print(chisqtest)
    print(paste("P value at significant level of ",input$conflvl2,"%: ", 
                qchisq(input$conflvl2/100,chisqtest[['parameter']][['df']])))
  })
  
  output$chisqstatplot <- renderPlot({
    if (input$ind_var2 == 'AGE_GROUP'){
      conttable <- xtabs(~AGE_GROUP + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'ARREST_BORO'){
      conttable <- xtabs(~ARREST_BORO + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'PERP_SEX'){
      conttable <- xtabs(~PERP_SEX + LAW_CAT_CD, data = arrest)
    } else if (input$ind_var2 == 'PERP_RACE'){
      conttable <- xtabs(~PERP_RACE + LAW_CAT_CD, data = arrest)
    } else {
      conttable <- xtabs(~WEEKDAY + LAW_CAT_CD, data = arrest)
    }
    
    chisqtest <- chisq.test(conttable)
    df <- chisqtest[['parameter']][['df']]
    chisq.setup(df=df, ncp=0)
    
    chisq.curve(df=df, ncp=0, alpha = 1 - input$conflvl2/100)
  })
  #################LOGISTIC#################
  race<-rep(0,5)
  boro <- rep(0,4)
  week <-rep(0,6)
  age<-rep(0,4)
  # use coefficients directly to save computing time for the app
  logitinterc <- -351.4
  logitcoef <- c(-0.3155,
                 0.1744,
                 0.02009,
                 -0.08325,
                 -0.09025,
                 -0.5244,
                 -0.4128,
                 -0.1176,
                 -0.2163,
                 -0.6173,
                 -0.1746,
                 -0.3901,
                 -0.246,
                 -0.1409,
                 0.0789,
                 -0.1829,
                 -0.1235,
                 -0.1957,
                 -0.6891,
                 -0.7911,
                 -1.007,
                 -0.6545)
  
  output$prediction <- renderPlot({
    library(matrixStats)
    year = format(as.Date(input$YEARMONTH, format="yyyy-mm"),"%Y")
    month = format(as.Date(input$YEARMONTH, format="yyyy-mm"),"%m")
    
    if (input$PERP_RACE != 0) {
      race[[as.numeric(input$PERP_RACE)]]<- 1
    }
    if (input$BORO != 0) {
      boro[[as.numeric(input$BORO)]]<- 1
    }
    if (input$WEEKDAY != 0) {
      week[[as.numeric(input$WEEKDAY)]]<- 1
    }
    if (input$AGEGROUP != 0) {
      age[[as.numeric(input$AGEGROUP)]]<- 1
    }
    
    inputDS=as.numeric(c(input$sex, year, month, race, boro,week, age))
    odds = exp(logitinterc+ crossprod(inputDS,logitcoef))
    prob = odds/(1+ odds)
    
    prob.ds <- data.frame(
      category=c("Violent", "Non-violent"),
      Probablity= c(prob,1-prob)
    )
    
    prob.ds$ymax <- cumsum(prob.ds$Probablity)
    prob.ds$ymin <- c(0, head(prob.ds$ymax, n=-1))
    prob.ds$labelPosition <- (prob.ds$ymax + prob.ds$ymin) / 2
    prob.ds$label <- paste0(prob.ds$category,": ", round(prob.ds$Probablity*100,2), " %")
    
    ggplot(prob.ds, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
      scale_fill_brewer(palette="Pastel2") +
      scale_color_brewer(palette="Dark2") +
      coord_polar(theta="y") +
      xlim(c(0, 4)) +
      theme_void() +
      theme(legend.position = "none",plot.title = element_text(face="bold"))+
      ggtitle("Predicted Probability")
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(pheatmap)
### Daten Laden ###
heart <- read.csv(file = "/Users/christian.willmann/Desktop/heart.csv")

### Quellen ### https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
urlcat <- a("Link", href="https://i.redd.it/eknbn2b019v71.png")


ui <- fluidPage(
    tabsetPanel(type = "tabs",
                tabPanel("Startseite",
                         sidebarPanel(
                             h4("Stochastik Gruppe I"),
                             p("Robert Seidentopf s0537217"),
                             p("Ataullah SHinwari s058"),
                             p("Christian Willmann s0581616"),
                             img(src = "HTW_logo_V2.png"),
                             p( br(),
                                tagList("URL Katze:", urlcat)
                             )
                         ),
                         mainPanel(
                             h1(strong(("Korrelation ist nicht gleich Kausalität."))), br(),
                             img(src = "Katze_Korrelation.png"), br(),
                             
                         )),
                tabPanel("Der Datensatz", 
                         sidebarLayout(position = "left", fluid = TRUE,sidebarPanel(selectInput("plotname", "Plot auswählen", c("Altersverteilung","Art des Brustschmerzes","Ruhepulsverteilung", "Cholesterolspiegel","Blutzuckerspiegelverteilung","Ergebnisse des Ruhe-Elektrokardiogramms", "Höchster gemessener Herzschlag", "Angina durch Belastung", "OldPeak", "ST_Slope", "HeartDisease")), actionButton("heat", "Heatmap"),checkboxGroupInput("checkbox", "Datansatz auswählen", choices = c("Rohdaten", "ohne Ausreißer"), selected = "Rohdaten")), mainPanel(plotlyOutput("selectplot"), br(), plotOutput("heat")))),
                tabPanel("Satz von Bayes")
                #Quelle: https://shiny.rstudio.com/articles/tabsets.html
                
                
    ))



server <- function(input, output) {

output$selectplot <- renderPlotly({   
    if(input$checkbox == "Rohdaten"){
    if(input$plotname == "Altersverteilung"){
      heart_age <- heart %>% group_by(Age, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_age, aes(x = Age, y = n)) 
                       + scale_y_continuous(name = "Anzahl") 
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3")) #, labels = c("M" = "männlich","F" = "weiblich")
                       + ggtitle("Altersverteilung") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "Art des Brustschmerzes"){
      heart_chestpaintype <- heart %>% group_by(ChestPainType, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_chestpaintype, aes(x = ChestPainType, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("Brustschmerz") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "Ruhepulsverteilung"){
      heart_restingbp <- heart %>% group_by(RestingBP, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_restingbp, aes(x = RestingBP, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("Ruhepulsverteilung") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "Cholesterolspiegelverteilung"){
      heart_cholesterol <- heart %>% group_by(Cholesterol, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_cholesterol, aes(x = Cholesterol, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("Cholesterolspiegelverteilung") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "Blutzuckerspiegelverteilung"){
      heart_fastingbs <- heart %>% group_by(FastingBS, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_fastingbs, aes(x = FastingBS, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("Blutzuckerspiegelverteilung") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "Ergebnisse des Ruhe-Elektrokardiogramms"){
      heart_restingecg <- heart %>% group_by(RestingECG, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_restingecg, aes(x = RestingECG, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("Ergebnisse des Ruhe-Elektrokardiogramms") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "Höchster gemessener Herzschlag"){
      heart_maxhr <- heart %>% group_by(MaxHR, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_maxhr, aes(x = MaxHR, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("Höchster gemessener Herzschlag") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "Angina durch Belastung"){
      heart_exerciseangina <- heart %>% group_by(ExerciseAngina, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_exerciseangina, aes(x = ExerciseAngina, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("Angina durch Belastung") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "OldPeak"){
      heart_oldpeak <- heart %>% group_by(Oldpeak, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_oldpeak, aes(x = Oldpeak, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("OldPeak") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "ST_Slope"){
      heart_st_slope <- heart %>% group_by(ST_Slope, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_st_slope, aes(x = ST_Slope, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("ST_Slope") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }
    if(input$plotname == "HeartDisease"){
      heart_heartdisease <- heart %>% group_by(HeartDisease, Sex) %>% count()
      Plot <- ggplotly(ggplot(heart_heartdisease, aes(x = HeartDisease, y = n)) 
                       + scale_y_continuous(name = "Anzahl")
                       + scale_fill_manual("Legende",values = c("M" = "chartreuse4","F" = "chartreuse3"))
                       + ggtitle("HeartDisease") 
                       + theme(plot.title = element_text(size = 11, hjust = 0.5)) 
                       + geom_bar(aes(fill = Sex), stat = "identity", position = "dodge", width = 0.7))
    }}
    Plot
    })
  observeEvent(input$heat, {
    if(input$checkbox == "Rohdaten"){
    heart$Sex <- as.numeric(c("M" = 0, "F" = 1)[heart$Sex])
    heart$ChestPainType <- as.numeric(c("ATA" = 0, "NAP" = 1, "ASY" = 2, "TA" = 3)[heart$ChestPainType])
    heart$RestingECG <- as.numeric(c("Normal" = 0, "ST" = 1, "LVH" = 2)[heart$RestingECG])
    heart$ExerciseAngina <- as.numeric(c("N" = 0, "Y" = 1)[heart$ExerciseAngina])
    heart$ST_Slope <- as.numeric(c("Up" = 0, "Flat" = 1, "Down" = 2))
    heart_cor <- cor(as.matrix(heart))
    heatplot <- pheatmap(heart_cor, display_numbers = T, main = "Heatmap der Rohdaten" )
    output$heat <-renderPlot({heatplot})
    }
  })
  }

  
shinyApp(ui = ui, server = server)

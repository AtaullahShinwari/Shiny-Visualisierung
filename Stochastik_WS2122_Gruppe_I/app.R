library(shiny)
library(tidyverse)
library(ggplot2)
### Daten Laden ###
heart <- read.csv(file = "C:/Users/rseid/Desktop/Stochastik/Abgabe/data/heart.csv")

### Quellen ### https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
urlcat <- a("Link", href="https://i.redd.it/eknbn2b019v71.png")


ui <- fluidPage(
    tabsetPanel(type = "tabs",
                tabPanel("Startseite",
                         sidebarPanel(
                             h4("Stochastik Gruppe I"),
                             p("Robert Seidentopf s0537217"),
                             p("Ata"),
                             p("Chris"),
                             img(src = "HTW_logo_V2.png"),
                             p( br(),
                                tagList("URL Katze:", urlcat)
                             )
                         ),
                         mainPanel(
                             h1(strong(("Korrelation ist nicht gleich Kausalität."))), br(),
                             img(src = "Katze_Korrelation.png"), br(),
                             
                         )),
                tabPanel("Exploration"),
                tabPanel("Satz von Bayes")
                #Quelle: https://shiny.rstudio.com/articles/tabsets.html
                
                
    ))



server <- function(input, output) {

  heart_age <- heart %>% group_by(Age) %>% count()
  heart_age_barplot <- ggplot(heart_age, aes(x = Age, y = n)) + scale_y_continuous(name = "Anzahl") + ggtitle("Altersverteilung") + theme(plot.title = element_text(size = 11, hjust = 0.5)) + geom_bar(stat = "identity", colour = "yellowgreen")
  heart_age_barplot
}

shinyApp(ui = ui, server = server)

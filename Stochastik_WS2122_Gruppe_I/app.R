library(shiny)

### Daten Laden ###


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
################PUSH TEST
  TEST
}

shinyApp(ui = ui, server = server)

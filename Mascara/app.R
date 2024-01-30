#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

setwd("G:/Drives compartilhados/3-CCD/25 Data Stewards/4 - Opioides/Cancelamentos_base/")
load("Cancelamentos_reduzido.RData")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Máscara com o código dos usuários do painel de cancelamento"),

    # Show a table of id_usuario
    mainPanel(
      tabPanel("Tabela", dataTableOutput("mascara"))
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mascara <- renderDataTable({
       mascara_cancelamento
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

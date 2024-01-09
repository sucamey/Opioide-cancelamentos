#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required libraries
library(shiny)
library(ggplot2)
library(qcc)
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggQC)
library(bslib)

setwd("G:/Drives compartilhados/3-CCD/25 Data Stewards/GIT/Opioide-cancelamentos")
source("cancelamento-processamento-dados.R")
load("G:/Drives compartilhados/3-CCD/25 Data Stewards/GIT/Opioide-cancelamentos/Dados/Cancelamentos_reduzido.RData")



# UI logic
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "cerulean"),
  titlePanel("Gráfico de controle de qualidade para cancelamento"),
  tabsetPanel(
    tabPanel("Geral",
    sidebarLayout(
      sidebarPanel(
        # Filter input (e.g., select box, slider, etc.)
        selectInput("classe", "Selecione classe de medicamento:", choices = unique(Cancelamentos_reduzido$Clase), selected = NULL),
        dateRangeInput("periodo", "Selecione Período:", 
                       start = min(Cancelamentos_reduzido$Fecha), 
                       end = max(Cancelamentos_reduzido$Fecha),
                       #format, 
                       #startview,
                       #weekstart, language, separator, width,
                       #autoclose
        ),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Gráfico U", plotOutput("U_geral")), 
          tabPanel("Violações", dataTableOutput("Violacoes_geral")),
          tabPanel("Dias com violações", dataTableOutput("Dias_Violacoes_geral")),
#          tabPanel("Pareto",  plotOutput("Pareto_geral")), 
          tabPanel("Usuários top cancelamentos",  dataTableOutput("Top_cancelamentos_geral"))
        ),
        width = 10
      )
    )
  ),
  tabPanel("Por dispensário",
    sidebarLayout(
      sidebarPanel(
        # Filter input (e.g., select box, slider, etc.)
        selectInput("dispensario", "Selecione dispensário:", choices = unique(Cancelamentos_reduzido$dispensario), selected = NULL),
        selectInput("tipo_med", "Selecione classe de medicamento:", choices = unique(Cancelamentos_reduzido$Clase), selected = NULL),
        dateRangeInput("int_tempo", "Selecione Período:", 
                       start = min(Cancelamentos_reduzido$Fecha), 
                       end = max(Cancelamentos_reduzido$Fecha),
                       #format, 
                       #startview,
                       #weekstart, language, separator, width,
                       #autoclose
        ),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Gráfico U", plotOutput("U")), 
          tabPanel("Violações", dataTableOutput("Violacoes")),
          tabPanel("Dias com violações", dataTableOutput("Dias_Violacoes")),
#          tabPanel("Pareto",  plotOutput("Pareto")), 
          tabPanel("Usuários top cancelamentos",  dataTableOutput("Top_cancelamentos"))
        ),
        width = 10
      )
    )
  )
  )
)


# Server logic
server <- function(input, output) {
  # Filtered data based on the input selection
################################# Painel Geral
  filtered_data_geral <- reactive({
    if (!is.null(input$classe) & !is.null(input$periodo)) {
      as_tibble(Cancelamentos_reduzido_geral) %>% 
        filter(
          Fecha <= max(input$periodo) & Fecha >= min(input$periodo),
          Clase == input$classe)
    } else {
      data
    }
  })  
  
  pareto_data_geral <- reactive({
    if (!is.null(input$classe) & !is.null(input$periodo)) {
      Cancelamentos_usuario_dia_geral %>% 
        filter(
          Fecha <= max(input$periodo) & Fecha >= min(input$periodo),
          Clase == input$classe)
    } else {
      data
    }
  }) 
  
  grafico_geral <- reactive({
    qcc(filtered_data_geral()$n_cancelamentos,
        type = "u",
        sizes = filtered_data_geral()$n_usuarios,
        rules = shewhart.rules)
  })
  
  # grafico_geral2 <-reactive({
  #   pareto_data_geral() %>%
  #     group_by(id_usuario) %>%
  #     summarize(Total_cancelamentos = sum(n)) %>% 
  #     ggplot(aes(x=id_usuario, y=Total_cancelamentos)) +
  #     theme(axis.text.x=element_text(angle=90,hjust=1, vjust=0.5)) +
  #     stat_pareto()
  # })
  
  violacao_geral <- reactive(
    filtered_data_geral() %>% 
      slice(grafico_geral()$violations$beyond.limits)
  )
  
  Top_cancelamentos_geral <- reactive(
    pareto_data_geral() %>%
      group_by(id_usuario) %>%
      summarize(
        Total_cancelamentos = sum(n),
        dias = n(),
        Maximo = max(n),
        Media = round(mean(n),1)) %>% 
      arrange(desc(Total_cancelamentos)) 
  )
  
  # U
  output$U_geral <- renderPlot({
    plot(grafico_geral())
  })
  
  # # Pareto
  # output$Pareto_geral <- renderPlot({
  #   plot(grafico_geral2())
  # })
  
  # Violações
  output$Violacoes_geral <- renderDataTable({
    violacao_geral()
  })
  
  # Dias com violações
  output$Dias_Violacoes_geral <- renderDataTable({
    pareto_data_geral() %>% 
      filter(Fecha %in% violacao_geral()$Fecha)
  })
  
  # Top cancelamentos
  output$Top_cancelamentos_geral <- renderDataTable(
    Top_cancelamentos_geral()
  )
  
###################################### Painel por dispensário
  filtered_data <- reactive({
    if (!is.null(input$dispensario) & !is.null(input$int_tempo) & !is.null(input$tipo_med)) {
      as_tibble(Cancelamentos_reduzido) %>% 
        filter(dispensario == input$dispensario, 
               Fecha <= max(input$int_tempo) & Fecha >= min(input$int_tempo),
               Clase == input$tipo_med)
    } else {
      data
    }
  })  

  pareto_data <- reactive({
    if (!is.null(input$dispensario) & !is.null(input$int_tempo) & !is.null(input$tipo_med)) {
      Cancelamentos_usuario_dia %>% 
        filter(dispensario == input$dispensario,
               Fecha <= max(input$int_tempo) & Fecha >= min(input$int_tempo),
               Clase == input$tipo_med)
    } else {
      data
    }
  }) 
  
  grafico <- reactive({
    qcc(filtered_data()$n_cancelamentos,
        type = "u",
        sizes = filtered_data()$n_usuarios,
        rules = shewhart.rules)
  })
  
  grafico2 <-reactive({
    pareto_data() %>%
      group_by(id_usuario) %>%
      summarize(Total_cancelamentos = sum(n)) %>% 
      ggplot(aes(x=id_usuario, y=Total_cancelamentos)) +
      theme(axis.text.x=element_text(angle=90,hjust=1, vjust=0.5)) +
      stat_pareto()
  })
  
  violacao <- reactive(
    filtered_data() %>% 
      slice(grafico()$violations$beyond.limits)
  )
  
  Top_cancelamentos <- reactive(
    pareto_data() %>%
      group_by(id_usuario) %>%
      summarize(
        Total_cancelamentos = sum(n),
        dias = n(),
        Maximo = max(n),
        Media = round(mean(n),1)) %>% 
      arrange(desc(Total_cancelamentos)) 
  )
  
  # U
  output$U <- renderPlot({
    plot(grafico())
  })
  
  # Pareto
  output$Pareto <- renderPlot({
    plot(grafico2())
  })
  
  # Violações
  output$Violacoes <- renderDataTable({
    violacao()
 })
  
  # Dias com violações
  output$Dias_Violacoes <- renderDataTable({
    pareto_data() %>% 
      filter(Fecha %in% violacao()$Fecha)
  })
  
  # Top cancelamentos
  output$Top_cancelamentos <- renderDataTable(
    Top_cancelamentos()
  )
}

# Run the Shiny app
shinyApp(ui, server)




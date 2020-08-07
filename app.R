library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(Hmisc)
library(openxlsx)
library(stringr)
library(zoo)

dados1_final <- setDT(readRDS("data/dados_direta_indireta_nao_automatica.rds"))
dados2_final <- setDT(readRDS("data/dados_indireta_automatica.rds"))

ui <- dashboardPage(
  
  dashboardHeader(title = "BNDES"),
  dashboardSidebar(
    
    sidebarMenu(
      
      id = "tab",
      menuItem("Estatisticas - Esfera 1", tabName = "estatisticas1", icon = icon("chart-bar")),
      menuItem("Estatísticas - Esfera 2", tabName = "estatisticas2", icon = icon("chart-bar"))
    
      )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "estatisticas1",
              fluidRow(
                
                box(dateRangeInput("dates1", "Datas", min = dados1[, min(data_final)], max = dados1[, max(data_final)],
                                          start = dados1[, min(data_final)], end = dados1[, max(data_final)], separator = "até", language = "pt"), height = "2cm"),
                box(selectInput("select1", "Porte do cliente", choices = list("Micro" = "MICRO",
                                                                              "Pequena" = "PEQUENA",
                                                                              "Média" = "MÉDIA",
                                                                              "Grande" = "GRANDE")), height = "2cm")
              ),
              
              fluidRow(
                
                valueBoxOutput("taxaJuros1"),
                valueBoxOutput("desembolsos1"),
                valueBoxOutput("contratacao1")
                
              ),
              
              fluidRow(
                
                plotOutput("plot1", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot2", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot3", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot4", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot5", height = 250)
                
              )
        
      ),
      
      tabItem(tabName = "estatisticas2",
              fluidRow(
                
                box(dateRangeInput("dates2", "Datas", min = dados1[, min(data_final)], max = dados1[, max(data_final)],
                                   start = dados1[, min(data_final)], end = dados1[, max(data_final)], separator = "até", language = "pt"), height = "2cm"),
                box(selectInput("select2", "Porte do cliente", choices = list("Micro" = "MICRO",
                                                                              "Pequena" = "PEQUENA",
                                                                              "Média" = "MÉDIA",
                                                                              "Grande" = "GRANDE")), height = "2cm")
              ),
              
              
              fluidRow(
                
                valueBoxOutput("taxaJuros2"),
                valueBoxOutput("desembolsos2"),
                valueBoxOutput("contratacao2")
                
              ),
              
              fluidRow(
                
                plotOutput("plot6", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot7", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot8", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot9", height = 250)
                
              ),
              
              fluidRow(
                
                plotOutput("plot10", height = 250)
                
              )
              
      )
      
    )
  
  )
  
)

server <- function(input, output){
  
  base <- reactive({
    
    if(input$tab=="estatisticas1"){
      
      dados1_final <- dados1_final[ymd(data) %in% seq.Date(ymd(input$dates1[1]), ymd(input$dates1[2]), by = "day")]
      dados1_final <- dados1_final[porte_cliente==input$select1]
      
    }
    
    else{
      
      dados2_final <- dados2_final[ymd(data) %in% seq.Date(ymd(input$dates2[1]), ymd(input$dates2[2]), by = "day")]
      dados2_final <- dados2_final[porte_cliente==input$select2] 
      
    }
    
  })
  
  output$taxaJuros1 <- renderValueBox({
    
    valueBox(
      
      paste0(round(base()[variable=="taxa_juros_media", mean(value)], digits = 1), "% a.a."), "Taxa de juros média", icon = icon("chart-line"), color = "navy"
      
    )
    
  })
  
  output$desembolsos1 <- renderValueBox({
    
    valueBox(
      
      paste("R$", round(base()[variable=="valor_desembolsado_medio", mean(value)], digits = 1), "MM"), "Valor desembolsado médio", icon = icon("money-bill-alt"), color = "navy"
    
      )
    
  })
  
  output$contratacao1 <- renderValueBox({
    
    valueBox(
      
      paste("R$", round(base()[variable=="valor_contratado_medio", mean(value)], digits = 1), "MM"), "Valor contratado médio", icon = icon("money-bill-alt"), color = "navy"
    
      )
    
  })
  
  output$taxaJuros2 <- renderValueBox({
    
    valueBox(
      
      paste0(round(base()[variable=="taxa_juros_media", mean(value)], digits = 1), "% a.a."), "Taxa de juros média", icon = icon("chart-line"), color = "navy"
      
    )
    
  })
  
  output$desembolsos2 <- renderValueBox({
    
    valueBox(
      
      paste("R$", round(base()[variable=="valor_desembolsado_medio", mean(value)], digits = 1), "MM"), "Valor desembolsado médio", icon = icon("money-bill-alt"), color = "navy"
      
    )
    
  })
  
  output$contratacao2 <- renderValueBox({
    
    valueBox(
      
      paste("R$", round(base()[variable=="valor_contratado_medio", mean(value)], digits = 1), "MM"), "Valor contratado médio", icon = icon("money-bill-alt"), color = "navy"
      
    )
    
  })
  
  output$plot1 <- renderPlot(
    
    ggplot(base()[variable=="valor_contratado_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Valor contratado médio (R$ milhões)",
           subtitle = "Evolução do valor contratado médio no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot2 <- renderPlot(
    
    ggplot(base()[variable=="valor_desembolsado_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Valor desembolsado médio (R$ milhões)",
           subtitle = "Evolução do valor desembolsado médio no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot3 <- renderPlot(
    
    ggplot(base()[variable=="taxa_juros_media"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Taxa de juros média (% a.a.)",
           subtitle = "Evolução da taxa de juros média no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot4 <- renderPlot(
    
    ggplot(base()[variable=="prazo_carencia_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Prazo de carência médio (meses)",
           subtitle = "Evolução do prazo de carência médio no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot5 <- renderPlot(
    
    ggplot(base()[variable=="prazo_amortizacao_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Prazo de amortização médio (meses)",
           subtitle = "Evolução do prazo de amortização no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot6 <- renderPlot(
    
    ggplot(base()[variable=="valor_contratado_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Valor contratado médio (R$ milhões)",
           subtitle = "Evolução do valor contratado médio no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot7 <- renderPlot(
    
    ggplot(base()[variable=="valor_desembolsado_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Valor desembolsado médio (R$ milhões)",
           subtitle = "Evolução do valor desembolsado médio no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot8 <- renderPlot(
    
    ggplot(base()[variable=="taxa_juros_media"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Taxa de juros média (% a.a.)",
           subtitle = "Evolução da taxa de juros média no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot9 <- renderPlot(
    
    ggplot(base()[variable=="prazo_carencia_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Prazo de carência médio (meses)",
           subtitle = "Evolução do prazo de carência médio no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
  output$plot10 <- renderPlot(
    
    ggplot(base()[variable=="prazo_amortizacao_medio"], aes(x=ymd(data))) + 
      geom_bar(aes(y=value, colour = "Série"), stat = "identity") +
      geom_line(aes(y=media_movel, colour = "Média móvel")) +
      scale_color_manual(values = c(Série = "black", "Média móvel" = "red")) +
      labs(title = "Prazo de amortização médio (meses)",
           subtitle = "Evolução do prazo de amortização no tempo",
           color = "Legenda") +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            legend.position = "bottom")
    
  )
  
}

shinyApp(ui, server)

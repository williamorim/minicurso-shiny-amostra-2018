library(shiny)
library(shinydashboard) # install.packages("shinydashboard")
library(highcharter)
library(tidyverse)

df_pkmn <- read_rds("data-raw/df_pkmn.rds") %>% 
  filter(id_geracao != 0, !is.na(url_imagem))

df_cores <- df_pkmn %>% 
  distinct(tipo_1, cor_1)

gen_names <- c(paste("Geração", 1:7), "Outros")
gen_valores <- c(1:7, 0)

url_imagem <- 
  "https://raw.githubusercontent.com/phalt/pokeapi/master/data/Pokemon_XY_Sprites/"

# Shiny ui ----------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Pokemon Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Visão geral", tabName = "visao_geral"),
      menuItem(text = "Por tipo", tabName = "por_tipo"),
      menuItem(text = "Comparando tipos", tabName = "comp_tipos"),
      tags$hr(),
      shiny::checkboxGroupInput(
        inputId = "geracoes",
        label = "Incluir",
        choices = purrr::set_names(gen_valores, gen_names),
        selected = c(1, 2)
      )
    )
  ),
  dashboardBody(
    tags$link(
      rel = "stylesheet", 
      type = "text/css",  
      href="https://use.fontawesome.com/releases/v5.3.1/css/all.css"
    ),
    tabItems(
      tabItem(
        tabName = "visao_geral",
        column(
          width = 6,
          box(
            title = "Número por tipo",
            width = 12,
            plotOutput(outputId = "bar_plot")
          )
        ),
        column(
          width = 6,
          fluidRow(
            valueBoxOutput(outputId = "vg_peso", width = 8),
            imageOutput(outputId = "vg_peso_imagem")
          )
        )
      )
    )
  )
)


# Shiny server ------------------------------------------------------------

server <- function(input, output, session) {
  
  df_gen <- reactive({
    df_pkmn %>% 
      filter(id_geracao %in% input$geracoes)
  })
  
  output$bar_plot <- renderPlot({
    
    req(input$geracoes)
    
    cores <- purrr::set_names(df_cores$cor_1, df_cores$tipo_1)
    
    df_gen() %>%
      gather(posicao, tipo, tipo_1, tipo_2) %>%
      filter(!is.na(tipo)) %>%
      count(tipo) %>% 
      mutate(tipo = forcats::fct_reorder(tipo, n)) %>% 
      ggplot(aes(x = tipo, y = n, fill = tipo)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = cores) +
      theme_minimal()
    
  })
  
  output$vg_peso <- renderValueBox({
    
    req(input$geracoes)
    
    mais_pesado <- df_gen() %>% 
      filter(peso == max(peso, na.rm = TRUE))
    
    valueBox(
      value = paste(mais_pesado$peso[1], "kg"),
      subtitle = mais_pesado$pokemon[1],
      icon = icon("weight-hanging")
    )
  })
  
  output$vg_peso_imagem <- renderImage({
    
    req(input$geracoes)
    
    mais_pesado <- df_gen() %>% 
      filter(peso == max(peso, na.rm = TRUE))
    
    temp <- tempdir()
      
    download.file(
      paste0(url_imagem, mais_pesado$url_imagem[1]),
      destfile = paste0(temp, "/imagem.png")
    )
    
    png(temp, width = 30, height = 30)
    
    list(
      src = paste0(temp, "/imagem.png"),
      alt = mais_pesado$pokemon[1]
    )
    
  }, deleteFile = FALSE)
  
}

shinyApp(ui, server)
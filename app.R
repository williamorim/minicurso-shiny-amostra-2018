library(shiny)
library(shinydashboard) # install.packages("shinydashboard")
library(highcharter)
library(tidyverse)


# Funções -----------------------------------------------------------------

source("R/utils.R")

# Dados -------------------------------------------------------------------

df_pkmn <- read_rds("data-raw/df_pkmn.rds") %>% 
  filter(id_geracao != 0, !is.na(url_imagem))

df_cores <- df_pkmn %>% 
  distinct(tipo_1, cor_1)

geracoes_nomes <- c(paste("Geração", 1:6))
geracoes_valores <- c(1:6)

tipos <- df_pkmn %>%
  filter(!is.na(tipo_1)) %>% 
  distinct(tipo_1) %>% 
  arrange(tipo_1) %>% 
  flatten_chr()

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
        label = "Gerações",
        choices = purrr::set_names(geracoes_valores, geracoes_nomes),
        selected = 1
      ),
      actionButton(inputId = "botao_geracoes", label = "Incluir")
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
        fluidRow(
          column(
            width = 6,
            box(
              title = "Quantidade por tipo",
              width = 12,
              plotOutput(outputId = "bar_plot", height = "550px")
            )
          ),
          column(
            width = 6,
            fluidRow(
              infoBoxOutput(outputId = "vg_peso", width = 8),
              imageOutput(outputId = "vg_peso_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_altura", width = 8),
              imageOutput(outputId = "vg_altura_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_hp", width = 8),
              imageOutput(outputId = "vg_hp_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_ataque", width = 8),
              imageOutput(outputId = "vg_ataque_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_defesa", width = 8),
              imageOutput(outputId = "vg_defesa_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_velocidade", width = 8),
              imageOutput(outputId = "vg_velocidade_imagem", height = "100px")
            )
          )
        )
      ),
      tabItem(
        tabName = "por_tipo",
        fluidRow(
          box(
            width = 12,
            column(
              width = 3,
              selectInput(
                inputId = "tipo_selecionado",
                label = "Tipo",
                choices = tipos,
                selected = "grama"
              )
            ),
            column(
              width = 3,
              infoBoxOutput("n_apenas_primeiro_tipo", width = 12)
            ),
            column(
              width = 3,
              infoBoxOutput("n_primeiro_tipo", width = 12)
            ),
            column(
              width = 3,
              infoBoxOutput("n_segundo_tipo", width = 12)
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Distribuição dos status",
              width = 12,
              plotOutput(outputId = "boxplot_por_tipo", height = "550px")
            )
          ),
          column(
            width = 6,
            fluidRow(
              infoBoxOutput(outputId = "por_tipo_peso", width = 8),
              imageOutput(outputId = "por_tipo_peso_imagem", height = "100px"),
              infoBoxOutput(outputId = "por_tipo_altura", width = 8),
              imageOutput(outputId = "por_tipo_altura_imagem", height = "100px"),
              infoBoxOutput(outputId = "por_tipo_hp", width = 8),
              imageOutput(outputId = "por_tipo_hp_imagem", height = "100px"),
              infoBoxOutput(outputId = "por_tipo_ataque", width = 8),
              imageOutput(outputId = "por_tipo_ataque_imagem", height = "100px"),
              infoBoxOutput(outputId = "por_tipo_defesa", width = 8),
              imageOutput(outputId = "por_tipo_defesa_imagem", height = "100px"),
              infoBoxOutput(outputId = "por_tipo_velocidade", width = 8),
              imageOutput(outputId = "por_tipo_velocidade_imagem", height = "100px")
            )
          )
        )
      ),
      tabItem(
        tabName = "comp_tipos",
        fluidRow(
          box(
            width = 12,
            column(
              width = 3,
              selectInput(
                inputId = "tipo_selecionado_comp_1",
                label = "Tipo 1",
                choices = tipos,
                selected = "água"
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "tipo_selecionado_comp_2",
                label = "Tipo 2",
                choices = tipos,
                selected = "fogo"
              )
            )
          )
        ),
        fluidRow(
          plotOutput("boxplot_comp_tipos")
        )
      )
    )
  )
)


# Shiny server ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Base filtrada
  
  df_gen <- eventReactive(input$botao_geracoes, {
    
    req(input$geracoes)
    
    df_pkmn %>% 
      filter(id_geracao %in% input$geracoes)
  }, ignoreNULL = FALSE)
  
  df_tipo <- reactive({
    
    df_ <- df_gen() %>% 
      filter(
        tipo_1 == input$tipo_selecionado | 
          tipo_2 == input$tipo_selecionado
      )
    
    if(nrow(df_) == 0) {
      req(NULL)
    }
    else {
      df_
    }
    
  })
  
  # Aba "Comparando tipos"
  
  output$boxplot_comp_tipos <- renderPlot({
    
    df_tipo_1 <- df_gen() %>% 
      filter(
        tipo_1 == input$tipo_selecionado_comp_1 | 
          tipo_2 == input$tipo_selecionado_comp_1
      ) %>% 
      mutate(tipo = input$tipo_selecionado_comp_1)
    
    df_tipo_2 <- df_gen() %>% 
      filter(
        tipo_1 == input$tipo_selecionado_comp_2 | 
          tipo_2 == input$tipo_selecionado_comp_2
      ) %>% 
      mutate(tipo = input$tipo_selecionado_comp_2)
    

    cores <- c(
      df_cores$cor_1[df_cores$tipo_1 == input$tipo_selecionado_comp_1],
      df_cores$cor_1[df_cores$tipo_1 == input$tipo_selecionado_comp_2]
    )
    
    cores <- purrr::set_names(
      cores, 
      c(input$tipo_selecionado_comp_1, input$tipo_selecionado_comp_2)
    )
    
    bind_rows(df_tipo_1, df_tipo_2) %>% 
      gather(stats, valor,
             hp, ataque, defesa, velocidade, ataque_especial, defesa_especial) %>% 
      ggplot(aes(x = stats, y = valor, fill = tipo)) +
      geom_boxplot() +
      scale_fill_manual(values = cores) +
      theme_minimal()
          
  })
  
  # Aba "Por tipo"
  
  output$n_apenas_primeiro_tipo <- renderInfoBox({
    
    n <- df_gen() %>% 
      filter(tipo_1 == input$tipo_selecionado, is.na(tipo_2)) %>% 
      nrow()
    
    infoBox(
      value = n,
      title = paste("Apenas", input$tipo_selecionado),
      icon = icon("hashtag"),
      color = "blue",
      fill = TRUE
    )
    
  })
  
  output$n_primeiro_tipo <- renderInfoBox({
    
    n <- df_gen() %>% 
      filter(tipo_1 == input$tipo_selecionado, !is.na(tipo_2)) %>% 
      nrow()
    
    infoBox(
      value = n,
      title = paste("Primeiro tipo"),
      icon = icon("hashtag"),
      color = "blue",
      fill = TRUE
    )
    
  })
  
  output$n_segundo_tipo <- renderInfoBox({
    
    n <- df_gen() %>% 
      filter(tipo_2 == input$tipo_selecionado) %>% 
      nrow()
    
    infoBox(
      value = n,
      title = paste("Segundo tipo"),
      icon = icon("hashtag"),
      color = "blue",
      fill = TRUE
    )
    
  })
  
  output$boxplot_por_tipo <- renderPlot({
    
    df_tipo() %>% 
      gather(stats, valor, 
             hp, ataque, defesa, velocidade, ataque_especial, defesa_especial) %>% 
      ggplot(aes(x = stats, y = valor)) + 
      geom_boxplot() + 
      theme_minimal()
    
  })
  
  # Peso
  output$por_tipo_peso <- criar_value_box(
    df_gen = df_tipo, 
    var = peso, 
    uni = "Kg",
    icone = "weight-hanging",
    gen = input$geracoes
  )
  output$por_tipo_peso_imagem <- renderizar_imagem(
    df_gen = df_tipo, 
    var = peso, 
    gen = input$geracoes
  )
  
  # Altura
  output$por_tipo_altura <- criar_value_box(
    df_gen = df_tipo, 
    var = altura, 
    uni = "m",
    icone = "long-arrow-alt-up",
    gen = input$geracoes
  )
  output$por_tipo_altura_imagem <- renderizar_imagem(
    df_gen = df_tipo, 
    var = altura, 
    gen = input$geracoes
  )
  
  # HP
  output$por_tipo_hp <- criar_value_box(
    df_gen = df_tipo, 
    var = hp, 
    uni = "",
    icone = "heart",
    gen = input$geracoes
  )
  output$por_tipo_hp_imagem <- renderizar_imagem(
    df_gen = df_tipo, 
    var = hp, 
    gen = input$geracoes
  )
  
  # Ataque
  output$por_tipo_ataque <- criar_value_box(
    df_gen = df_tipo, 
    var = ataque, 
    uni = "",
    icone = "hand-rock",
    gen = input$geracoes
  )
  output$por_tipo_ataque_imagem <- renderizar_imagem(
    df_gen = df_tipo, 
    var = ataque, 
    gen = input$geracoes
  )
  
  # Defesa
  output$por_tipo_defesa <- criar_value_box(
    df_gen = df_tipo, 
    var = defesa, 
    uni = "",
    icone = "shield-alt",
    gen = input$geracoes
  )
  output$por_tipo_defesa_imagem <- renderizar_imagem(
    df_gen = df_tipo, 
    var = defesa, 
    gen = input$geracoes
  )
  
  # Velocidade
  output$por_tipo_velocidade <- criar_value_box(
    df_gen = df_tipo, 
    var = velocidade, 
    uni = "",
    icone = "angle-double-right ",
    gen = input$geracoes
  )
  output$por_tipo_velocidade_imagem <- renderizar_imagem(
    df_gen = df_tipo, 
    var = velocidade, 
    gen = input$geracoes
  )
  
  
  
  # Aba "Visão geral"
  
  output$bar_plot <- renderPlot({
    
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
  
  # Peso
  output$vg_peso <- criar_value_box(
    df_gen = df_gen, 
    var = peso, 
    uni = "Kg",
    icone = "weight-hanging",
    gen = input$geracoes
  )
  output$vg_peso_imagem <- renderizar_imagem(
    df_gen = df_gen, 
    var = peso, 
    gen = input$geracoes
  )
  
  # Altura
  output$vg_altura <- criar_value_box(
    df_gen = df_gen, 
    var = altura, 
    uni = "m",
    icone = "long-arrow-alt-up",
    gen = input$geracoes
  )
  output$vg_altura_imagem <- renderizar_imagem(
    df_gen = df_gen, 
    var = altura, 
    gen = input$geracoes
  )
  
  # HP
  output$vg_hp <- criar_value_box(
    df_gen = df_gen, 
    var = hp, 
    uni = "",
    icone = "heart",
    gen = input$geracoes
  )
  output$vg_hp_imagem <- renderizar_imagem(
    df_gen = df_gen, 
    var = hp, 
    gen = input$geracoes
  )
  
  # Ataque
  output$vg_ataque <- criar_value_box(
    df_gen = df_gen, 
    var = ataque, 
    uni = "",
    icone = "hand-rock",
    gen = input$geracoes
  )
  output$vg_ataque_imagem <- renderizar_imagem(
    df_gen = df_gen, 
    var = ataque, 
    gen = input$geracoes
  )
  
  # Defesa
  output$vg_defesa <- criar_value_box(
    df_gen = df_gen, 
    var = defesa, 
    uni = "",
    icone = "shield-alt",
    gen = input$geracoes
  )
  output$vg_defesa_imagem <- renderizar_imagem(
    df_gen = df_gen, 
    var = defesa, 
    gen = input$geracoes
  )
  
  # Velocidade
  output$vg_velocidade <- criar_value_box(
    df_gen = df_gen, 
    var = velocidade, 
    uni = "",
    icone = "angle-double-right ",
    gen = input$geracoes
  )
  output$vg_velocidade_imagem <- renderizar_imagem(
    df_gen = df_gen, 
    var = velocidade, 
    gen = input$geracoes
  )
  
}


shinyApp(ui, server)
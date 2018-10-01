criar_value_box <- function(df_gen, var, titulo, icone, uni, gen) {
  
  renderInfoBox({
    
    req(gen)
    
    df <- df_gen()
    
    var <- enquo(var)
    var_name <- quo_name(var)
    
    var_max <- max(df[,var_name])
    
    filtro <- df %>% 
      filter(!!var == var_max) %>% 
      slice(1)
    
    infoBox(
      value = paste(filtro[,var_name], uni),
      subtitle = filtro$pokemon,
      title = titulo,
      icon = icon(icone),
      color = "red",
      fill = TRUE
    )
    
  })
  
}



renderizar_imagem <- function(df_gen, var, gen) {
  
  renderImage({
    
    req(gen)
    
    df <- df_gen()
    
    var <- enquo(var)
    var_name <- quo_name(var)
    
    var_max <- max(df[,var_name])
    
    filtro <- df %>% 
      filter(!!var == var_max) %>% 
      slice(1)
    
    temp <- tempdir()
    
    url_imagem <- 
      "https://raw.githubusercontent.com/phalt/pokeapi/master/data/Pokemon_XY_Sprites/"
    
    download.file(
      paste0(url_imagem, filtro$url_imagem),
      destfile = paste(temp, filtro$pokemon, sep = "/")
    )
    
    list(
      src = paste(temp, filtro$pokemon, sep = "/"),
      alt = filtro$pokemon
    )
    
  }, deleteFile = TRUE)
  
}

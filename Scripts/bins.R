

bins = function(depth, value, station_id,  min_bin, max_bin, interval, cast_filter = F) {
  
  
  
  df.initial = data.frame(depth = depth, valor = value)
  
  
  # add casts
  
  if(cast_filter == T) {
    
    
    require(ggplot2)
    require(plotly)
    
    grafico = ggplot(df.initial, aes(x = 1:nrow(df.initial), y = -depth)) +
      geom_point()
    
    
    print(ggplotly(grafico))
    
    
    n_casts = as.numeric(strsplit(x = readline("Entre com o número de casts"), split = ',')[[1]])
    
    final_cast = as.numeric(strsplit(x = readline("Entre com os pontos finais de cada cast separado por virgulas"), split = ',')[[1]])
    
    df.initial$Cast = NA
    
    
    
    if(n_casts == 1) {
      
      df.initial$Cast[1:final_cast[1]] = 1
      
      
    }
    
    
    if(n_casts == 2) {
      
      df.initial$Cast[1:final_cast[1]] = 1
      df.initial$Cast[final_cast[1]:final_cast[2]] = 2
      
      
    }
    
    if(n_casts == 3) {
      
    df.initial$Cast[1:final_cast[1]] = 1
    df.initial$Cast[final_cast[1]:final_cast[2]] = 2
    df.initial$Cast[final_cast[2]:final_cast[3]] = 3
    
    }
    
    if(n_casts == 4) {
      
      df.initial$Cast[1:final_cast[1]] = 1
      df.initial$Cast[final_cast[1]:final_cast[2]] = 2
      df.initial$Cast[final_cast[2]:final_cast[3]] = 3
      df.initial$Cast[final_cast[3]:final_cast[4]] = 4
      
    }
    
  } 
  
  if(cast_filter == F) {
    
    df.initial$Cast = 1
    n_casts = 1
  }
  
  
  bins_values = seq(from = min_bin, to = max_bin, by = interval)
  
  df.final = data.frame(bins = bins_values, cast = rep(x = 1:n_casts, times = 1, each = length(bins_values)))
  
  
  
  for(k in 1:nrow(df.final)) {
    
    filtrado = filter(df.initial, depth > df.final$bins[k] & depth < df.final$bins[k+1] & Cast == df.final$cast[k])
    
    # Remover quartis
    
    
    if(ncol(filtrado) > 2) {
      
      quartile = quantile(filtrado[,132], na.rm = T)
      
      filtrado2 = filter(filtrado, valor.X490 > quartile[2] & valor.X490 < quartile[4])
      
      
      df.final[k,3:ncol(filtrado)] = apply(filtrado[,-c(1)], MARGIN = 2, FUN = function(x) median(x, na.rm=TRUE))
      
      df.final$bins[k] = (df.final$bins[k]+df.final$bins[k+1])/2
      
      print(k)
      
    }
    
    if(ncol(filtrado) == 2) {
      
      quartile = quantile(filtrado$valor, na.rm = T)
      
      filtrado = filter(filtrado, valor > quartile[2] & valor < quartile[4])
      
      df.final[k,3:ncol(filtrado)] = median(filtrado[,-c(1)], na.rm = T)
      
      df.final$bins[k] = (df.final$bins[k]+df.final$bins[k+1])/2
      
    }
    
  }
  
  names(df.final) = c('Depth','Cast', names(value))
  
  df.final = data.frame(station_id = station_id[1], df.final)
  
  return(df.final)
  
}

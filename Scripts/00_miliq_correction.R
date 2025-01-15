## Corrigir branco


miliq_correction = function(atenuacao, absorcao, miliq_absorcao, miliq_atenuacao, path_salvar, station_name) {
  
  
  PT = list(ate = atenuacao, abs = absorcao)

  
  for(i in 1:nrow(PT$ate)) {
    
    
    PT$ate[i, 1:351] = t(PT$ate[i, 1:351]) - c(miliq_atenuacao[,1])
    PT$abs[i, 1:351] = t(PT$abs[i, 1:351]) - c(miliq_absorcao[,1])
    
    
  }
  
  
  ## Exemplo antes e depois da correcao 
  
  ymax = max(t(PT$abs[1,1:351]))
  
  matplot(y = t(PT$abs[1,1:351]), x= c(400:750), type = 'l', ylim = c(0,ymax)) 
  par(new=T)
  matplot(y = t(absorcao[1,1:351]), x= c(400:750), type = 'l' ,ylim = c(0,ymax), col = 'red') 
  
  
  ymax = max(t(PT$ate[1,1:351]))
  
  matplot(y = t(PT$ate[1,1:351]), x= c(400:750), type = 'l', ylim = c(0,ymax)) 
  par(new=T)
  matplot(y = t(atenuacao[1,1:351]), x= c(400:750), type = 'l' ,ylim = c(0,ymax), col = 'red') 
  
  
  write.csv(x = PT$ate, file = paste(path_salvar, '/', station_name, '_', 'atenuation_interpolated_miliq', '.csv', sep  = ''))
  write.csv(x = PT$abs, file = paste(path_salvar, '/', station_name, '_', 'absorption_interpolated_miliq', '.csv', sep  = ''))
  
  
  return(PT)
  
  
}


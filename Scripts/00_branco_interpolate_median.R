## Correçao do Branco

### Extrai os dados do ACS e interpola de 1 em 1 nm - também faz um merge com dados do CTD do INPE

require(dplyr)
require(data.table)
require(ggplot2)

extrai_branco = function(path_acs, path_ctd, path_salvar, station_name, filter_acs) {
  
  ## Essa funçao vai:
  # 1) Ler dados do ACS
  # 2) Ler dados do CTD ()
  
  
  # Lê o arquivo do ACS quando sai do WETVIEW/COMPASSS
  
  dados <- data.frame(read.delim(path_acs, skip = 96, 
                                 header=TRUE))
  
  # Re-organiza os dados
  
  dados2 = dados
  names(dados2) <- dados[1,]
  dados2 <- dados[-1,]
  dados2$Depth = NULL

  # Selecao dos dados de Absorcao
  abs <- dados2 %>% select(contains("a")) %>% select(-contains('Salinity'))
  abs = data.frame(t(abs))
  
  # Selecao dos dados de atenuacao 
  ate <- dados2 %>% select(contains('c')) %>% select(-contains(c('iTemp.C.','Conduct')))
  ate = data.frame(t(ate))
  
  depth = dados2$Depth
  
  
  # Pegando os comprimentos de onda 
  
  for(i in 1:ncol(abs)) {
    
    abs[,i] <- as.numeric(as.character(abs[,i]))
    ate[,i] <- as.numeric(as.character(ate[,i]))
    
  }
  

  
  # Cria um data.frame novo para interpolar os resultados de 1 em 1 nanometro
  
  abs.n = data.frame(Wave = c(400:750))
  ate.n = data.frame(Wave = c(400:750))
  
  for(i in 1:ncol(abs)) {
    
    abs.n[,i+1] = approx(x =  gsub(x = rownames(abs), pattern = 'a', replacement = '') %>% as.numeric(), y = abs[,i], xout = c(400:750), rule = 2)$y
    ate.n[,i+1] = approx(x =  gsub(x = rownames(ate), pattern = 'c', replacement = '') %>% as.numeric(), y = ate[,i], xout = c(400:750), rule = 2)$y
    
    
  }
  
  # organizando para salvar os dados
  
  ate.n.t = t(ate.n[,-1]) %>% data.frame()
  abs.n.t = t(abs.n[,-1]) %>% data.frame()
  
  names(ate.n.t) = paste('X', ate.n$Wave, sep = '')
  names(abs.n.t) = paste('X', abs.n$Wave, sep = '')
  
  ate.n.t$Depth = depth
  abs.n.t$Depth = depth
  
  t = list(ate.n.t, abs.n.t)
  names(t) <- c('ate', 'abs')
  
  branco.ate =  data.frame(apply(t$ate, median, MARGIN = 2))
  branco.abs =  data.frame(apply(t$abs, median, MARGIN = 2))
  
  
  write.csv(x = branco.ate, file = paste(path_salvar, '/', station_name,'_', 'atenuation_interpolated', '.csv', sep  = ''))
  write.csv(x = branco.abs, file = paste(path_salvar, '/', station_name,'_', 'absorption_interpolated', '.csv', sep  = ''))
  
  t = list(branco.ate, branco.abs)
  names(t) <- c('ate', 'abs')
  
  
  return(t)
  
}


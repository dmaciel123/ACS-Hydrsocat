### Extrai os dados do ACS e interpola de 1 em 1 nm - também faz um merge com dados do CTD do INPE

require(dplyr)
require(data.table)
require(ggplot2)

extrai_acs = function(path_acs, path_ctd, merge_ctd = T, path_salvar, station_name, filter_acs) {
  
  ## Essa funçao vai:
  # 1) Ler dados do ACS
  # 2) Ler dados do CTD ()
  
  
  dir.create(path_salvar)
  
  # Lê o arquivo do ACS quando sai do WETVIEW/COMPASSS
  
  dados <- data.frame(read.delim(path_acs, skip = 96, 
                                 header=TRUE))
  
  # Re-organiza os dados
  
  dados2 = dados
  names(dados2) <- dados[1,]
  dados2 <- dados[-1,]
  dados2$Depth = NULL
  dados_ctd = fread(path_ctd, fill= T)
  
  names(dados_ctd)[c(1,2)] = c("Time.ms.", "Depth")
  
  
  dados_ctd = filter(dados_ctd, `Temp(C)` > 0 & `Temp(C)`  < 40)
  
  if(merge_ctd == T) {
    
    ## plot rapido ctd
    
    ctd.melt = melt(dados_ctd, c('Depth', 'Time.ms.'))
    
    plts_ctd = ggplot(ctd.melt, aes(y = -Depth, x = value)) + 
      geom_point(aes(col = variable)) +
      theme_minimal() +
      facet_wrap(~variable, scale = 'free') +
      theme(panel.grid.major = element_line(colour = "#d3d3d3"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            text=element_text(family = "Tahoma"),
            axis.title = element_text(face="bold", size = 10),
            axis.text.x = element_text(colour="black", size = 10),
            axis.text.y = element_text(colour="black", size = 10),
            axis.line = element_line(size=2, colour = "black"),
            strip.text = element_text(size=10)) +
      theme(plot.margin = unit(c(4,4,4,4), "lines"))
    
    
    write.csv(x = dados_ctd, file = paste(path_salvar, '/', station_name,'_' ,'CTD_tabela', '.csv', sep  = ''))
    ggsave(paste(path_salvar, '/',station_name, '_', 'CTD', '.jpeg', sep  = ''), plts_ctd, 
           width = 15, height = 10, bg = 'white')
    
    
    # Merge com o CTD do INPE a profundidade do CTD nos plots pra gente ver
    
    dados2 = merge(dados2, dados_ctd, by = 'Time.ms.')
    
    
    
    
  }
  
  # Selecao dos dados de Absorcao
  abs <- dados2 %>% select(contains("a")) %>% select(-contains('Salinity'))
  abs = data.frame(t(abs))
  
  # Selecao dos dados de atenuacao 
  ate <- dados2 %>% select(contains('c')) %>% select(-contains(c('iTemp.C.','Conduct')))
  ate = data.frame(t(ate))
  
  depth = dados2$Depth
  salinity = dados2$`Sal(PSU)`
  temperature = dados2$`Temp(C)`
  
  
  # Pegando os comprimentos de onda 
  
  for(i in 1:ncol(abs)) {
    
    abs[,i] <- as.numeric(as.character(abs[,i]))
    ate[,i] <- as.numeric(as.character(ate[,i]))
    
  }
  
  
  # Se, filtrar ACS == Verdadeiro, vamos escolher quais medidas selecionar baseados no tempo 
  
  if(filter_acs == T) {
    
    par(mfrow=c(1,2))
    plot(t(abs[30,]))
    plot(t(ate[30,]))
    
    excluir = as.numeric(strsplit(x = readline("Entre com os pontos para selecionar ABS"), split = ',')[[1]])
    abs = abs[,excluir[1]:excluir[2]]
    ate = ate[,excluir[1]:excluir[2]]
    
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
  
  # Add depth, salinity and temperature
  ate.n.t$Depth = depth
  ate.n.t$temperature = temperature
  ate.n.t$salinity = salinity
  
  abs.n.t$Depth = depth
  abs.n.t$temperature = temperature
  abs.n.t$salinity = salinity
  
  t = list(ate.n.t, abs.n.t)
  names(t) <- c('ate', 'abs')
  
  write.csv(x = t$ate, file = paste(path_salvar, '/', station_name, '_', 'atenuation_interpolated', '.csv', sep  = ''))
  write.csv(x = t$abs, file = paste(path_salvar, '/', station_name, '_', 'absorption_interpolated', '.csv', sep  = ''))
  
  return(t)
  
}


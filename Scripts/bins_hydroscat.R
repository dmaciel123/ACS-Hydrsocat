

bins = function(depth, value, station_id,  min_bin, max_bin, interval) {
  
  
  df.final = data.frame(bins = seq(from = min_bin, to = max_bin, by = interval))
  
  df.initial = data.frame(depth = depth, valor = value)
  
  for(k in 1:nrow(df.final)) {
    
    filtrado = filter(df.initial, depth > df.final$bins[k] & depth < df.final$bins[k+1])
    
    # Remover quartis
    
    
    if(ncol(filtrado) > 2) {
      
      quartile = quantile(filtrado[,4], na.rm = T)
      
      filtrado2 = filter(filtrado, valor.bb510 > quartile[2] & valor.bb510 < quartile[4])
      
      
      df.final[k,2:ncol(filtrado)] = apply(filtrado[,-1], MARGIN = 2, FUN = function(x) median(x, na.rm=TRUE))
      df.final$bins[k] = (df.final$bins[k]+df.final$bins[k+1])/2
      
      print(k)
      
    }
    
    if(ncol(filtrado) == 2) {
      
      quartile = quantile(filtrado$valor, na.rm = T)
      
      filtrado = filter(filtrado, valor > quartile[2] & valor < quartile[4])
      
      df.final[k,2:ncol(filtrado)] = median(filtrado[,-1], na.rm = T)
      df.final$bins[k] = (df.final$bins[k]+df.final$bins[k+1])/2
      
      
    }
    
  }
  
  names(df.final) = c('Depth', names(value))
  
  df.final = data.frame(station_id = station_id[1], df.final)
  
  return(df.final)
  
}



hh_process = function(path, campanha, nomes_novos, profile = T, 
                      min_bin = 0, 
                      max_bin = 50,
                      interval = 1) {
  
  
  source("Scripts/bins_hydroscat.R")
  
  data = list.files(path = path, pattern = '.dat', full.names = T)
  
  path_save = paste('Outputs/', campanha, 'Hydroscat', sep = '/')
  
  
  for(i in 1:length(data)) {
    
    
    if(i > 1) {
      
      pt = data[i] %>% strsplit('/') %>% last() %>% gsub(pattern = '.dat', replacement = '') %>% last()
      
      nome_pt = nomes_novos[i]
      
      data_unico = fread(data[i], skip = 20,header = F) %>% mutate(ponto = nome_pt)
      
      data_unico$V35 = NULL
      
      dados_juntos = rbind(data_unico, dados_juntos)
      
      
    }
    
    if(i == 1) {
      
      pt = data[i] %>% strsplit('/') %>% last() %>% gsub(pattern = '.dat', replacement = '') %>% last()
      
      
      dados_juntos = fread(data[i], skip = 20,header = F) %>% mutate(ponto = nomes_novos[i])
      dados_juntos$V35 = NULL
      
    }
    
  }
  
  
  names = read.table(data[i], skip = 35, header = F, nrows = 1, sep = ',') %>% t()
  
  
  names(dados_juntos) = c(names, 'Ponto')
  
  
  ### Arrumando os dados que precisa
  
  if(profile == T) {
    
    
    bbp_sel = select(dados_juntos, c('Time', 'Depth', 'Ponto',
                                     'bb420', 'bb442',
                                     'bb470','bb510', 'bb590', 'bb700')) %>% filter(Depth > 1)
  }
  
  if(profile == F) {
    
    
    bbp_sel = select(dados_juntos, c('Time', 'Depth', 'Ponto',
                                     'bb420', 'bb442',
                                     'bb470','bb510', 'bb590', 'bb700')) 
  }
  
  
  matplot(t(bbp_sel[20,-c(1:2)]), type = 'l', ylim = c(0,0.1))
  
  final = bbp_sel
  
  final$station_id = paste(bbp_sel$Ponto, sep = '')
  
  ids = unique(final$station_id)
  
  for(i in ids) {
    
    pre_bin = filter(final, station_id == i) 
    
    
    
    
    final.bin = bins(depth = pre_bin$Depth, value = pre_bin[,4:9], station_id = pre_bin$station_id,
                     min_bin = min_bin, max_bin = max_bin, interval = interval)
    
    
    melted = melt(final.bin, c('station_id', 'Depth'))
    
    grafico = ggplot(melted, aes(y = -Depth, x = value)) +
      geom_point(aes(color = variable), size = 0.4) + 
      facet_wrap(~variable) +
      scale_x_continuous(limits = c(0,max(melted$value))) +
      theme_bw()
    
    ggsave(paste(path_save, '/', i, '_bbp_bin.jpeg', sep = ''), grafico)
    
    
    melted$variable = gsub(x = melted$variable, pattern = 'bb', replacement = '') %>% as.numeric()
    
    ggplot(melted, aes(x = variable, y = value, color = Depth, group = Depth)) +
      geom_line() +
      labs(title = paste('bbp - Ponto ', i),
           x = "Comprimento de Onda",
           y = "Valores",
           color = "Depth (m)") +
      theme_minimal() +
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
    
    write.csv(final.bin, paste(path_save, '/', i, '_bbp_bin.csv', sep = ''))
    
    
    
    if(exists('resultado_final') == F) {
      
      resultado_final = final.bin
      
    } else {
      
      resultado_final = rbind(resultado_final, final.bin)
      
    }
    
  }
  
  write.csv(na.omit(resultado_final), paste(path_save, 'final_bbp_binado.csv', sep = '/'))
  
}




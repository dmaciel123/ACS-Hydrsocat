source('Scripts/bins.R')





acs_run = function(caminho_miliq, caminho_acs, ymax, temperature, temperature_ref , salinity, salinity_ref, 
                   filter_acs = F, tscor = F, scat_cor = F,amostra, path_ctd, path_salvar, 
                   xmax, ymin, scat_cor_715 = F, prop_cor =F, kirk = F, CFS = 0.18, max_bin = 50) {
  
  ########### O SCRIPT COMECA AQUI !!!!!!!!!! #### ACIMA SAO AS FUNCOES NECESSARIAS PARA ELE RODAR !
  
  
  #Extrai a mediana da MILIQ (Amostra tres aqui no caso)
  
  print('Extract miliq reference')
  branco = extrai_acs(diretorio_arquivo = caminho_miliq, 
                      temperature = temperature_ref, branco = TRUE,
                      salinity = salinity_ref, path_ctd = path_ctd,
                      filter_acs = filter_acs,path_salvar = path_salvar, amostra = amostra,
                      tscor = TRUE, 
                      kirk = F, 
                      CFS = F,
                      scat_cor_715=F, 
                      prop_cor = F)
  print('Miliq reference - sucesss')
  
  ## Media do branco 
  
  branco.ate =  data.frame(apply(branco$ate, median, MARGIN = 2))
  branco.abs =  data.frame(apply(branco$abs, median, MARGIN = 2))
  

  ####### Abrindo os arquviso do ACS dos cultivos (existem 7 arquivos)
  
  print('Extract ACS')
  
  PT = extrai_acs(diretorio_arquivo = caminho_acs, 
                  temperature = temperature, 
                  salinity = salinity, path_salvar = path_salvar, amostra = amostra,
                  kirk = kirk, path_ctd=path_ctd,
                  CFS = CFS,
                  filter_acs = filter_acs, 
                  tscor = T, 
                  scat_cor_715=scat_cor_715, 
                  prop_cor = prop_cor)
  
  print('ACS sucess!')
  
  plt_profile = plot.acs(depth = PT$abs$Depth,xmax = xmax, ymin = ymin,
                         absorcao = PT$abs$X443, 
                         atenuacao = PT$ate$X443, amostra = amostra)
  
  ggsave(paste(path_salvar, '/profile_sem_bin', amostra, '.jpeg', sep  = ''), plt_profile, bg = 'white')
  
  
  
  for(i in 1:nrow(PT$ate)) {
    
    
    PT$ate[i, 1:351] = t(PT$ate[i, 1:351]) - c(branco.ate[,1])
    PT$abs[i, 1:351] = t(PT$abs[i, 1:351]) - c(branco.abs[,1])
    
    
  }

  
  
  
  ## parte de binar
  
  abs.binada = bins(depth = PT$abs$Depth, value = PT$abs[,-352], station_id = amostra, min_bin = 0, max_bin = max_bin, interval = 0.5)
  ate.binada = bins(depth = PT$ate$Depth, value = PT$ate[,-352], station_id = amostra, min_bin = 0, max_bin = max_bin, interval = 0.5)
  
  plt_profile = plot.acs(depth = abs.binada$Depth,xmax = xmax, ymin = ymin,
                         absorcao = abs.binada$X560, 
                         atenuacao = ate.binada$X560, amostra = amostra)
  
  ggsave(paste(path_salvar, '/profile_bin', amostra, '.jpeg', sep  = ''), plt_profile, bg = 'white')
  
  print(plt_profile)
  
  abs.melted = melt(abs.binada, 'Depth')
  ate.melted = melt(ate.binada, 'Depth')
  
  abs.melted$variable = gsub(x = abs.melted$variable, pattern = 'X', replacement = '') %>% as.numeric()
  ate.melted$variable = gsub(x = ate.melted$variable, pattern = 'X', replacement = '') %>% as.numeric()
  
  plts = data.frame(Wave = abs.melted$variable, Depth = abs.melted$Depth, abs = abs.melted$value, ate = ate.melted$value) %>% na.omit()
  plts$Wave_Depth = paste(plts$Wave, plts$Depth)
  
  plts$abs = as.numeric(plts$abs)
  plts$ate = as.numeric(plts$ate)
  
 
  
  
  print("plota absorcao spectral")
  abs_plt = ggplot(plts, aes(x = Wave, y = abs, color = Depth, group = Depth)) +
    geom_line() +
    labs(title = paste('Coeficiente de Absorcao - Ponto ', amostra),
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
  
  ggsave(paste(path_salvar, '/absorcao', amostra, '.jpeg', sep  = ''), abs_plt, bg = 'white')
  
  print("plota atenuacao spectral")
  
  ate_plt = ggplot(plts, aes(x = Wave, y = ate, color = Depth, group = Depth)) +
    geom_line() +
    labs(title = paste('Coeficiente de Atenuacao - Ponto ', amostra),
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
  
  ggsave(paste(path_salvar, '/atenuacao', amostra, '.jpeg', sep  = ''), ate_plt, bg = 'white')
  
  
  ## Salvando o resultado em txt
  
  abs.binada
  ate.binada
  
  write.csv(abs.binada, paste(path_salvar, 'abs_binada' ,amostra, '.csv' ,sep = ''))
  write.csv(ate.binada, paste(path_salvar, 'ate_binada' ,amostra, '.csv' ,sep = ''))
  
  return(list(abs = abs.binada, ate = ate.binada))
  
  print('FINALIZADO COM SUCESSO')
  
  
}


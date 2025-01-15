plots_acs = function(abs.binada, ate.binada, max_abs, max_ate, station_name, path_salvar) {
  
  
  require(ggplot2)
  
  # perfis abs e ate
  
  abs.plot = select(abs.binada, contains(c('Depth', 'Cast', 'X'))) %>% select(-contains('.1')) %>% melt(c('Depth', 'Cast')) 
  abs.plot$variable = gsub(abs.plot$variable, pattern = 'X', replacement = '') %>% as.numeric()
  
  
  ate.plot = select(ate.binada, contains(c('Depth', 'Cast', 'X'))) %>% select(-contains('.1')) %>% melt(c('Depth', 'Cast')) 
  ate.plot$variable = gsub(ate.plot$variable, pattern = 'X', replacement = '') %>% as.numeric()
  
  dados = data.frame(Wv = abs.plot$variable,Cast = abs.plot$Cast, depth = abs.plot$Depth, abs = abs.plot$value, ate = ate.plot$value)
  dados = filter(dados, abs < max_abs | ate < max_ate)
  
  dados = na.omit(dados) %>% melt(c('Wv', 'depth', 'Cast'))
  
  dados$Cast_depth = paste(dados$Cast, dados$depth)
  dados$variable = paste(dados$variable, 'Cast', dados$Cast)
  
  
  espectral = ggplot(dados, aes(y = value, x = Wv, color = depth, group = Cast_depth)) +
    geom_line() +
    facet_wrap(~variable, scale = 'free') +
    labs(title=station_name) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          #plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
          axis.title = element_text(face="bold", size = 15),
          axis.text.x = element_text(colour="black", size = 15),
          axis.text.y = element_text(colour="black", size = 15),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=15, face='bold')) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) 
  
  
  # perfil abs e ate
  
  perfil_plt = filter(dados, Wv == 490)
  
  
  perfli = ggplot(perfil_plt, aes(y = -depth, x = value, color = Cast)) +
    geom_point() +
    facet_wrap(~variable, scale = 'fixed')  +
    scale_y_continuous(limits = c(NA, 0)) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          #plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
          axis.title = element_text(face="bold", size = 15),
          axis.text.x = element_text(colour="black", size = 15),
          axis.text.y = element_text(colour="black", size = 15),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=15, face='bold')) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) 
  
  ggsave(plot = espectral, width = 10, height = 7, filename =  paste(path_salvar, '/', station_name,'_', 'plot_spectral', '.jpeg', sep  = '') )
  ggsave(plot = perfli, width = 10, height = 7, filename =  paste(path_salvar, '/', station_name,'_', 'plot_perfil', '.jpeg', sep  = '') )
  
  
  
  
  
}


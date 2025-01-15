source('Scripts/bins.R')

extrai_acs = function(diretorio_arquivo, wave.a, wave.t,scat_cor_715 = F,prop_cor = F, path_ctd, 
                      path_salvar, amostra,
                      abs_ou_atenuacao, temperature, branco = FALSE, salinity, filter_acs = F, tscor = T, 
                      kirk = F, CFS = 0.18) {
  
  # Lê o arquivo do ACS quando sai do WETVIEW/COMPASSS
  dados <- data.frame(read.delim(diretorio_arquivo, skip = 96, 
                                 header=TRUE))
  
  
  # Re-organiza os dados
  
  dados2 = dados
  names(dados2) <- dados[1,]
  dados2 <- dados[-1,]
  dados2$Depth = NULL
  dados_ctd = fread(path_ctd, fill= T)
  
  names(dados_ctd)[c(1,2)] = c("Time.ms.", "Depth")
  
  
  dados_ctd = filter(dados_ctd, `Temp(C)` > 0 & `Temp(C)`  < 40)
  
  if(branco == FALSE) {
    
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
    
    
    write.csv(x = dados_ctd, file = paste(path_salvar, '/CTD_tabela', amostra, '.csv', sep  = ''))
    ggsave(paste(path_salvar, '/CTD', amostra, '.jpeg', sep  = ''), plts_ctd, bg = 'white')
    
      
    
  
    # Adiciona a profundidade do CTD nso plots pra gente ver
    
    dados2 = merge(dados2, dados_ctd, by = 'Time.ms.')
    
  }
  
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
  
  # Correçao de temperatura e salinidade baseado no manual do ACS
  
  if(tscor == TRUE) {
    
    # Temperature and salinity correction
    
    # Coeficientes para correcao de temperatura e salinidade tabelados a partir do manual 
    # do ACS
    ts = fread('Data/calibration/temp_salinity_cor.csv')
    
    # Cria uma tabela com Wave entre 400-750 e interpola os valores dos coeficientes de 1 
    # em 1 nanmoetro
    
    tscor = data.frame(Wave = c(400:750))
    
    tscor$alfa_t = approx(x = ts$Wave, y = ts$alfa_t, xout = c(400:750))$y
    tscor$delta_alfa_T = approx(x = ts$Wave, y = ts$delta_alfa_T, xout = c(400:750))$y
    tscor$alfa_s_c = approx(x = ts$Wave, y = ts$alfa_s_c, xout = c(400:750))$y
    tscor$delta_afla_s_c = approx(x = ts$Wave, y = ts$delta_afla_s_c, xout = c(400:750))$y
    tscor$alfa_s_a = approx(x = ts$Wave, y = ts$alfa_s_a, xout = c(400:750))$y
    tscor$delta_alfa_s_a = approx(x = ts$Wave, y = ts$delta_alfa_s_a, xout = c(400:750))$y
    
    # Temperature correction
    
    temperature = temperature
    temperature_ref = 19.2
    salinnity = salinity
    
    # Correcao de temperatura e salinidade
    abs.n[,-1]    = abs.n[,-1]-(tscor$alfa_t*(temperature-temperature_ref)+tscor$alfa_s_a*salinity)
    ate.n[,-1]    = ate.n[,-1]-(tscor$alfa_t*(temperature-temperature_ref)+tscor$alfa_s_c*salinity)
    
    # Se valor < 0, igualar a zero
    
    #abs.n[abs.n < 0] <- 0
    #ate.n[ate.n < 0] <- 0
    
    
  }
  
  
  
  if(scat_cor_715 == TRUE) {
    
    # Abosprtion tube scattering correction based on a fixed offset at 715nm
    # (Section 5.6 ACS Manual) 
    
    a_ref = filter(abs.n, Wave == 715) %>% summarise_each(funs = 'median')
    
    a_ref.m = matrix(ncol = ncol(a_ref), nrow = nrow(abs.n)) %>% data.frame()
    a_ref.m[1:nrow(a_ref.m), ] = a_ref
    
    
    abs.n[,-1] = abs.n[,-1]-a_ref.m[,-1]
    
    
  }
  
  
  if(prop_cor == TRUE) {
    
    # Scattering Proportional correction (Section 5.6 ACS Manual) 
    
    a_ref = filter(abs.n, Wave == 715) %>% summarise_each(funs = 'median')
    c_ref = filter(ate.n, Wave == 715) %>% summarise_each(funs = 'median')
    
    a_ref.m = matrix(ncol = ncol(a_ref), nrow = nrow(abs.n)) %>% data.frame()
    a_ref.m[1:nrow(a_ref.m), ] = a_ref
    
    c_ref.m = matrix(ncol = ncol(c_ref), nrow = nrow(ate.n)) %>% data.frame()
    c_ref.m[1:nrow(c_ref.m), ] = c_ref
    
    
    abs.n[,-1] = abs.n[,-1]-a_ref.m[,-1]/(c_ref.m[,-1]-a_ref.m[,-1])*(ate.n[,-1]-abs.n[,-1])
    
    
  }
  
  
  if(kirk == TRUE) {
    
    # Kirk Scattering correction (Section 5.6 ACS Manual) 
    # See Sander de Carvalho et al. (2015) for more information
    
    abs.n[,-1] = abs.n[,-1]-CFS*(ate.n[,-1]-abs.n[,-1])
    
    
  }
  
  # calculando os valores medianos e desvio padrao 
  
  ate.n.t = t(ate.n[,-1]) %>% data.frame()
  abs.n.t = t(abs.n[,-1]) %>% data.frame()
  
  names(ate.n.t) = paste('X', ate.n$Wave, sep = '')
  names(abs.n.t) = paste('X', abs.n$Wave, sep = '')
  
  ate.n.t$Depth = depth
  abs.n.t$Depth = depth
  
    t = list(ate.n.t, abs.n.t)
  names(t) <- c('ate', 'abs')
  
  
  
  
  return(t)
  
}

#Plota o branco (Ou qualquer outro dado que escolher)
plot.acs = function(depth, absorcao, atenuacao, amostra, xmax, ymin) {
  
  plt = data.frame(depth, absorcao, atenuacao)
  
  size_axis = 20
  a = ggplot(data = plt, aes(y = -depth)) +
    # Adicionamos a linha e as barras de erro para absorção
    geom_point(aes(x = absorcao, color = "Absorção")) +

    # Adicionamos a linha e as barras de erro para espalhamento

    # Adicionamos a linha e as barras de erro para atenuação
    geom_point(aes(x = atenuacao, color = "Atenuação")) +
    scale_y_continuous(limits = c(ymin, 0)) +
    scale_x_continuous(limits = c(0, xmax)) +
    
    # Melhoramos a aparência do gráfico
    labs(title = amostra,
         x = "Absorcao/Atenuacao (m-1)",
         y = "Valores",
         color = "Legenda") +
    theme_minimal() +
    scale_color_manual(values = c("Absorção" = "blue", "Espalhamento" = "green", "Atenuação" = "red")) +
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines"))
  
  print(a)
  
}


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


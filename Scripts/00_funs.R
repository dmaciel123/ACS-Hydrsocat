extrai_acs = function(diretorio_arquivo, wave.a, wave.t,scat_cor_715 = F,prop_cor = F, 
                      abs_ou_atenuacao, temperature, salinity, filter_acs = F, tscor = T, kirk = F, CFS = 0.18) {
  
  # Lê o arquivo do ACS quando sai do WETVIEW/COMPASSS
  dados <- data.frame(read.delim(diretorio_arquivo, skip = 96, 
                                 header=TRUE))
 
  # Re-organiza os dados
  
  dados2 = dados
  names(dados2) <- dados[1,]
  dados2 <- dados[-1,]
  
  # Selecao dos dados de Absorcao
  abs <- dados2 %>% select(contains("a")) %>% select(-contains('Salinity'))
  abs = data.frame(t(abs))
  
  # Selecao dos dados de atenuacao 
  ate <- dados2 %>% select(contains('c')) %>% select(-contains(c('iTemp.C.','Conduct')))
  ate = data.frame(t(ate))
  
  depth = dados$Depth
  
  # Pegando os comprimentos de onda 
  
  for(i in 1:ncol(abs)) {
    
    abs[,i] <- as.numeric(as.character(abs[,i]))
    ate[,i] <- as.numeric(as.character(ate[,i]))
    
  }
  

  # Se, filtrar ACS == Verdadeiro, vamos escolher quais medidas selecionar baseados no tempo 
  
  if(filter_acs == F) {
    
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
  
  abs.t = data.frame(apply(abs.n[,-1], median, MARGIN = 1))
  ate.t = data.frame(apply(ate.n[,-1], median, MARGIN = 1))
  abs.sd = data.frame(apply(abs.n[,-1], sd, MARGIN = 1))
  ate.sd = data.frame(apply(ate.n[,-1], sd, MARGIN = 1))
  
  abs.t$Wave  = 400:750
  ate.t$Wave  = 400:750
  abs.sd$Wave = 400:750
  ate.sd$Wave = 400:750
  
  
  t = data.frame(abs.t$Wave, abs.t[,1], ate.t[,1], abs.sd[,1], ate.sd[,1])
  names(t) <- c("Wave", "ABS", "ATE", "ABS_SD", "ATE_SD")
  
  
  
  
  return(t)
  
}

#Plota o branco (Ou qualquer outro dado que escolher)
plot.acs = function(absorcao, atenuacao, espalhamento, absorcao.sd,espalhamento.sd,  atenuacao.sd, wavelength, amostra, ymax) {
  
  plt = data.frame(wavelength, absorcao, espalhamento, atenuacao, absorcao.sd, espalhamento.sd, atenuacao.sd)
  
  size_axis = 20
  a = ggplot(data = plt, aes(x = wavelength)) +
    # Adicionamos a linha e as barras de erro para absorção
    geom_line(aes(y = absorcao, color = "Absorção")) +
    geom_errorbar(aes(ymin = absorcao - absorcao.sd, ymax = absorcao + absorcao.sd, color = "Absorção"), 
                  alpha = 0.2, width = 3) +
    
    # Adicionamos a linha e as barras de erro para espalhamento
    geom_line(aes(y = espalhamento, color = "Espalhamento")) +
    geom_errorbar(aes(ymin = espalhamento - espalhamento.sd, ymax = espalhamento + espalhamento.sd, color = "Espalhamento"),
                  width = 3, alpha = 0.2) +
    
    # Adicionamos a linha e as barras de erro para atenuação
    geom_line(aes(y = atenuacao, color = "Atenuação")) +
    geom_errorbar(aes(ymin = atenuacao - atenuacao.sd, ymax = atenuacao + atenuacao.sd, color = "Atenuação"), 
                  width = 3, alpha = 0.2) +
    scale_y_continuous(limits = c(0, ymax)) +
    
    # Melhoramos a aparência do gráfico
    labs(title = amostra,
         x = "Comprimento de Onda",
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
                   filter_acs = F, tscor = F, scat_cor = F, scat_cor_715 = F, prop_cor =F, kirk = F, CFS = 0.18) {
  
  ########### O SCRIPT COMECA AQUI !!!!!!!!!! #### ACIMA SAO AS FUNCOES NECESSARIAS PARA ELE RODAR !
  
  
  #Extrai a mediana da MILIQ (Amostra tres aqui no caso)
  
  branco = extrai_acs(diretorio_arquivo = caminho_miliq, 
                      temperature = temperature_ref, 
                      salinity = salinity_ref, 
                      filter_acs = filter_acs,
                      tscor = TRUE, 
                      kirk = F, 
                      CFS = F,
                      scat_cor_715=F, 
                      prop_cor = F)
  
  
  ####### Abrindo os arquviso do ACS dos cultivos (existem 7 arquivos)
  
  PT = extrai_acs(diretorio_arquivo = caminho_acs, 
                  temperature = temperature, 
                  salinity = salinity,
                  kirk = kirk, 
                  CFS = CFS,
                  filter_acs = filter_acs, 
                  tscor = T, 
                  scat_cor_715=scat_cor_715, 
                  prop_cor = prop_cor)
  
  
  PT$ABS.MILIQ.COR = PT$ABS - branco$ABS
  PT$ATE.MILIQ.COR = PT$ATE - branco$ATE
  PT$ESP.MILIQ.COR = PT$ATE.MILIQ.COR - PT$ABS.MILIQ.COR
  
  PT$ABS.MILIQ.COR.SD = PT$ABS_SD - branco$ABS_SD
  PT$ATE.MILIQ.COR.SD = PT$ATE_SD - branco$ATE_SD
  PT$ESP.MILIQ.COR.SD = PT$ATE.MILIQ.COR.SD - PT$ABS.MILIQ.COR.SD
  
  
  res = plot.acs(wavelength = PT$Wave,ymax=ymax,
                 absorcao = PT$ABS.MILIQ.COR, 
                 atenuacao = PT$ATE.MILIQ.COR, 
                 espalhamento = PT$ESP.MILIQ.COR, 
                 absorcao.sd = PT$ABS.MILIQ.COR.SD, 
                 atenuacao.sd = PT$ATE.MILIQ.COR.SD, 
                 espalhamento.sd = PT$ESP.MILIQ.COR.SD, amostra = caminho_acs)
  
  write.csv(PT, paste(caminho_acs, 'TESTE_abs_ate_esp', '.csv' ,sep = ''))
  
  return(res)
  
  
}


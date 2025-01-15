


  
  
ts_correction = function(abs, ate, path_salvar, station_name, temperatura, salinidade) {  
  
  # 1) Correçao de temperatura e salinidade baseado no manual do ACS
  
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
  
  temperature = temperatura
  temperature_ref = 19.2
  salinity = salinidade
  
  # Correcao de temperatura e salinidade
  
  
  for(i in 1:nrow(abs)) {
    
    abs[i,-c(1:3, 355:357)]    = abs[i,-c(1:3, 355:357)]-(tscor$alfa_t*(temperature[i]-temperature_ref)+tscor$alfa_s_a*salinity[i])
    ate[i,-c(1:3, 355:357)]  = ate[i,-c(1:3, 355:357)]-(tscor$alfa_t*(temperature[i]-temperature_ref)+tscor$alfa_s_c*salinity[i])
  
  }
  
  write.csv(x = ate, file = paste(path_salvar, '/', station_name, '_', 'atenuation_interpolated_miliq_binada_tsCor', '.csv', sep  = ''))
  write.csv(x = abs, file = paste(path_salvar, '/', station_name, '_', 'absorption_interpolated_miliq_binada_tsCor', '.csv', sep  = ''))
  
  return(list(abs_ts = abs, ate_ts = ate))
  
}





scat_715_correction = function(abs,path_salvar, station_name) {  
  
  abs_715 = abs$X715
  

  for(i in 3:353) {
    
    
    abs[,i] = abs[,i] - abs_715 
    
    
  }
  
  write.csv(x = abs, file = paste(path_salvar, '/', station_name, '_', 'absorption_interpolated_miliq_binada_tsCor_scate715cor', '.csv', sep  = ''))
  
  
  return(abs)
  
}


prop_correction = function(abs, ate, wv_ref,path_salvar, station_name) {
  
  a_ref = select(abs, contains(as.character(wv_ref))) 
  c_ref = select(ate, contains(as.character(wv_ref))) 
  
  
  
  for(i in 3:353) {
    
    
    abs[,i] = abs[,i]-a_ref[,1]/(c_ref[,1]-a_ref[,1])*(abs[,i]-ate[,i])
    
    
  }
  
  write.csv(x = abs, file = paste(path_salvar, '/', station_name, '_', 'absorption_interpolated_miliq_binada_tsCor_prop', '.csv', sep  = ''))
  
  return(abs)
  
  
}

kirk_correction = function(abs,ate, CFS,path_salvar, station_name) {
  
  
  
  for(i in 3:353) {
    
    
    abs[,i] = abs[,i]-CFS*(ate[,i]-abs[,i])
    
    
  }
  
  write.csv(x = abs, file = paste(path_salvar, '/', station_name, '_', 'absorption_interpolated_miliq_binada_tsCor_kirk', '.csv', sep  = ''))
  
  return(abs)
  
  
}

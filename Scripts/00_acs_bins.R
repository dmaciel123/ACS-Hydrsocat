source('Scripts/bins.R')


acs_bins = function(atenuacao, absorcao, min_bin, max_bin, interval, station_name, path_salvar, cast_filter = F) {
  
  
  
  abs.binada = bins(depth = absorcao$Depth, value =   absorcao, station_id = station_name, min_bin = min_bin, max_bin = max_bin, interval = interval, cast_filter = cast_filter)
  ate.binada = bins(depth = atenuacao$Depth, value = atenuacao, station_id = station_name, min_bin = min_bin, max_bin = max_bin, interval = interval, cast_filter = cast_filter)
  
  lista = list(abs = abs.binada, ate = ate.binada)
  
  
  write.csv(x = lista$ate, file = paste(path_salvar, '/', station_name, '_', 'atenuation_interpolated_miliq_binada', '.csv', sep  = ''))
  write.csv(x = lista$abs, file = paste(path_salvar, '/', station_name, '_', 'absorption_interpolated_miliq_binada', '.csv', sep  = ''))
  
  return(lista)
  
  
}


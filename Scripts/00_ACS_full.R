## teste funcoes acs

source('Scripts/00_extrai_acs.R')
source('Scripts/00_acs_bins.R')
source('Scripts/00_miliq_correction.R')
source('Scripts/00_branco_interpolate_median.R')
source('Scripts/00_acs_corrections.R')
source('Scripts/00_plot_acs.R')
source('Scripts/00_CTD_BIN.R')



acs_run = function(path_acs, 
                   path_ctd_acs = NA, 
                   path_ctd_castway = NA,
         filter_acs = F, 
         merged_ctd = T, 
         path_salvar = NA,
         station_name = NA, 
         path_miliq, 
         min_bin = 0,
         max_bin = 50, 
         interval = 1, 
         use_ctd_castway = NA) { 
  
      
      
      
      
    a = extrai_acs(path_acs = path_acs,
                     filter_acs = F,
                     path_salvar = save_path,
                     merge_ctd = T,
                     path_ctd = path_ctd,
                     station_name = station_name)
    
    b = extrai_branco(path_acs = path_miliq, 
                      filter_acs = F,
                      path_salvar = save_path,
                      station_name = 'miliq_reference')
    
    c = miliq_correction(atenuacao = a$ate, absorcao = a$abs, 
                         path_salvar = save_path,
                         station_name = station_name,
                         miliq_absorcao = b$abs, miliq_atenuacao = b$ate)
    
    
    # Aqui irá binar e separar por casts os dados do ACS. Irá criar uma coluna nova chamada 'Cast" com o numero do cast
    # Para isso, precisa selecionar primeiro o número de casts a partir do perfil e depois os pontos finais de cada cast
    
    
    d = acs_bins(atenuacao = c$ate, absorcao = c$abs,cast_filter = T,
                 path_salvar = save_path,
                 station_name = station_name, min_bin = min_bin, max_bin = max_bin, interval = interval)
    
    # Plota os dados do ACS - absorcao e atenuacao
    
    plots_acs(abs.binada = d$abs, ate.binada = d$ate, max_abs = 3, 
              max_ate = 15, station_name = station_name, path_salvar = save_path)
    
    
    
    
    ### Aqui por exemplo eu posso carregar os dados de salinidade vindos do proprio ACS
    
    
    e = ts_correction(abs = d$abs, ate = d$ate,
                      salinidade = d$abs$salinity, temperatura = d$abs$temperature,
                      path_salvar = save_path,
                      station_name = station_name)
    
    
    
    # Uso dos dados do CTD externo. Se for usar ele, precisa BINAR antes. 
    # Se nao, pode ignorar essa parte
    
    
    
    if(use_ctd_castway == T) {
      
      a = fread(path_ctd_castway, skip = 28)
      
      ctd = ctd_bins(Depth = a$Pressure, 
                     Temp = a$Temperature, 
                     Cond = a$Conductivity,
                     cast_filter = T, path_salvar = save_path,
                     station_name = station_name, min_bin = min_bin, max_bin = max_bin, interval = interval)
      
      
      e = ts_correction(abs = d$abs, ate = d$ate,
                        salinidade = ctd$salinidade, temperatura = ctd$Temp,
                        path_salvar = save_path,
                        station_name = station_name)
      
      
    }
    
    
    #### Aqui é o exemplo usando os dados do CTD CASTWAY
    
    
    ##### Correçao 715 off. Usa aqui o dado corrigido pela salinidad e temperatura
    
    
    f = scat_715_correction(abs = e$abs_ts,
                            path_salvar = save_path,
                            station_name = station_name)
    
    ##### Correçao 715 off. Usa aqui o dado corrigido pela salinidad e temperatura
    
    g = prop_correction(abs = e$abs, ate = e$ate_ts, wv_ref = 715,
                        path_salvar = save_path,
                        station_name = station_name)
    
    ##### Correçao 715 off. Usa aqui o dado corrigido pela salinidad e temperatura
    
    h = kirk_correction(abs = e$abs, ate = e$ate_ts, CFS = 0.12,
                        path_salvar = save_path,
                        station_name = station_name)



}


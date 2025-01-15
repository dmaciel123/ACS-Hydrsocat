# funcao para rodar o ACS

source("Scripts/00_ACS_full.R")


## Leia os paths dos arquivos: ACS, CTD e MILI-Q ACS

path_acs =  'Data/Campanhas/SaoSeb_12_2024/ACS/archive_21_ACS.433'
path_ctd = 'Data/Campanhas/SaoSeb_12_2024/ACS/archive_22_CTD-ENGR.433'
path_miliq ='Data/Campanhas/CEBIMAR_01_10_2024/acs/miliq_30_09_2024'

# Nome do ponto - cheque com o ponto acima
station_name  = "Ponto_02"

# Path para a pasta em que os arquivos serao salvos. 
# O diretorio final terá uma pasta com o nome de cada ponto para facilitar

save_path = paste('Outputs/SaoSeb_12_2024/', station_name, sep = '')

# Arquivo do CTD do castway caso formos usalo

ctd_castway_path = 'Data/Campanhas/SaoSeb_12_2024/CTD/dados_zipadps/CC1409005_20241204_154754.csv'


### Funcao para rodar 

acs_run(path_acs = path_acs, # Aqruivo ACS
        path_ctd_acs = path_ctd,  # Arquivo CTD ACS
        path_miliq = path_miliq, # Path da MILI-Q medida em laboratorio
        path_ctd_castway = ctd_castway_path, # Arquivo CTD Castway
        use_ctd_castway = T ,# Deixe como TRUE se quiser. usar o CTD do castway. Caso contrario, deixe como FALSE
        station_name = station_name, #nome do ponto
        path_salvar = save_path, #pasta de salvamento
        filter_acs = F, # quero remover pontos dos dados do acs?
        merged_ctd = T, # deixar marcado - ele faz um merge com os dados do CTD do ACS para ter as profs corretas
        min_bin = 0, # menor valor do bin
        max_bin = 50, #maior valor do bin
        interval = 1) # intervalo do bin)



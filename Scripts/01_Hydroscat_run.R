### Hydroscat data

require(data.table)
require(dplyr)
require(ggplot2)

source("Scripts/bins_hydroscat.R")

## data

path = 'Data/Campanhas/SaoSeb_12_2024/Hydroscat/'
campanha = 'SaoSeb_12_2024'

hh_process(path = path,
           campanha = campanha, profile = T,min_bin = 0, max_bin = 50, interval = 1,
           nomes_novos = c("CanalSaoSeb_12_2024_Ponto_01_normal_cdom",
                           "CanalSaoSeb_12_2024_Ponto_02",
                           "CanalSaoSeb_12_2024_Ponto_03",
                           "CanalSaoSeb_12_2024_Ponto_04_CDOM",
                           "CanalSaoSeb_12_2024_Ponto_04",
                           "CanalSaoSeb_12_2024_Ponto_05",
                           "CanalSaoSeb_12_2024_Ponto_06"))

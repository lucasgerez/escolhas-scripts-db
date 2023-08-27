# ------------------------------------------------------------------- #
#                 PNAD Contínua: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Objetivo: sistematizar os dados da PNAD contínua 


# Pacotes
load.lib <- c( "PNADcIBGE", "dplyr", "magrittr", "survey" )

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))



# PNAD Continua anual ---------


# PNAD Continua trimestral ---------


# Teste com um trimestre

pnad_file <- get_pnadc(year = 2019, quarter = 1) 


survey_design_df <- svydesign(
  
  id = ~UPA,
  strata = ~Estrato,
  weights = ~V1027
  data = pnad_file
  
  
)




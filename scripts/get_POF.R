# ------------------------------------------------------------------- #
#                     POF: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Para mais informações do que é a POF:
# https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e


# Caminho para os dados da POF: 
setwd('F:/Drive/BASES DE DADOS BRUTOS/POF/Microdados/Dados')

list.files()

# Leitura dos arquivos oficiais

# Para o load dos arquivos o setwd precisa localizar os arquivos em TXT
source('../Leitura_microdados/R/Leitura dos Microdados - R.R')






setwd('C:/Users/user/Projetos_GIT/escolhas-scripts-db')



# Pacotes necessários
used_pkgcv <- function() {
  
  # required packages
  load.lib <- c( 'dplyr')
  
  # Carregando os pacotes e instalando o que ainda não temos
  install.lib <- load.lib[ !load.lib %in% installed.packages() ]
  
  for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
  sapply( load.lib, require, character = TRUE )
  rm(list = c("install.lib", "lib", "load.lib"))
  
  
  # Check if 'Synth' package is installed
  if (!requireNamespace("POFIBGE", quietly = TRUE)) {
    # If not installed, try installing it
    install.packages("external_packages/POFIBGE_0.1.6.tar.gz", repos = NULL, type = "source")
    
  } else {
    message("'POFIBGE' package is already installed.")
  }
  
  library('POFIBGE')
  
}
used_pkgcv()


POFIBGE::get_pof()





# Caso ainda não tenha o pacote pof instalado
# remotes::install_github("tomasbarcellos/pof")
library(pof, dplyr)

aluguel <- pof::ler_aluguel(2018)

# Domicílio - Nível domicílio
domicilios <- pof::ler_domicilio(2018) %>% 
  janitor::clean_names() %>% 
  mutate(
    cod_dom = paste0(cod_upa, num_dom)
  )

pof::downaload_pof(2018)



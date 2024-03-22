# ------------------------------------------------------------------- #
#                 PNAD Contínua: Tabelas 
# ------------------------------------------------------------------- #

# Autor: Lucas Gerez Foratto


# Script consolidado com os dados da PNAD contínua

inicial <- Sys.time()

## Funções necessárias para as tabelas ----

# result_path       <- 'E:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF' # sempre checar se o caminho se mantém
# estratos_path     <- 'E:/Drive/BASES DE DADOS BRUTOS/POF/POF 2017-2018/Microdados/Documentacao_20221226/Estratos POF 2017-2018.xls' # sempre checar se o caminho se mantém
# dados_brutos_path <- 'E:/Drive/BASES DE DADOS BRUTOS'

# Caminho dos scripts na máquina local
git_path <- 'C:/Users/user/Documents/Projetos_github/escolhas-scripts-db/'

# Vamos rodar as funções no Git
source(file.path( git_path, "scripts/funcoes_pnadc.R" ), encoding = 'UTF-8')













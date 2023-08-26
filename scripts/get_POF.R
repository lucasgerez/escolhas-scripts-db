# ------------------------------------------------------------------- #
#                     POF: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Para mais informações do que é a POF:
# https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e


# Caminho para os dados da POF: 
setwd('F:/Drive/BASES DE DADOS BRUTOS/POF/Microdados/Dados')

# Leitura dos arquivos oficiais

# Etapa 1: obtenção das tabelas concluida -----
# Abrindo a POF e salvando os arquivos no mesmo caminho em formato RDS
source('../Leitura_microdados/R/Leitura dos Microdados - R.R')

arquivos <- list.files()
arquivos[grep('.rds', arquivos)]

# Etapa 2: Gastos com alimento ------ 

# Base de Consumo alimentar 
readRDS("CONSUMO_ALIMENTAR.rds")

# Selecionando o Paraná
uf <- 41

## Tab 1 ----  
# Gasto com alimentos dentro e fora de casa, por extrato de renda familiar,em relação aos outros tipos de despesas


## Tab 2 ----  
# Gasto com alimentos dentro e fora de casa, por extrato de renda familiar, tipo de alimento

## Tab 3 ----  
# Evolução da quantidade anual per capita adquirida por grupos alimentares

## Tab 4 ----  
# Locais de compra dos produtos alimentares

## Tab 5 ----  
# Prevalência de Insegurança Alimentar e Nutricional (IAN) nos domicílios 

## Tab 6 ----  
# Aquisição alimentar domiciliar em Curitiba, de acordo com a Classificação NOVA

## Tab 7 ----  
# Média de aquisição alimentar per capita anual














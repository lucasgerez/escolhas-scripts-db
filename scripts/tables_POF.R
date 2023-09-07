# ------------------------------------------------------------------- #
#                     POF: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Para mais informações do que é a POF:
# https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e

# Uma vez rodado o script get_pof.R, temos os microdados brutos e RDS

# Caminho para os dados da POF: 
setwd('F:/Drive/BASES DE DADOS BRUTOS/POF/Microdados/Dados')


arquivos <- list.files()
arquivos <- arquivos[grep('.rds', arquivos)]

# Etapa 2: Gastos com alimento ------ 

# Base de Consumo alimentar 
readRDS("CONSUMO_ALIMENTAR.rds")

# Argumentos
# Selecionando o Paraná
uf <- 41

# Tabelas requeridas pelo Pedro

# Tabela 3. Distribuição da renda familiar, despesas e gasto alimentar mensais per capita por décimos de renda familiar per capita – RMSP, 2017/2018.
# Tabela 4. Evolução da participação dos gastos alimentares nas despesas gerais e da alimentação no domicílio sobre o gasto alimentar — RMSP e Brasil, 1987/88, 1995/96, 2002/03, 2008/09 e 2017/18.
# Tabela 5. Composição do orçamento alimentar segundo grupos de alimentos e tipos de alimentação fora do domicílio, por décimos de renda familiar per capita — RMSP, 2017/18.
# Figura 8. Distribuição das despesas de consumo por grupos de despesa, segundo décimos de renda familiar per capita – RMSP, 2017/2018.
# Figura 9. Participação da alimentação fora do domicílio no gasto alimentar, segundo décimos de renda familiar per capita – RMSP, 2017/2018.
# Figura 11. Evolução na quantidade adquirida para o domicílio por grupos de alimentos — RMSP 1987/88, 1995/96, 2002/03, 2008/09 e 2017/18.
# Figura 12. Distribuição do número de aquisições, por locais de compra por e grupos de alimentos — Brasil, 2008/09.




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









# ------------------------------------------------------------------- #
#                     POF: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Para mais informações do que é a POF:
# https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e

# exemplo de tratamento da base
# https://rpubs.com/amrofi/microdados_pof

# Importante: A pesquisa tem como unidade de investigação o domicílio e é realizada por amostragem

# O desenho atual da amostra da POF foi estruturado de tal modo que propicia a publicação de resultados
# nos seguintes níveis: Brasil, Grandes Regiões, e também por situações urbana e rural.

# Uma vez rodado o script get_pof.R, temos os microdados brutos e RDS

# Arquivos de documentação
# F:\Drive\BASES DE DADOS BRUTOS\POF\Microdados\Documentacao_20221226

# Para mais informações sobre o dicionário das variáveis: 
# Dicionários de váriaveis.xlsx

# Para mais informações sobre os estratos: 
# Estratos POF 2017-2018.xlsx
# Para mais informações sobre os estratos: 
# Estratos POF 2017-2018.xlsx
  # ESTRATO_POF	Identifica os estratos do plano amostral da pesquisa: 
  # estratificações Geográfica e Estatística. No nível geográfico, a estratificação compreende: 
  # área urbana para o município da capital, resto da região metropolitana, resto da UF e área rural. 
  # A estratificação estatística foi realizada a partir das definições implementadas na Amostra Mestra, 
  # que utiliza informações da variável renda total do domicílio, obtida a partir dos dados do Censo 2010.  

# Nota sobre os pesos: 
# PESO: UPA a nível de domicilio
# PESO_FINAL: já com os estratos


# Para a tabela de renda per capita vamos utilizar a variável de renda monetária



# Caminho para os dados da POF: 
setwd('F:/Drive/BASES DE DADOS BRUTOS/POF/Microdados/Dados')

# pacotes
library(dplyr)

arquivos <- list.files()
arquivos <- arquivos[grep('.rds', arquivos)]


# Igual a tabela3 do estudo de SP Passos que precisamos fazer -----

  # Calcular a renda per capita total
  # Calcular o decis de renda familiar per capita
  # Calcular despesa total mensal familiar per capita
  # Calcular despesa em consumo mensal familiar per capita
  # Calcular gasto mensal per capita com alimentação fora de casa
  # Calcular gasto mensal per capita com alimentação em casa
  # Calcular participação alimentação nas despesas de consumo
  # Calcular participação alimentação fora de casa no gasto alimentar



# Exemplo com despesas com alimentação -----

# Leitura do REGISTRO - CADERNETA COLETIVA (Questionario POF 3)
caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")

cad_coletiva <- transform(subset(transform(caderneta_coletiva, codigo = round(V9001/100)  # [1]
), codigo < 86001 | codigo > 89999), valor_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO *
                                                       PESO_FINAL)/12  # [3] 
)
rm(caderneta_coletiva)  # para reduzir o uso da memoria

desp_individual <- subset(transform(despesa_individual, codigo = round(V9001/100)  # [1]
), QUADRO == 24 | codigo == 41001 | codigo == 48018 | codigo == 49075 |
  codigo == 49089)  # [2]

desp_individual <- transform(desp_individual, valor_mensal = ifelse(QUADRO == 24 |
                                                                      QUADRO == 41, (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12, (V8000_DEFLA *
                                                                                                                                          V9011 * FATOR_ANUALIZACAO * PESO_FINAL)/12)  # [3] 
)
rm(despesa_individual)


# As duas tabelas precisam ter o mesmo conjunto de variaveis Identificacao dos
# nomes das variaveis das tabelas a serem juntadas:
nomes_cad <- names(cad_coletiva)
nomes_desp <- names(desp_individual)

# Identificacao das variaveis exclusivas a serem incluidas na outra tabela:
incl_cad <- nomes_desp[!nomes_desp %in% nomes_cad]
incl_desp <- nomes_cad[!nomes_cad %in% nomes_desp]

# Criando uma tabela com NAs das variaveis ausentes em cada tabela
col_ad_cad <- data.frame(matrix(NA, nrow(cad_coletiva), length(incl_cad)))
names(col_ad_cad) <- incl_cad
col_ad_desp <- data.frame(matrix(NA, nrow(desp_individual), length(incl_desp)))
names(col_ad_desp) <- incl_desp

# Acrescentando as colunas ausentes em cada tabela:
cad_coletiva <- cbind(cad_coletiva, col_ad_cad)
desp_individual <- cbind(desp_individual, col_ad_desp)

# Juntando (empilhando) as tabelas com conjuntos de variaveis iguais
junta_ali <- rbind(cad_coletiva, desp_individual)  # [1]

morador_uc <- unique(readRDS("MORADOR.rds")[, c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                                                "COD_UPA", "NUM_DOM", "NUM_UC", "PESO_FINAL")  # Apenas variaveis com informacoes das UC's no arquivo 'MORADOR.rds'
])  # Apenas um registro por UC


head(morador_uc)

soma_familia <- sum(morador_uc$PESO_FINAL)


# A gente precisa achar essa tabela
tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 

merge1 <- merge(junta_ali, tradutor_alimentacao, by.x = "codigo", by.y = "Codigo")  # [1]
merge1 <- merge1[!is.na(merge1$valor_mensal), ]  # [2]

# Somando os valores mensais de cada grupo de códigos, segundo cada nivel, conforme consta no tradutor
soma_final_0 <- aggregate(valor_mensal ~ Nivel_0, data = merge1, sum)
names(soma_final_0) <- c("nivel", "soma")

soma_final_1 <- aggregate(valor_mensal ~ Nivel_1, data = merge1, sum)
names(soma_final_1) <- c("nivel", "soma")

soma_final_2 <- aggregate(valor_mensal ~ Nivel_2, data = merge1, sum)
names(soma_final_2) <- c("nivel", "soma")

soma_final_3 <- aggregate(valor_mensal ~ Nivel_3, data = merge1, sum)
names(soma_final_3) <- c("nivel", "soma")


# Empilhando as somas obtidas no passo anterior.
soma_final <- rbind(soma_final_0, soma_final_1, soma_final_2, soma_final_3)  # [1]

# Calculando a despesa média mensal de cada grupo de códigos, segundo cada nível, conforme consta no tradutor.
merge2 <- data.frame(soma_final, soma_familia = soma_familia)
merge2 <- transform(merge2, media_mensal = round(soma/soma_familia, 2))


# Preciso atualizar aqui!!! 
indice_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/indice_Alimentacao.xls")

# Juntando o arquivo das despesas medias mensais de cada grupo de codigos com o
# arquivo de indice, para organizar os itens da tabela

merge3 <- merge(merge2, indice_alimentacao)
merge3 <- merge3[order(merge3$Indice), c(5, 1, 6, 4)]  # [2]

## TABELA FINAL -----
print(merge3)



# Leitura do REGISTRO - DESPESA INDIVIDUAL (Questionario POF 4)

despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")

# Base de Consumo alimentar 
consumo_alimentar <- readRDS("CONSUMO_ALIMENTAR.rds")
rendimento_trabalho <- readRDS("RENDIMENTO_TRABALHO.rds")


# funcao para classificar os decis de renda

classify_deciles <- function(data, variable) {
  
  names(data)[which(names(data) == variable)] <- 'var'
  
  decis <- quantile(data$var, probs = seq(0,1,.1), na.rm = T) 
  
  cat('Distribuição dos decis:\n')
  print(round(decis, 2))
  
  # Classificação dos decis 
  
  data$decis <- cut(data$var, breaks = decis, labels = F, include.lowest = T, na.pass = TRUE)
  
  names(data)[which(names(data) == 'var' )] <- variable
  
  return(data)
}

# PAREI AQUI. A princípio deu certo
pessoas_pof <- 
  consumo_alimentar %>% 
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC, COD_INFOR.MANTE, QUADRO ) %>%
  summarise(RENDA_TOTAL = weighted.mean(RENDA_TOTAL, PESO_FINAL)) %>%
  classify_deciles(variable = 'RENDA_TOTAL')


# Etapa 2: Gastos com alimento ------ 

head(consumo_alimentar)

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









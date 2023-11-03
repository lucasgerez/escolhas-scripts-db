# do-file 2
# Ricardo Campante C. Vale
# Criando a função para extrair totais, médias e percentuais médios de consumo por
# decis de estrato específico
# 30 de outubro de 2023 (YYYYMMDD = 20231030)

# preambulo -------------------------------------------
rm(list = ls())

# Caminho para os dados da POF: 
setwd('C:/Users/ricar/Documents/consultoria/gerez/dados pof')

# Caminho para exportar as tabelas
export <- 'C:/Users/ricar/Documents/consultoria/gerez/resultados'

# pacotes
library(dplyr)
library(tidyr)
library(Hmisc) # para mexer com os dados amostrais
library(openxlsx)
library(survey)
library(srvyr)


# Criar funcoes para tabelas -----------------------------------------------------
# Tabelas são por grupos de consumo e por estrato definido

f_xtile_filter <- function(df, variavel, var_peso, nova_var, n, estrato) {
  
  x <- 1 / n
  
  df <- filter(df, ESTRATO_POF %in% estrato)
  
  percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
  df[[nova_var]] <- cut(df[[variavel]], 
      breaks = percentis,
      labels = F, 
      include.lowest = T, 
      na.pass = TRUE)
  return(df)
  
}


# Apenas as médias por decis --------------------------------------------------#

f_medias <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(despesa_total = survey_mean( desp_total_pc  ,  na.rm = TRUE)) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
  
}

f_medias_mon <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(despesa_total_mon = survey_mean( desp_total_mon_pc  ,  na.rm = TRUE)) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
  
}

# Por grupos de consumo -------------------------------------------------------#
# totais
f_medias_g <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(across(desp_pc_g1:desp_pc_g22, \(x) survey_mean(x,  na.rm = TRUE))) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
  
}
  
f_perc_medios_g <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(across(starts_with("perc_g"), \(x) survey_mean(x,  na.rm = TRUE))) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
  
}

f_totais_g <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(across(desp_pc_g1:desp_pc_g22, \(x) survey_total(x,  na.rm = TRUE))) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
}
  
# monetarios
f_medias_mon_g <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(across(desp_mon_pc_g1:desp_mon_pc_g22, \(x) survey_mean(x,  na.rm = TRUE))) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
}
  
f_perc_mon_medios_g <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(across(starts_with("perc_mon_g"), \(x) survey_mean(x,  na.rm = TRUE))) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
  
}
  
f_totais_mon_g <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(across(desp_mon_pc_g1:desp_mon_pc_g22, \(x) survey_total(x,  na.rm = TRUE))) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
}







# do-file 2
# Ricardo Campante C. Vale
# Criando a função para extrair totais, médias e percentuais médios de consumo por
# decis de estrato específico
# 30 de outubro de 2023 (YYYYMMDD = 20231030)

# preambulo -------------------------------------------
# rm(list = ls())

# Caminho para os dados da POF: 
# setwd('C:/Users/ricar/Documents/consultoria/gerez/dados pof')
# 
# # Caminho para exportar as tabelas
# export <- 'C:/Users/ricar/Documents/consultoria/gerez/resultados'


# Pacotes
load.lib <- c( "tidyr", "dplyr", "openxlsx", "survey", "srvyr", 'Hmisc' )

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))


# Funcoes POF 2002 e 2008 -------------------------------------------------


# Função para a POF de 2002 a 2008
f_xtile_filter_2018 <- function(df, variavel, var_peso, nova_var, n, estrato, estrato_var_name = 'estrato_pof') {
  
  x <- 1 / n
  
  df <- filter(df, {{estrato_var_name}} %in% estrato)
  
  percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
  df[[nova_var]] <- cut(df[[variavel]], 
                        breaks = percentis,
                        labels = F, 
                        include.lowest = T, 
                        na.pass = TRUE)
  return(df)
  
}


# Calculo da renda pc 
f_inc_dist_filter_2018 <- function(pof_svy, variavel, var_peso, nova_var, n, estrato, 
                                   estrato_var_name = 'estrato_pof') {
  
  options(warn = -1)
  
  # Cálculo da Renda dom pc total
  aux <- pof_svy %>%
    summarise(renda_dom_pc_disp = survey_mean( get(variavel),  na.rm = TRUE)) %>%
    select(renda_dom_pc_disp) %>%
    as.data.frame()
  
  df <- pof_svy$variables
  
  x <- 1 / n
  
  df <- filter(df, {{estrato_var_name}} %in% estrato)
  
  
  percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
  
  tab <- data.frame(decis = 1:(n+1), renda_dom_pc_disp = c(percentis[-1], aux$renda_dom_pc_disp))
  
  tab$renda_dom_pc_disp <- paste0("R$ ", prettyNum(round(tab$renda_dom_pc_disp,0), big.mark = ".", small.mark = ","))
  tab$renda_dom_pc_disp[1] <- paste0("Até ", tab$renda_dom_pc_disp[1])
  tab$renda_dom_pc_disp[10] <- paste0("Acima de ", tab$renda_dom_pc_disp[9])
  tab$decis[11] <- 'Total'
  
  return(tab)
  
}

# Despesas gerais
f_medias_2002_2008 <- function(df, percentis) {
  
  df <- df %>%
    group_by({{percentis}}) %>%
    summarise(despesa_total = survey_mean( despesas_mensais_totais_per_capita,  na.rm = TRUE),
              despesa_alimentos = survey_mean( despesas_mensais_alimentacao_per_capita,  na.rm = TRUE),
              despesa_habitacao = survey_mean( despesas_mensais_moradia_per_capita,  na.rm = TRUE),
              despesa_transporte = survey_mean( despesas_mensais_transporte_per_capita,  na.rm = TRUE),
              despesa_saude = survey_mean( despesas_mensais_saude_per_capita,  na.rm = TRUE),
              despesa_emprestimo = survey_mean( despesas_mensais_emprestimos_per_capita,  na.rm = TRUE)
    ) %>%
    mutate(despesa_outras = despesa_total - (despesa_alimentos + despesa_habitacao +
                                               despesa_transporte + despesa_saude + 
                                               despesa_emprestimo) ) %>%
    
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
  
}

f_medias_2018 <- function(df, percentis) {
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  df <- df$variables %>%
    group_by({{percentis}}) %>%
    summarise(despesa_total = survey_mean( sum_desp_nivel3/pessoas_dom,  na.rm = TRUE),
              despesa_alimentos = survey_mean( pc_desp_nivel3_Alimentacao,  na.rm = TRUE),
              despesa_alimentos_no_dom = survey_mean( pc_desp_nivel4_Alimentacao_no_dom,  na.rm = TRUE),
              despesa_alimentos_fora_dom = survey_mean( pc_desp_nivel4_Alimentacao_fora_dom,  na.rm = TRUE),
              despesa_habitacao = survey_mean( pc_desp_nivel3_Habitacao,  na.rm = TRUE),
              despesa_transporte = survey_mean( pc_desp_nivel3_Transporte,  na.rm = TRUE),
              despesa_saude = survey_mean( pc_desp_nivel3_Saude,  na.rm = TRUE),
              despesa_emprestimo = survey_mean( pc_desp_nivel3_emprestimo,  na.rm = TRUE)
    ) %>%
    mutate(despesa_outras = despesa_total - (despesa_alimentos + despesa_habitacao +
                                               despesa_transporte + despesa_saude + 
                                               despesa_emprestimo) ) %>%
    
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  return(df)
  
}

f_alimentos_no_dom_2018 <- function(df, percentis) {
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  # Bloco 1: cálculo do valor despendido
  df <- df %>%
    group_by({{percentis}}) %>% # {{percentis}}
    summarise(careais = survey_mean( pc_desp_nivel5_dentro_dom_cereais,  na.rm = TRUE),
              farinhas = survey_mean( pc_desp_nivel5_dentro_dom_farinhas_e_massas,  na.rm = TRUE),
              tuberculos = survey_mean( pc_desp_nivel5_dentro_dom_tuberculos_raizes,  na.rm = TRUE),
              acucares = survey_mean( pc_desp_nivel5_dentro_dom_acucares_e_derivados,  na.rm = TRUE),
              verduras = survey_mean( pc_desp_nivel5_dentro_dom_legumes_e_verduras,  na.rm = TRUE),
              frutas = survey_mean( pc_desp_nivel5_dentro_dom_frutas,  na.rm = TRUE),
              carnes_pescados = survey_mean( pc_desp_nivel5_dentro_dom_carnes_e_pescados,  na.rm = TRUE),
              aves_ovos = survey_mean( pc_desp_nivel5_dentro_dom_aves_e_ovos,  na.rm = TRUE),
              leites_derivados = survey_mean( pc_desp_nivel5_dentro_dom_leites_e_derivados,  na.rm = TRUE),
              panificados = survey_mean( pc_desp_nivel5_dentro_dom_panificados,  na.rm = TRUE),
              oleos_gorduras = survey_mean( pc_desp_nivel5_dentro_dom_oleos_e_gorduras,  na.rm = TRUE),
              bebidas_infusoes = survey_mean( pc_desp_nivel5_dentro_dom_bebidas_e_infusoes,  na.rm = TRUE),
              elatados = survey_mean( pc_desp_nivel5_dentro_dom_enlatados,  na.rm = TRUE),
              sal = survey_mean( pc_desp_nivel5_dentro_dom_sal,  na.rm = TRUE),
              preparados = survey_mean( pc_desp_nivel5_dentro_dom_alimentos_preparados,  na.rm = TRUE),
              outros = survey_mean( pc_desp_nivel5_dentro_dom_outros,  na.rm = TRUE)) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  # Formato final 
  df_t <- t(df)
  df_t <- df_t[-1,] %>% as.data.frame()
  colnames(df_t) <- c(1:ncol(df_t))
  
  # Vamos então fazer a conta do percentual do gasto com cada grupo de alimento
  col_sums <- colSums(df_t)
  
  # Create a new data frame for percentages
  percentage_df <- as.data.frame(apply(df_t, 1, function(col) col / col_sums )) # * 100
  percentage_df <- round(percentage_df,2)
  
  
  if (ncol(percentage_df) == 1) {
    
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 'Total')
    
  } else {
    
    # Aqui temos que transpor antes
    percentage_df <- t(percentage_df)
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 1:(ncol(percentage_df)-1))
    
  }
  
  
  return(percentage_df)
  
}

f_alimentos_fora_dom_2018 <- function(df, percentis) {
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  
  # Bloco 1: cálculo do valor despendido
  df <- df %>%
    group_by({{percentis}}) %>% # {{percentis}}
    summarise(almoco_jantar = survey_mean( pc_desp_nivel5_fora_dom_almoco_e_jantar,  na.rm = TRUE),
              lanches = survey_mean( pc_desp_nivel5_fora_dom_lanches,  na.rm = TRUE),
              cafe_leite_choco = survey_mean( pc_desp_nivel5_fora_dom_cafe_leite_choco,  na.rm = TRUE),
              sand_salgados = survey_mean( pc_desp_nivel5_fora_dom_sanduiches_e_salgados,  na.rm = TRUE),
              beb_alcoolicas = survey_mean( pc_desp_nivel5_fora_dom_cervejas_e_outras_alcoólicas,  na.rm = TRUE),
              refri = survey_mean( pc_desp_nivel5_fora_dom_refri_e_outras_nao_alcooicas,  na.rm = TRUE),
              outras = survey_mean( pc_desp_nivel5_fora_dom_outras,  na.rm = TRUE),
              ) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  # Formato final 
  df_t <- t(df)
  df_t <- df_t[-1,] %>% as.data.frame()
  colnames(df_t) <- c(1:ncol(df_t))
  
  # Vamos então fazer a conta do percentual do gasto com cada grupo de alimento
  col_sums <- colSums(df_t)
  
  # Create a new data frame for percentages
  percentage_df <- as.data.frame(apply(df_t, 1, function(col) col / col_sums )) # * 100
  percentage_df <- round(percentage_df,2)
  
  
  
  if (ncol(percentage_df) == 1) {
    
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 'Total')
    
  } else {
    
    # Aqui temos que transpor antes
    percentage_df <- t(percentage_df)
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 1:(ncol(percentage_df)-1))
    
  }
  
  
  
  return(percentage_df)
  
}

f_tipo_process_2018 <- function(df, percentis) {
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  # Bloco 1: cálculo do valor despendido
  df <- df %>%
    group_by({{percentis}}) %>% # {{percentis}}
    summarise(in_natura = survey_mean( pc_desp_nivel6_tipo_in_natura,  na.rm = TRUE),
              ing_culinario = survey_mean( pc_desp_nivel6_tipo_ing_culinario,  na.rm = TRUE),
              processado = survey_mean( pc_desp_nivel6_tipo_processado,  na.rm = TRUE),
              ultraprocessado = survey_mean( pc_desp_nivel6_tipo_ultraprocessado,  na.rm = TRUE),
              ) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  # Formato final 
  df_t <- t(df)
  df_t <- df_t[-1,] %>% as.data.frame()
  colnames(df_t) <- c(1:ncol(df_t))
  
  # Vamos então fazer a conta do percentual do gasto com cada grupo de alimento
  col_sums <- colSums(df_t)
  
  # Create a new data frame for percentages
  percentage_df <- as.data.frame(apply(df_t, 1, function(col) col / col_sums )) # * 100
  percentage_df <- round(percentage_df,2)
  
  
  
  if (ncol(percentage_df) == 1) {
    
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 'Total')
    
  } else {
    
    # Aqui temos que transpor antes
    percentage_df <- t(percentage_df)
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 1:(ncol(percentage_df)-1))
    
  }
  
  
  return(percentage_df)
  
}

f_consumo_kcal_2018 <- function(df, percentis) {
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  # Bloco 1: cálculo do valor despendido
  df <- df %>%
    group_by({{percentis}}) %>% # {{percentis}}
    summarise(kcal_natura = survey_mean( pc_consumo_nivel6_tipo_in_natura,  na.rm = TRUE),
              kcal_ing_culinario = survey_mean( pc_consumo_nivel6_tipo_ing_culinario,  na.rm = TRUE),
              kcal_processado = survey_mean( pc_consumo_nivel6_tipo_processado,  na.rm = TRUE),
              kcal_ultraprocessado = survey_mean( pc_consumo_nivel6_tipo_ultraprocessado,  na.rm = TRUE),
    ) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  # Formato final 
  df_t <- t(df)
  df_t <- df_t[-1,] %>% as.data.frame()
  colnames(df_t) <- c(1:ncol(df_t))
  
  # Vamos então fazer a conta do percentual do gasto com cada grupo de alimento
  col_sums <- colSums(df_t)
  
  # Create a new data frame for percentages
  percentage_df <- as.data.frame(apply(df_t, 1, function(col) col / col_sums )) # * 100
  percentage_df <- round(percentage_df,2)
  
  
  
  if (ncol(percentage_df) == 1) {
    
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 'Total')
    
  } else {
    
    # Aqui temos que transpor antes
    percentage_df <- t(percentage_df)
    percentage_df <- data.frame(grupo = rownames(percentage_df), percentage_df)
    colnames(percentage_df) <- c('grupo', 1:(ncol(percentage_df)-1))
    
  }
  
  
  return(percentage_df)
  
}

f_inseguranca_2018 <- function(df) {
  
  df = pof_svy
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  # Bloco 1 Insegurança Alimentar no estrato como um todo 
  
  # Bloco 1: 
  df1 <- df %>%
    group_by(inseguranca_alimentar) %>% 
    summarise(pop_geral = survey_prop()) %>%
    select(-(ends_with("_se"))) 
    
  df2 <- df %>%
    filter(chefe_dom_over_65 == 1) %>% 
    group_by(inseguranca_alimentar) %>% 
    summarise(fam_chefe_over_65 = survey_prop()) %>%
    select(-(ends_with("_se"))) 
  
  df3 <- df %>%
    filter(chefe_dom_analf == 1) %>% 
    group_by(inseguranca_alimentar) %>% 
    summarise(fam_chefe_analf = survey_prop()) %>%
    select(-(ends_with("_se"))) 
  
  df4 <- df %>%
    filter(chefe_dom_EF == 1) %>% 
    group_by(inseguranca_alimentar) %>% 
    summarise(fam_chefe_EF = survey_prop()) %>%
    select(-(ends_with("_se"))) 
  
  df5 <- df %>%
    filter(chefe_dom_EM == 1) %>% 
    group_by(inseguranca_alimentar) %>% 
    summarise(fam_chefe_EM = survey_prop()) %>%
    select(-(ends_with("_se"))) 
  
  df6 <- df %>%
    filter(chefe_dom_mulher == 1) %>% 
    group_by(inseguranca_alimentar) %>% 
    summarise(fam_chefe_mulher = survey_prop()) %>%
    select(-(ends_with("_se")))
  
  df7 <- df %>%
    filter(chefe_dom_negro == 1) %>% 
    group_by(inseguranca_alimentar) %>% 
    summarise(fam_chefe_dom_negro = survey_prop()) %>%
    select(-(ends_with("_se")))
  
  df8 <- df %>%
    filter(chefe_dom_mulher == 1, chefe_dom_negro == 1, chefe_dom_EF == 1) %>% 
    group_by(inseguranca_alimentar) %>% 
    summarise(fam_chefe_mulh_negra_EF = survey_prop()) %>%
    select(-(ends_with("_se")))
  
  
  df1 %>% 
    left_join(df2, by = 'inseguranca_alimentar') %>%
    left_join(df3, by = 'inseguranca_alimentar') %>%
    left_join(df4, by = 'inseguranca_alimentar') %>%
    left_join(df5, by = 'inseguranca_alimentar') %>%
    left_join(df6, by = 'inseguranca_alimentar') %>%
    left_join(df7, by = 'inseguranca_alimentar') %>% 
    left_join(df8, by = 'inseguranca_alimentar') 
  
}



# ANTIGO ----  ------------------------------------------------------------

# f_xtile_filter_2018 <- function(df, variavel, var_peso, nova_var, n, estrato) {
  
  require(Hmisc)
  
  x <- 1 / n
  
  df <- filter(df, ESTRATO_POF %in% estrato)
  
  percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
  df[[nova_var]] <- cut(df[[variavel]],
                        breaks = percentis,
                        labels = F,
                        include.lowest = T,
                        na.pass = TRUE)
  return(df)
  
# }


# f_medias_2018 <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(despesa_total = survey_mean( desp_total_pc,  na.rm = TRUE),
#               despesa_alimentos = survey_mean( desp_pc_g1,  na.rm = TRUE),
#               despesa_habitacao = survey_mean( desp_pc_g2,  na.rm = TRUE),
#               despesa_transporte = survey_mean( desp_pc_g4,  na.rm = TRUE),
#               despesa_saude = survey_mean( desp_pc_g6,  na.rm = TRUE),
#               despesa_emprestimo = survey_mean( desp_pc_g19,  na.rm = TRUE)
#     ) %>%
#     mutate(despesa_outras = despesa_total - (despesa_alimentos + despesa_habitacao +
#                                                despesa_transporte + despesa_saude + 
#                                                despesa_emprestimo) ) %>%
#     
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
#   
# }

# 
# # Criar funcoes para tabelas -----------------------------------------------------
# # Tabelas são por grupos de consumo e por estrato definido
# 
# f_xtile_filter <- function(df, variavel, var_peso, nova_var, n, estrato) {
#   
#   x <- 1 / n
#   
#   df <- filter(df, ESTRATO_POF %in% estrato)
#   
#   percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
#   df[[nova_var]] <- cut(df[[variavel]], 
#       breaks = percentis,
#       labels = F, 
#       include.lowest = T, 
#       na.pass = TRUE)
#   return(df)
#   
# }
# 
# 
# # Calculo da renda pc 
# f_inc_dist_filter <- function(pof_svy, variavel, var_peso, nova_var, n, estrato) {
#   
#   options(warn = -1)
#   
#   # Cálculo da Renda dom pc total
#   
#   aux <- pof_svy %>%
#     summarise(renda_dom_pc_disp = survey_mean( PC_RENDA_DISP,  na.rm = TRUE)) %>%
#     select(renda_dom_pc_disp) %>%
#     as.data.frame()
#   
#   df <- pof_svy$variables
#   
#   x <- 1 / n
#   
#   df <- filter(df, ESTRATO_POF %in% estrato)
#   
#   percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
#   
#   tab <- data.frame(decis = 1:(n+1), renda_dom_pc_disp = c(percentis[-1], aux$renda_dom_pc_disp))
#   
#   tab$renda_dom_pc_disp <- paste0("R$ ", prettyNum(round(tab$renda_dom_pc_disp,0), big.mark = ".", small.mark = ","))
#   tab$renda_dom_pc_disp[1] <- paste0("Até ", tab$renda_dom_pc_disp[1])
#   tab$renda_dom_pc_disp[10] <- paste0("Acima de ", tab$renda_dom_pc_disp[9])
#   tab$decis[11] <- 'Total'
#   
#   return(tab)
#   
# }
# 
# 
# 
# 
# # Apenas as médias por decis --------------------------------------------------#
# 
# f_medias <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(despesa_total = survey_mean( desp_total_pc  ,  na.rm = TRUE)) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
#   
# }
# 
# f_medias_mon <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(despesa_total_mon = survey_mean( desp_total_mon_pc  ,  na.rm = TRUE)) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
#   
# }
# 
# # Por grupos de consumo -------------------------------------------------------#
# # totais
# f_medias_g <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(across(desp_pc_g1:desp_pc_g22, \(x) survey_mean(x,  na.rm = TRUE))) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
#   
# }
#   
# f_perc_medios_g <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(across(starts_with("perc_g"), \(x) survey_mean(x,  na.rm = TRUE))) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
#   
# }
# 
# f_totais_g <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(across(desp_pc_g1:desp_pc_g22, \(x) survey_total(x,  na.rm = TRUE))) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
# }
#   
# # monetarios
# f_medias_mon_g <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(across(desp_mon_pc_g1:desp_mon_pc_g22, \(x) survey_mean(x,  na.rm = TRUE))) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
# }
#   
# f_perc_mon_medios_g <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(across(starts_with("perc_mon_g"), \(x) survey_mean(x,  na.rm = TRUE))) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
#   
# }
#   
# f_totais_mon_g <- function(df, percentis) {
#   
#   df <- df %>%
#     group_by({{percentis}}) %>%
#     summarise(across(desp_mon_pc_g1:desp_mon_pc_g22, \(x) survey_total(x,  na.rm = TRUE))) %>%
#     select(-(ends_with("_se"))) %>%
#     as.data.frame()
#   
#   return(df)
# }







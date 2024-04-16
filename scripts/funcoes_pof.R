
# ------------------------------------------------ # 

#               Funcoes - Tabelas POF

# ------------------------------------------------ # 


cat('Loading the packages .... \n')
# Pacotes
load.lib <- c( "tidyr", "dplyr", "openxlsx", "survey", "srvyr", 'Hmisc', 'janitor', 'readxl' )


# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))




# Estratos ----------------------------------------------------------------

cat('Elaborando a tabela com os estratos .... \n')

get_estratos_f <- function(estrato) {
  
  if (is.na(estrato))  {
    
    valor <- NA
    
  } else if (nchar(estrato) == 9) {
    
    valor <- as.numeric(substr(estrato,1,4)):as.numeric(substr(estrato,6,9)) 
    
  } else {
    
    valor <- as.numeric(estrato)
    
  }
  
  return(valor)
  
}

gen_estratos_df_f <- function(estratos_path) {
  
  estratos <- read_excel(path = estratos_path, sheet = 'Plan3')
  
  lst.estratos <- list()
  
  for (linha in 1:nrow(estratos)) {
    
    # apenas os estratos da capital
    estratos_cap_aux <- get_estratos_f(estrato = estratos$capital[linha])
    estratos_capital_ref <- data.frame(uf = estratos$UF[linha], macrorregiao = estratos$regiao[linha], regiao = 'capital', estratos = estratos_cap_aux)
    
    # estratos de toda a RM (incluindo a capital): se só tiver da capital volta NA
    estratos_rm_aux <- get_estratos_f(estrato = estratos$rm[linha])
    
    # UF sem rural
    estratos_uf_aux <- get_estratos_f(estrato = estratos$resto_uf[linha])
    
    
    if ( any(is.na(estratos_rm_aux))) {
      
      estratos_rm_ref <- data.frame(uf = estratos$UF[linha], macrorregiao = estratos$regiao[linha], regiao = 'rm', estratos = estratos_rm_aux)
      estratos_uf_ref <- data.frame(uf = estratos$UF[linha], macrorregiao = estratos$regiao[linha], regiao = 'uf', estratos = c(estratos_cap_aux, estratos_uf_aux))
      
    } else {
      
      estratos_rm_ref <- data.frame(uf = estratos$UF[linha], macrorregiao = estratos$regiao[linha], regiao = 'rm', estratos = c(estratos_cap_aux, estratos_rm_aux))
      estratos_uf_ref <- data.frame(uf = estratos$UF[linha], macrorregiao = estratos$regiao[linha], regiao = 'uf', estratos = c(estratos_cap_aux, estratos_rm_aux, estratos_uf_aux))
      
    }
    
    estratos_rural_aux <- get_estratos_f(estrato = estratos$rural[linha])
    estratos_rural_ref <- data.frame(uf = estratos$UF[linha], macrorregiao = estratos$regiao[linha],  regiao = 'rural', estratos = estratos_rural_aux)
    
    df_estratos <- bind_rows(estratos_capital_ref, estratos_rm_ref, estratos_uf_ref, estratos_rural_ref)
    
    lst.estratos[[linha]] <- df_estratos
    
    
  }
  
  df_estratos <- bind_rows(lst.estratos)
  
  return(df_estratos)
  
}

df_estratos <- gen_estratos_df_f(estratos_path = estratos_path)


# Funcoes POF 2018 -------------------------------------------------

cat('Loading the required functions .... \n')

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
f_medias_2018 <- function(df, percentis) {
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  
  df <- df %>%
    group_by({{percentis}}) %>% # 
    summarise(despesa_total = survey_mean( sum_desp_nivel3/pessoas_dom,  na.rm = TRUE),
              despesa_alimentos = survey_mean( pc_desp_nivel3_Alimentacao,  na.rm = TRUE),
              despesa_alimentos_no_dom = survey_mean( pc_desp_nivel4_Alimentacao_no_dom,  na.rm = TRUE),
              despesa_alimentos_fora_dom = survey_mean( pc_desp_nivel4_Alimentacao_fora_dom,  na.rm = TRUE),
              despesa_habitacao = survey_mean( pc_desp_nivel3_Habitacao,  na.rm = TRUE),
              despesa_vestuario = survey_mean( pc_desp_nivel3_Vestuario,  na.rm = TRUE),
              despesa_transporte = survey_mean( pc_desp_nivel3_Transporte,  na.rm = TRUE),
              despesa_higiene = survey_mean( pc_desp_nivel3_Higiene,  na.rm = TRUE),
              despesa_saude = survey_mean( pc_desp_nivel3_Saude,  na.rm = TRUE),
              despesa_educacao = survey_mean( pc_desp_nivel3_Educacao,  na.rm = TRUE),
              despesa_cultura = survey_mean( pc_desp_nivel3_Lazer_e_cult,  na.rm = TRUE),
              despesa_fumo = survey_mean( pc_desp_nivel3_Fumo,  na.rm = TRUE),
              despesa_serv_pessoais = survey_mean( pc_desp_nivel3_Serv_pessoais,  na.rm = TRUE),
              despesa_diversas = survey_mean( pc_desp_nivel3_diversos,  na.rm = TRUE)
    ) %>%
    mutate(despesa_consumo = despesa_alimentos + despesa_habitacao + despesa_vestuario + despesa_transporte +
             despesa_higiene + despesa_saude + despesa_educacao + despesa_cultura + despesa_fumo +
             despesa_serv_pessoais + despesa_diversas) %>%
    select({{percentis}}, despesa_total, despesa_consumo, despesa_alimentos,
           despesa_alimentos_no_dom, despesa_alimentos_fora_dom,
           despesa_habitacao, despesa_transporte, despesa_saude ) %>%
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

f_alimentos_kg_2018 <- function(df) {
  
  # Vamos substituir os missings por zero
  df$variables[is.na(df$variables)] <- 0
  
  
  # Bloco 1: consumo de alimentos em kg
  df <- df %>%
    summarise(careais = (365/10^3)*survey_mean(  pc_consumo_gr_dom_cereais,  na.rm = TRUE),
              farinhas = (365/10^3)*survey_mean( pc_consumo_gr_dom_farinhas_e_massas,  na.rm = TRUE),
              tuberculos = (365/10^3)*survey_mean( pc_consumo_gr_dom_tuberculos_raizes,  na.rm = TRUE),
              acucares = (365/10^3)*survey_mean( pc_consumo_gr_dom_acucares_e_derivados,  na.rm = TRUE),
              verduras = (365/10^3)*survey_mean( pc_consumo_gr_dom_legumes_e_verduras,  na.rm = TRUE),
              frutas = (365/10^3)*survey_mean( pc_consumo_gr_dom_frutas,  na.rm = TRUE),
              carnes_pescados = (365/10^3)*survey_mean( pc_consumo_gr_dom_carnes_e_pescados,  na.rm = TRUE),
              aves_ovos = (365/10^3)*survey_mean( pc_consumo_gr_dom_aves_e_ovos,  na.rm = TRUE),
              leites_derivados = (365/10^3)*survey_mean( pc_consumo_gr_dom_leites_e_derivados,  na.rm = TRUE),
              panificados = (365/10^3)*survey_mean( pc_consumo_gr_dom_panificados,  na.rm = TRUE),
              oleos_gorduras = (365/10^3)*survey_mean( pc_consumo_gr_dom_oleos_e_gorduras,  na.rm = TRUE),
              bebidas_infusoes = (365/10^3)*survey_mean( pc_consumo_gr_dom_bebidas_e_infusoes,  na.rm = TRUE),
              elatados = (365/10^3)*survey_mean( pc_consumo_gr_dom_enlatados,  na.rm = TRUE),
              sal = (365/10^3)*survey_mean( pc_consumo_gr_dom_sal,  na.rm = TRUE),
              preparados = (365/10^3)*survey_mean( pc_consumo_gr_dom_alimentos_preparados,  na.rm = TRUE)) %>%
    select(-(ends_with("_se"))) %>%
    as.data.frame()
  
  
  # Formato final 
  df_t <- t(df) %>% as.data.frame()
  # df_t <- df_t[-1,] %>% as.data.frame()
  colnames(df_t) <- c(1:ncol(df_t))
  
  # Vamos então fazer a conta do percentual do gasto com cada grupo de alimento
  col_sums <- colSums(df_t)
  
  # Create a new data frame for percentages
  percentage_df <- as.data.frame(apply(df_t, 1, function(col) col / col_sums )) # * 100
  percentage_df <- round(percentage_df,2)
  
  df_export <- cbind(df_t, percentage_df)
  
  df_export[,1] <- round(df_export[,1], 2)
  
  names(df_export) <- c('Consumo kg por ano', '% do total')
  
  return(df_export)
  
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

f_inseguranca_2018_br <- function(pof_br) {
  
  aux <- pof_br %>% 
    group_by(ESTRATO_POF) %>% 
    summarise(n = n()) %>% 
    filter(n > 1 )
  
  cat('\nData cleanning for the sample')
  pof_br <- pof_br %>% filter(ESTRATO_POF %in% aux$ESTRATO_POF )
  
  cat('\nDefining as survey design')
  df <- as_survey(svydesign( ids = ~COD_UPA, 
                             strata = ~ESTRATO_POF,
                             weights = ~peso_final_fam, 
                             data = pof_br, 
                             check.strata = TRUE))

  
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

f_gasto_alimentacao_estrato_2018 <- function(df, estrato, region_name, year) {
  
  df <- filter(df, ESTRATO_POF %in% estrato)
  
  df_tab <- df %>%
    summarise(renda_dom_pc_disp = survey_mean( PC_RENDA_DISP,  na.rm = TRUE),
              despesa_alimentos = survey_mean( pc_desp_nivel3_Alimentacao,  na.rm = TRUE),
              despesa_alimentos_no_dom = survey_mean( pc_desp_nivel4_Alimentacao_no_dom,  na.rm = TRUE),
              despesa_alimentos_fora_dom = survey_mean( pc_desp_nivel4_Alimentacao_fora_dom,  na.rm = TRUE),
              despesa_habitacao = survey_mean( pc_desp_nivel3_Habitacao,  na.rm = TRUE),
              despesa_vestuario = survey_mean( pc_desp_nivel3_Vestuario,  na.rm = TRUE),
              despesa_transporte = survey_mean( pc_desp_nivel3_Transporte,  na.rm = TRUE),
              despesa_higiene = survey_mean( pc_desp_nivel3_Higiene,  na.rm = TRUE),
              despesa_saude = survey_mean( pc_desp_nivel3_Saude,  na.rm = TRUE),
              despesa_educacao = survey_mean( pc_desp_nivel3_Educacao,  na.rm = TRUE),
              despesa_cultura = survey_mean( pc_desp_nivel3_Lazer_e_cult,  na.rm = TRUE),
              despesa_fumo = survey_mean( pc_desp_nivel3_Fumo,  na.rm = TRUE),
              despesa_serv_pessoais = survey_mean( pc_desp_nivel3_Serv_pessoais,  na.rm = TRUE),
              despesa_diversas = survey_mean( pc_desp_nivel3_diversos,  na.rm = TRUE)
    ) %>%
    mutate(despesa_consumo = despesa_alimentos + despesa_habitacao + despesa_vestuario + despesa_transporte +
             despesa_higiene + despesa_saude + despesa_educacao + despesa_cultura + despesa_fumo +
             despesa_serv_pessoais + despesa_diversas) %>%
    mutate(perc_aliment_orcamento = round(100*despesa_alimentos/despesa_consumo),
           perc_aliment_no_domicilio = round(100*despesa_alimentos_no_dom/despesa_alimentos)) %>%
    mutate(unidade_analise = region_name, pof_year = year) %>%
    select(unidade_analise, pof_year, perc_aliment_orcamento, perc_aliment_no_domicilio) %>%
    
    as.data.frame()
  
  return(df_tab)
  
  
  
}


f_gasto_alimentacao_br_2018 <- function(pof_br, region_name) {
  
  
  aux <- pof_br %>% 
    group_by(ESTRATO_POF) %>% 
    summarise(n = n()) %>% 
    filter(n > 1 )
  
  cat('\nData cleanning for the sample')
  pof_br <- pof_br %>% filter(ESTRATO_POF %in% aux$ESTRATO_POF )
  
  cat('\nDefining as survey design')
  pof_svy <- as_survey(svydesign(ids = ~COD_UPA, 
                                 strata = ~ESTRATO_POF,
                                 weights = ~peso_final_fam, 
                                 data = pof_br, 
                                 check.strata = TRUE))
  
  df_tab <- pof_svy %>%
    summarise(renda_dom_pc_disp = survey_mean( PC_RENDA_DISP,  na.rm = TRUE),
              despesa_alimentos = survey_mean( pc_desp_nivel3_Alimentacao,  na.rm = TRUE),
              despesa_alimentos_no_dom = survey_mean( pc_desp_nivel4_Alimentacao_no_dom,  na.rm = TRUE),
              despesa_alimentos_fora_dom = survey_mean( pc_desp_nivel4_Alimentacao_fora_dom,  na.rm = TRUE),
              despesa_habitacao = survey_mean( pc_desp_nivel3_Habitacao,  na.rm = TRUE),
              despesa_vestuario = survey_mean( pc_desp_nivel3_Vestuario,  na.rm = TRUE),
              despesa_transporte = survey_mean( pc_desp_nivel3_Transporte,  na.rm = TRUE),
              despesa_higiene = survey_mean( pc_desp_nivel3_Higiene,  na.rm = TRUE),
              despesa_saude = survey_mean( pc_desp_nivel3_Saude,  na.rm = TRUE),
              despesa_educacao = survey_mean( pc_desp_nivel3_Educacao,  na.rm = TRUE),
              despesa_cultura = survey_mean( pc_desp_nivel3_Lazer_e_cult,  na.rm = TRUE),
              despesa_fumo = survey_mean( pc_desp_nivel3_Fumo,  na.rm = TRUE),
              despesa_serv_pessoais = survey_mean( pc_desp_nivel3_Serv_pessoais,  na.rm = TRUE),
              despesa_diversas = survey_mean( pc_desp_nivel3_diversos,  na.rm = TRUE)
    ) %>%
    mutate(despesa_consumo = despesa_alimentos + despesa_habitacao + despesa_vestuario + despesa_transporte +
             despesa_higiene + despesa_saude + despesa_educacao + despesa_cultura + despesa_fumo +
             despesa_serv_pessoais + despesa_diversas) %>%
    mutate(perc_aliment_orcamento = round(100*despesa_alimentos/despesa_consumo),
           perc_aliment_no_domicilio = round(100*despesa_alimentos_no_dom/despesa_alimentos)) %>%
    mutate(unidade_analise = region_name, pof_year = 2018) %>%
    select(unidade_analise, pof_year, perc_aliment_orcamento, perc_aliment_no_domicilio) %>%
    
    as.data.frame()
  
  return(df_tab)
  
  
  
}


# Função para a POF de 2002 a 2008

cat('\n\n Getting the data .... ')

pof_2002 <- read.csv( file.path(dados_brutos_path,'POF/POF 2002-2003/raw/pof_2002.csv') ) %>% clean_names()

cat('\nTabela wide da Pof 2002:', nrow(pof_2002), 'linhas...')

pof_2008 <- read.csv(file.path(dados_brutos_path,'POF/POF 2008-2009/raw/pof_2008.csv')) %>% clean_names()

cat('\nTabela wide da Pof 2008:', nrow(pof_2008), 'linhas...')

# Adicionando a variável de ano
pof_2002$ano <- 2002
pof_2008$ano <- 2008

# Vamos renomear algumas variaveis de 2008 para facilitar
pof_2008 <- pof_2008 %>% 
  rename(uf = cod_uf, 
         estrato = cod_domc,
         n_morador = qtd_morador_domc,
         fator = fator_expansao2,
         domcl = cod_tipo_domc,
         quant_uc = qtd_uc,
         renda_per_capita = renda_total_per_capita)


# Juntando as duas POFs
pof <- bind_rows( pof_2002, pof_2008 )

# Ficando apenas com o painel completo 
rm(pof_2002, pof_2008); gc()


# Criação da variável estrato igual em 2018
pof <- pof %>% 
  mutate( estrato_pof = case_when(
    estrato < 10 ~ paste0(uf, "0", estrato),
    estrato >= 10 ~ paste0(uf, estrato)
  )) %>% 
  mutate(estrato_pof = as.numeric(estrato_pof),
         peso_fam = n_morador*fator, 
         COD_UPA = as.numeric(paste0(uf, estrato_pof,
                                     tipo_reg, domcl, id_dom, quant_uc))) 

# Vamos ficar apenas com as variáveis necessárias à tabela (todos os intens de consumo devem ser incluidos)

pof_res <- pof %>% select(ano, uf, estrato_pof,
                          tipo_reg, domcl, 
                          id_dom, quant_uc,
                          peso_fam, COD_UPA,
                          despesas_mensais_totais_per_capita, 
                          despesas_mensais_alimentacao_per_capita,
                          despesas_mensais_takein_food_per_capita,
                          despesas_mensais_takeout_food_per_capita,
                          despesas_mensais_moradia_per_capita,
                          despesas_mensais_vestuario_per_capita,
                          despesas_mensais_transporte_per_capita, 
                          despesas_mensais_higiene_per_capita,
                          despesas_mensais_saude_per_capita,
                          despesas_mensais_educacao_per_capita,
                          despesas_mensais_lazer_per_capita,
                          despesas_mensais_fumo_per_capita,
                          despesas_mensais_servicos_pessoais_per_capita,
                          despesas_mensais_despesas_diversas_per_capita,
                          renda_per_capita)
rm(pof)


## Functions for 2002 and 2008 ----
f_gasto_alimentacao_estrato_2002_2008 <- function(df, estrato, region_name, year) {
  
  df <- filter(df, estrato_pof %in% estrato)
  
  df_tab <- df %>%
    summarise(renda_dom_pc_disp = survey_mean( renda_per_capita,  na.rm = TRUE),
              despesa_alimentos = survey_mean( despesas_mensais_alimentacao_per_capita,  na.rm = TRUE),
              despesa_alimentos_no_dom = survey_mean( despesas_mensais_takein_food_per_capita,  na.rm = TRUE),
              despesa_alimentos_fora_dom = survey_mean( despesas_mensais_takeout_food_per_capita,  na.rm = TRUE),
              despesa_habitacao = survey_mean( despesas_mensais_moradia_per_capita,  na.rm = TRUE),
              despesa_vestuario = survey_mean( despesas_mensais_vestuario_per_capita,  na.rm = TRUE),
              despesa_transporte = survey_mean( despesas_mensais_transporte_per_capita,  na.rm = TRUE),
              despesa_higiene = survey_mean( despesas_mensais_higiene_per_capita,  na.rm = TRUE),
              despesa_saude = survey_mean( despesas_mensais_saude_per_capita,  na.rm = TRUE),
              despesa_educacao = survey_mean( despesas_mensais_educacao_per_capita,  na.rm = TRUE),
              despesa_cultura = survey_mean( despesas_mensais_lazer_per_capita,  na.rm = TRUE),
              despesa_fumo = survey_mean( despesas_mensais_fumo_per_capita,  na.rm = TRUE),
              despesa_serv_pessoais = survey_mean( despesas_mensais_servicos_pessoais_per_capita,  na.rm = TRUE),
              despesa_diversas = survey_mean( despesas_mensais_despesas_diversas_per_capita,  na.rm = TRUE)
    ) %>%
    mutate(despesa_consumo = despesa_alimentos + despesa_habitacao + despesa_vestuario + despesa_transporte +
           despesa_higiene + despesa_saude + despesa_educacao + despesa_cultura + despesa_fumo +
           despesa_serv_pessoais + despesa_diversas) %>%
    mutate(perc_aliment_orcamento = round(100*despesa_alimentos/despesa_consumo),
           perc_aliment_no_domicilio = round(100*despesa_alimentos_no_dom/despesa_alimentos)) %>%
    mutate(unidade_analise = region_name, pof_year = year) %>%
    select(unidade_analise, pof_year, perc_aliment_orcamento, perc_aliment_no_domicilio) %>%
    
    as.data.frame()
  
  return(df_tab)
  
  
  
}

# mesma coisa do estrato, mas aqui para o Brasil
f_gasto_alimentacao_br_2002_2008 <- function(pof_br, region_name, year) {
  
  cat('\nFiltering the year')
  
  pof_br <- pof_br %>% filter(ano == year)
  
  aux <- pof_br %>% 
    group_by(estrato_pof) %>% 
    summarise(n = n()) %>% 
    filter(n > 1 )
  
  cat('\nData cleanning for the sample')
  pof_br <- pof_br %>% filter(estrato_pof %in% aux$estrato_pof )
  
  cat('\nDefining as survey design')
  pof_svy <- as_survey(svydesign(ids = ~COD_UPA, 
                                 strata = ~estrato_pof,
                                 weights = ~peso_fam, 
                                 data = pof_br, 
                                 check.strata = TRUE))
  
  df_tab <- pof_svy %>%
    summarise(renda_dom_pc_disp = survey_mean( renda_per_capita,  na.rm = TRUE),
              despesa_alimentos = survey_mean( despesas_mensais_alimentacao_per_capita,  na.rm = TRUE),
              despesa_alimentos_no_dom = survey_mean( despesas_mensais_takein_food_per_capita,  na.rm = TRUE),
              despesa_alimentos_fora_dom = survey_mean( despesas_mensais_takeout_food_per_capita,  na.rm = TRUE),
              despesa_habitacao = survey_mean( despesas_mensais_moradia_per_capita,  na.rm = TRUE),
              despesa_vestuario = survey_mean( despesas_mensais_vestuario_per_capita,  na.rm = TRUE),
              despesa_transporte = survey_mean( despesas_mensais_transporte_per_capita,  na.rm = TRUE),
              despesa_higiene = survey_mean( despesas_mensais_higiene_per_capita,  na.rm = TRUE),
              despesa_saude = survey_mean( despesas_mensais_saude_per_capita,  na.rm = TRUE),
              despesa_educacao = survey_mean( despesas_mensais_educacao_per_capita,  na.rm = TRUE),
              despesa_cultura = survey_mean( despesas_mensais_lazer_per_capita,  na.rm = TRUE),
              despesa_fumo = survey_mean( despesas_mensais_fumo_per_capita,  na.rm = TRUE),
              despesa_serv_pessoais = survey_mean( despesas_mensais_servicos_pessoais_per_capita,  na.rm = TRUE),
              despesa_diversas = survey_mean( despesas_mensais_despesas_diversas_per_capita,  na.rm = TRUE)
    ) %>%
    mutate(despesa_consumo = despesa_alimentos + despesa_habitacao + despesa_vestuario + despesa_transporte +
             despesa_higiene + despesa_saude + despesa_educacao + despesa_cultura + despesa_fumo +
             despesa_serv_pessoais + despesa_diversas) %>%
    mutate(perc_aliment_orcamento = round(100*despesa_alimentos/despesa_consumo),
           perc_aliment_no_domicilio = round(100*despesa_alimentos_no_dom/despesa_alimentos)) %>%
    mutate(unidade_analise = region_name, pof_year = year) %>%
    select(unidade_analise, pof_year, perc_aliment_orcamento, perc_aliment_no_domicilio) %>%
    
    as.data.frame()
  
  return(df_tab)
  
  
  
}

f_xtile_filter_2002_2008 <- function(df, variavel, var_peso, nova_var, n, estrato) {
  
  x <- 1 / n
  
  df <- filter(df, estrato_pof %in% estrato)
  
  percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
  df[[nova_var]] <- cut(df[[variavel]], 
                        breaks = percentis,
                        labels = F, 
                        include.lowest = T, 
                        na.pass = TRUE)
  return(df)
  
}

# Calculo da renda pc 
f_inc_dist_filter_2002_2008 <- function(pof_svy, variavel, var_peso, nova_var, n, estrato) {
  
  options(warn = -1)
  
  # Cálculo da Renda dom pc total
  aux <- pof_svy %>%
    summarise(renda_dom_pc_disp = survey_mean( get(variavel),  na.rm = TRUE)) %>%
    select(renda_dom_pc_disp) %>%
    as.data.frame()
  
  df <- pof_svy$variables
  
  x <- 1 / n
  
  df <- filter(df, estrato_pof %in% estrato)
  
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
              despesa_alimentos_no_dom = survey_mean( despesas_mensais_takein_food_per_capita,  na.rm = TRUE),
              despesa_alimentos_fora_dom = survey_mean( despesas_mensais_takeout_food_per_capita,  na.rm = TRUE),
              despesa_habitacao = survey_mean( despesas_mensais_moradia_per_capita,  na.rm = TRUE),
              despesa_vestuario = survey_mean( despesas_mensais_vestuario_per_capita,  na.rm = TRUE),
              despesa_transporte = survey_mean( despesas_mensais_transporte_per_capita,  na.rm = TRUE),
              despesa_higiene = survey_mean( despesas_mensais_higiene_per_capita,  na.rm = TRUE),
              despesa_saude = survey_mean( despesas_mensais_saude_per_capita,  na.rm = TRUE),
              despesa_educacao = survey_mean( despesas_mensais_educacao_per_capita,  na.rm = TRUE),
              despesa_cultura = survey_mean( despesas_mensais_lazer_per_capita,  na.rm = TRUE),
              despesa_fumo = survey_mean( despesas_mensais_fumo_per_capita,  na.rm = TRUE),
              despesa_serv_pessoais = survey_mean( despesas_mensais_servicos_pessoais_per_capita,  na.rm = TRUE),
              despesa_diversas = survey_mean( despesas_mensais_despesas_diversas_per_capita,  na.rm = TRUE)
    ) %>%
    mutate(despesa_consumo = despesa_alimentos + despesa_habitacao + despesa_vestuario + despesa_transporte +
             despesa_higiene + despesa_saude + despesa_educacao + despesa_cultura + despesa_fumo +
             despesa_serv_pessoais + despesa_diversas) %>%
    select({{percentis}}, despesa_total, despesa_consumo, despesa_alimentos, despesa_alimentos_no_dom, despesa_alimentos_fora_dom, despesa_habitacao, despesa_transporte, despesa_saude ) %>%
    as.data.frame()
  
  return(df)
  
}

cat("Let's go to the tables")


# Funcao compila POF -----

compila_tabelas_pof_f <- function(dimensoes, df_pof_2002_2008, df_pof_2018) {
  
  
  ## POF 2002 e 2008 ----
  
  # fonte
  # https://github.com/CaoBittencourt/Data-Science-Python/blob/main/pof_2008.rar
  
  anos <- c(2002,2008)
  
  lst.years <- list()
  lst.food.expend <- list()
  
  expand.grid(anos, dimensoes)
  
  for (y in anos) {
    
    cat('\nBase para o ano', y)
    
    lst.tab <- list()
    
    for (d in dimensoes) {
      
      cat('\n  Dimensão', d, paste(Sys.time()))
      
      # Tabelas 
      
      # Caso tenha apenas uma observação, vamos duplicá-la para não perder
      
      # Incluindo a informação de decil e já filtrando para o estrato desejado
      pof_estrato <- f_xtile_filter_2002_2008(df_pof_2002_2008[df_pof_2002_2008$ano == y,], 
                                              variavel = "renda_per_capita",
                                              var_peso = "peso_fam",
                                              nova_var = "decis", 
                                              n = 10, 
                                              estrato = get(d) )  %>% # 
        filter(!is.na(decis))
      
      aux <- pof_estrato %>% 
        group_by(estrato_pof) %>% 
        summarise(n = n()) %>% 
        filter(n > 1 )
      
      pof_estrato <- pof_estrato %>% filter(estrato_pof %in% aux$estrato_pof )
      
      # Definindo o data.frame como objeto amostral
      # Transformando o arquivo em survey  
      pof_svy <- as_survey(svydesign(ids = ~COD_UPA, 
                                     strata = ~estrato_pof,
                                     weights = ~peso_fam, 
                                     data = pof_estrato, 
                                     check.strata = TRUE))
      
      
      # Calculo do percentual gasto com alimento sem ser por decis
      
      cat('\n    Tabela de gastos com alimentos dentro e fora do dom ...')
      
      
      aux2 <- f_gasto_alimentacao_estrato_2002_2008(df = pof_svy, estrato = get(d), region_name = d, year = y)
      
      lst.food.expend[[paste0(aux2$unidade_analise,"_", aux2$pof_year)]] <- aux2
      
      
      cat('\n    Tabela de gastos por grandes grupos por decis ...')
      
      # Renda domiciliar per capita disponível
      tab_renda <- f_inc_dist_filter_2002_2008(pof_svy = pof_svy,
                                               variavel = "renda_per_capita",
                                               var_peso = "peso_fam",
                                               nova_var = "decis",
                                               n = 10,
                                               estrato = get(d) ) # get(d)
      
      t1 <- f_medias_2002_2008(pof_svy, decis)
      pof_svy$variables$um <- 999 # para o total
      t1b <- f_medias_2002_2008(pof_svy, um)
      names(t1b)[1] <- "decis"
      t1b$decis <- 'Total'
      t1 <- rbind(t1, t1b)
      
      # Juntando tabela 1, 2 e 3
      lst.tab[[match(d, dimensoes)]] <-
        tab_renda %>%
        left_join(t1, by = 'decis')
      
      
    }
    
    # nomes para facilitar
    names(lst.tab) <- dimensoes
    
    lst.years[[match(y, anos)]] <- lst.tab
    
  }
  
  names(lst.years) <- c("y_2002", "y_2008")
  
  # Acho que faz sentido reorganizar essa tabela na hora de exportar
  tabela_orcamento_alim <- bind_rows(lst.food.expend)
  
  # Vamos fazer as listas: 
  lst.tab         <- list()
  lst.perc        <- list()
  lst.fora.dom    <- list()
  lst.proc        <- list()
  lst.kcal        <- list()
  lst.reais.kcal  <- list()
  lst.kg          <- list()
  lst.inseguranca <- list()
  lst.food.expend <- list()
  
  ## POF 2018 ----
  
  for (d in dimensoes) {
    
    cat('\n  Dimensão', d, paste(Sys.time()))
    
    # Tabelas 
    
    # Caso tenha apenas uma observação, vamos duplicá-la para não perder
    
    # Incluindo a informação de decil e já filtrando para o estrato desejado
    pof_estrato <- f_xtile_filter_2018(df_pof_2018, 
                                       variavel = "PC_RENDA_DISP",
                                       var_peso = "peso_final_fam",
                                       nova_var = "decis", 
                                       n = 10, 
                                       estrato = get(d), 
                                       estrato_var_name = ESTRATO_POF )  %>% # 
      filter(!is.na(decis))
    
    aux <- pof_estrato %>% 
      group_by(ESTRATO_POF) %>% 
      summarise(n = n()) %>% 
      filter(n > 1 )
    
    pof_estrato <- pof_estrato %>% filter(ESTRATO_POF %in% aux$ESTRATO_POF )
    
    # Definindo o data.frame como objeto amostral
    # Transformando o arquivo em survey  
    pof_svy <- as_survey(svydesign(ids = ~COD_UPA, 
                                   strata = ~ESTRATO_POF,
                                   weights = ~peso_final_fam, 
                                   data = pof_estrato, 
                                   check.strata = TRUE))
    
    
    # Renda domiciliar per capita disponível
    tab_renda <- f_inc_dist_filter_2018(pof_svy = pof_svy, 
                                        variavel = "PC_RENDA_DISP", 
                                        var_peso = "peso_final_fam",
                                        nova_var = "decis", 
                                        n = 10, 
                                        estrato = get(d),
                                        estrato_var_name = ESTRATO_POF ) # get(d)
    
    pof_svy$variables$um <- 999 # para o total
    
    cat('\n     Tabela 1 - grandes grupos', paste(Sys.time()))
    
    # Bloco de gastos por grandes grupos
    t1 <- f_medias_2018(pof_svy, decis)
    t1b <- f_medias_2018(pof_svy, um)
    names(t1b)[1] <- "decis"
    t1b$decis <- 'Total'
    t1 <- rbind(t1, t1b)
    t1 <- left_join(tab_renda, t1, by = 'decis')
    
    # Tabela 1
    lst.tab[[match(d, dimensoes)]] <- t1
    
    
    cat('\n     Tabela 2 - % gasto em alimentos', paste(Sys.time()))
    
    # Percentual gasto em tipos de alimentos
    t2  <- f_alimentos_no_dom_2018(pof_svy, decis)
    t2b <- f_alimentos_no_dom_2018(pof_svy, um) #  %>% t()
    t2  <- left_join(t2, t2b, by = 'grupo' )
    
    # Tabela 2
    lst.perc[[match(d, dimensoes)]] <- t2
    
    
    cat('\n     Tabela 3 - % gasto em alimentos fora do dom', paste(Sys.time()))
    
    # Percentual gasto em tipos de alimentos fora do dom
    t3  <- f_alimentos_fora_dom_2018(pof_svy, decis)
    t3b <- f_alimentos_fora_dom_2018(pof_svy, um)
    t3  <- left_join(t3, t3b, by = 'grupo' )
    
    # Tabela 3
    lst.fora.dom[[match(d, dimensoes)]] <- t3
    
    
    cat('\n     Tabela 4 - % gasto por tipo de processamento', paste(Sys.time()))
    
    # Percentual gasto por tipo de processamento
    t4 <- f_tipo_process_2018(pof_svy, decis)
    t4b <- f_tipo_process_2018(pof_svy, um)
    t4  <- left_join(t4, t4b, by = 'grupo' )
    
    # Tabela 4
    lst.proc[[match(d, dimensoes)]] <- t4
    
    
    cat('\n     Tabela 5 - Kcal consumido', paste(Sys.time()))
    
    # Kcal consumido
    t5 <- f_consumo_kcal_2018(pof_svy, decis)
    t5b <- f_consumo_kcal_2018(pof_svy, um)
    t5  <- left_join(t5, t5b, by = 'grupo' )
    
    # Tabela 5
    lst.kcal[[match(d, dimensoes)]] <- t5
    
    t6 <- round(t4[-1]/t5[-1],2)
    t6 <- cbind(t4[,1], t6)
    
    cat('\n     Tabela 6 - R$/Kcal consumido', paste(Sys.time()))
    
    # Tabela 6: R$/Kcal consumido
    lst.reais.kcal[[match(d, dimensoes)]] <- t6
    
    
    # Tabela 7: Insegurança alimentar e intersecções
    lst.inseguranca[[match(d, dimensoes)]] <- f_inseguranca_2018(pof_svy)
    
    
    ## Kg ----
    
    cat('\n     Tabela 7 - Kg anuais consumidos', paste(Sys.time()))
    
    # Tabela com AQUISIÇAO DE PRODUTOS EM KG - para isso temos que reprocessar a POF
    
    # Kcal consumido
    t7 <- f_alimentos_kg_2018(pof_svy)
    
    t7$Grupos <- rownames(t7)
    
    t7 <- t7 %>% select(Grupos, `Consumo kg por ano`, `% do total`)
    
    # Tabela 7
    lst.kg[[match(d, dimensoes)]] <- t7 
    
    
    # Gastos com alimentação em relação ao orçamento
    t8 <- f_gasto_alimentacao_estrato_2018(df = pof_svy, estrato = get(d), region_name = d, year = 2018)
    
    lst.food.expend[[match(d, dimensoes)]] <- t8
    
  }
  
  # nomes para facilitar
  names(lst.tab)         <- dimensoes
  names(lst.perc)        <- dimensoes
  names(lst.fora.dom)    <- dimensoes
  names(lst.proc)        <- dimensoes
  names(lst.kcal)        <- dimensoes
  names(lst.reais.kcal)  <- dimensoes
  names(lst.inseguranca) <- dimensoes
  names(lst.kg)          <- dimensoes
  
  
  # Acho que faz sentido reorganizar essa tabela na hora de exportar
  tabela_orcamento_alim2018 <- bind_rows(lst.food.expend)
  
  # Tabela consolidada de alimentos
  tabela_orcamento_alim <- rbind(tabela_orcamento_alim, tabela_orcamento_alim2018)
  
  tabela_orcamento_alim <- tabela_orcamento_alim %>% arrange(unidade_analise, pof_year)
  
  # Exportando as tableas ----
  
  # TEMOS QUE JUNTAR COM AS DE 2018
  
  # Feito isso vamos exportar as tabelas (de 2002 a 2018)
  
  leia.me <- data.frame( identificador = c('01','02'),
                         nivel_geografico = c('Informações COM rural',
                                              'Informações SEM rural'),
                         unidade = c('Todas as unidades apresentadas correspondem a valores per capita calculados para o domicílio'))
  
  #### tabela_orcamento_alim precisar ser exportada! 
  
  sheets <- list("leia_me" = leia.me,
                 
                 "tabela_orcamento_alim" = tabela_orcamento_alim,
                 
                 "desp_01_2002" = lst.years$y_2002$estrato_COM_rural, 
                 "desp_02_2002" = lst.years$y_2002$estrato_SEM_rural,
                 
                 "desp_01_2008" = lst.years$y_2008$estrato_COM_rural, 
                 "desp_02_2008" = lst.years$y_2008$estrato_SEM_rural,
                 
                 # Bloco 1: Despesas nos grandes grupos
                 "desp_grupo_01_2018" = lst.tab$estrato_COM_rural, 
                 "desp_grupo_02_2018" = lst.tab$estrato_SEM_rural,
                 
                 # Bloco 2: share por tipo de alimento
                 "desp_alim_dom_01_2018" = as.data.frame(lst.perc$estrato_COM_rural), 
                 "desp_alim_dom_02_2018" = lst.perc$estrato_SEM_rural,
                 
                 # Bloco 3: share por tipo de alimento FORA do domicilio
                 "desp_alim_fora_dom_01_2018" = lst.fora.dom$estrato_COM_rural, 
                 "desp_alim_fora_dom_02_2018" = lst.fora.dom$estrato_SEM_rural,
                 
                 # Bloco 4: share por tipo de processamento
                 "desp_alim_tipo_proc_01_2018" = lst.proc$estrato_COM_rural, 
                 "desp_alim_tipo_proc_02_2018" = lst.proc$estrato_SEM_rural,
                 
                 # consumo em kg por tipo de alimento
                 "consumo_kg_01_2018" = lst.kg$estrato_COM_rural, 
                 "consumo_kg_02_2018" = lst.kg$estrato_SEM_rural,
                 
                 # Bloco 5: share por tipo de processamento
                 "consumo_perc_kcal_01_2018" = lst.kcal$estrato_COM_rural, 
                 "consumo_perc_kcal_02_2018" = lst.kcal$estrato_SEM_rural,
                 
                 # Bloco 6: share por tipo de processamento
                 "reais_por_kcal_01_2018" = lst.reais.kcal$estrato_COM_rural, 
                 "reais_por_kcal_02_2018" = lst.reais.kcal$estrato_SEM_rural,
                 
                 # Bloco 7: insegurança alimentar
                 "inseg_alim_01_2018" = lst.inseguranca$estrato_COM_rural, 
                 "inseg_alim_02_2018" = lst.inseguranca$estrato_SEM_rural
                 
                 
  )
  
  
  return(sheets); gc()
  
}










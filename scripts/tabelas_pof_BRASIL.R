
# ------------------------------------------------------------------------- # 

# DADOS DA POF: tabelas consolidadas a nível Brasil

# ------------------------------------------------------------------------- # 

# RM, Curitiba e demais regiões

# Script consolidado com os dados da POF


## Funções necessárias para as tabelas ----

result_path <- 'F:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF' # sempre checar se o caminho se mantém

estratos_path <- 'E:/Drive/BASES DE DADOS BRUTOS/POF/POF 2017-2018/Microdados/Documentacao_20221226/Estratos POF 2017-2018.xls' # sempre checar se o caminho se mantém

# Caminho dos scripts na máquina local
git_path <- 'C:/Users/user/Documents/Projetos_github/escolhas-scripts-db/'

# Vamos rodar as funções no Git
source(file.path( git_path, "scripts/funcoes_pof.R" ), encoding = 'UTF-8')

inicial <- Sys.time()


# PAREI AQUI --------------------------------------------------------------

library(readxl)

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

estratos <- read_excel(path = estratos_path, sheet = 'Plan3')

estratos$capital_list <- list()
estratos$rm_list <- list()
estratos$resto_uf_list <- list()
estratos$rural_list <- list()

lst.estratos <- list()

for (linha in 1:nrow(estratos)) {
  
  estratos_aux <- get_estratos_f(estrato = estratos$capital[linha])
  estratos_capital_ref <- data.frame(uf = estratos$UF[linha], regiao = 'capital', estratos = estratos_aux)
  
  estratos_aux <- get_estratos_f(estrato = estratos$rm[linha])
  estratos_rm_ref <- data.frame(uf = estratos$UF[linha], regiao = 'rm', estratos = estratos_aux)
    
  estratos_resto_uf_ref <- get_estratos_f(estrato = estratos$resto_uf[linha])
  estratos_resto_uf_ref <- data.frame(uf = estratos$UF[linha], regiao = 'resto_uf_ref', estratos = estratos_aux)
  
  estratos_aux <- get_estratos_f(estrato = estratos$rural[linha])
  estratos_rural_ref <- data.frame(uf = estratos$UF[linha], regiao = 'rural_ref', estratos = estratos_aux)
  
  df_estratos <- bind_rows(estratos_capital_ref, estratos_rm_ref, estratos_resto_uf_ref, estratos_rural_ref)
  
  lst.estratos[[linha]] <- df_estratos
  
  
}


df_estratos <- bind_rows(lst.estratos)



# Estratos Brasil
estratos <- read.xlsx(xlsxFile = file.path(estratos_path),sheet = 'Plan1')


# Arranjos geográficos a serem definidos
estrato_uf_com_rural        <- 4101:4135
estrato_uf_sem_rural        <- 4101:4124
estrato_uf_sem_rm_sem_rural <- 4108:4124
estrato_rm                  <- 4101:4108
estrato_rm_sem_capital      <- 4106:4108
estrato_capital             <- 4101:4105

# Todos os estratos que faremos
dimensoes <- c('estrato_uf_com_rural', 'estrato_uf_sem_rural',
               'estrato_uf_sem_rm_sem_rural', 'estrato_rm',
               'estrato_rm_sem_capital', 'estrato_capital')



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
    pof_estrato <- f_xtile_filter_2002_2008(pof_res[pof_res$ano == y,], 
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


# Tabela do Brasil
aux1 <- f_gasto_alimentacao_br_2002_2008(pof_br = pof_res, region_name = 'Brasil', year = 2002)
aux2 <- f_gasto_alimentacao_br_2002_2008(pof_br = pof_res, region_name = 'Brasil', year = 2008)

# temos que calcular isso para 2018 tbm

# isso vai se juntar com 
tabela_orcamento_alim <- rbind(tabela_orcamento_alim, aux1, aux2)


## POF 2018 ----

# Base de dados consolidadas
pof_2018 <- readRDS( file.path( git_path, "data/pof_fam_wide_2018.RDS") )


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

for (d in dimensoes) {
  
  cat('\n  Dimensão', d, paste(Sys.time()))
  
  # Tabelas 
  
  # Caso tenha apenas uma observação, vamos duplicá-la para não perder
  
  # Incluindo a informação de decil e já filtrando para o estrato desejado
  pof_estrato <- f_xtile_filter_2018(pof_2018, 
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


# Tabela do Brasil
aux3 <- f_gasto_alimentacao_br_2018(pof_br = pof_2018, region_name = 'Brasil')

# Tabela consolidada de alimentos
tabela_orcamento_alim <- rbind(tabela_orcamento_alim, tabela_orcamento_alim2018, aux3)

tabela_orcamento_alim <- tabela_orcamento_alim %>% arrange(unidade_analise, pof_year)


# Vamos fazer o de Insegurança alimentar para o Brasil para comparar

inseg_br <- f_inseguranca_2018_br(pof_2018)


# Exportando as tableas ----

# TEMOS QUE JUNTAR COM AS DE 2018

# Feito isso vamos exportar as tabelas (de 2002 a 2018)

leia.me <- data.frame( identificador = c('01','02', '03', '04','05', '06'),
                       nivel_geografico = c('UF incluindo rural',
                                            'UF SEM incluir rural',
                                            'Regiões da UF fora da RM SEM incluir rural',
                                            'Região Metropolitana (RM)',
                                            'RM exceto capital (Curitiba)',
                                            'Capital (Curitiba)'),
                       unidade = c('Todas as unidades apresentadas correspondem a valores per capita calculados para o domicílio'))

#### tabela_orcamento_alim precisar ser exportada! 

sheets <- list("leia_me" = leia.me,
               
               "tabela_orcamento_alim" = tabela_orcamento_alim,
               
               "desp_01_2002" = lst.years$y_2002$estrato_uf_com_rural, 
               "desp_02_2002" = lst.years$y_2002$estrato_uf_sem_rural,
               "desp_03_2002" = lst.years$y_2002$estrato_uf_sem_rm_sem_rural,
               "desp_04_2002" = lst.years$y_2002$estrato_rm,
               "desp_05_2002" = lst.years$y_2002$estrato_rm_sem_capital,
               "desp_06_2002" = lst.years$y_2002$estrato_capital,
               
               "desp_01_2008" = lst.years$y_2008$estrato_uf_com_rural, 
               "desp_02_2008" = lst.years$y_2008$estrato_uf_sem_rural,
               "desp_03_2008" = lst.years$y_2008$estrato_uf_sem_rm_sem_rural,
               "desp_04_2008" = lst.years$y_2008$estrato_rm,
               "desp_05_2008" = lst.years$y_2008$estrato_rm_sem_capital,
               "desp_06_2008" = lst.years$y_2008$estrato_capital,
               
               # Bloco 1: Despesas nos grandes grupos
               "desp_grupo_01_2018" = lst.tab$estrato_uf_com_rural, 
               "desp_grupo_02_2018" = lst.tab$estrato_uf_sem_rural,
               "desp_grupo_03_2018" = lst.tab$estrato_uf_sem_rm_sem_rural,
               "desp_grupo_04_2018" = lst.tab$estrato_rm,
               "desp_grupo_05_2018" = lst.tab$estrato_rm_sem_capital,
               "desp_grupo_06_2018" = lst.tab$estrato_capital,
               
               # Bloco 2: share por tipo de alimento
               "desp_alim_dom_01_2018" = as.data.frame(lst.perc$estrato_uf_com_rural), 
               "desp_alim_dom_02_2018" = lst.perc$estrato_uf_sem_rural,
               "desp_alim_dom_03_2018" = lst.perc$estrato_uf_sem_rm_sem_rural,
               "desp_alim_dom_04_2018" = lst.perc$estrato_rm,
               "desp_alim_dom_05_2018" = lst.perc$estrato_rm_sem_capital,
               "desp_alim_dom_06_2018" = lst.perc$estrato_capital,
               
               # Bloco 3: share por tipo de alimento FORA do domicilio
               "desp_alim_fora_dom_01_2018" = lst.fora.dom$estrato_uf_com_rural, 
               "desp_alim_fora_dom_02_2018" = lst.fora.dom$estrato_uf_sem_rural,
               "desp_alim_fora_dom_03_2018" = lst.fora.dom$estrato_uf_sem_rm_sem_rural,
               "desp_alim_fora_dom_04_2018" = lst.fora.dom$estrato_rm,
               "desp_alim_fora_dom_05_2018" = lst.fora.dom$estrato_rm_sem_capital,
               "desp_alim_fora_dom_06_2018" = lst.fora.dom$estrato_capital,
               
               # Bloco 4: share por tipo de processamento
               "desp_alim_tipo_proc_01_2018" = lst.proc$estrato_uf_com_rural, 
               "desp_alim_tipo_proc_02_2018" = lst.proc$estrato_uf_sem_rural,
               "desp_alim_tipo_proc_03_2018" = lst.proc$estrato_uf_sem_rm_sem_rural,
               "desp_alim_tipo_proc_04_2018" = lst.proc$estrato_rm,
               "desp_alim_tipo_proc_05_2018" = lst.proc$estrato_rm_sem_capital,
               "desp_alim_tipo_proc_06_2018" = lst.proc$estrato_capital,
               
               # consumo em kg por tipo de alimento
               "consumo_kg_01_2018" = lst.kg$estrato_uf_com_rural, 
               "consumo_kg_02_2018" = lst.kg$estrato_uf_sem_rural,
               "consumo_kg_03_2018" = lst.kg$estrato_uf_sem_rm_sem_rural,
               "consumo_kg_04_2018" = lst.kg$estrato_rm,
               "consumo_kg_05_2018" = lst.kg$estrato_rm_sem_capital,
               "consumo_kg_06_2018" = lst.kg$estrato_capital,
               
               
               # Bloco 5: share por tipo de processamento
               "consumo_perc_kcal_01_2018" = lst.kcal$estrato_uf_com_rural, 
               "consumo_perc_kcal_02_2018" = lst.kcal$estrato_uf_sem_rural,
               "consumo_perc_kcal_03_2018" = lst.kcal$estrato_uf_sem_rm_sem_rural,
               "consumo_perc_kcal_04_2018" = lst.kcal$estrato_rm,
               "consumo_perc_kcal_05_2018" = lst.kcal$estrato_rm_sem_capital,
               "consumo_perc_kcal_06_2018" = lst.kcal$estrato_capital,
               
               
               # Bloco 6: share por tipo de processamento
               "reais_por_kcal_01_2018" = lst.reais.kcal$estrato_uf_com_rural, 
               "reais_por_kcal_02_2018" = lst.reais.kcal$estrato_uf_sem_rural,
               "reais_por_kcal_03_2018" = lst.reais.kcal$estrato_uf_sem_rm_sem_rural,
               "reais_por_kcal_04_2018" = lst.reais.kcal$estrato_rm,
               "reais_por_kcal_05_2018" = lst.reais.kcal$estrato_rm_sem_capital,
               "reais_por_kcal_06_2018" = lst.reais.kcal$estrato_capital,
               
               
               # Bloco 7: insegurança alimentar
               "inseg_alim_01_2018" = lst.inseguranca$estrato_uf_com_rural, 
               "inseg_alim_02_2018" = lst.inseguranca$estrato_uf_sem_rural,
               "inseg_alim_03_2018" = lst.inseguranca$estrato_uf_sem_rm_sem_rural,
               "inseg_alim_04_2018" = lst.inseguranca$estrato_rm,
               "inseg_alim_05_2018" = lst.inseguranca$estrato_rm_sem_capital,
               "inseg_alim_06_2018" = lst.inseguranca$estrato_capital,
               
               "inseg_alim_br_2018" = inseg_br
               
               
               
)


write.xlsx(sheets, file = file.path(result_path, "curitiba_pof_tabelas.xlsx"))



final <- Sys.time()






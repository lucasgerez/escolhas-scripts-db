

# Caminho git
git_path <- 'C:/Users/user/Projetos_GIT/escolhas-scripts-db'

# Vamos rodar as funções no Git
source(file.path( git_path, "scripts/01_criar_funcoes.R" ), encoding = 'UTF-8')

# Base de dados consolidadas
pof_2018 <- readRDS( file.path( git_path, "data/pof_fam_wide_2018.RDS") )

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

# Vamos fazer 3 listas: 
lst.tab      <- list()
lst.perc     <- list()
lst.fora.dom <- list()
lst.proc     <- list()


for (d in dimensoes) {
  
  cat('\n  Dimensão', d, paste(Sys.time()))
  
  # Tabelas 
  
  # Caso tenha apenas uma observação, vamos duplicá-la para não perder
  
  # Incluindo a informação de decil e já filtrando para o estrato desejado
  pof_estrato <- f_xtile_filter_2002_2008(pof_2018, 
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
  tab_renda <- f_inc_dist_filter_2002_2008(pof_svy = pof_svy, 
                                           variavel = "PC_RENDA_DISP", 
                                           var_peso = "peso_final_fam",
                                           nova_var = "decis", 
                                           n = 10, 
                                           estrato = get(d),
                                           estrato_var_name = ESTRATO_POF ) # get(d)
  
  # Bloco de gastos por grandes grupos
  t1 <- f_medias_2018(pof_svy, decis)
  pof_svy$variables$um <- 999 # para o total
  t1b <- f_medias_2018(pof_svy, um)
  names(t1b)[1] <- "decis"
  t1b$decis <- 'Total'
  t1 <- rbind(t1, t1b) 
  t1 <- left_join(tab_renda, t1, by = 'decis') 
  
  # Tabela 1
  lst.tab[[match(d, dimensoes)]] <- t1 
  
  
  # Percentual gasto em tipos de alimentos
  t2  <- f_alimentos_no_dom_2018(pof_svy, decis)
  t2b <- f_alimentos_no_dom_2018(pof_svy, um) #  %>% t()
  t2  <- left_join(t2, t2b, by = 'grupo' )
  
  # Tabela 2
  lst.perc[[match(d, dimensoes)]] <- t2 
  
  
  # Percentual gasto em tipos de alimentos fora do dom
  t3  <- f_alimentos_fora_dom_2018(pof_svy, decis)
  t3b <- f_alimentos_fora_dom_2018(pof_svy, um) 
  t3  <- left_join(t3, t3b, by = 'grupo' )
  
  # Tabela 3
  lst.fora.dom[[match(d, dimensoes)]] <- t3 
  
  
  # Percentual gasto por tipo de processamento
  t4 <- f_tipo_process_2018(pof_svy, decis)
  t4b <- f_tipo_process_2018(pof_svy, um) 
  t4  <- left_join(t4, t4b, by = 'grupo' )
  
  # Tabela 4
  lst.proc[[match(d, dimensoes)]] <- t4 
  
  
}

# nomes para facilitar
names(lst.tab)      <- dimensoes
names(lst.perc)     <- dimensoes
names(lst.fora.dom) <- dimensoes
names(lst.proc)     <- dimensoes

leia.me <- data.frame( identificador = c('01','02', '03', '04','05', '06'),
                       nivel_geografico = c('UF incluindo rural',
                                            'UF SEM incluir rural',
                                            'Regiões da UF fora da RM SEM incluir rural',
                                            'Região Metropolitana (RM)',
                                            'RM exceto capital (Curitiba)',
                                            'Capital (Curitiba)'))

lst.tab$estrato_uf_com_rural
lst.perc$estrato_uf_com_rural
lst.fora.dom$estrato_uf_com_rural
lst.proc$estrato_uf_com_rural



sheets <- list("leia_me" = leia.me,
               
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
               "desp_alim_tipo_proc_06_2018" = lst.proc$estrato_capital
               
               )



library(openxlsx)

# Tabela Final
write.xlsx(sheets, "F:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF/pof_tabelas_2018.xlsx")






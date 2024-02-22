
# Tabelas para as POFs de 2002 e 2008
library(dplyr)
library(janitor)
library(survey)
library(srvyr)
library(openxlsx)


# fonte
# https://github.com/CaoBittencourt/Data-Science-Python/blob/main/pof_2008.rar


pof_2002 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2002-2003/raw/pof_2002.csv') %>% clean_names()

cat('\nTabela wide da Pof 2002:', nrow(pof_2002), 'linhas...')

pof_2008 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2008-2009/raw/pof_2008.csv') %>% clean_names()

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
  
# Vamos ficar apenas com as variáveis que fazer sentido
pof_res <- pof %>% select(ano, uf, estrato_pof,
                          tipo_reg, domcl, 
                          id_dom, quant_uc,
                          peso_fam, COD_UPA,
                          despesas_mensais_totais_per_capita, 
                          despesas_mensais_alimentacao_per_capita,
                          despesas_mensais_moradia_per_capita,
                          despesas_mensais_transporte_per_capita, 
                          despesas_mensais_emprestimos_per_capita,
                          despesas_mensais_saude_per_capita,
                          renda_per_capita)


## Funções necessárias para as tabelas ----

# Caminho git
git_path <- 'C:/Users/user/Projetos_GIT/escolhas-scripts-db'

# Vamos rodar as funções no Git
source(file.path( git_path, "scripts/01_criar_funcoes.R" ), encoding = 'UTF-8')

anos <- c(2002,2008)

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

lst.years <- list()

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
  
  leia.me <- data.frame( identificador = c('01','02', '03', '04','05', '06'),
                         nivel_geografico = c('UF incluindo rural',
                                              'UF SEM incluir rural',
                                              'Regiões da UF fora da RM SEM incluir rural',
                                              'Região Metropolitana (RM)',
                                              'RM exceto capital (Curitiba)',
                                              'Capital (Curitiba)'))
  
  lst.years[[match(y, anos)]] <- lst.tab
  
}

names(lst.years) <- c("y_2002", "y_2008")


# Feito isso vamos exportar as tabelas

leia.me <- data.frame( identificador = c('01','02', '03', '04','05', '06'),
                       nivel_geografico = c('UF incluindo rural',
                                            'UF SEM incluir rural',
                                            'Regiões da UF fora da RM SEM incluir rural',
                                            'Região Metropolitana (RM)',
                                            'RM exceto capital (Curitiba)',
                                            'Capital (Curitiba)'))

sheets <- list("leia_me" = leia.me,
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
               "desp_06_2008" = lst.years$y_2008$estrato_capital
               )


write.xlsx(sheets, "F:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF/pof_tabelas_grandes_grupos_2002_2008.xlsx")











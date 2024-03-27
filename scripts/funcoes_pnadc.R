# ------------------------------------------------------------------- #
#                 PNAD Contínua: Funções necessárias 
# ------------------------------------------------------------------- #


# Funções PNADc ------


# Funcao para leitura e tratamento da pnad
trat_pnad <- function(ano, visita) {
  
  
  # CNAE de interesse para o projeto do escolhas (Talvez para o agroalimentar)
  # Cadeia Agroalimentar: produção agropecuária; 
  # transformação agroindustrial e indústria 
  # alimentar; redes de comercialização e distribuição; e 
  # serviços
  
  # página 23 do relatório parte 1: https://escolhas.org/publicacao/agricultura-urbana-em-belem/
  
  # CNAE 2.0, especificamente as seções,
  # Produção primária ou agropecuária: Seção A (Agricultura, Pecuária, Produção Florestal, Pesca e Aquicultura)
  # Comércio de alimentos: Grupos 46.2 (Com Matérias-primas Agrícolas e Animais 
  # Vivos), 46.3 (Comércio de Produtos Alimentícios, Bebidas e Fumos), 47.1 
  # (Supermercado e Hipermercado) e 56.1 (Comércio Ambulante e Feiras);
  # Serviços alimentares: Divisão 56 (Alimentação) da Seção I (Alojamento e Alimentação)
  
  cat('\nSelecionando as CNAEs de interesse ...')
  
  cnae1 <- data.frame(descricao = rep('Produção agropecuária. extrativa florestal pesca e aquicultura',32),
                      cnae = c('01101','01102','01103',
                               '01104','01105','01106',
                               '01107','01108','01109',
                               '01110','01111','01112',
                               '01113','01114','01115',
                               '01116','01117','01118',
                               '01119','01201','01202',
                               '01203','01204','01205',
                               '01206','01207','01208',
                               '01209','01401','01402',
                               '01500','01999')) 
  
  cnae2 <- data.frame(descricao = rep('Comércio', 4), 
                      cnae = c('48020', '48030', '48080', '48100'))
  
  cnae3 <- data.frame(descricao = rep('Serviços de alimentação', 3),
                      cnae = c('56011','56012','56020'))
  
  
  cnae4 <- data.frame(descricao = c(rep('Befeciamento industrial', 10)),
                      cnae = c('10010','10021','10022','10030','10091','10092','10093','10099','11000','12000'))
  
  
  df_cnae <- rbind(cnae1,cnae2,cnae3,cnae4); rm(cnae1,cnae2,cnae3,cnae4)
  
  cat('\nLendo a PNAD ...')
  
  # Etapa 1: load da base
  pnad <- readRDS( file.path( pnad.path, paste0("pnad_anual_", ano, "_visita", visita, ".RDS")) )
  
  
  # Vamos usar a documentação de 2012 aqui
  
  # Etapa 1: vamos ficar apenas com as variáveis de interesse para diminuir o consumo de memória
  
  # Essas variáveis podem mudar de ano para ano (precisamos checar isso)
  variaveis <- c("UF", "Capital", "RM_RIDE", "UPA", "Estrato",
                 "V1008", # Número de seleção do domicílio
                 "V1014", # Painel
                 "V1022", # Situação do domicílio
                 "V1023", # Tipo de área (identifica se é capital, resto da RM, resto do estado)
                 "V1032", # Peso do domicílio e das pessoas com calibração
                 "V2001", # Número de pessoas no domicílio
                 "V2005", # Condição no domicílio (responsável é o um)
                 "V2007", # Sexo (1= MASC; 2 = FEM)
                 "V2009", # idade do morador
                 "V2010", # Cor ou raça
                 "V4013", # CNAE ativ principal
                 "VD3004", # Escolaridade 
                 "VD4001", # PEA
                 "VD4002", # Ocupadas e não ocupadas
                 # "VD4004", # Subocupadas
                 "VD4005", # Desalentadas
                 "VD4008", # Tipo de ocupação
                 "VD4009", # Posição na ocupação
                 "VD4010", # Atividade do Estabelecimento
                 "VD4011", # Atividade dos trabalhadores
                 "VD4013", # horas trabalhadas (todos os trabalhos)
                 "VD4016", # Rendimento mensal habitual do trabalho principal
                 "VD4019", # Rendimento mensal habitual todos os trabalhos 
                 "VD5007", # Renda dom total (habitual)
                 "VD5008" # Renda dom pc (habitual)
  )
  
  # Para 2020 e 2021 não temos VD5007 VD5008
  if (ano %in% 2020:2021) { 
    cat('\nPara 2020 e 2021 não temos VD5007 e VD5008. Utilizaremos VDI5007 e VDI5008, corrigindo o 99999999 para NA')
    
    # Retirando os casos de renda dom com info 99999999
    pnad <- pnad %>% 
      mutate(VD5007 = ifelse(VDI5007 == 99999999, NA,VDI5007),
             VD5008 = ifelse(VDI5008 == 99999999, NA,VDI5008))
      
    
    }
  
  
  
  
  # Tal como na POF, vamos montar a nossa base a nível de domicílio e a partir disso fazer as contas
  pnad <- pnad[variaveis]; gc()
  
  
  pnad <- pnad %>% 
    rename(unidade_geo = V1023, 
           num_pessoas_dom = V2001,
           cor_raca = V2010,
           sexo = V2007,
           cond_dom = V2005,
           escolaridade = VD3004,
           peso_dom = V1032,
           cnae = V4013,
           idade = V2009,
           PEA = VD4001,
           ocupadas = VD4002,
           # subocupadas = VD4004, # muda depois de 2017
           desalentadas = VD4005,
           tipo_ocupacap = VD4008,
           atividade_estabelecimentos = VD4010,
           atividade_trabalhadores = VD4011,
           renda_trab_princ = VD4016,
           renda_trab_todas_fontes = VD4019,
           renda_dom_total = VD5007,
           renda_dom_pc = VD5008,
           horas_trabalhadas = VD4013
    ) %>% 
    mutate(sexo = case_when(
      sexo == "Homem" ~ "Masculino",
      sexo == "Mulher" ~ "Feminino", 
      .default = sexo))
  
  
  
  pnad <- pnad %>%
    mutate(setor = case_when( 
      atividade_estabelecimentos %in% c( 'Alojamento e alimentação', 
                                         'Serviços domésticos', 
                                         'Transporte, armazenagem e correio', 
                                         'Construção', 
                                         'Educação, saúde humana e serviços sociais', 
                                         'Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas', 
                                         'Atividades mal definidas' ) ~ 'Serviços',---
        atividade_estabelecimentos %in% c( 'Comércio, reparação de veículos automotores e motocicletas' ) ~ 'Comércio',
      atividade_estabelecimentos %in% c( 'Indústria geral') ~ 'Indústria',
      atividade_estabelecimentos %in% c( 'Agricultura, pecuária, produção florestal, pesca e aquicultura') ~ 'Agricultura',
      atividade_estabelecimentos %in% c( 'Administração pública, defesa e seguridade social') ~ 'Adm. Pública',
      TRUE ~ NA
    ), 
    d_agroalimentar = ifelse(cnae %in% df_cnae$cnae, 1, 0 )) %>% 
    left_join(df_cnae, by = 'cnae'); gc() 
  
  return(pnad)
  
}

# Geração das tabelas específicas para o estudo
tabelas_pnad <- function(estado, unidade_analise, pnad_data, year ) {
  
  cat('\n Tab1 Proporção de pessoas ocupadas')
  
  # Proporção de pessoas ocupadas na RM
  tab1 <- 
    pnad_data %>%
    filter(!is.na(ocupadas)) %>%
    group_by(ocupadas) %>%
    summarise(total = sum(peso_dom)) %>% 
    mutate(prop = round(100*total/sum(total),2) )
  
  
  #
  # TOTAL
  #
  
  cat('\n Tab2 Participação das pessoas brancas na força de trabalho')
  
  # Participação das pessoas brancas na força de trabalho
  tab2 <- 
    pnad_data %>%
    mutate(d_brancos = ifelse(cor_raca == 'Branca', 1, 0)) %>%
    filter(!is.na(ocupadas)) %>%
    group_by(d_brancos) %>%
    summarise(ocupados = sum(peso_dom)) %>% 
    mutate(prop = round(100*ocupados/sum(ocupados),2) ) %>%
    filter(d_brancos == 1) %>%
    select(prop)
  
  cat('\n Tab3 Participação das mulheres na força de trabalho')
  # Participação das mulheres na força de trabalho
  tab3 <- 
    pnad_data %>%
    filter(!is.na(ocupadas)) %>%
    group_by(sexo) %>%
    summarise(ocupados = sum(peso_dom)) %>% 
    mutate(prop = round(100*ocupados/sum(ocupados),2) ) %>%
    filter(sexo == 'Feminino') %>%
    select(prop)
  
  
  
  #
  # SETOR AGROALIMENTAR
  #
  
  cat('\n Tab4 Participação das pessoas brancas na força de trabalho no setor agroalimentar')
  # Participação das pessoas brancas na força de trabalho
  tab4 <- 
    pnad_data %>%
    mutate(d_brancos = ifelse(cor_raca == 'Branca', 1, 0)) %>%
    filter(!is.na(ocupadas), d_agroalimentar == 1 ) %>%
    group_by(d_brancos) %>%
    summarise(ocupados = sum(peso_dom)) %>% 
    mutate(prop = round(100*ocupados/sum(ocupados),2) ) %>%
    filter(d_brancos == 1) %>%
    select(prop)
  
  cat('\n Tab5 Participação das mulheres na força de trabalho')
  # Participação das mulheres na força de trabalho
  tab5 <- 
    pnad_data %>%
    filter(!is.na(ocupadas), d_agroalimentar == 1 ) %>%
    group_by(sexo) %>%
    summarise(ocupados = sum(peso_dom)) %>% 
    mutate(prop = round(100*ocupados/sum(ocupados),2) ) %>%
    filter(sexo == 'Feminino') %>%
    select(prop)
  
  
  tab_comp_raca_sexo <- data.frame(year = y, 
                                   UF = estado, 
                                   unidade_analise = unidade_analise,
                                   particip_brancos_total = tab2$prop,
                                   particip_brancos_agroali = tab4$prop,
                                   particip_mulheres_total = tab3$prop,
                                   particip_mulheres_agroali = tab5$prop)
  
  
  cat('\n Tab6 Ocupação por setor (percentual)')
  # Ocupação por setor (percentual)
  tab6 <- 
    pnad_data %>%
    filter(!is.na(ocupadas), !is.na(setor)) %>%
    group_by(setor) %>%
    summarise(ocupados = sum(peso_dom)) %>% 
    mutate(prop = round(100*ocupados/sum(ocupados),2) )
  
  tab6_aux <- 
    pnad_data %>%
    filter(!is.na(ocupadas), !is.na(setor), d_agroalimentar == 1) %>%
    summarise(ocupados = sum(peso_dom)) %>% 
    mutate(prop = round(100*ocupados/sum(tab6$ocupados),2) )
  
  
  tab6 <- rbind(tab6, 
                data.frame(setor = "Agroalimentar", ocupados = tab6_aux$ocupados, prop = tab6_aux$prop),
                data.frame(setor = "Total", ocupados = sum(tab6$ocupados), prop = 100))
  
  # Evolução da ocupação e do salário médio por setores
  
  cat('\n Tab7 salário médio por setores')
  # Se a gente souber o que constitui o agroalimentar é só acrescentar aqui
  tab7 <- 
    pnad_data %>% 
    filter(!is.na(setor)) %>%
    group_by(setor) %>%
    summarise(rendimento = weighted.mean(renda_trab_princ, w = peso_dom, na.rm = T))
  
  # Renda média Agroalimentar
  tab7_aux <- 
    pnad_data %>% 
    filter(!is.na(setor), d_agroalimentar == 1) %>%
    summarise(rendimento = weighted.mean(renda_trab_princ, w = peso_dom, na.rm = T))
  
  
  # Renda média total
  tab7_aux2 <- 
    pnad_data %>% 
    filter(!is.na(setor)) %>%
    summarise(rendimento = weighted.mean(renda_trab_princ, w = peso_dom, na.rm = T))
  
  tab7 <- rbind(tab7, 
                data.frame(setor = 'Agroalimentar', rendimento = tab7_aux$rendimento),
                data.frame(setor = 'Total', rendimento = tab7_aux2$rendimento))
  
  
  # Juncao da tab 2 e 3
  tab7 <- left_join(tab6, tab7, by = 'setor')
  
  
  # Salário por sexo 
  cat('\n Tab8 Salário por sexo')
  tab8 <- 
    pnad_data %>% 
    filter(ocupadas == 'Pessoas ocupadas') %>%
    group_by(sexo, cor_raca) %>%
    summarise(participacao = sum(peso_dom),
              sexo_rendimento = weighted.mean(renda_trab_princ, w = peso_dom, na.rm = T)) %>%
    mutate(participacao = participacao/sum(participacao)) %>%
    pivot_wider(names_from = sexo, values_from = c(participacao, sexo_rendimento)) %>%
    mutate(setor = 'Total')
  
  cat('\n Tab9 Agroalimentar')
  # Agroalimentar
  tab9 <- 
    pnad_data %>% 
    filter(ocupadas == 'Pessoas ocupadas') %>%
    filter(!is.na(setor), d_agroalimentar == 1) %>%
    group_by(sexo, cor_raca) %>%
    summarise(participacao = sum(peso_dom),
              sexo_rendimento = weighted.mean(renda_trab_princ, w = peso_dom, na.rm = T)) %>%
    mutate(participacao = participacao/sum(participacao)) %>% 
    pivot_wider(names_from = sexo, values_from = c(participacao, sexo_rendimento)) %>%
    mutate(setor = 'Agroalimentar')
  
 
  tab8 <- bind_rows(tab8, tab9)
  
  cat('\n Tab10 Ocupação no setor agroalimentar')
  # Ocupação no setor agroalimentar
  tab10 <- 
    pnad_data %>%
    filter(!is.na(ocupadas), d_agroalimentar == 1) %>%
    group_by(descricao) %>%
    summarise(total = sum(peso_dom)) %>% 
    mutate(prop = round(100*total/sum(total),2) )
  
  
  
  # Acrescentando as colunas de estado e unidade de análise
  tab1$UF = estado
  tab1$unidade_analise = unidade_analise
  tab1$ano = year
  tab1 <- tab1 %>% select(ano, UF, unidade_analise, ocupadas, total, prop )
  
  tab7$UF = estado
  tab7$unidade_analise = unidade_analise
  tab7$ano = year
  tab7 <- tab7 %>% select(ano, UF, unidade_analise, setor, ocupados, prop, rendimento )
  
  tab8$UF = estado
  tab8$unidade_analise = unidade_analise
  tab8$ano = year
  tab8 <- tab8 %>% select(ano, UF, unidade_analise, setor,
                          cor_raca, participacao_Masculino,
                          participacao_Feminino, 
                          sexo_rendimento_Masculino,
                          sexo_rendimento_Feminino )
  
  tab10$UF = estado
  tab10$unidade_analise = unidade_analise
  tab10$ano = year
  tab10 <- tab10 %>% select(ano, UF, unidade_analise, descricao, total, prop )
  
  
  # Resultados exportados
  lst <- list()
  lst$ocupadas <- tab1
  lst$setor_rendimento <- tab7
  lst$setor_sexo <- tab8
  lst$agro_raca_sexo <- tab_comp_raca_sexo
  lst$ocup_agroali <- tab10
  
  return(lst)
  
  
}


# Dataframe com todas as combinações que conseguirmos

# Níveis: 
# UF; # RM; # Capital

cat('\nGerando o objeto com os "estados" da analise...')

estados <- c( 
  # Sudeste
  "São Paulo",
  "Rio de Janeiro",
  
  # Norte
  "Pará",
  "Acre",
  "Amazonas",
  "Rondônia",
  "Amapá",
  "Roraima",
  "Tocantins",
  
  
  # Nordeste
  "Pernambuco",
  "Maranhão",
  
  # Centro-Oeste
  "Mato Grosso",
  "Goiás",
  
  # Sul
  'Paraná'
  )
  



## Montando o conjunto de possibilidades da lista ----

cat('\nMontando o conjunto de possibilidades da lista no df_resultados...')

years <- 2012:2022
blocos <- c('UF', 'RM', 'Capital')
df_resultados <- expand.grid(years, estados, blocos)
names(df_resultados) <- c('year', 'estado', 'regiao')
df_resultados <- df_resultados %>% arrange(year, estado, regiao )
df_resultados$num_row <- 1:nrow(df_resultados)



cat('\nFuncoes trat_pnad e tabelas_pnad e objeto df_resultados prontos para o uso!')

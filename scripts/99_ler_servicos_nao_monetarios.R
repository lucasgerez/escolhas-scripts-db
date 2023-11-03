## A primeira coisa adicional a se fazer é abrir os serviços não monetários das bases
# 2 e 4


input <- 'C:/Users/ricar/Downloads/'
export <- 'C:/Users/ricar/Documents/consultoria/gerez/dados pof/'

SERVICO_NAO_MONETARIO_POF2 <- 
  read.fwf(paste0(input, "SERVICO_NAO_MONETARIO_POF2.txt") 
           , widths = c(2,4,1,9,2,1,2,2,7,2,10,2,2,10,
                        1,12,10,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V8000", "V9010",
                           "V9011", "V1904", "V1905", "DEFLATOR",
                           "V8000_DEFLA", "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL","V9004")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(SERVICO_NAO_MONETARIO_POF2,paste0(export, "SERVICO_NAO_MONETARIO_POF2.rds"))


# REGISTRO - SERVI?OS N?O MONET?RIOS/POF 4

SERVICO_NAO_MONETARIO_POF4 <- 
  read.fwf(paste0(input, "SERVICO_NAO_MONETARIO_POF4.txt")
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2,2,
                        1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", 
                           "COD_INFORMANTE", "QUADRO", "SEQ",
                           "V9001", "V9002", "V8000", "V9010", "V9011",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(SERVICO_NAO_MONETARIO_POF4,paste0(export, "SERVICO_NAO_MONETARIO_POF4.rds"))
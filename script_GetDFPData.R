library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)
library(purrr)

# Informações das Empresas

df_info <- get_info_companies()

# 1.1 Qtd Empresas por Setor e Segmentos de Governança Corporativas

df_info %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n))

# 1.2 Filtrar setor com mais empresas canceladas: 

qtd_setCancelada <- df_info %>%
  filter(SIT_REG=="CANCELADA") %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n)) 

setMaisCanc <- as.vector(qtd_setCancelada[1, ])

print(paste("Setor com mais empresas canceladas: ", setMaisCanc$SETOR_ATIV))
print(paste("Número de empresas desse setor que foram canceladas: ", setMaisCanc$n))

# 1.3 Filtrar do setor com mais empresas canceladas o maior motivo de cancelamento: 


qtd_motCancel <- df_info %>%
  filter(SETOR_ATIV==setMaisCanc$SETOR_ATIV) %>%
  count(MOTIVO_CANCEL) %>%
  arrange(desc(n))

motivoMaisCanc <- as.vector(qtd_motCancel[1, ])

print(paste("O motivo que mais cancelou empresas foi: ", motivoMaisCanc$MOTIVO_CANCEL))
print(paste("Número de empresas desse setor que foram canceladas por esse motivo: ", motivoMaisCanc$n))

# 1.4 Cria um df com os dados das empresas canceladas do setor

qtd_empresasAtv <- df_info %>%
  filter(SIT_REG=="ATIVO") %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n))


energia_eletrica <- df_info %>%
  filter(SIT_REG=="ATIVO" & SETOR_ATIV=="Energia Elétrica" & CATEG_REG=="Categoria A" & TP_MERC=="BOLSA")


library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)
library(purrr)

# Informações das Empresas

df_info <- get_info_companies()

# 1.1 Filtrar setor com mais empresas canceladas: 

qtd_setCancelada <- df_info %>%
  filter(SIT_REG=="CANCELADA") %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n)) 

setMaisCanc <- qtd_setCancelada[1, ]

print(paste("Setor com mais empresas canceladas: ", setMaisCanc$SETOR_ATIV))
print(paste("Número de empresas desse setor que foram canceladas: ", setMaisCanc$n))

# 1.2 Filtrar do setor com mais empresas canceladas o maior motivo de cancelamento: 

qtd_motCancel <- df_info %>%
  filter(SETOR_ATIV==setMaisCanc$SETOR_ATIV) %>%
  count(MOTIVO_CANCEL) %>%
  arrange(desc(n))

motivoMaisCanc <- qtd_motCancel[1, ]

print(paste("O motivo que mais cancelou empresas foi: ", motivoMaisCanc$MOTIVO_CANCEL))
print(paste("Número de empresas desse setor que foram canceladas por esse motivo: ", motivoMaisCanc$n))

# 1.3 Cria um df com os dados das empresas canceladas do setor
df_empresasCancel <- df_info %>%
  filter(SETOR_ATIV==setMaisCanc$SETOR_ATIV & SIT_REG=="CANCELADA" & MOTIVO_CANCEL==motivoMaisCanc$MOTIVO_CANCEL)




# 2.1 Filtra o setor com mais empresas com registro ATIVO
qtd_empresasAtv <- df_info %>%
  filter(SIT_REG=="ATIVO") %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n))

setComMaisEmprAtv <- qtd_empresasAtv[1, ]

print(paste("Setor com mais empresas ativas: ", setComMaisEmprAtv$SETOR_ATIV))
print(paste("Número de empresas desse setor que estão ativas: ", setComMaisEmprAtv$n))

# 2.2 Filtrar as empresas desse setor

df_emprSetAtv <- df_info %>%
  filter(SIT_REG=="ATIVO" & SETOR_ATIV=="Energia Elétrica")

# 2.3 Filtrar as empresas desse setor que são da Categoria A e são negociadas no mercado BOLSA

df_CateA_Bolsa <- df_emprSetAtv %>%
  filter(CATEG_REG=="Categoria A" & TP_MERC=="BOLSA")

  


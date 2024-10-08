library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)


Indice_liquidez <- function(ATCIRCULANTE,PSCIRCULANTE) {
  ILiquidez = ATCIRCULANTE/PSCIRCULANTE
  
  return(ILiquidez)
  
}


Giro_estoque <- function(Custo_Venda, At_estoque){
  GIestoque = Custo_Venda*-1/At_estoque
  
  return(GIestoque)
}


Marg_Lucro <- function(Receita_financeiro, Receita_Venda){
  MARGlucro = Receita_financeiro/Receita_Venda
  
  return(MARGlucro)
}


Liquidez_Imediata <- function(At_caixa, Ps_circulante){
  
  LImediata = (At_caixa / Ps_circulante)
  
  return(LImediata)
}


Indice_Caixa <- function( At_caixa, Ps_circulante){
  Icaixa = At_caixa / Ps_circulante
  return(Icaixa) 
}


Giro_conta_receber <- function(Receita_Venda, At_receber) {
  GIcontasReceber = Receita_Venda / At_receber
  
  return(GIcontasReceber)
}


ROA <- function(Receita_financeiro, At_total){
  
  RETSOBREATIVO = Receita_financeiro / At_total
  
  return(RETSOBREATIVO)
}


Endividamento_Total <- function(At_total, Balanco_comercial){
  INDIENDIVTOTAL = (At_total - Balanco_comercial) / At_total
  
  return(INDIENDIVTOTAL)
}


Multi_patrimonio_liq <- function(Balanco_comercial, At_total){
  
  MULTIPATRITOTAL = Balanco_comercial / At_total
  
  return(MULTIPATRITOTAL)
}



analisar_empresa <- function(companies_ids) {
  df_info <- get_info_companies()
  for (id_company in companies_ids) {
    company_name <- df_info %>%
      filter(id_company == CD_CVM) %>%
      select(DENOM_SOCIAL) %>%
      pull()
    
    # Baixando dados DFP
    l_dfp <- get_dfp_data(companies_cvm_codes = id_company,
                          type_docs = '*',  # pegar todos os tipos de documentos
                          type_format = 'con',  # consolidado
                          first_year = 2022,
                          last_year = 2023)
    
    fr_ativ <- l_dfp$'DF Consolidado - Balanço Patrimonial Ativo' %>%
      select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
    At_circulante <- unlist(fr_ativ[2, 4])
    At_estoque <- unlist(fr_ativ[16, 4])
    At_total <- unlist(fr_ativ[1, 4])
    At_caixa <- unlist(fr_ativ[3, 4])
    At_receber <- unlist(fr_ativ[10, 4])
    
    fr_passiv <- l_dfp$'DF Consolidado - Balanço Patrimonial Passivo' %>%
      select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
    Ps_circulante <- unlist(fr_passiv[2, 4])
    
    Mut_Patrimoliq <- l_dfp$'DF Consolidado - Demonstração das Mutações do Patrimônio Líquido'%>%
      select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
    
    fr_dre <- l_dfp$'DF Consolidado - Demonstração do Resultado' %>%
      select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
    Custo_Venda <- unlist(fr_dre[2, 4])
    Receita_financeiro <- unlist(fr_dre[53, 4])
    Receita_Venda <- unlist(fr_dre[1, 4])
    Balanco_comercial <- unlist(Mut_Patrimoliq[152, 4])
    
    cat("Empresa ID:", id_company, "\n")
    cat("Nome da Empresa:", company_name, "\n")
    cat("Indice de liquidez: ", Indice_liquidez(At_circulante, Ps_circulante), "\n")
    cat("Liquidez imediata: ", Liquidez_Imediata(At_circulante, Ps_circulante), "\n")
    cut(Custo_Venda)
    cut(At_estoque)
    cat("Giro de estoque: ", Giro_estoque(Custo_Venda, At_estoque), "\n")
    cat("Giro de contas a receber: ", Giro_conta_receber(Receita_Venda, At_receber), "\n")
    cat("Margem de lucro: ", Marg_Lucro(Receita_financeiro, Receita_Venda), "\n")
    cat("Índice caixa: ", Indice_Caixa(At_caixa, Ps_circulante), "\n")
    cat("ROA: ", ROA(Receita_financeiro, At_total), "\n")
    Endividamento_Total
    cat("Endividamento_Total: ", Endividamento_Total(At_total, Blanco_comercial), "\n")
    cat("Multiplicador do Patrimônio Líquido: ", Multi_patrimonio_liq(Balanco_comercial, At_total), "\n")
    
  }
}

#Análise das empresas (rode a função "analisar_empresas" para ver os indices)

# Setor: Extração Mineral
# Empresas: Vale
# Empresa escolhida: Vale
# Motivo: A Vale, empresa de extração de minério, além de não apresentar nenhum concorrente no 
# ano de 2022 e 2023 ativo na bolsa de valores, sua produção de minério de ferro atingiu 
# 321,2 milhões de toneladas, com alta de 4,3% sobre o ano anterior. O volume ficou acima 
# da meta fixada para a empresa no ano passado, que era de 315 milhões de toneladas. 
# Assim, mostrando-se uma empresa promissora e segura para investimentos.
# Fonte: Relatório de produção e vendas da Vale no 4T23 e 2023
analisar_empresa(c(4170))


# Setor: Energia
# Empresas: Eletrobras (escolha inicial), Neoenergia, Serena Geração S.A.
# Empresa escolhida: Eletrobras
# Motivo: Analisando as três empresas, percebemos que a Eletrobras mostrou uma vantagem econômica 
# visto a comparação dos índices(índice de liquidez, liquidez imediata, índice caixa), se mostrando 
# mais segura para investimentos. Além disso, seus resultados financeiros apresentados em seu 
# relatório de 2023 mostraram um crescimento de 8% em relação ao ano anterior. Seu lucro 
# líquido anual atingiu R$ 4,4 bilhões, um aumento de 21% em relação a 2022, demonstrando o 
# impacto positivo do aumento das receitas de transmissão, a adequação de custos e despesas e 
# a simplificação da estrutura administrativa da companhia.
# Fonte: Relatório Anual 2023

analisar_empresa(c(2437, 15539, 23426))


# Setor: Banco
# Empresas: Banco do Brasil (escolha inicial), Banco BMG S/A, Bradesco S.A. 
# Empresa escolhida: Banco do Brasil
# Motivo: #Decidimos continuar com a empresa Banco do Brasil, pois, através dos resultados obtidos de 
# comparação de indicadores (índice de liquidez, liquidez imediata, giro de estoque e índice 
# caixa), é a que apresenta a maior vantagem. Além disso, foi-se analisado em seu relatório anual 
# de 2023, um registro do maior lucro líquido ajustado da empresa, de mais de R$ 35,6 bilhões. 
# Crescimento de 11,4% em relação a 2022. Além disso, foi adicionado R$ 86,1 bilhões em valor à 
# sociedade, quando consideramos pagamento de impostos, salários, dividendos entre outros 
# componentes, um crescimento de 7,3% em relação a 2022.
# Fonte: Relatório anual 2023
analisar_empresa(c(1023, 24600, 906))


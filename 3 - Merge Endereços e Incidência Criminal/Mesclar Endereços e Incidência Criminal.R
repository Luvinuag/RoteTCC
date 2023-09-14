# Carregue as bibliotecas necessárias
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)

# Carregue as duas tabelas de dados do Excel
tabela1 <- read_excel("BaseMunicipioTaxaMes_tratado.xlsx")
tabela2 <- read_excel("CoordTempDist.xlsx")
censo_populacao <- read_excel("Censo População - Baixada Fluminense.xlsx")

# Adiciona as colunas de incidência nas origens

# Una as duas tabelas usando a coluna "fmun" como referência
nova_tabela <- merge(tabela1, tabela2, by = "fmun_cod")

# Renomeie as colunas 

colnames(nova_tabela)[colnames(nova_tabela) == "Incidencia_Nivel_1"] <- "Incidencia_Nivel_1_A"

colnames(nova_tabela)[colnames(nova_tabela) == "Incidencia_Nivel_2"] <- "Incidencia_Nivel_2_A"


# Adiciona as colunas de incidência nos destinos

nova_tabela2 <- merge(nova_tabela, tabela1, by.x = "fmun_cod B", by.y = "fmun_cod")


colnames(nova_tabela2)[colnames(nova_tabela2) == "Incidencia_Nivel_1"] <- "Incidencia_Nivel_1_B"

colnames(nova_tabela2)[colnames(nova_tabela2) == "Incidencia_Nivel_2"] <- "Incidencia_Nivel_2_B"


# Exclua as colunas repetidas
nova_tabela2 <- nova_tabela2[, !(colnames(nova_tabela2) %in% c("fmun.y", "regiao.y", "fmun.x", "regiao.x"))]


# Adiciona a coluna de população da tabela do censo de população
nova_tabela3 <- merge(nova_tabela2, censo_populacao, by.x = "fmun A", by.y = "Local")

# Renomeie a coluna da população
colnames(nova_tabela3)[colnames(nova_tabela3) == "População"] <- "Populacao_A"

# Adiciona a coluna de população da tabela do censo de população
nova_tabela4 <- merge(nova_tabela3, censo_populacao, by.x = "fmun B", by.y = "Local")

# Renomeie a coluna da população
colnames(nova_tabela4)[colnames(nova_tabela4) == "População"] <- "Populacao_B"

# Calcula o índice de criminalidade

nova_tabela4$Indice_Criminalidade_1_A <- (nova_tabela4$Incidencia_Nivel_1_A / nova_tabela4$Populacao_A) * 100

nova_tabela4$Indice_Criminalidade_2_A <- (nova_tabela4$Incidencia_Nivel_2_A / nova_tabela4$Populacao_A) * 100

nova_tabela4$Indice_Criminalidade_1_B <- (nova_tabela4$Incidencia_Nivel_1_B / nova_tabela4$Populacao_B) * 100

nova_tabela4$Indice_Criminalidade_2_B <- (nova_tabela4$Incidencia_Nivel_2_B / nova_tabela4$Populacao_B) * 100



# Defina uma função para calcular o nível de criminalidade condicional
calcular_nivel_criminalidade <- function(media, valor) {
  if (media > 1.25 * valor) {
    return("Alto Nivel Criminal")
  } else if (media < 0.75 * valor) {
    return("Baixo Nivel Criminal")
  } else {
    return("Médio Nivel Criminal")
  }
}

# Calcule as médias das colunas de incidência
media_incidencia_1_A <- mean(nova_tabela4$Indice_Criminalidade_1_A, na.rm = TRUE)
media_incidencia_1_B <- mean(nova_tabela4$Indice_Criminalidade_1_B, na.rm = TRUE)
media_incidencia_2_A <- mean(nova_tabela4$Indice_Criminalidade_2_A, na.rm = TRUE)
media_incidencia_2_B <- mean(nova_tabela4$Indice_Criminalidade_2_B, na.rm = TRUE)

# Crie as colunas condicionais usando a função
nova_tabela4$Nivel_Criminal_1_A <- sapply(nova_tabela4$Indice_Criminalidade_1_A, 
                                          calcular_nivel_criminalidade, media = media_incidencia_1_A)
nova_tabela4$Nivel_Criminal_1_B <- sapply(nova_tabela4$Indice_Criminalidade_1_B, 
                                          calcular_nivel_criminalidade, media = media_incidencia_1_B)
nova_tabela4$Nivel_Criminal_2_A <- sapply(nova_tabela4$Indice_Criminalidade_2_A, 
                                          calcular_nivel_criminalidade, media = media_incidencia_2_A)
nova_tabela4$Nivel_Criminal_2_B <- sapply(nova_tabela4$Indice_Criminalidade_2_B, 
                                          calcular_nivel_criminalidade, media = media_incidencia_2_B)


nova_tabela4$Resultado_Comp_1A_2A <- case_when(
  nova_tabela4$Nivel_Criminal_1_A == "Médio Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Médio Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_A == "Alto Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Alto Nivel Criminal" ~ "Rota Risco Alto",
  nova_tabela4$Nivel_Criminal_1_A == "Baixo Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Baixo Nivel Criminal" ~ "Rota Risco Baixo",
  nova_tabela4$Nivel_Criminal_1_A == "Médio Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Alto Nivel Criminal" ~ "Rota Risco Alto",
  nova_tabela4$Nivel_Criminal_1_A == "Alto Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Médio Nivel Criminal" ~ "Rota Risco Alto",
  nova_tabela4$Nivel_Criminal_1_A == "Alto Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Baixo Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_A == "Baixo Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Alto Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_A == "Baixo Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Médio Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_A == "Médio Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_A == "Baixo Nivel Criminal" ~ "Rota Risco Médio",
  
  TRUE ~ NA_character_
)


nova_tabela4$Resultado_Comp_1B_2B <- case_when(
  nova_tabela4$Nivel_Criminal_1_B == "Médio Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Médio Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_B == "Alto Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Alto Nivel Criminal" ~ "Rota Risco Alto",
  nova_tabela4$Nivel_Criminal_1_B == "Baixo Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Baixo Nivel Criminal" ~ "Rota Risco Baixo",
  nova_tabela4$Nivel_Criminal_1_B == "Médio Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Alto Nivel Criminal" ~ "Rota Risco Alto",
  nova_tabela4$Nivel_Criminal_1_B == "Alto Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Médio Nivel Criminal" ~ "Rota Risco Alto",
  nova_tabela4$Nivel_Criminal_1_B == "Alto Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Baixo Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_B == "Baixo Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Alto Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_B == "Baixo Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Médio Nivel Criminal" ~ "Rota Risco Médio",
  nova_tabela4$Nivel_Criminal_1_B == "Médio Nivel Criminal" & nova_tabela4$Nivel_Criminal_2_B == "Baixo Nivel Criminal" ~ "Rota Risco Médio",
  
  TRUE ~ NA_character_
)


nova_tabela4$Resultado_Comp_Geral <- case_when(
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Médio" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Médio" ~ "Rota Risco Médio",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Alto" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Alto" ~ "Rota Risco Alto",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Baixo" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Baixo" ~ "Rota Risco Baixo",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Médio" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Alto" ~ "Rota Risco Alto",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Alto" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Médio" ~ "Rota Risco Alto",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Alto" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Baixo" ~ "Rota Risco Médio",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Baixo" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Alto" ~ "Rota Risco Médio",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Baixo" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Médio" ~ "Rota Risco Médio",
  nova_tabela4$Resultado_Comp_1B_2B == "Rota Risco Médio" & nova_tabela4$Resultado_Comp_1A_2A == "Rota Risco Baixo" ~ "Rota Risco Médio",
  TRUE ~ NA_character_
)


# Salve a nova tabela em um arquivo Excel
write.xlsx(nova_tabela4, "Indice_Criminalidade_Localizado.xlsx")



# Carregar pacotes

library(dplyr)
library(readxl)
library(openxlsx)

 #Carregar a base de dados em .CSV

dados <- read.csv("BaseMunicipioMensal.csv", sep = ";", header = T)

# Substitui todo o vázio por 0

dados[dados == ""] <- 0

# Converter as colunas 7 até a 60 em valores númericos e remove as vírgulas

for (i in 7:60) {
  dados[, i] <- as.numeric(gsub(",", "", dados[, i]))
}

# Criar colunas de incidência criminal nivel 1 (possuem ação indireta com o transporte de carga)

dados <- dados %>%
  mutate(Incidencia_Nivel_1 = hom_doloso + lesao_corp_morte + latrocinio + cvli + hom_por_interv_policial + letalidade_violenta + 
           tentat_hom + lesao_corp_dolosa + estupro + hom_culposo + lesao_corp_culposa + roubo_transeunte + roubo_celular + 
           roubo_em_coletivo + roubo_rua + roubo_comercio + roubo_residencia + roubo_banco + roubo_cx_eletronico + roubo_conducao_saque + 
           roubo_apos_saque + roubo_bicicleta + outros_roubos + furto_transeunte + furto_coletivo + furto_celular + furto_bicicleta + 
           outros_furtos + extorsao + estelionato + apreensao_drogas + posse_drogas + trafico_drogas + apreensao_drogas_sem_autor + 
           recuperacao_veiculos + apf + aaapai + cmp + cmba + ameaca + pessoas_desaparecidas + encontro_cadaver + encontro_ossada + 
           pol_militares_mortos_serv + pol_civis_mortos_serv)

# Criar colunas de incidência criminal nivel 2 (possuem ação direta com transporte de carga)

dados <- dados %>%
  mutate(Incidencia_Nivel_2 = roubo_carga + roubo_veiculo + sequestro + sequestro_relampago + furto_veiculos)
         
# Filtrar coluna "regiao" para trazer apenas o texto "Baixada Fluminense" - (O estudo será focado em simular rotas na baixada fluminense)

dados <- dados %>%
  filter(regiao == "Baixada Fluminense")

# Remoção de colunas que foram somadas na separação por incidência nível 1 e nível 2

dados <- dados[,-c(3:5, 7:60)]

# Agrupar as linhas da tabela baseadas na coluna "fmun_cod", "fmun" e "região" e calculo de média das colunas de incidência

dados_agrupados <- dados %>%
    group_by(fmun_cod, fmun, regiao) %>%
    summarize_all(funs(sum))


# Salvar dados tratados

write.xlsx(dados_agrupados, "BaseMunicipioTaxaMes_tratado.xlsx")
  

#Modelo com apenas o modelo tipo A

library(igraph)
library(geosphere)
library(openxlsx)
library(plyr)
library(crayon)


######################### VARIÁVEIS ##################################

# Definição de variáveis


# Define matrix de distâncias (Municipios entre si)
matriz_dist <- matrix(c(0, 17, 59, 71, 66, 75, 75, 76, 89, 89, 94, 107, 121,
                        17, 0, 46, 58, 53, 62, 62, 63, 76, 76, 81, 94, 108,
                        61, 49, 0, 13, 9, 15, 17, 19, 31, 55, 50, 62, 59,
                        72, 59, 17, 0, 6, 7, 5, 9, 22, 45, 40, 53, 67,
                        66, 53, 7, 8, 0, 5, 10, 13, 26, 49, 44, 57, 53,
                        72, 59, 17, 7, 8, 0, 3, 8, 26, 49, 44, 56, 49,
                        75, 62, 20, 5, 10, 3, 0, 4, 22, 45, 40, 53, 67,
                        75, 63, 20, 7, 12, 8, 3, 0, 17, 41, 36, 48, 63,
                        73, 61, 34, 23, 26, 25, 20, 16, 0, 28, 23, 35, 49,
                        88, 76, 59, 48, 51, 50, 44, 41, 30, 0, 14, 11, 41,
                        92, 79, 51, 39, 42, 42, 36, 32, 22, 13, 0, 19, 32,
                        108, 95, 66, 54, 57, 57, 51, 47, 36, 12, 20, 0, 47,
                        119, 106, 58, 66, 69, 49, 63, 59, 48, 40, 30, 49, 0), nrow =13,
                      dimnames = list(c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita",
                                        "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai"),
                                      c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita", 
                                        "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")))


# Define border matrix (Municipios  entre si)
matriz_fronteira <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0,
                             0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0,
                             0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0,
                             0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0,
                             0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0), nrow =13,
                           dimnames = list(c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita",
                                             "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai"),
                                           c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita", 
                                             "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")))


# Define o Nivel de Criminalidade
Indice_Criminalidade_Localizado <- read.xlsx("Indice_Criminalidade_Localizado.xlsx")


# Definição de Municipio com Risco
municipios_risco_Tipo_A <- c("Sao Joao de Meriti")


# Filtre os municípios com classificação "Alto Nivel Criminal"
municipios_risco_Tipo_A <- unique(Indice_Criminalidade_Localizado$fmun.A[Indice_Criminalidade_Localizado$Nivel_Criminal_1_A == "Alto Nivel Criminal"])

municipios_risco_Tipo_B <- c("Seropedica")

# Definição de Origem e Destino (Campo Variável)
origem <- "Itaguai"
destino <- "Nova Iguacu"

############################### Restrições Variáveis ##############################

# Definição da Jornada de Trabalho em Horas do Motorista (Campo Variável)
jornada_estabelecida <- (5)*60 #minutos arrendondados

#Definição de Horário de Ínicio de Operação (Campo Variável)
inicio_operacao <- as.numeric(8) #horas arrendondadas

#Definição de Horário Limite de Entrega ao Cliente (Campo Variável)
termino_cliente <- as.numeric(18) #horas arrendondadas

#Definição de Cubagem de Veículo/Frota
cubagem_frota <- 80 #cubagem em M³

#Definição de Cubagem da Carga
cubagem_carga <- 70 #cubagem em M³

############################### Funções Variáveis Tipo A ##########################

# Calcula a Rota mais curta entre a origem e destino considerando os parâmetros de Riscos e Fronteiras - Algoritmo A* para Risco tipo A
rota_a_estrela_Tipo_A <- function(matriz_dist, matriz_fronteira, municipios_risco_Tipo_A, origem, destino) {
  
  # Definir os nomes dos municípios da matriz de distância
  municipios <- rownames(matriz_dist)
  
  # Definição do conjunto de municípios que irão avaliados pelo algoritmo, iniciando com a origem e tendo a inclusão dos demais municipios à medida que o algoritmo roda
  conjunto_aberto <- c(origem)
  
  # Definição da array que armazena o município anterior no caminho mais curto do ponto de origem até cada município.
  veio_de <- rep("", length(municipios))
  names(veio_de) <- municipios
  
  # Definição da array que armazena o custo do caminho mais curto do ponto de origem até cada município. 
  pontuacao_g <- rep(Inf, length(municipios))
  names(pontuacao_g) <- municipios
  pontuacao_g[origem] <- 0
  
  # Definição da array que armazena a estimativa do custo total do caminho mais curto do ponto de origem até o destino, passando por cada município.
  pontuacao_f <- rep(Inf, length(municipios))
  names(pontuacao_f) <- municipios
  pontuacao_f[origem] <- matriz_dist[match(origem, municipios), match(destino, municipios)]
  
  # Verificação do conjunto_aberto maior que zero
  while (length(conjunto_aberto) > 0) {
    # Identificar o município com o pontuacao_f mais baixo
    municipio_atual <- conjunto_aberto[which.min(pontuacao_f[conjunto_aberto])]
    
    # Verificação se o municipio_atual consta como destino
    if (municipio_atual == destino) {
      # Construção de Rota
      rota_A <- c()
      while (municipio_atual != "") {
        rota_A <- c(municipio_atual, rota_A)
        municipio_atual <- veio_de[municipio_atual]
      }
      return(rota_A)
    }
    
    # Remoção do municipio_atual do conjunto_aberto
    conjunto_aberto <- setdiff(conjunto_aberto, municipio_atual)
    
    # Identificação de todos os municípios vizinhos que fazem fronteira com o municipio_atual e não são municípios de risco
    vizinhos <- municipios[matriz_fronteira[match(municipio_atual, municipios), ] == 1 & !municipios %in% municipios_risco_Tipo_A]
    
    # Loop para cada vizinho
    for (vizinho in vizinhos) {
      # Calculo do pontuacao_g provisório para os vizinhos
      pontuacao_g_provisoria <- pontuacao_g[municipio_atual] + matriz_dist[match(municipio_atual, municipios), match(vizinho, municipios)]
      
      # Verificação para se o pontuacao_g provisório é maior que o pontuacao_g
      if (pontuacao_g_provisoria < pontuacao_g[vizinho]) {
        # Caso o pontuacao_g provisório for melhor, as matrizes serão atualizadas
        veio_de[vizinho] <- municipio_atual
        pontuacao_g[vizinho] <- pontuacao_g_provisoria
        
        # Atualização de pontuacao_f dos vizinhos
        pontuacao_f[vizinho] <- pontuacao_g[vizinho] + matriz_dist[match(vizinho, municipios), match(destino, municipios)]
        
        # Adicionar o vizinho no conjunto_aberto, caso ele não esteja
        if (!vizinho %in% conjunto_aberto) {
          conjunto_aberto <- c(conjunto_aberto, vizinho)
        }
      }
    }
  }
  
  # Esgotamos todas as rotas possíveis e não encontramos um caminho para o destino
  return(c())
}

# Função soma das distância em rota (Considerando a Matrix de distância e a rota gerada)
calcula_distancia_rota <- function(rota_A, matriz_dist) {
  # Inicializa a distância total como 0
  distancia_total <- 0
  
  # Loop para cada par de municípios consecutivos na rota
  for (i in seq_along(rota_A)[-length(rota_A)]) {
    # Obter o município atual e o próximo município
    municipio_atual <- rota_A[i]
    proximo_municipio <- rota_A[i + 1]
    
    # Adicionar a distância entre o município atual e o próximo município à distância total
    distancia_total <- distancia_total + matriz_dist[match(municipio_atual, rownames(matriz_dist)), match(proximo_municipio, rownames(matriz_dist))]
  }
  
  return(distancia_total)
}


# Função que calcula o tempo de viagem (considerando a distância total e a velocidade)
calcula_tempo_de_viagem <- function(distancia_total, velocidade = 60) {
  # Converte a velocidade para km/min
  velocidade_km_min <- velocidade / 60
  
  # Calcula o tempo de viagem em minutos
  tempo_de_viagem_minutos <- distancia_total / velocidade_km_min
  
  # Converte o tempo de viagem para horas e minutos
  horas <- floor(tempo_de_viagem_minutos / 60)
  minutos <- round(tempo_de_viagem_minutos %% 60)
  
  # Formata o resultado como uma string
  tempo_de_viagem_formatado <- sprintf("%d horas e %d minutos", horas, minutos)
  
  return(tempo_de_viagem_formatado)
}

# Função que calcula o tempo de viagem (considerando a distância total e a velocidade)
calcula_tempo_de_jornada <- function(distancia_total, velocidade = 60) {
  # Converte a velocidade para km/min
  velocidade_km_min <- velocidade / 60
  
  # Calcula o tempo de viagem em minutos
  tempo_de_jornada_minutos <- distancia_total / velocidade_km_min
  
  return(tempo_de_jornada)
}

tempo_jornada_A <- distancia_rota_A/(60/60)


# Função soma das distância em rota (Considerando a Matrix de distância e a rota gerada)
calcula_distancia_rota <- function(rota_A, matriz_dist) {
  # Inicializa a distância total como 0
  distancia_total <- 0
  
  # Loop para cada par de municípios consecutivos na rota
  for (i in seq_along(rota_A)[-length(rota_A)]) {
    # Obter o município atual e o próximo município
    municipio_atual <- rota_A[i]
    proximo_municipio <- rota_A[i + 1]
    
    # Adicionar a distância entre o município atual e o próximo município à distância total
    distancia_total <- distancia_total + matriz_dist[match(municipio_atual, rownames(matriz_dist)), match(proximo_municipio, rownames(matriz_dist))]
  }
  
  return(distancia_total)
}

# Função para verificar se a rota contém pontos de conexão entre divisa com classificação "Rota Risco Alto"
contem_risco_alto <- function(rota, municipios_risco_Tipo_A) {
  for (i in seq_along(rota)[-length(rota)]) {
    municipio_atual <- rota[i]
    proximo_municipio <- rota[i + 1]
    
    if (municipio_atual != destino && proximo_municipio != origem) {
      if (Indice_Criminalidade_Localizado[Indice_Criminalidade_Localizado$fmun.A == municipio_atual & 
                                          Indice_Criminalidade_Localizado$fmun.B == proximo_municipio, "Resultado_Comp_Geral"] == "Rota Risco Alto") {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Calcula a Rota mais curta entre a origem e destino considerando os parâmetros de Riscos, Fronteiras e Resultado_Comp_Geral
rota_a_estrela_Tipo_A <- function(matriz_dist, matriz_fronteira, origem, destino, municipios_risco_Tipo_A) {
  
  # Verificação se a rota contém risco alto entre divisa
  if (contem_risco_alto(rota_A, municipios_risco_Tipo_A)) {
    # Ajuste da rota para evitar risco alto entre divisa
    rota_A <- ajustar_rota_para_risco_alto(rota_A, municipios_risco_Tipo_A)
  }
  
  # Função que ajusta a rota para evitar risco alto entre divisa
  ajustar_rota_para_risco_alto <- function(rota, municipios_risco_Tipo_A) {
    nova_rota <- c(origem)
    for (i in seq_along(rota)[-length(rota)]) {
      municipio_atual <- rota[i]
      proximo_municipio <- rota[i + 1]
      
      if (municipio_atual != destino && proximo_municipio != origem) {
        if (Indice_Criminalidade_Localizado[Indice_Criminalidade_Localizado$fmun.A == municipio_atual & 
                                            Indice_Criminalidade_Localizado$fmun.B == proximo_municipio, "Resultado_Comp_Geral"] != "Rota Risco Alto") {
          nova_rota <- c(nova_rota, proximo_municipio)
        }
      }
    }
    nova_rota <- c(nova_rota, destino)
    return(nova_rota)
  }
  
  
  

#results

rota_A <- rota_a_estrela_Tipo_A(matriz_dist = matriz_dist,
                                matriz_fronteira = matriz_fronteira,
                                municipios_risco_Tipo_A = municipios_risco_Tipo_A,
                                origem = origem,
                                destino = destino)


distancia_rota_A <- calcula_distancia_rota(rota_A, matriz_dist)
distancia_total_A <- calcula_distancia_rota(rota_A, matriz_dist)
tempo_jornada_A <- distancia_rota_A/(60/60)
tempo_de_viagem_A <- calcula_tempo_de_viagem(distancia_total_A, velocidade = 60)
hora_chegada <- (inicio_operacao) + floor(tempo_jornada_A/60)



# Exibe o Resultado de Distância das Rotas Geradas
if (length(rota_A) == 0) {
  cat(crayon::red("\nRota com tipo A Inviável"))
} else {
  cat("\nROTA: A rota sugerida para o tipo A percorre os seguintes municípios:", rota_A, ". A soma das distâncias entre os municípios na sequência é:", distancia_rota_A, "Kilometros, com tempo médio de:", tempo_de_viagem_A)
  

# Exibe o Resultado de Distância das Rotas Geradas
if (distancia_rota_A == 0) {
  cat(crayon::red("\nRota com tipo A Inviável"))
} else {
  cat("\nROTA: A rota sugerida para o tipo A percorre os seguintes municípios:", rota_A, ". A soma das distâncias entre os municípios na sequência é:", distancia_rota_A, "Kilometros, com tempo médio de:", tempo_de_viagem_A)
  
  if (tempo_jornada_A > jornada_estabelecida) {
    cat(crayon::red("\nNEGATIO: A rota teve um tempo de viagem superior à jornada estabelecida. Logo deverá ser considerado tempo de descanso para o motorista."))
  }
  
  cat(("\nPONTOS DE OBSERVAÇÃO:"))
  
  # Verifica se o tempo de chegada não é superior à hora de término
  if (hora_chegada <= termino_cliente) {
    cat(crayon::green("\nPOSITIVO: A viagem pode ser realizada dentro do horário planejado."))
  } else {
    cat(crayon::red("\nNEGATIVO:O tempo de chegada excede a hora de término de recebimento, a viagem deverá considerar parada noturna."))
  }
  
  # Verifica se a capacidade da frota suporta a carga
  if (cubagem_frota <= cubagem_carga) {
    cat(crayon::green("\nPOSITIVO: A capacidade do veículo/frota é coerente com a carga."))
  } else {
    cat(crayon::red("\nNEGATIVO: A cubagem da carga excede a capacidade do veículo/frota."))
  }
}




library(igraph)
library(openxlsx)

# Criar uma matriz de adjacência para representar as divisas entre os municípios
divisas <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0), nrow =13, byrow = TRUE)

# Nomear as linhas e colunas da matriz com os nomes dos municípios
rownames(divisas) <- c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita",
                       "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")
colnames(divisas) <- c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita",
                       "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")

# Criar uma matriz de distâncias para representar as distâncias entre os municípios
distancias <- matrix(c(0, 17, 59, 71, 66, 75, 75, 76, 89, 89, 94, 107, 121,
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
                       119, 106, 58, 66, 69, 49, 63, 59, 48, 40, 30, 49, 0), nrow =13, byrow = TRUE)

# Nomear as linhas e colunas da matriz com os nomes dos municípios
rownames(distancias) <- c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita",
                          "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")
colnames(distancias) <- c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita",
                          "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")


# Definição de Origem e Destino (Campo Variável)
origem <- "Nilopolis"
destino <- "Itaguai"


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




# Define o Nivel de Criminalidade
Indice_Criminalidade_Localizado <- read.xlsx("Indice_Criminalidade_Localizado.xlsx")


# Filtre os municípios com classificação "Alto Nivel Criminal" ou "Risco Médio"
municipios_risco_Tipo_B <- unique(Indice_Criminalidade_Localizado$fmun.A[Indice_Criminalidade_Localizado$Resultado_Comp_1A_2A %in% c("Rota Risco Alto", "Rota Risco Médio")])



############################### Funções Variáveis Tipo B ##########################

# Criar um grafo a partir das matrizes de adjacência e distâncias
g <- graph_from_adjacency_matrix(divisas * distancias)

# Remover as divisas entre os municípios de risco e os demais
divisas[rownames(divisas) %in% municipios_risco_Tipo_B, ] <- 0
divisas[, colnames(divisas) %in% municipios_risco_Tipo_B] <- 0

pesos <- distancias
pesos[divisas == 0] <- Inf  # Define pesos infinitos para divisas inexistentes
municipios_risco_indices <- match(municipios_risco_Tipo_B, rownames(pesos))
pesos[municipios_risco_indices, municipios_risco_indices] <- Inf  # Define pesos infinitos para municípios de risco
g <- graph_from_adjacency_matrix(pesos, mode = "undirected", weighted = TRUE)


# Encontrar a melhor rota entre o município de origem e o município de destino
rota <- get.shortest.paths(g, from = origem, to = destino, weights = E(g)$weight)

# Calcular a distância total da rota
distancia_total <- 0
for (i in 1:(length(rota$vpath[[1]]) - 1)) {
  distancia_total <- distancia_total + distancias[rota$vpath[[1]][i], rota$vpath[[1]][i + 1]]
}

# Calcular o tempo gasto para percorrer a distância a 60 km/h
velocidade_km_h <- 60

# Calcular o tempo gasto para percorrer a distância a 60 km/h em horas
tempo_horas <- distancia_total / velocidade_km_h
tempo_minutos <- (tempo_horas - floor(tempo_horas)) * 60
tempo_horas <- floor(tempo_horas)

# Define os identificadores
tempo_jornada_A <- distancia_total/(60/60)
tempo_de_viagem_A <- tempo_horas
hora_chegada <- (inicio_operacao) + floor(tempo_jornada_A/60)


# Verificar se a rota encontrada é vazia
if (length(V(g)[rota$vpath[[1]]]$name) == 2 && divisas[origem, destino] == 0) {
  cat("\nROTA: A rota não pode ser gerada porque não existe uma rota entre o município de origem e o município de destino que atendam os parâmetros de risco Tipo B.")
} else {
  # Imprimir a rota encontrada
  cat("\nROTA: A rota sugerida para o tipo C (Todas os Niveis de Risco) percorre os seguintes municípios:", V(g)[rota$vpath[[1]]]$name, ". A soma das distâncias entre os municípios na sequência é:", distancia_total, "Kilometros, com tempo médio de:",  tempo_horas, "horas e", tempo_minutos, "minutos")
  
  if (tempo_jornada_A > jornada_estabelecida) {
    cat(crayon::red("\nNEGATIO: A rota teve um tempo de viagem superior à jornada estabelecida. Logo deverá ser considerado tempo de descanso para o motorista."))
  }
}

cat(("\nPONTOS DE OBSERVAÇÃO:"))

# Verifica se o tempo de chegada não é superior à hora de término
if (hora_chegada <= termino_cliente) {
  cat(crayon::green("\nPOSITIVO: A viagem pode ser realizada dentro do horário planejado."))
} else {
  cat(crayon::red("\nNEGATIVO:O tempo de chegada excede a hora de término de recebimento, a viagem deverá considerar parada noturna."))
}

# Verifica se a capacidade da frota suporta a carga
if (cubagem_frota >= cubagem_carga) {
  cat(crayon::green("\nPOSITIVO: A capacidade do veículo/frota é coerente com a carga."))
} else {
  cat(crayon::red("\nNEGATIVO: A cubagem da carga excede a capacidade do veículo/frota."))
}






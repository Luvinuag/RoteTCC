library(igraph)

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

# Criar um vetor com os nomes dos municípios de risco
municipios_risco <- c("Seropedica")

# Remover as divisas entre os municípios de risco e os demais
divisas[rownames(divisas) %in% municipios_risco, ] <- 0
divisas[, colnames(divisas) %in% municipios_risco] <- 0

# Criar um grafo a partir das matrizes de adjacência e distâncias
g <- graph_from_adjacency_matrix(divisas * distancias)

# Definir o município de origem e o município de destino
origem <- "Itaguai"
destino <- "Nova Iguacu"

# Verificar se o município de origem ou o município de destino estão no vetor de municípios de risco
if (origem %in% municipios_risco || destino %in% municipios_risco) {
  cat("A rota não pode ser gerada porque o município de origem ou o município de destino estão no vetor de municípios de risco.")
} else {
  # Encontrar a melhor rota entre o município de origem e o município de destino
  rota <- get.shortest.paths(g, from = origem, to = destino)
  
  # Verificar se a rota encontrada é vazia
  if (length(rota$vpath[[1]]) == 0) {
    cat("A rota não pode ser gerada porque não existe uma rota entre o município de origem e o município de destino que não passe por um município de risco.")
  } else {
    # Imprimir a rota encontrada
    cat("A melhor rota entre", origem, "e", destino, "é:", V(g)[rota$vpath[[1]]]$name)
  }
}


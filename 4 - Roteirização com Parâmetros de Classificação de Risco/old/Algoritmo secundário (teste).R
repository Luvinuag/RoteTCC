# Carregue as bibliotecas necessárias
library(tidyverse)
library(geosphere)

# Defina suas variáveis
origem <- c(-43.2096, -22.9035) # coordenadas da origem
destino <- c(-46.6333, -23.5505) # coordenadas do destino
municipios <- data.frame(
  nome = c("Municipio1", "Municipio2", "Municipio3"),
  longitude = c(-44.2768, -45.5321, -46.6250),
  latitude = c(-23.1791, -23.2218, -24.0953)
  
) # lista de coordenadas dos municípios
matriz_divisa <- matrix(c(0,1,1,1,0,1,1,1,0), nrow = 3) # matriz de divisa entre os municípios
municipios_risco <- c("Municipio2") # lista de municípios com risco

# Remova os municípios com risco da lista de municípios
municipios <- municipios[!municipios$nome %in% municipios_risco,]

# Calcule as distâncias entre a origem e os municípios
distancias_origem <- distHaversine(origem, municipios[,c("longitude", "latitude")])

# Calcule as distâncias entre os municípios e o destino
distancias_destino <- distHaversine(municipios[,c("longitude", "latitude")], destino)

# Calcule as distâncias entre os municípios
distancias_municipios <- as.matrix(distHaversine(municipios[,c("longitude", "latitude")], municipios[,c("longitude", "latitude")]))

# Aplique o algoritmo de Clarke e Wright para encontrar a rota otimizada
savings <- matrix(0, nrow = nrow(municipios), ncol = nrow(municipios))
for (i in 1:nrow(municipios)) {
  for (j in 1:nrow(municipios)) {
    if (i != j & matriz_divisa[i,j] == 1) {
      savings[i,j] <- distancias_origem[i] + distancias_destino[j] - distancias_municipios[i,j]
    }
  }
}
route <- c(1)
while (sum(savings) > 0) {
  max_saving <- which(savings == max(savings), arr.ind = TRUE)[1,]
  i <- max_saving[1]
  j <- max_saving[2]
  if (i %in% route) {
    if (which(route == i) == length(route)) {
      route <- c(route, j)
    } else {
      next
    }
  } else if (j %in% route) {
    if (which(route == j) == length(route)) {
      route <- c(route, i)
    } else {
      next
    }
  } else {
    route <- c(route, i, j)
  }
  savings[i,j] <- 0
  savings[j,i] <- 0
}

# Imprima a rota otimizada
cat("Rota otimizada:", paste(municipios[route,"nome"], collapse = " -> "))


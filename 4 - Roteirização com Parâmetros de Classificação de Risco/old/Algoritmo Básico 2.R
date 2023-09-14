library(igraph)
library(geosphere)
library (openxlsx)
library (plyr)



######################### VARIÁVEIS ##################################

# Definição de variáveis


# Define matrix de distâncias (Municipios entre si)
dist_matrix <- matrix(c(0, 17, 59, 71, 66, 75, 75, 76, 89, 89, 94, 107, 121,
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
border_matrix <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
                  
  
# Define a lista de coordenadas (Coordenadas dos municipios)
coordinates_list <- read.xlsx("Locais.xlsx")
  
# Define risk municipalities
risk_municipalities <- c("Nilopolis")

# Define origin and destination
origin <- "Duque de Caxias"
destination <- "Queimados"

############################### Funções ############################################

 # Função de Clarke_Wright (Rota visando de saving)
clarke_wright_route <- function(dist_matrix, border_matrix, risk_municipalities, origin, destination) {
  # Get the names of the municipalities from the distance matrix
  municipalities <- rownames(dist_matrix)
  
  # Initialize the route with the origin
  route <- c(origin)
  
  # Set the current municipality to the origin
  current_municipality <- origin
  
  # Repeat until we reach the destination
  while (current_municipality != destination) {
    # Find all neighboring municipalities that share a border with the current municipality
    neighbors <- municipalities[border_matrix[current_municipality, ] == 1]
    
    # Remove any neighbors that are classified as "risk"
    neighbors <- setdiff(neighbors, risk_municipalities)
    
    # Calculate the savings for each neighbor
    savings <- sapply(neighbors, function(neighbor) {
      dist_matrix[current_municipality, destination] + dist_matrix[origin, neighbor] - dist_matrix[current_municipality, neighbor]
    })
    
    # Find the neighbor with the maximum savings
    next_municipality <- names(which.max(savings))
    
    # Add the next municipality to the route
    route <- c(route, next_municipality)
    
    # Set the current municipality to the next municipality
    current_municipality <- next_municipality
    
    # Print status message
    cat(paste0("Current municipality: ", current_municipality, "\n"))
  }
  
  return(route)
}
  

  # Função que calcula o tempo de viagem (considerando a lista de coordenadas e a rota gerada)
calculate_travel_time <- function(coordinates_list, route, speed = 60) {
  total_distance <- 0
  for (i in 1:(length(route) - 1)) {
    coord1 <- coordinates_list[[route[i]]]
    coord2 <- coordinates_list[[route[i + 1]]]
    total_distance <- total_distance + geosphere::distVincentySphere(coord1, coord2)
  }
  travel_time <- total_distance / (speed * 1000/60)
  return(travel_time)
}

# Função soma das distância em rota (Considerando a Matrix de distância e a rota gerada)
sum_route_distance <- function(dist_matrix, route) {
  total_distance <- 0
  for (i in 1:(length(route) - 1)) {
    total_distance <- total_distance + dist_matrix[route[i], route[i + 1]]
  }
  return(total_distance)
}


 ####################### GRÁFICO #########################
  
  # cria um grafo a partir da matriz
  grafo_municipios <- graph.adjacency(border_matrix)
  
  # define as cores dos vértices (municípios) que fazem divisa
  V(grafo_municipios)$color <- ifelse(
    degree(grafo_municipios) > -1, "blue", "red"
  )
  
  # plota o grafo
  plot(
    grafo_municipios,
    vertex.size = 08,
    vertex.label.dist = 0.5,
    vertex.label.cex = 1,
    vertex.label.color = "blue",
    vertex.color = "white",
    layout = layout.circle
    
  )
  

  
  ######################## Resultados - Clarke and Wright ###################################
  
  

  
  
  # Calculate route using Clarke and Wright savings algorithm
  route <- clarke_wright_route(dist_matrix = dist_matrix,
                               border_matrix = border_matrix,
                               risk_municipalities = risk_municipalities,
                               origin = origin,
                               destination = destination)
  
  

# Print route
route


# Calcula a soma das distâncias entre os municípios na sequência
total_distance <- sum_route_distance(dist_matrix = dist_matrix, route = route)

# Exibe o resultado
cat("A soma das distâncias entre os municípios na sequência é:", total_distance)


# Calcula o tempo de viagem entre os municípios na sequência
travel_time <- calculate_travel_time(coordinates_list = coordinates_list,
                                     route = route,
                                     speed = 60)
travel_time     
# Exibe o resultado
cat("O tempo de viagem entre os municípios na sequência é:", travel_time, "minutos")       







  ######################## Resultados Alternativo - Bfs ###################################
  
  # Calcula a rota com o menor número de municípios possível usando o algoritmo BFS
  route <- bfs_route(border_matrix = border_matrix,
                     risk_municipalities = risk_municipalities,
                     origin = origin,
                     destination = destination)
  
  # Exibe a rota resultante
  cat("A rota com o menor número de municípios possível é:", paste(route, collapse = ", "))

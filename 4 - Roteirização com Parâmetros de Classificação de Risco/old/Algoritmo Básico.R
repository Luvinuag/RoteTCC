library(igraph)
library(geosphere)

######################### VARIÁVEIS ##################################

# Definição de variáveis

# Define distance matrix (Distância em quilometros entre os municipios)
dist_matrix <- matrix(c(0, 10, 20, 30, 40,
                        10, 0, 15, 25, 35,
                        20, 15, 0, 12, 22,
                        30, 25, 12, 0, 18,
                        40, 35, 22, 18, 0), nrow = 5,
                      dimnames = list(c("Municipio A", "Municipio B", "Municipio C", "Municipio D", "Municipio E"),
                                      c("Municipio A", "Municipio B", "Municipio C", "Municipio D", "Municipio E")))

# Define border matrix (Municipios que fazem divisa entre si)
border_matrix <- matrix(c(0,1 ,1 ,0 ,0 ,
                          1 ,0 ,1 ,1 ,0 ,
                          1 ,1 ,0 ,1 ,1 ,
                          0 ,1 ,1 ,0 ,1 ,
                          0 ,0 ,1 ,1 ,0 ), nrow =5,
                        dimnames = list(c("Municipio A", "Municipio B", "Municipio C", "Municipio D", "Municipio E"),
                                        c("Municipio A", "Municipio B", "Municipio C", "Municipio D", "Municipio E")))

# Define a lista de coordenadas (Coordenadas dos municipios)
coordinates_list <- list(
  "Municipio A" = c(-23.5505, -46.6333),
  "Municipio B" = c(-22.9068, -43.1729),
  "Municipio C" = c(-19.8157, -43.9542),
  "Municipio D" = c(-15.7942, -47.8825),
  "Municipio E" = c(-3.71839, -38.5434)
)

# Define risk municipalities
risk_municipalities <- c("Municipio C")

# Define origin and destination
origin <- "Municipio A"
destination <- "Municipio E"

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

bfs_route <- function(border_matrix, risk_municipalities, origin, destination) {
  # Cria um grafo a partir da matriz de fronteiras
  graph <- graph_from_adjacency_matrix(border_matrix, mode = "undirected")
  
  # Remove os municípios de "risco" do grafo
  graph <- delete_vertices(graph, risk_municipalities)
  
  # Encontra o caminho mais curto entre o município de origem e o município de destino usando o algoritmo BFS
  path <- shortest_paths(graph, origin, destination, output = "vpath", algorithm = "bfs")[[1]]
  
  # Extrai os nomes dos municípios do caminho
  route <- names(path)
  
  return(route)
}

dijkstra_route <- function(dist_matrix, border_matrix, risk_municipalities, origin, destination) {
  # Cria um grafo a partir da matriz de distâncias
  graph <- graph_from_adjacency_matrix(dist_matrix * border_matrix, mode = "undirected", weighted = TRUE)
  
  # Remove os municípios de "risco" do grafo
  graph <- delete_vertices(graph, risk_municipalities)
  
  # Encontra o caminho mais curto entre o município de origem e o município de destino usando o algoritmo de Dijkstra
  path <- shortest_paths(graph, origin, destination, output = "vpath")[[1]]
  
  # Extrai os nomes dos municípios do caminho
  #route_alternative <- names(path)
  
  return(route_alternative)
}


 ####################### GRÁFICO #########################
  
  # cria um grafo a partir da matriz
  grafo_municipios <- graph.adjacency(border_matrix)
  
  # define as cores dos vértices (municípios) que fazem divisa
  V(grafo_municipios)$color <- ifelse(
    degree(grafo_municipios) > 0, "blue", "red"
  )
  
  # plota o grafo
  plot(
    grafo_municipios,
    vertex.size = 30,
    vertex.label.dist = 0.5,
    vertex.label.cex = 1,
    layout = layout.circle
  )
  
  ######################## Resultados - Bfs ###################################
  
  # Calcula a rota com o menor número de municípios possível usando o algoritmo BFS
  route <- bfs_route(border_matrix = border_matrix,
                     risk_municipalities = risk_municipalities,
                     origin = origin,
                     destination = destination)
  
  # Exibe a rota resultante
  cat("A rota com o menor número de municípios possível é:", paste(route, collapse = ", "))
  
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


############## Resultados algoritmo alternativo #####################


# Calcula a rota mais econômica usando o algoritmo de Dijkstra
route_alternative <- dijkstra_route(dist_matrix = dist_matrix,
                                    border_matrix = border_matrix,
                                    risk_municipalities = risk_municipalities,
                                    origin = origin,
                                    destination = destination)

# Print route_alternative
route_alternative

# Exibe a rota resultante
cat("A rota mais econômica é:", paste(route_alternative, collapse = ", "))


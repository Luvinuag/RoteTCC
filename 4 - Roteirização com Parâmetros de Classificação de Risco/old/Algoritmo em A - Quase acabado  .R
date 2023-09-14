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

# Definição de Municipio com Risco
risk_municipalities_Type_A <- c("Sao Joao de Meriti")

risk_municipalities_Type_B <- c("Seropedica")

risk_municipalities_Type_C <- c("Seropedica", "Queimados", "Japeri")

# Definição de Origem e Destino (Campo Variável)
origin <- "Itaguai"
destination <- "Nova Iguacu"


########################### FUNÇÕES ##########################################

# Calcula a Rota mais curta entre a origem e destino considerando os parâmetros de Riscos e Fronteiras - Algoritmo A* para Risco tipo A
a_star_route_Type_A <- function(dist_matrix, border_matrix, risk_municipalities_Type_A, origin, destination) {
  # Get the names of the municipalities from the distance matrix
  municipalities <- rownames(dist_matrix)
  
  # Initialize the open set with the origin
  open_set <- c(origin)
  
  # Initialize the came_from array
  came_from <- rep("", length(municipalities))
  names(came_from) <- municipalities
  
  # Initialize the g_score array
  g_score <- rep(Inf, length(municipalities))
  names(g_score) <- municipalities
  g_score[origin] <- 0
  
  # Initialize the f_score array
  f_score <- rep(Inf, length(municipalities))
  names(f_score) <- municipalities
  f_score[origin] <- dist_matrix[match(origin, municipalities), match(destination, municipalities)]
  
  # Repeat until the open set is empty
  while (length(open_set) > 0) {
    # Find the municipality in the open set with the lowest f_score
    current_municipality <- open_set[which.min(f_score[open_set])]
    
    # Check if we have reached the destination
    if (current_municipality == destination) {
      # We have reached the destination, construct the path
      route_A <- c()
      while (current_municipality != "") {
        route_A <- c(current_municipality, route_A)
        current_municipality <- came_from[current_municipality]
      }
      return(route_A)
    }
    
    # Remove the current municipality from the open set
    open_set <- setdiff(open_set, current_municipality)
    
    # Find all neighboring municipalities that share a border with the current municipality and are not risk municipalities
    neighbors <- municipalities[border_matrix[match(current_municipality, municipalities), ] == 1 & !municipalities %in% risk_municipalities_Type_A]
    
    # Loop over each neighbor
    for (neighbor in neighbors) {
      # Calculate the tentative g_score for the neighbor
      tentative_g_score <- g_score[current_municipality] + dist_matrix[match(current_municipality, municipalities), match(neighbor, municipalities)]
      
      # Check if the tentative g_score is better than the current g_score of the neighbor
      if (tentative_g_score < g_score[neighbor]) {
        # The tentative g_score is better, update the came_from and g_score arrays
        came_from[neighbor] <- current_municipality
        g_score[neighbor] <- tentative_g_score
        
        # Update the f_score of the neighbor
        f_score[neighbor] <- g_score[neighbor] + dist_matrix[match(neighbor, municipalities), match(destination, municipalities)]
        
        # Add the neighbor to the open set if it is not already in it
        if (!neighbor %in% open_set) {
          open_set <- c(open_set, neighbor)
        }
      }
    }
  }
  
  # We have exhausted all possible routes and did not find a path to the destination
  return(c())
}




# Calcula a Rota mais curta entre a origem e destino considerando os parâmetros de Riscos e Fronteiras - Algoritmo A* para Risco tipo B
a_star_route_Type_B <- function(dist_matrix, border_matrix, risk_municipalities_Type_B, origin, destination) {
  # Get the names of the municipalities from the distance matrix
  municipalities <- rownames(dist_matrix)
  
  # Initialize the open set with the origin
  open_set <- c(origin)
  
  # Initialize the came_from array
  came_from <- rep("", length(municipalities))
  names(came_from) <- municipalities
  
  # Initialize the g_score array
  g_score <- rep(Inf, length(municipalities))
  names(g_score) <- municipalities
  g_score[origin] <- 0
  
  # Initialize the f_score array
  f_score <- rep(Inf, length(municipalities))
  names(f_score) <- municipalities
  f_score[origin] <- dist_matrix[match(origin, municipalities), match(destination, municipalities)]
  
  # Repeat until the open set is empty
  while (length(open_set) > 0) {
    # Find the municipality in the open set with the lowest f_score
    current_municipality <- open_set[which.min(f_score[open_set])]
    
    # Check if we have reached the destination
    if (current_municipality == destination) {
      # We have reached the destination, construct the path
      route_B <- c()
      while (current_municipality != "") {
        route_B <- c(current_municipality, route_B)
        current_municipality <- came_from[current_municipality]
      }
      return(route_B)
    }
    
    # Remove the current municipality from the open set
    open_set <- setdiff(open_set, current_municipality)
    
    # Find all neighboring municipalities that share a border with the current municipality and are not risk municipalities
    neighbors <- municipalities[border_matrix[match(current_municipality, municipalities), ] == 1 & !municipalities %in% risk_municipalities_Type_B]
    
    # Loop over each neighbor
    for (neighbor in neighbors) {
      # Calculate the tentative g_score for the neighbor
      tentative_g_score <- g_score[current_municipality] + dist_matrix[match(current_municipality, municipalities), match(neighbor, municipalities)]
      
      # Check if the tentative g_score is better than the current g_score of the neighbor
      if (tentative_g_score < g_score[neighbor]) {
        # The tentative g_score is better, update the came_from and g_score arrays
        came_from[neighbor] <- current_municipality
        g_score[neighbor] <- tentative_g_score
        
        # Update the f_score of the neighbor
        f_score[neighbor] <- g_score[neighbor] + dist_matrix[match(neighbor, municipalities), match(destination, municipalities)]
        
        # Add the neighbor to the open set if it is not already in it
        if (!neighbor %in% open_set) {
          open_set <- c(open_set, neighbor)
        }
      }
    }
  }
  
  # We have exhausted all possible routes and did not find a path to the destination
  return(c())
}

# Calcula a Rota mais curta entre a origem e destino considerando os parâmetros de Riscos e Fronteiras - Algoritmo A* para Risco tipo C
a_star_route_Type_C <- function(dist_matrix, border_matrix, risk_municipalities_Type_C, origin, destination) {
  # Get the names of the municipalities from the distance matrix
  municipalities <- rownames(dist_matrix)
  
  # Initialize the open set with the origin
  open_set <- c(origin)
  
  # Initialize the came_from array
  came_from <- rep("", length(municipalities))
  names(came_from) <- municipalities
  
  # Initialize the g_score array
  g_score <- rep(Inf, length(municipalities))
  names(g_score) <- municipalities
  g_score[origin] <- 0
  
  # Initialize the f_score array
  f_score <- rep(Inf, length(municipalities))
  names(f_score) <- municipalities
  f_score[origin] <- dist_matrix[match(origin, municipalities), match(destination, municipalities)]
  
  # Repeat until the open set is empty
  while (length(open_set) > 0) {
    # Find the municipality in the open set with the lowest f_score
    current_municipality <- open_set[which.min(f_score[open_set])]
    
    # Check if we have reached the destination
    if (current_municipality == destination) {
      # We have reached the destination, construct the path
      route_C <- c()
      while (current_municipality != "") {
        route_C <- c(current_municipality, route_C)
        current_municipality <- came_from[current_municipality]
      }
      return(route_C)
    }
    
    # Remove the current municipality from the open set
    open_set <- setdiff(open_set, current_municipality)
    
    # Find all neighboring municipalities that share a border with the current municipality and are not risk municipalities
    neighbors <- municipalities[border_matrix[match(current_municipality, municipalities), ] == 1 & !municipalities %in% risk_municipalities_Type_C]
    
    # Loop over each neighbor
    for (neighbor in neighbors) {
      # Calculate the tentative g_score for the neighbor
      tentative_g_score <- g_score[current_municipality] + dist_matrix[match(current_municipality, municipalities), match(neighbor, municipalities)]
      
      # Check if the tentative g_score is better than the current g_score of the neighbor
      if (tentative_g_score < g_score[neighbor]) {
        # The tentative g_score is better, update the came_from and g_score arrays
        came_from[neighbor] <- current_municipality
        g_score[neighbor] <- tentative_g_score
        
        # Update the f_score of the neighbor
        f_score[neighbor] <- g_score[neighbor] + dist_matrix[match(neighbor, municipalities), match(destination, municipalities)]
        
        # Add the neighbor to the open set if it is not already in it
        if (!neighbor %in% open_set) {
          open_set <- c(open_set, neighbor)
        }
      }
    }
  }
  
  # We have exhausted all possible routes and did not find a path to the destination
  return(c())
}


# Função soma das distância em rota (Considerando a Matrix de distância e a rota gerada)

calculate_route_distance <- function(route, dist_matrix) {
  # Initialize the total distance to 0
  total_distance <- 0
  
  # Loop over each pair of consecutive municipalities in the route
  for (i in seq_along(route)[-length(route)]) {
    # Get the current municipality and the next municipality
    current_municipality <- route[i]
    next_municipality <- route[i + 1]
    
    # Add the distance between the current municipality and the next municipality to the total distance
    total_distance <- total_distance + dist_matrix[match(current_municipality, rownames(dist_matrix)), match(next_municipality, rownames(dist_matrix))]
  }
  
  return(total_distance)
}


################################### GRÁFICO ##########################################

# cria um grafo a partir da matriz
grafo_municipios <- graph.adjacency(border_matrix,)

# plota o grafo
dev.new(width = 30, height = 30)
plot(
  grafo_municipios,
  vertex.size = 8,
  vertex.label.dist = 1,
  vertex.label.cex = 1,
  vertex.label.color = "blue",
  vertex.color = "white",
  layout = layout.auto
  
)


################################ RESULTADOS #################################

route_A <- a_star_route_Type_A(dist_matrix = dist_matrix,
                      border_matrix = border_matrix,
                      risk_municipalities_Type_A = risk_municipalities_Type_A,
                      origin = origin,
                      destination = destination)


route_B <- a_star_route_Type_B(dist_matrix = dist_matrix,
                       border_matrix = border_matrix,
                       risk_municipalities_Type_B = risk_municipalities_Type_B,
                       origin = origin,
                       destination = destination)


route_C <- a_star_route_Type_C(dist_matrix = dist_matrix,
                       border_matrix = border_matrix,
                       risk_municipalities_Type_C = risk_municipalities_Type_C,
                       origin = origin,
                       destination = destination)

  #Calcular a Soma das Distâncias entre os Municípios com risco por tipo
route_distance_A <- calculate_route_distance(route_A, dist_matrix)
route_distance_B <- calculate_route_distance(route_B, dist_matrix)
route_distance_C <- calculate_route_distance(route_C, dist_matrix)


#Exibe o Resultado de Distância das Rotas Geradas

if (route_distance_A == 0) {
  cat("Rota com tipo A Inviável")
} else {
  cat("A rota sugerida para o tipo A percorre os seguintes municipios:", route_A, ". A soma das distâncias entre os municípios na sequência é:", route_distance_A, "Kilometros")
}

if (route_distance_B == 0) {
  cat("Rota com tipo B Inviável")
} else {
  cat("A rota sugerida para o tipo A percorre os seguintes municipios:", route_B, ". A soma das distâncias entre os municípios na sequência é:", route_distance_B, "Kilometros")
}

if (route_distance_C == 0) {
  cat("Rota com tipo C Inviável")
} else {
  cat("A rota sugerida para o tipo A percorre os seguintes municipios:", route_C, ". A soma das distâncias entre os municípios na sequência é:", route_distance_C, "Kilometros")
}


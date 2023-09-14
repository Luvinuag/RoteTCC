library(httr)
library (openxlsx)


# Define distance matrix (Distância em quilometros entre os municipios)

#Chave de API do Bing Maps
api_key <- "AgmgALnI7AUXh3mhYJbVIF7MxmcP-tUnPuAmXFKbry8EKrUECKFIek8EbY9Td3nn"

# Leia a lista de locais e coordenadas de um arquivo Excel
data <- read.xlsx("Locais.xlsx")
locations <- lapply(seq_len(nrow(data)), function(i) {
  list(name = data[i, "Local"], coordinates = as.numeric(strsplit(data[i, "Coordenadas"], ",")[[1]]))
})

# Construa a URL da solicitação da API
origins <- paste(sapply(locations, function(x) paste(x$coordinates[1], x$coordinates[2], sep = ",")), collapse = ";")
destinations <- origins
url <- paste0("http://dev.virtualearth.net/REST/v1/Routes/DistanceMatrix?origins=",
              origins, "&destinations=", destinations,
              "&travelMode=driving&key=", api_key)

# Solicitação da API
response <- GET(url)

# Verifique se a solicitação foi bem-sucedida
if (http_status(response)$category == "Success") {
  # Extraia os dados da resposta
  content <- content(response)
  results <- content$resourceSets[[1]]$resources[[1]]$results
  
  # Crie uma matriz para armazenar os resultados
  n <- length(locations)
  dist_matrix <- matrix(nrow = n, ncol = n)
  
  # Preencha a matriz com os resultados
  for (result in results) {
    origin_index <- result$originIndex + 1
    destination_index <- result$destinationIndex + 1
    distance_in_meters <- result$travelDistance * 1000
    distance_in_kilometers <- distance_in_meters / 1000
    dist_matrix[origin_index, destination_index] <- distance_in_kilometers
  }
  
  # Imprima a matriz de distâncias
  cat("A matriz de distâncias entre os locais é:\n")
  rownames(dist_matrix) <- sapply(locations, function(x) x$name)
  colnames(dist_matrix) <- sapply(locations, function(x) x$name)
  print(dist_matrix)
  
  write.csv(dist_matrix, "Dist_matrix.csv")
}


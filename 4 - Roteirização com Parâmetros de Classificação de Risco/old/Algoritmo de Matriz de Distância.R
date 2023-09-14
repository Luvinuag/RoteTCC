library(httr)
library(xlsx)

# Insira sua chave de API do Bing Maps aqui
api_key <- "AgmgALnI7AUXh3mhYJbVIF7MxmcP-tUnPuAmXFKbry8EKrUECKFIek8EbY9Td3nn"


# Defina a lista de locais e coordenadas
locations <- list(
  list(name = "Belford Roxo", coordinates = c(-22.763889,-43.398889)),
  list(name = "Duque de Caxias", coordinates = c(-22.785948,-43.3131828)),
  list(name = "Guapimirim", coordinates = c(-22.5322952,-42.9899036))
)

# Construa a URL da solicitação da API
origins <- paste(sapply(locations, function(x) paste(x$coordinates[1], x$coordinates[2], sep = ",")), collapse = ";")
destinations <- origins
url <- paste0("http://dev.virtualearth.net/REST/v1/Routes/DistanceMatrix?origins=",
              origins, "&destinations=", destinations,
              "&travelMode=driving&key=", api_key)

# Faça a solicitação da API
response <- GET(url)

# Verifique se a solicitação foi bem-sucedida
if (http_status(response)$category == "Success") {
  # Extraia os dados da resposta
  content <- content(response)
  results <- content$resourceSets[[1]]$resources[[1]]$results
  
  # Crie uma matriz para armazenar os resultados
  n <- length(locations)
  distance_matrix <- matrix(nrow = n, ncol = n)
  
  # Preencha a matriz com os resultados
  for (result in results) {
    origin_index <- result$originIndex + 1
    destination_index <- result$destinationIndex + 1
    distance_in_meters <- result$travelDistance * 1000
    distance_in_kilometers <- distance_in_meters / 1000
    distance_matrix[origin_index, destination_index] <- distance_in_kilometers
  }
  
  # Imprima a matriz de distâncias
  cat("A matriz de distâncias entre os locais é:\n")
  rownames(distance_matrix) <- sapply(locations, function(x) x$name)
  colnames(distance_matrix) <- sapply(locations, function(x) x$name)
  print(distance_matrix)
} else {
  # Imprima uma mensagem de erro se a solicitação falhar
  cat("A solicitação da API falhou.\n")
}


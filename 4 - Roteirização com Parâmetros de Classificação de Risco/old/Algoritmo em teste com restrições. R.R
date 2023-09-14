library(igraph)
library(geosphere)
library(openxlsx)
library(plyr)

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
                        119, 106, 58, 66, 69, 49, 63, 59, 48, 40, 30, 49, 0), nrow = 13,
                      dimnames = list(c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita",
                                        "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai"),
                                      c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita", 
                                        "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")))

# Define border matrix (Municipios entre si)
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
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0), nrow = 13,
                           dimnames = list(c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita", 
                                             "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai"),
                                           c("Guaramirim", "Mage", "Duque de Caxias", "Belford Roxo", "Sao Joao de Meriti","Nilopolis","Mesquita", 
                                             "Nova Iguacu","Queimados","Japeri","Seropedica", "Paracambi", "Itaguai")))

# Define a lista de coordenadas (Coordenadas dos municipios)
lista_de_coordenadas <- read.xlsx("Locais.xlsx")

# Definição de Municipio com Risco
municipios_risco_Tipo_A <- c("Seropedica")
municipios_risco_Tipo_B <- c("Seropedica")

# Definição de Origem e Destino (Campo Variável)
origem <- "Itaguai"
destino <- "Nova Iguacu"

# Definição da capacidade máxima do caminhão
capacidade_maxima <- 100

# Definição dos municípios de entrega casada (origem até destino e destino até segundo destino)
entrega_casada_origem <- TRUE
entrega_casada_destino <- FALSE

# Definição do segundo destino (caso entrega casada esteja ativada)
segundo_destino <- "Queimados"

# Definição da restrição de retorno à origem (ativa/desativa)
retorno_origem <- FALSE

############################### Funções Variáveis Tipo A ##########################

# Calcula a Rota mais curta entre a origem e destino considerando os parâmetros de Riscos e Fronteiras - Algoritmo A* para Risco tipo A
rota_a_estrela_Tipo_A <- function(matriz_dist, matriz_fronteira, municipios_risco_Tipo_A, origem, destino, capacidade_maxima, retorno_origem) {
  # Definir os nomes dos municípios da matriz de distância
  municipios <- rownames(matriz_dist)
  
  # Definição do conjunto de municípios que serão avaliados pelo algoritmo, iniciando com a origem e tendo a inclusão dos demais municípios à medida que o algoritmo roda
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
    
    # Remoção do município atual do conjunto aberto
    conjunto_aberto <- setdiff(conjunto_aberto, municipio_atual)
    
    # Obtenção dos municípios vizinhos ao município atual
    vizinhos <- rownames(matriz_dist)[matriz_fronteira[match(municipio_atual, municipios), ] == 1]
    
    # Loop para avaliar os vizinhos
    for (vizinho in vizinhos) {
      # Cálculo do custo acumulado até o vizinho partindo do município atual
      custo_g <- pontuacao_g[municipio_atual] + matriz_dist[match(municipio_atual, municipios), match(vizinho, municipios)]
      
      # Verificação se o vizinho é um município de risco tipo A e ajuste do custo
      if (vizinho %in% municipios_risco_Tipo_A) {
        custo_g <- custo_g * 2
      }
      
      # Verificação se a restrição de retorno à origem está ativada
      if (retorno_origem) {
        # Cálculo do custo para retornar à origem antes de ir para o próximo destino
        custo_retorno_origem <- custo_g + matriz_dist[match(vizinho, municipios), match(origem, municipios)]
        
        # Verificação se o custo acumulado até o vizinho é menor que o custo registrado até o momento
        if (custo_retorno_origem < pontuacao_g[vizinho]) {
          # Atualização dos custos e referências para o vizinho
          veio_de[vizinho] <- municipio_atual
          pontuacao_g[vizinho] <- custo_retorno_origem
          pontuacao_f[vizinho] <- pontuacao_g[vizinho] + matriz_dist[match(vizinho, municipios), match(destino, municipios)]
          
          # Adição do vizinho ao conjunto aberto se ainda não estiver presente
          if (!(vizinho %in% conjunto_aberto)) {
            conjunto_aberto <- c(conjunto_aberto, vizinho)
          }
        }
      } else {
        # Verificação se o caminhão conseguirá transportar a carga
        if (custo_g <= capacidade_maxima) {
          # Verificação se o caminhão passará pelo segundo destino antes de ir para o destino final
          if (entrega_casada_destino && vizinho == segundo_destino) {
            custo_g <- custo_g + matriz_dist[match(vizinho, municipios), match(destino, municipios)]
          }
          
          # Verificação se o custo acumulado até o vizinho é menor que o custo registrado até o momento
          if (custo_g < pontuacao_g[vizinho]) {
            # Atualização dos custos e referências para o vizinho
            veio_de[vizinho] <- municipio_atual
            pontuacao_g[vizinho] <- custo_g
            pontuacao_f[vizinho] <- pontuacao_g[vizinho] + matriz_dist[match(vizinho, municipios), match(destino, municipios)]
            
            # Adição do vizinho ao conjunto aberto se ainda não estiver presente
            if (!(vizinho %in% conjunto_aberto)) {
              conjunto_aberto <- c(conjunto_aberto, vizinho)
            }
          }
        }
      }
    }
  }
  
  # Caso não seja possível encontrar uma rota, retornar um valor vazio
  return(NULL)
}

######################### Execução #####################################

# Verificar se a restrição de retorno à origem está ativada ou desativada
if (retorno_origem) {
  # Executar o algoritmo considerando a restrição de retorno à origem
  rota_curta <- rota_a_estrela_Tipo_A(matriz_dist, matriz_fronteira, municipios_risco_Tipo_A, origem, destino, capacidade_maxima, retorno_origem)
} else {
  # Executar o algoritmo sem a restrição de retorno à origem
  rota_curta <- rota_a_estrela_Tipo_A(matriz_dist, matriz_fronteira, municipios_risco_Tipo_A, origem, destino, capacidade_maxima, retorno_origem)
}

# Verificação se uma rota foi encontrada
if (!is.null(rota_curta)) {
  # Imprime a rota mais curta encontrada
  print(paste("Rota mais curta:", paste(rota_curta, collapse = " -> ")))
  
  # Calcula a distância total percorrida
  distancia_total <- sum(matriz_dist[match(rota_curta[-length(rota_curta)], rownames(matriz_dist)), match(rota_curta[-1], rownames(matriz_dist))])
  
  # Calcula o tempo gasto
  tempo <- calcular_tempo(distancia_total)
  
  # Imprime a distância e o tempo gasto
  print(paste("Distância percorrida:", distancia_total, "km"))
  print(paste("Tempo gasto:", tempo$horas, "horas e", tempo$minutos, "minutos"))
} else {
  # Imprime uma mensagem informando que não foi possível encontrar uma rota
  print("Não foi possível encontrar uma rota")
}



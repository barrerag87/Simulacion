# Simulación de un partido de tenis con resultado por juego y marcador punto a punto

# Parámetros iniciales
set.seed(123)  # Para reproducibilidad

# Nombres de los jugadores
jugador1 <- "Jugador A"
jugador2 <- "Jugador B"

# Probabilidades iniciales de ganar
prob_jugador1 <- 0.55  # Probabilidad de ganar para Jugador A
prob_jugador2 <- 0.45  # Probabilidad de ganar para Jugador B

# Función para obtener el marcador de tenis
obtener_marcador <- function(puntos) {
  if (puntos == 0) {
    return("0")
  } else if (puntos == 1) {
    return("15")
  } else if (puntos == 2) {
    return("30")
  } else if (puntos == 3) {
    return("40")
  } else {
    return("Juego")
  }
}

# Función para simular un juego con regla de "ventaja"
simular_juego <- function(prob1, prob2) {
  puntos_jugador1 <- 0
  puntos_jugador2 <- 0
  log_juego <- list()  # Para registrar el log del juego
  
  while (TRUE) {
    ganador <- ifelse(runif(1) < prob1, jugador1, jugador2)
    
    if (ganador == jugador1) {
      puntos_jugador1 <- puntos_jugador1 + 1
    } else {
      puntos_jugador2 <- puntos_jugador2 + 1
    }
    
    log_juego <- c(log_juego, list(paste(obtener_marcador(puntos_jugador1), "-", obtener_marcador(puntos_jugador2))))
    
    # Verificar si alguien ha ganado el juego
    if ((puntos_jugador1 >= 4 & (puntos_jugador1 - puntos_jugador2 >= 2)) |
        (puntos_jugador2 >= 4 & (puntos_jugador2 - puntos_jugador1 >= 2))) {
      break
    }
  }
  
  return(list(ganador = ganador, log = log_juego))
}

# Función para simular un set con resultado por juego
simular_set_con_juegos <- function(prob1, prob2) {
  puntos_para_ganar_set <- 6
  diferencia_para_ganar <- 2
  
  juegos_jugador1 <- 0
  juegos_jugador2 <- 0
  log_set <- list()  # Para registrar el log del set
  
  # Simular el set
  while (TRUE) {
    resultado_juego <- simular_juego(prob1, prob2)
    log_set <- c(log_set, list(resultado_juego$log))
    
    if (resultado_juego$ganador == jugador1) {
      juegos_jugador1 <- juegos_jugador1 + 1
    } else {
      juegos_jugador2 <- juegos_jugador2 + 1
    }
    
    # Verificar si alguien ha ganado el set
    if ((juegos_jugador1 >= puntos_para_ganar_set & (juegos_jugador1 - juegos_jugador2 >= diferencia_para_ganar)) |
        (juegos_jugador2 >= puntos_para_ganar_set & (juegos_jugador2 - juegos_jugador1 >= diferencia_para_ganar))) {
      break
    }
  }
  
  return(list(juegos_jugador1 = juegos_jugador1, juegos_jugador2 = juegos_jugador2, log = log_set))
}

# Simulación del partido con resultado por juego
simular_partido_con_juegos <- function(prob1, prob2, num_sets_para_victoria) {
  sets_ganados_jugador1 <- 0
  sets_ganados_jugador2 <- 0
  set_results <- list()  # Para almacenar resultados de cada set
  
  while (sets_ganados_jugador1 < num_sets_para_victoria & sets_ganados_jugador2 < num_sets_para_victoria) {
    resultado_set <- simular_set_con_juegos(prob1, prob2)
    set_results <- c(set_results, list(resultado_set))
    
    if (resultado_set$juegos_jugador1 > resultado_set$juegos_jugador2) {
      sets_ganados_jugador1 <- sets_ganados_jugador1 + 1
    } else {
      sets_ganados_jugador2 <- sets_ganados_jugador2 + 1
    }
  }
  
  return(list(ganador = ifelse(sets_ganados_jugador1 > sets_ganados_jugador2, jugador1, jugador2),
              sets_jugador1 = sets_ganados_jugador1,
              sets_jugador2 = sets_ganados_jugador2,
              resultados_sets = set_results))
}

# Simular el partido
resultado_partido <- simular_partido_con_juegos(prob_jugador1, prob_jugador2, num_sets_para_victoria)

# Imprimir el log por juego
for (set_result in resultado_partido$resultados_sets) {
  set_num <- 1
  for (juego in set_result$log) {
    print(paste("Juego", set_num))
    for (punto in juego) {
      print(punto)
    }
    set_num <- set_num + 1
  }
}

# Imprimir el resultado final del partido
print(paste("Ganador:", resultado_partido$ganador))
print(paste("Sets ganados por", jugador1, ":", resultado_partido$sets_jugador1))
print(paste("Sets ganados por", jugador2, ":", resultado_partido$sets_jugador2))


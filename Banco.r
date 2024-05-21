library(simmer)
library(simmer.plot)
library(ggplot2)

# Crear el entorno de simulación
env <- simmer("Bank")

# Definir el proceso de los clientes en el banco
customer_trajectory <- trajectory("Customer's Path") %>%
  seize("teller", 1) %>%
  timeout(function() runif(1, min=5, max=15)) %>% # tiempo en el cajero entre 5 y 15 minutos
  release("teller", 1)

# Añadir recursos (cajeros) y generadores de clientes al entorno
env %>%
  add_resource("teller", 3) %>% # 3 cajeros
  add_generator("customer", customer_trajectory, function() rexp(1, 1/10)) # llegada de clientes cada 10 minutos en promedio

# Ejecutar la simulación durante 8 horas (480 minutos)
env %>% run(until=480)

# Obtener y mostrar resultados
arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)

# Tabla de resumen de llegadas de clientes
print(arrivals)

# Tabla de resumen de uso de recursos
print(resources)

# Graficar las llegadas de clientes
ggplot(arrivals, aes(x=end_time, y=activity_time, color=name)) +
  geom_point() +
  labs(title="Llegadas de Clientes", x="Tiempo (minutos)", y="Tiempo de actividad (minutos)") +
  theme_minimal()

# Graficar la utilización de los cajeros
ggplot(resources, aes(x=time, y=server, color=resource)) +
  geom_step() +
  labs(title="Utilización de Cajeros", x="Tiempo (minutos)", y="Número de cajeros ocupados") +
  theme_minimal()

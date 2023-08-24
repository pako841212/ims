# Cargar las librerías necesarias
install.packages("lmtest")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("MatchIt")
install.packages("DMwR2")
install.packages("ROSE")
install.packages("ggrepel")
library(dplyr)
library(lmtest)
library(dplyr)
library(lubridate)
library(lmtest)
library(stargazer)
library(ggplot2)
library(MatchIt)
library(mice)
library(DMwR2)  
library(ROSE)
library(sandwich)
library(ggrepel)


# Modelo DiD
did_model <- lm(ta_sal ~ tratado + PostAumento + DiD, data = son1)

# Resumen del modelo
summary(did_model)

# Prueba de efecto significativo
coeftest(did_model, vcov = vcovHC(did_model)) 




#Grafica para 0 y 1 por separado:

# 1. Generar predicciones
son1$predictions <- predict(did_model, newdata = son1)

# 2. Crear conjunto de datos para contrafactual
contrafactual_data <- son1 %>%
  select(Fecha, PostAumento, tratado, predictions)

# 3. Calcular diferencias y graficar
ggplot(contrafactual_data, aes(x = Fecha, y = predictions, color = as.factor(PostAumento))) +
  geom_line() +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed") +
  facet_wrap(~ tratado) +
  labs(title = "Contrafactual: Cambio en predicciones antes y después del aumento",
       x = "Fecha", y = "Predicciones") +
  scale_color_discrete(name = "Post Aumento", labels = c("Antes", "Después")) +
  theme_minimal()




#Grafica para 0 y 1 pero juntas:

# 1. Generar predicciones
son1$predictions <- predict(did_model, newdata = son1)

# 2. Crear conjunto de datos para contrafactual
contrafactual_data <- son1 %>%
  select(Fecha, tratado, predictions) %>%
  distinct()  # Mantener solo una observación por Fecha y grupo

# 3. Calcular diferencias y graficar
ggplot(contrafactual_data, aes(x = Fecha, y = predictions, group = tratado, color = as.factor(tratado))) +
  geom_line() +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed") +
  labs(title = "Contrafactual: Cambio en predicciones antes y después del aumento",
       x = "Fecha", y = "Predicciones") +
  scale_color_discrete(name = "Grupo", labels = c("Control", "Tratamiento")) +
  theme_minimal()



#Grafica con el "ta" "observado" vs "predicciones":

# Crear la gráfica
ggplot(son1, aes(x = Fecha, y = ta_sal, group = tratado, color = as.factor(tratado))) +
  geom_line() +
  geom_point(aes(shape = as.factor(tratado))) +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed") +
  labs(title = "Nivel de Empleo Observado vs. Predicciones",
       x = "Fecha", y = "Nivel de Empleo") +
  scale_color_discrete(name = "Grupo", labels = c("Control", "Tratamiento")) +
  scale_shape_discrete(name = "Grupo", labels = c("Control", "Tratamiento")) +
  theme_minimal()


#GRafica con el "ta" "observado" vs "predicciones":

# Crear la gráfica
ggplot(contrafactual_data, aes(x = Fecha, y = ta_sal, group = tratado, color = as.factor(tratado))) +
  geom_line() +
  geom_point(aes(shape = as.factor(tratado))) +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed") +
  labs(title = "Nivel de Empleo Observado vs. Predicciones",
       x = "Fecha", y = "Nivel de Empleo") +
  scale_color_discrete(name = "Grupo", labels = c("Control", "Tratamiento")) +
  scale_shape_discrete(name = "Grupo", labels = c("Control", "Tratamiento")) +
  theme_minimal()




# Gráfico de barras para visualizar el efecto
effects <- did_model$coefficients["DiD"]
results <- data.frame(Effect = effects)

ggplot(results, aes(x = 1, y = Effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = Effect, ymax = Effect), width = 0.2) +
  labs(title = "Efecto del aumento del salario mínimo en el nivel de empleo",
       y = "Efecto en el Nivel de Empleo") +
  theme_minimal()













# Cargar las librerías necesarias
library(socviz)
library(lmtest)
library(car)

# Cargar los datos
data(organdata)

# Aplicar transformaciones logarítmicas
organdata$log_donors <- log(organdata$donors)
organdata$log_gdp <- log(organdata$gdp)
organdata$log_health <- log(organdata$health)
organdata$log_roads <- log(organdata$roads)

# Crear el modelo
modelo <- lm(log_donors ~ log_gdp + log_health + log_roads, data = organdata)

# Resumen del modelo
summary(modelo)

# Diagnósticos
par(mfrow = c(2,2))
plot(modelo)

# Tests
shapiro.test(residuals(modelo))
bptest(modelo)
dwtest(modelo)
vif(modelo)

# Análisis de variables relevantes
coef_summary <- summary(modelo)$coefficients
relevancia_variables <- coef_summary[order(abs(coef_summary[,"t value"]), decreasing = TRUE), ]
print(relevancia_variables)
# -----------------------------------------------------------------------------
# 0. Carga de paquetes y datos
# -----------------------------------------------------------------------------

# Se importa la base de datos de créditos empresariales
datos_credito <- read.csv('credit_portfolio.csv', header = TRUE, sep = ";")

# Librerías

library(margins)   # Para calcular efectos marginales
library(pROC)      # Para curva ROC
library(ggplot2)   # Para visualizaciones

# -----------------------------------------------------------------------------
# 1. Caracterización del portafolio
# -----------------------------------------------------------------------------

# Histograma del monto de crédito
hist(datos_credito$MontoCredito,
     main = "Distribución del Monto de Crédito",
     xlab = "Monto de Crédito",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "white",
     breaks = 30)

# Valor total del portafolio (exposición total)
saldo_total_portafolio <- sum(datos_credito$MontoCredito)
cat("Saldo total del portafolio:\n"); print(saldo_total_portafolio)

# Índice de concentración HHI (Herfindahl-Hirschman Index)
# Mide qué tan concentrado está el portafolio en pocos créditos
hhi_portafolio <- 10000 * sum((datos_credito$MontoCredito / saldo_total_portafolio)^2)
cat("Índice de concentración HHI (0-10000):\n"); print(hhi_portafolio)

# Porcentaje de deudores en incumplimiento (frecuencia de default)
porc_deudores_incumplidos <- sum(datos_credito$EstadoIncumplimiento) / 
  length(datos_credito$EstadoIncumplimiento)
cat("Porcentaje de deudores en incumplimiento:\n"); print(porc_deudores_incumplidos)

# Porcentaje del saldo total en incumplimiento (exposición en default)
porc_saldo_incumplido <- sum(datos_credito$MontoCredito * 
                               datos_credito$EstadoIncumplimiento) / saldo_total_portafolio
cat("Porcentaje del saldo total en incumplimiento:\n"); print(porc_saldo_incumplido)

# -----------------------------------------------------------------------------
# 2. Construcción del modelo
# -----------------------------------------------------------------------------

# Modelo completo Logit
modelo_logit_full <- glm(EstadoIncumplimiento ~ 
                           TamanoEmpresa + AntiguedadEmpresa + IngresosAnuales + 
                           UtilidadNeta + ActivoTotal + PasivoTotal + 
                           Rentabilidad + Liquidez + Endeudamiento + 
                           MontoCredito + PlazoMeses + TasaInteres + 
                           ScoreCrediticio,
                         data = datos_credito, 
                         family = binomial(link = "logit"))
summary(modelo_logit_full)

# Modelo completo Probit
modelo_probit_full <- glm(EstadoIncumplimiento ~ 
                            TamanoEmpresa + AntiguedadEmpresa + IngresosAnuales + 
                            UtilidadNeta + ActivoTotal + PasivoTotal + 
                            Rentabilidad + Liquidez + Endeudamiento + 
                            MontoCredito + PlazoMeses + TasaInteres + 
                            ScoreCrediticio,
                          data = datos_credito, 
                          family = binomial(link = "probit"))
summary(modelo_probit_full)

# Selección de variables mediante procedimiento stepwise
step(modelo_logit_full, direction = "both")
step(modelo_probit_full, direction = "both")

# Modelos reducidos con variables seleccionadas

modelo_logit_1 <- glm(EstadoIncumplimiento ~ 
                        TamanoEmpresa + Liquidez + Endeudamiento + 
                        MontoCredito + ScoreCrediticio,
                      data = datos_credito, 
                      family = binomial(link = "logit"))
summary(modelo_logit_1)

modelo_logit_2 <- glm(EstadoIncumplimiento ~ 
                        TamanoEmpresa + Liquidez + Endeudamiento + 
                        ScoreCrediticio,
                      data = datos_credito, 
                      family = binomial(link = "logit"))
summary(modelo_logit_2)

modelo_logit_3 <- glm(EstadoIncumplimiento ~ 
                        Liquidez + Endeudamiento + ScoreCrediticio,
                      data = datos_credito, 
                      family = binomial(link = "logit"))
summary(modelo_logit_3)

modelo_probit_1 <- glm(EstadoIncumplimiento ~ 
                         TamanoEmpresa + Liquidez + Endeudamiento + 
                         MontoCredito + ScoreCrediticio,
                       data = datos_credito, 
                       family = binomial(link = "probit"))
summary(modelo_probit_1)

modelo_probit_2 <- glm(EstadoIncumplimiento ~ 
                         TamanoEmpresa + Liquidez + Endeudamiento + 
                         ScoreCrediticio,
                       data = datos_credito, 
                       family = binomial(link = "probit"))
summary(modelo_probit_2)

modelo_probit_3 <- glm(EstadoIncumplimiento ~ 
                         Liquidez + Endeudamiento + ScoreCrediticio,
                       data = datos_credito, 
                       family = binomial(link = "probit"))
summary(modelo_probit_3)

# Comparación de modelos usando AIC
AIC(modelo_logit_1, modelo_logit_2, modelo_probit_1, modelo_probit_2, modelo_probit_3)

# Modelo final seleccionado
modelo_final <- modelo_probit_3

# -----------------------------------------------------------------------------
# 3. Análisis de resultados
# -----------------------------------------------------------------------------

summary(modelo_final)

# Efectos marginales (impacto de cada variable sobre la probabilidad de incumplimiento)
efectos_marginales <- margins(modelo_final)
summary(efectos_marginales)

# -----------------------------------------------------------------------------
# 4. Estimación de probabilidades
# -----------------------------------------------------------------------------

# Probabilidad estimada de incumplimiento (PD) para cada deudor
resultados_pd <- data.frame(
  prob_incumplimiento = modelo_final$fitted,
  incumplimiento_real = datos_credito$EstadoIncumplimiento
)

# Distribución de probabilidades estimadas
ggplot(resultados_pd, aes(x = prob_incumplimiento, fill = factor(incumplimiento_real))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Probabilidades Estimadas",
       x = "Probabilidad de Incumplimiento (PD)",
       y = "Densidad",
       fill = "Incumplimiento") +
  theme_minimal()

# Curva ROC y AUC
curva_roc <- roc(resultados_pd$incumplimiento_real, resultados_pd$prob_incumplimiento, smooth = TRUE)
plot(curva_roc, col = "blue", main = "Curva ROC")
print(curva_roc)

# -----------------------------------------------------------------------------
# 5. Pérdida esperada
# -----------------------------------------------------------------------------

# Base con probabilidades estimadas
datos_portafolio <- data.frame(datos_credito, 
                               PD = modelo_final$fitted)

# Pérdida esperada individual (PE = PD * LGD)
datos_portafolio$perdida_esperada_individual <- datos_portafolio$PD * datos_portafolio$PDI

# Pérdida esperada total del portafolio
perdida_esperada_total <- sum(datos_portafolio$perdida_esperada_individual * 
                                datos_portafolio$MontoCredito)

# Pérdida esperada como proporción del portafolio
perdida_esperada_pct <- perdida_esperada_total / saldo_total_portafolio

cat("Pérdida esperada total del portafolio:\n"); print(perdida_esperada_total)
cat("Pérdida esperada como % del portafolio:\n"); print(perdida_esperada_pct)

# -----------------------------------------------------------------------------
# 6. Credit VaR (Simulación Bootstrap)
# -----------------------------------------------------------------------------

# Parámetros base
n_observaciones <- length(datos_portafolio$MontoCredito) / 2
exposicion <- datos_portafolio$MontoCredito
pdi <- datos_portafolio$PDI
pd <- datos_portafolio$PD

n_simulaciones <- 100000

perdidas_simuladas <- numeric(n_simulaciones)
perdidas_simuladas_pct <- numeric(n_simulaciones)

# Simulación de pérdidas mediante bootstrap
for (i in 1:n_simulaciones) {
  
  # Remuestreo con reemplazo
  idx <- sample(1:n_observaciones, n_observaciones, replace = TRUE)
  
  # Simulación de defaults (variable Bernoulli)
  default_simulado <- rbinom(n_observaciones, 1, pd[idx])
  
  # Cálculo de pérdidas
  perdidas_simuladas[i] <- sum(exposicion[idx] * pdi[idx] * default_simulado)
  
  # Pérdida como proporción del portafolio simulado
  perdidas_simuladas_pct[i] <- perdidas_simuladas[i] / sum(exposicion[idx])
}

# Métricas de riesgo
perdida_esperada_simulada <- mean(perdidas_simuladas_pct)
credit_var_99 <- quantile(perdidas_simuladas_pct, 0.99)

perdida_no_esperada <- credit_var_99 - perdida_esperada_simulada

# Cálculo de provisiones y capital
provisiones <- perdida_esperada_simulada * saldo_total_portafolio
capital_requerido <- perdida_no_esperada * saldo_total_portafolio

cat("Provisiones requeridas:\n"); print(provisiones)
cat("Capital económico requerido (pérdida no esperada):\n"); print(capital_requerido)
cat("Pérdida esperada simulada (% portafolio):\n"); print(perdida_esperada_simulada)
cat("Pérdida no esperada (% portafolio):\n"); print(perdida_no_esperada)
cat("Credit VaR al 99%:\n"); print(credit_var_99)

# -----------------------------------------------------------------------------
# 7. Visualización
# -----------------------------------------------------------------------------

# Distribución de pérdidas simuladas
densidad_perdidas <- density(perdidas_simuladas_pct)

plot(densidad_perdidas, 
     main = "Distribución de pérdidas como porcentaje del portafolio")

abline(v = perdida_esperada_simulada, col = "red", lwd = 2, lty = 2)
abline(v = credit_var_99, col = "red", lwd = 2, lty = 2)

text(perdida_esperada_simulada, 0.00000002, 
     paste("PE =", round(perdida_esperada_simulada, 2)), col = "red", pos = 4)

text(credit_var_99, 0.00000002, 
     paste("CreditVaR 99% =", round(credit_var_99, 2)), col = "red", pos = 4)

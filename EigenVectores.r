#Ejemplo en R
#Datos https://www.cienciadedatos.net/documentos/35_principal_component_analysis

datos <- data.frame(X1 = c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1),
                    X2 = c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9))
datos

#Datos centrados por la media
datos_centrados <- datos
datos_centrados$X1 <- datos$X1 - mean(datos$X1)
datos_centrados$X2 <- datos$X2 - mean(datos$X2)
datos_centrados

#Covarianza
matriz_cov <- cov(datos_centrados)
matriz_cov

#EigenVectores
eigen <- eigen(matriz_cov)
eigen$values
eigen$vectors

#Traspuesta de EigenVectores
t_eigenvectors <- t(eigen$vectors)
t_eigenvectors

#Traspuesta de datos centrados
t_datos_centrados <- t(datos_centrados)
t_datos_centrados

#
# Producto matricial
pc_scores <- t_eigenvectors %*% t_datos_centrados
rownames(pc_scores) <- c("PC1", "PC2")

# Se vuelve a transponer para que los datos estén en modo tabla
t(pc_scores)


datos_recuperados <- t(eigen$vectors %*% pc_scores)
datos_recuperados[, 1] <- datos_recuperados[, 1] + mean(datos$X1)
datos_recuperados[, 2] <- datos_recuperados[, 2] + mean(datos$X2)
datos_recuperados


#######################################
#######################################
#######################################
############ 20/05/2021 ###############
#### Reconstrucion de las parcelas ####
##        mixtas y puras de        ####
##      P,pinea  & P. pinaster     ####
#######################################

rm(list = ls())

library("reshape2")
library ("stringr")
library("dplyr")
library("doBy")
library("ggplot2")

## dado que la base de datos


setwd("C:/Marta/Tesis Marta/Trabajos/4º Articulo/Analisis/Reconstruccion parcelas/Reconstrucion parcelas/24-09-Marta")

## cargo la base de los cores
Db <- read.delim("BaseCoresCorregida_mod_2.csv", sep = ";", dec = ",", header = TRUE)

# cargo la base de datos con la informacion de la parcelas
base_plots <- read.delim("Base de datos Valladolid_completa&edad.csv", sep = ";", dec = ",", header = TRUE)


# seleciono los datos con los que voy a trabajar.
# Elimino los tococnes, las encinas y las mediciones de la base.
# solamente quiero los dos cores a la alura de pecho

# lo elimino de la base donde tengo las mediciones de la parcela
base_plots$Sp <- as.character(base_plots$Sp)
base_plots <- base_plots[! base_plots$Sp == "T",]
base_plots <- base_plots[! base_plots$Sp == "45",]
base_plots <- base_plots[,c(1,11,12,24)] #limpio un poco la base de datos 


# lo elimino de la base de los cores
cores_to_select = c("A1", "A2")
DataBase <- Db %>%
  filter(IC %in% cores_to_select)


#########################
## ---- Base cores 
#########################
# comienzo con los cores: Estos arboles fueron barrenados en el año 2016 y voy a reconstruir las parcelas 15 años, por lo tanto hasta el año 2001.
# lo primero es calcular ese incremento


# voy a crear la columna "code.tree" que la voy a necesitar mas adelante

DataBase$code.tree <- paste(DataBase$Plot, DataBase$Tree, DataBase$Sp, sep = "-")

# por lo tanto, de momento, seleciono el periodo 2016-2001
head(DataBase)
colnames(DataBase)
base <- DataBase[,c(1:16,22)]
head(base)

# cada medicion tengo que  ultiplicarlo por dos. Ya que voy a trabajar con diametro
# y lo que tengo aqui es el incremento radial.

base[,c(2:16)] <- apply(base[,c(2:16)], 2, function (x){
  #x <- x*0.01 # lo paso a mm (las unidades son en centesimas de mm) No hace falta en mi caso
  x2 <- x*2  # multiplico por 2 para tener el incremento en diametro en vez del incremento radial
  return(x2)
})

# ademas, solamente quiero una medicion por arbol, como tengo dos, realizo la media de los dos cores (A1-A2)
base <- base[,-1]
base <- aggregate(base[,1:16], list(base$code.tree), mean, na.rm=TRUE)
warnings()
colnames(base)[1]<- "code.tree" # me cambia el nombre, asi que se lo vuelvo a cambiar
colnames(base)
base <- base[,-17]
head(base)


#lo que tengo ahora es el incremento en DIAMETRO
# Incroporo el diametro medio del arbol medido durante el ultimo año

head(base_plots)
base_plots$code.tree <- paste(base_plots$Parcela, base_plots$Arbol, base_plots$Sp, sep = "-")

#de base_plots solo estoy interesada en el diametro, hago una sub-base

base_diam <- base_plots[, c(4, 5)]

# quiero calcular id para cada arbol barrenado 
colnames(base)[2:16] <- c(2001:2015)

# ahora ya lo pongo en version larga, cambio los nombres de las columnas y transformo las variables
# al formato que le corresponde
Data <- melt(base, id.vars = c("code.tree"))
colnames(Data) <- c("Code","Year", "Id")

# lo ordeno
Data$Year <- as.numeric(as.character(Data$Year))
Data <- with(Data, Data[order(Code,-Year),])
Data  <- Data[!Data$Id == "NaN",]
# calculo lo que crece cada arbol en ese periodo (Id_period)
tree.growth <- Data %>%
  group_by(Code) %>%
  summarise(Id_period = sum(Id))

tree.growth <- as.data.frame(tree.growth)
colnames(tree.growth)[1] <- "code.tree"

base.completa <- merge(tree.growth, base_diam, by = "code.tree")

#base.completa$DN_cm <- base.completa$DN_mm/10
base.completa$DN._2016 <- as.numeric(base.completa$DN._2016)

head(base.completa)

#necesito un codigo para cada parcela 
base.completa$code.tree <- as.character(base.completa$code.tree)
List.code <- strsplit(base.completa$code.tree, "-")
aplot <- lapply(List.code, function(x) x[1])
aplot <- do.call("c", aplot)
base.completa$plot <- aplot


asp = lapply(List.code, function(x) x[3])
asp = do.call("c", asp)
base.completa$sp <- asp

base.completa$code.plot <- paste(base.completa$plot,base.completa$sp, sep = "-")
head(base.completa)
########################################
########################################
# aplico la regresion lineal para cada parcela

lista_plot <- split(base.completa, base.completa$code.plot)


## ahora, aplico la regresion lineal para cada parcela
reg.bc1 <- lapply(seq_along(lista_plot), function(x){
  
  ala <- lista_plot[[x]]
  regres <- lm(ala$Id_period ~ ala$DN._2016)
  ## extraigo los valores que me interesan de la regsion
  regres <- coef(regres)
  return(regres)
})

## para que no se haga lio, le doy a cada regresion
## el nombre de la parcela que le corresponde
names(reg.bc1) <- names(lista_plot)


df_reg.bc1 <-  do.call("rbind", reg.bc1)
df_reg.bc1 <- as.data.frame(df_reg.bc1)
head(df_reg.bc1) # aqui tengo los parametros de la regresion 

########################################
########################################

# vuelvo a la base de datos de las parcelas. A los arboles que no han sido barrenados
trees <- c(base$code.tree)
base_arboles_noBarr <- base_plots[!base_plots$code.tree%in%trees,]
base_arboles_noBarr$DN._2016 <- as.numeric(base_arboles_noBarr$DN._2016)

df_reg.bc1$code.plot <- paste(row.names(df_reg.bc1))
base_arboles_noBarr$code.plot <- paste(base_arboles_noBarr$Parcela, 
                                       base_arboles_noBarr$Sp, sep = "-")

## ahora los voy a unir
base.completa_2 <- merge(base_arboles_noBarr, df_reg.bc1, by = "code.plot")
names(base.completa_2)[7] <- c("a")
names(base.completa_2)[8] <- c("b")
base.completa_2$id_period <- base.completa_2$a + (base.completa_2$b * base.completa_2$DN._2016)
head(base.completa_2) # ya tengo lo que crecen los arboles no barraneados en el periodo

########################################
########################################
length(unique(base.completa_2$code.tree))
### !!!!!!!!!!
# sin embargo hay un problema (regresion, valores negativos)
# pro lo que voy a eliminar esos arboles con crecimiento negativo. 

error <- base.completa_2[base.completa_2$id_period<0,]
error.trees <- c(error$code.tree)
base.completa_2 <- base.completa_2[!base.completa_2$code.tree%in%error.trees,]

# pierdo 20 arboles solamente
########################################
########################################
test <- with(base.completa_2, base.completa_2[order(id_period),])
########################################
########################################

# vuelvo a la base de los cores
head(base.completa)
# lo que crece durante los 15 años
crec.period <- base.completa[,c(1,2)]

#necesito la base anterior 
names(Data)[1] <- "code.tree"

base.completa.cores <- merge(crec.period, Data, by = "code.tree")



#calculop el porcentage de crecimiento de cada año

base.completa.cores$Perc <- (base.completa.cores$Id * 100)/base.completa.cores$Id_period
head(base.completa.cores)


# voy a comprobarlo

Total_crec <- summaryBy(list(c("Perc"), c("code.tree")),
                        data = base.completa.cores,
                        FUN = c(sum), na.rm =TRUE)



#ahora necesito la media por parcela y año, de esos percentages
# primero necesito el codigo de la parcela

base.completa.cores$code.tree <- as.character(base.completa.cores$code.tree)
List.code <- strsplit(base.completa.cores$code.tree, "-")
aplot <- lapply(List.code, function(x) x[1])
aplot <- do.call("c", aplot)
base.completa.cores$plot <- aplot


asp = lapply(List.code, function(x) x[3])
asp = do.call("c", asp)
base.completa.cores$sp <- asp

base.completa.cores$code.plot <- paste(base.completa.cores$plot, base.completa.cores$sp, sep = "-")
head(base.completa.cores)


#asi calculo lo que crece cada parcela de media en porcentage
perc.plot <- base.completa.cores %>%
  group_by(code.plot, Year) %>%
  summarise(perce.plot = mean(Perc))

perc.plot <- as.data.frame(perc.plot)
colnames(perc.plot)

# compruebo
Total_plot <- summaryBy(list(c("perce.plot"), c("code.plot")),
                        data = perc.plot,
                        FUN = c(sum), na.rm =TRUE)

# creo un codigo de parcela en base arboles no barrenados

base.completa_2$code.plot <- paste(base.completa_2$Parcela, base.completa_2$Sp, sep = "-")
head(base.completa_2)
final.df <- merge(base.completa_2, perc.plot, by = "code.plot")
head(final.df)

colnames(final.df)
final.df <- final.df[,c(1,5,6,9,10,11)]
final.df$id.year <- (final.df$id * final.df$perce.plot)/100
head(final.df)

# compruebo que tengo 100 parecelas 
length(unique(final.df$code.plot))
# RECUERDA MARTA, SON INCREMENTOS DIAMETRALES, PARA RADIAL DIVIDE ENTRE DOS

# finalmente voy a sacar los codigos y a ordenar 
final.df$code.tree <- as.character(final.df$code.tree)
List.code <- strsplit(final.df$code.tree, "-")
aplot <- lapply(List.code, function(x) x[1])
aplot <- do.call("c", aplot)
final.df$plot <- aplot

atree <- lapply(List.code, function(x) x[2])
atree <- do.call("c", atree)
final.df$tree <- atree

asp = lapply(List.code, function(x) x[3])
asp = do.call("c", asp)
final.df$sp <- asp

head(final.df)



final.df <- with(final.df, final.df[order(plot, tree, -Year),])


# necesito calcular el diametro de cada año


lista_tree <- split(final.df, final.df$code.tree)

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# prueba con un arbol
# tree <- final.df[final.df$code.tree=="1-8-23",]
# tree <- tree[, c("DN._2016", "id.year", "Year")]
# tree$DN._2016 <- tree$DN._2016*10
# tree$Cumis <- cumsum(tree$id.year)
# tree$NwDiam <- tree$DN._2016 - Cumis
# tree <- tree[, -4]
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
final.df <- split(final.df, final.df$code.tree)
final.df <- lapply(final.df, function(XX){
  XX$DN._2016 <- XX$DN._2016*10
  XX$Cumis <- cumsum(XX$id.year)
  XX$NwDiam <- XX$DN._2016 - XX$Cumis
  XX <- XX[, - 11]
  return(XX)
  })

final.df <- do.call("rbind", final.df)
head(final.df)




# tree <- data.table(DN._2016 = c(NA, tree$DN._2016),
#                    id.year = c(NA, tree$id.year))










## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@

final.df$ir.year <- final.df$id.year/2
final.df$BAI <- pi * ((final.df$ir.year)^2)
head(final.df,50)




#Incrementos$BAI <- pi * ((Incrementos$DN._2016/2))^2

#fitDat <- Incrementos %>% 
 # group_by(plot, tree) %>%
  #mutate(BAI_1 = BAI, BAI_2 = lead(BAI))
#dim(fitDat)
#fitDat <- na.omit(fitDat)


#fitDat$IBAI <- fitDat$BAI_1 - fitDat$BAI_2
#head(fitDat)










saveRDS(final.df, file = "C:/Marta/Tesis Marta/Trabajos/4º Artículo (bueno)/Analisis/Datos descriptivos/Paper competencia/base_def.rds")
write.csv(final.df, file = "C:/Marta/Tesis Marta/Trabajos/4º Artículo (bueno)/Analisis/Datos descriptivos/Paper competencia/base_def.csv", row.names = TRUE)

############################################################3

#final.df <- with(final.df, final.df[order(-id.year),])








library(tidyverse)
library(dplyr)
library(stringr)
library(agricolae)

baseComp <- read.csv2("sivigila_violencia.csv")
base <- baseComp %>%
  select("edad_","nombre_barrio","sexo_", "comuna","tip_cas_",
         "pac_hos_","mod_viol", "year_" ) %>%
  filter(baseComp$nombre_barrio!="SIN INFORMACION",baseComp$nombre_barrio!="Sin informacion",
         baseComp$comuna !="SIN INFORMACION",baseComp$comuna !="Sin informacion") 
base2 <- base %>%
  filter(base$comuna != "Corregimiento De Santa Elena", 
         base$comuna != "Santa Elena", base$comuna != "Corregimiento de San Cristobal", 
         base$comuna !="San Sebastian de Palmitas", base$comuna !="San Antonio de Prado",
         base$comuna !="Altavista")

base2$comuna <- str_replace(base2$comuna, "Doce De Octubre","Doce de Octubre")

base2 <- base2 %>%
  filter(base2$mod_viol != "8",base2$mod_viol != "11")

base$mod_viol <- factor(base$mod_viol)
base$pac_hos_ <- factor(base$pac_hos_)
base$tip_cas_ <- factor(base$tip_cas_)
base$sexo_ <- factor(base$sexo_)


#Gráficos  

tabla1 <- table(base2$comuna, rownames(base2$comuna))
tabla1 <- prop.table(tabla1)
xx <- barplot(tabla1, col= "aquamarine2",
              ylab='Frecuencia relativa', las=3, ylim = c(0, 0.12), 
              main = "Casos de violencia por comuna", cex.names  = 0.7)

text(x=xx, y=tabla1, pos=3, cex=0.8, col="black",
     label=round(tabla1, 4))

tabla2 <- table(base2$sexo_, base2$comuna)

g2 <- barplot(tabla2, beside = TRUE, las=3, ylab='Frecuencia',
        col = c("plum1", "aquamarine1"), ylim = c(0,5500), cex.names = 0.6,
        main = "Casos de violencia en las comunas por género")
text(x=g2, y=tabla2, pos=3, cex=0.8, col="black",
     label=round(tabla2, 4))
legend('topleft', legend=rownames(tabla2), bty='n',
       fill=c("plum1", "aquamarine1"))


## segunda variable

tabla3 <- table(base2$mod_viol)
tabla3 <- prop.table(tabla3)
g3 <- barplot(tabla3, col= "aquamarine",
              ylab='Frecuencia relativa', las=1, ylim = c(0, 0.6), 
              main = "Modo de violencia", cex.names  = 0.7)

text(x=g3, y=tabla3, pos=3, cex=0.8, col="black",
     label=round(tabla3, 4)*100)

tabla4 <- table(base2$year_)
tabla4 <- prop.table(tabla4)
g3 <- barplot(tabla4, col= "aquamarine",
              ylab='Frecuencia relativa', las=1, ylim = c(0, 0.4), 
              main = "Casos de violencia por año", cex.names  = 0.7)
text(x=g3, y=tabla4, pos=3, cex=0.8, col="black",
     label=round(tabla4, 4)*100)
curve(tabla4,col = "magenta")

#RESUMENES ESTADISTICOS

sexo <- factor(base2$sexo_, levels=c("F","M"),
            labels=c("Femenino","Masculino"))
ModViolencia <- factor(base2$mod_viol, levels=c("1","10", "12", "14", "2",
                                                "3", "4", "5", "6", "7", "SD"),
                labels=c("Física","Trata de personas","Actos sexuales con uso de la fuerza",
                         "Otros actos sexuales","Psicológica","Negligencia y abandona",
                         "Abuso sexual","Acoso sexual","Violación",
                         "Explotación sexual de niños y adolescentes","Sin información"))


tablaSexo <- table(sexo);
tablaSexo <- prop.table(tablaSexo)

tablaMod <- table(ModViolencia)
tablaMod <- prop.table(tablaMod)

prop.table(table(sexo,ModViolencia),2)

ed <- table.freq(hist(base2$edad_,plot=FALSE))
ed
a <- table.freq(base2$comuna)

summary(base2)

baseH <- base2 %>%
  filter(base2$sexo_ == "M")

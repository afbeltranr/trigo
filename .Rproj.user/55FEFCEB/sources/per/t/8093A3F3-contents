library(viridis)
library(plotrix)
m <- read.table("matrizbien.txt")
m2 <- m[,-2]
mt <- t(m2)
colnames(mt) <- as.character(unlist(mt[1,]))
mt2 <- mt[-1, ]
mt3 <- mt2 [,-1869]
h <- mt3[,298:366]

m3 <- t(mt3)
wn <- as.numeric(rownames(m3))
wn2 <- matrix(, nrow=1868, ncol=1)

for (k in 1:1868){

wn2[k,1]<- NA
}                               


for (l in 1:1868){

wn2[l,1]<- wn[l]
}

der30 <- matrix(,nrow=62, ncol=96)

#------------------- ------------------------------------
#creo una matriz que contendrá los elementos de la matriz inicial m3 por intervalos de 30
for (i in 1:62){
    for (j in 1:96){
der30[i,j] <- NA
}
}
#la lleno con objetos vacíos 
for (i in 1:62){
    for (j in 1:96){
der30[i,j] <- m3[i*30,j]
}
}


wn30<- matrix(,nrow=62, ncol=1)
for (i in 1:62){
wn30[i,1]<-wn2[i*30,1]
wn30_2<-wn30[-62,]
}
# Hasta aquí creé los intervalos, ahora haré la derivada 



deri30<- matrix(, nrow=62, ncol=96)


 for (i in 1:62){
    for (j in 1:96){
deri30[i,j] <- NA
}
}
#hasta aquí creo u obljeto vacío donde estarán los resultados

for(i in 1:96){
for (j in 1:61) {
deri30[j,i] <- (der30[j+1,i]-der30[j,i])/(wn30[j+1,1]-wn30[j,1])
}
}

deri2_30<- matrix(, ncol=96,nrow=61)#creo el objeto para la segunda derivada
 for (i in 1:61){
    for (j in 1:96){
deri2_30[i,j] <- NA
}
}
wn2_30_2<-wn30_2[-61]

for(i in 1:96){
for (j in 1:61) {
deri2_30[j,i] <- (deri30[j+1,i]-deri30[j,i])/(wn30[j+1,1]-wn30[j,1])
}
}


deri2_30t <- t(deri2_30)
deri2_30t_2 <-  deri2_30t[,-61]


frecuencias_30 <- matrix(,ncol=1,nrow=62)

for (i in 1:62){
frecuencias_30[i] <- wn2[i*30]
}
frecuencias_30_2 <- frecuencias_30[-c(61,62)]
colnames(deri2_30t_2) <-  frecuencias_30_2 
nombres<- rownames(h)
rownames(deri2_30t_2) <- nombres

muestrasbuenas <-deri2_30t_2[-c(73:95),]#extraigo las muestras hasta 213
class.muestrasbuenas <- matrix(c(2,2,2,3,3,2,2,2,3,3,3,4,4,4,2,2,2,3,3,3,4,4,2,2,3,3,4,4,5,5,2,2,2,3,3,3,4,4,4,5,5,5,2,2,2,3,3,3,4,4,4,5,5,5,2,6,2,6,2,6,3,3,3,3,3,3,4,4,4,5,5,5,3), nrow=73,ncol=1)
# Rango de Si-O

rangoSiO <-muestrasbuenas[,c(10:13)]
rangoLig <-muestrasbuenas[,c(12:25)]

#---------------------juntos-------------------

clasi <- read.table("clasifinal.txt")
clasif <- clasi[,2] 



juntos <- cbind(rangoSiO,rangoLig)

pcajuntos <- prcomp(juntos , center=TRUE)

valp <- (pcajuntos$sdev)^2
varian <- (valp*100)/sum(valp)
varian2 <- varian[c(1:10)]
color <- c("lightpink4","orangered","hotpink4","aquamarine", "darkolivegreen1")
color2 <- viridis(10)
color2i <- rev(color2)
# tiff("barras.tiff", width = 15, height = 15, units = 'cm', res = 800, compression = 'none')
barplot(varian2,
		ylab="Porcentaje de varianza",
	      xlab="Componentes",
	      col=color2,
	      names.arg=1:10)
# dev.off()
readline("barplot" )

coord <- pcajuntos$x
class <- class.muestrasbuenas[-c(67,27,35,36,4,5,19,34,73,20,18,12,11,10,9,8,7,6,59,55,1,2,3,30,42,41,71,29)]
# 
# 61.8312496 24.0193306  8.7290524  
 coord <- coord[-c(67,27,35,36,4,5,19,34,73,20,18,12,11,10,9,8,7,6,59,55,1,2,3,30,42,41,71,29),]

 #---------------------pc1 pc2 --------------------

 plot(coord[,1],
 	 coord[,2],
 	 col=class,
 	 pch=19,
 	 xlab="PC1 - 61.83%",
 	 ylab="PC2 - 24.01%")

 text(coord[,1],
 	coord[,2],
 	clasif,
 	cex=1,
 	pos=3)

 abline(h=0,
 	   v=0,
 	   lty=2)

# readline("PC1 PC2")

# NO CONLUYENTE
#--------------PC1 PC3 ---------------------------

#61.8312496 24.0193306  8.7290524  
# plot(coord[,1],
# 	 coord[,3], 
# 	 col=class,
# 	  pch=19,
# 	  xlab="PC1 -61.83%",
# 	  ylab="PC3 - 8.73%")

# text(coord[,1],
# 	 coord[,3], 
# 	 clasif, 
# 	 cex=1, 
# 	 pos=3)

# abline(h=0,
#        v=0, 
#        lty=2)

# legend("bottomleft", c("Straw","Leaf","Root","Inflorescence","strawpods"), col=c(2,3,4,5,6), pch=19)

# draw.ellipse(3.5e-6,-1e-6, a =0.6e-6, b = 1.8e-6, angle =-70, deg = TRUE, nv = 200, border = "blue", col = NA, lty = 2, lwd = 1, xlim=c(-3,3),ylim=c(-1,1.2))



# readline("PC1 PC3")

##------------------PC2-PC3----------------------

# 61.8312496 24.0193306  8.7290524 

# tiff("PC2PC3.tiff", width = 15, height = 15, units = 'cm', res = 800, compression = 'none')
# win.grap
 
plot(coord[,2], 
	 coord[,3],
	 col=class, 
	 pch=19,
	 xlab="PC2 - 24.02%",
	 ylab="PC3 - 8.73%",
	 # xlim=c(-2e-6,2e-6),
	  ylim=c(-3e-6,3e-6))



text(coord[,2], 
	 coord[,3], 
	 clasif, 
	 cex=1, 
	 pos=3)

abline(h=0, 
	   v=0, 
	   lty=2)


legend("bottomleft", c("Stem","Leaf","Root","Inflorescence","strawpods"), col=c(2,3,4,5,6), pch=19)

library(plotrix)

draw.ellipse(-4.1e-6,1.9e-6, a =0.5e-6, b = 0.4e-6, angle =90, deg = TRUE, nv = 200, border = "green", col = NA, lty = 2, lwd = 1, xlim=c(-3,3),ylim=c(-1,1.2))

draw.ellipse(-7.6e-6,2.3e-6, a =0.7e-6, b = 0.6e-6, angle =90, deg = TRUE, nv = 200, border = "green", col = NA, lty = 2, lwd = 1, xlim=c(-3,3),ylim=c(-1,1.2))


draw.ellipse(-0.95e-6,0.37e-6, a =0.24e-6, b = 0.78e-6, angle =82, deg = TRUE, nv = 200, border = "green", col = NA, lty = 2, lwd = 1, xlim=c(-3,3),ylim=c(-1,1.2))


draw.ellipse(-0.3e-6,-2.27e-6,a =0.4e-6, b = 0.12e-6, angle =0, deg = TRUE, nv = 200, border = "blue", col = NA, lty = 2, lwd = 1, xlim=c(-3,3),ylim=c(-1,1.2))


draw.ellipse(0.8e-6,-1.2e-6,a =1.4e-6, b = 0.35e-6, angle =47, deg = TRUE, nv = 200, border = "blue", col = NA, lty = 2, lwd = 1, xlim=c(-3,3),ylim=c(-1,1.2))


draw.ellipse(-0.4e-6,-1.8e-6,a =1e-6, b = 0.32e-6, angle =155, deg = TRUE, nv = 200, border = "blue", col = NA, lty = 2, lwd = 1, xlim=c(-3,3),ylim=c(-1,1.2))

# dev.off()
readline("PC2 PC3")



#------- tres PC lignina silicio---------------


# 61.8312496 24.0193306  8.7290524 
library(rgl)
r3dDefaults$windowRect <- c(0,50, 800, 800) 

plot3d(coord[,1],coord[,2],coord[,3], xlab="PC1 - 61.83%", ylab="PC2 - 24.02%", zlab="PC3 - 8.73%",col=class, type="s", size=0.8)
text3d(coord[,1],coord[,2],coord[,3],clasif, pos=3 , cex=1.6)
readline("tres PC silicio y lignina ")


# # "ps", "eps", "tex", "pdf", "svg", "pgf"
# rgl.postscript("graph.pgf", fmt="pgf")
#----------------gif--------------------------


# gif<- coord[,c(1:3)]

# write.table(gif, "gif.txt")

# plot3d(gif, xlab="PC1 - 49.66%", ylab="PC2 - 24.67%", zlab="PC3 - 11.61%",col=class, type="s", size=0.7)
# text3d(coord[,1],coord[,2],coord[,3],clasif, pos=3 , cex=1.6)
#       par3d(windowRect = c(0, 50, 800, 800))

# readline("no seras tu")
 Angle1 <- 2

 Angle <- rep(Angle1 * pi / 180, 360/Angle1)


# Animation.dir <- paste(getwd(), "/animation/", sep="")
 
#  for (i in seq(Angle)) {
#     view3d(userMatrix = rotate3d(par3d("userMatrix"),
#      Angle[i], 0, 0, 1))

#     rgl.snapshot(filename=paste(paste(Animation.dir, "frame-", sep=""),
#      sprintf("%03d", i), ".png", sep=""))
#       }
#   move.to.animation.dir <- paste("cd", Animation.dir, "&&") 
#  ImageMagick.code <- paste("convert -delay ", 5, " -loop 0 frame*.png animated.gif", sep="") 
#  system(paste(move.to.animation.dir, ImageMagick.code))


#------------- ellipse --------------------

# primero hay que agrupar, y de tales grupos obtener las coordenadas en 3D

# plot3d(coord[,1],coord[,2],coord[,3], xlab="PC1 - 61.83%", ylab="PC2 - 24.02%", zlab="PC3 - 8.73%",col=class, type="s", size=0.8)
# text3d(coord[,1],coord[,2],coord[,3],rownames(coord), pos=3 , cex=0.8)

# readline("nombres para agrupar")

# grupo1 <- coord[c(16,26),c(1,2,3)]

# x <- grupo1[,1]
# y <- grupo1[,2]
# z <- grupo1[,3]

# rgl.open() # Open a new RGL device
# rgl.points(x, y, z, color ="lightgray") # Scatter plot

# readline("puntos")


# rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
#   if( new.device | rgl.cur() == 0 ) {
#     rgl.open()
#     par3d(windowRect = 50 + c( 0, 0, width, width ) )
#     rgl.bg(color = bg )
#   }
#   rgl.clear(type = c("shapes", "bboxdeco"))
#   rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
# }

# rgl.open()
# rgl.spheres(x, y, z, r = 0.2e-6, color = "yellow")  # Scatter plot
# rgl.bbox(color = "#333377")

# readline("con fondo")



# www.sthda.com/english/wiki/print.php?id=211


# rgl.open()
# rgl.points(x, y, z, color = "#D95F02")
# ellips <- ellipse3d(cov(cbind(x,y,z)), centre=c(mean(x), mean(y), mean(z)), level = 0.95)

# shade3d(ellips, col = "#D95F02", alpha = 0.1, lit = FALSE)
# aspect3d(1,1,1)

# readline("sera que si")


# R code for rs

install.packages("raster")
library(raster)

setwd("/Users/leonardo/Desktop/dottorato/corsi e seminari_3rdYear/2020/Corso_D.Rocchini/lab")

getwd()
[1] "/Users/leonardo/Desktop/dottorato/corsi e seminari_3rdYear/2020/Corso_D.Rocchini/lab"

#brick: import a raster multilayer (landsat)

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: NIR

cl <- colorRampPalette(c('black','grey','light grey'))(100) # 
plot(p224r63_2011, col=cl)

#build a multipanel/multiframe (mfrow)
par(mfrow=c(2,2))
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)

clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B1_sre, col=clg)

clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_2011$B1_sre, col=clr)

cln <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(p224r63_2011$B4_sre, col=cln)

dev.off()

#RGB

plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# substitute NIR (near infrared) on top of the Blue 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# substitute the (near infrared) on top of green 
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

#####

p224r63_1988 <- brick("p224r63_1988_masked.grd")

plot(p224r63_1988)

par(mfrow=c(2,1)) 
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

#stretch a certain band range - histogram stretching
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist")

#DVI for the 2 periods of time
#DVI = NIR - R
#NDVI= (NIR - R) / (NIR + R)

dvi1988<- p224r63_1988$B4_sre - p224r63_1988$B3_sre
dvi2011<- p224r63_2011$B4_sre - p224r63_2011$B3_sre

[12:03] Duccio Rocchini
    
par(mfrow=c(2,1))
cldvi <- colorRampPalette(c('red','orange','green'))(100) # 
plot(dvi1988, col=cldvi)
plot(dvi2011, col=cldvi)


par(mfrow=c(2,1))
cldvi <- colorRampPalette(c('red','orange','green'))(100) 
plot(dvi1988, col=cldvi)
plot(dvi2011, col=cldvi)

[12:05] Duccio Rocchini
    
cldif <- colorRampPalette(c('blue','white','red'))(100) #
plot(difdvi, col=cldif)

# difference in time
difdvi <- dvi2011 - dvi1988
cldif <- colorRampPalette(c('blue','white','red'))(100) #
plot(difdvi, col=cldif)

[12:07] Duccio Rocchini
    install.packages("RStoolbox")
 
install.packages("RStoolbox")
library("RStoolbox")

#PCA - Multivariate analysis and reduction of dimenions  -->(increasing dimension by 10)
p224r63_2011res <- aggregate(p224r63_2011, fact=10)

#comparison befor and after resampling 
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011res, r=4, g=3, b=2, stretch="Lin")

#PCA on raster
p224r63_2011_pca <- rasterPCA(p224r63_2011res)

summary(p224r63_2011_pca$model)
Importance of components:
                          Comp.1      Comp.2       Comp.3
Standard deviation     1.2050671 0.046154880 0.0151509516
Proportion of Variance 0.9983595 0.001464535 0.0001578135
Cumulative Proportion  0.9983595 0.999824022 0.9999818357
                             Comp.4       Comp.5       Comp.6
Standard deviation     4.575220e-03 1.841357e-03 1.233374e-03
Proportion of Variance 1.439091e-05 2.330990e-06 1.045814e-06
Cumulative Proportion  9.999962e-01 9.999986e-01 9.999996e-01
                             Comp.7
Standard deviation     7.595367e-04
Proportion of Variance 3.966085e-07
Cumulative Proportion  1.000000e+00
>  
> summary(p224r63_2011_pca$model)
Importance of components:
                          Comp.1      Comp.2       Comp.3
Standard deviation     1.2050671 0.046154880 0.0151509516
Proportion of Variance 0.9983595 0.001464535 0.0001578135
Cumulative Proportion  0.9983595 0.999824022 0.9999818357
                             Comp.4       Comp.5       Comp.6
Standard deviation     4.575220e-03 1.841357e-03 1.233374e-03
Proportion of Variance 1.439091e-05 2.330990e-06 1.045814e-06
Cumulative Proportion  9.999962e-01 9.999986e-01 9.999996e-01
                             Comp.7
Standard deviation     7.595367e-04
Proportion of Variance 3.966085e-07
Cumulative Proportion  1.000000e+00

############
#the firts 3 componets have the most of information

plotRGB(p224r63_2011_pca$map, r=4, g=3, b=2, stretch="Lin")

plot(p224r63_2011_pca$map)

#landcover
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=5)
clclass <- colorRampPalette(c('red', 'green', 'yellow', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

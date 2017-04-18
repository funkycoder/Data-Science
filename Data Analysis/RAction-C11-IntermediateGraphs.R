
#SCATTER PLOTS
#-------------------------------------------------------------------------------
attach(mtcars)
plot(wt,mpg,
     main="Basic scatter plot of MPG vs Weight",
     xlab="Car weight (Lbs/100)",
     ylab="Miles per gallon", pch=19)
#linear line of best fit
abline(lm(mpg~wt),col="red",lwd=2,lty=1)
#smoothed line
lines(lowess(wt,mpg),col="blue",lwd=2,lty=2)

#car::scatterplot()
library(car)
scatterplot(mpg~wt | cyl, data=mtcars,lwd=2,
            main="Scatter plot of MPG vs Weight by #Cylinders",
            xlab="Car weight (Lbs/100)",
            ylab="Miles per gallon",
            legend.plot=TRUE,
            #points identified by mouse clicks 
            id.method="identify",
            labels=row.names(mtcars),
            boxplot="xy")

#Scatter plot matrices
#Scatter plot below and above diagonal are the same
pairs(~mpg+disp+drat+wt, data=mtcars,main="Basic Scatter Plot Matrix", upper.panel=NULL)

#car::scatterplotMatrix()
#liner and smoothed(loess) fit are added by defauts
#kernel density and rug plots are added to the principal diagonal
scatterplotMatrix(~mpg+disp+drat+wt,date=mtcars,
                  #suppresses lines showing spread and asymmetry
                  spread=FALSE,
                  lty.smooth=2,
                  main="Scatter Plot Matrix via car package")
scatterplotMatrix(~mpg+disp+drat+wt | cyl,date=mtcars,
                  #suppresses lines showing spread and asymmetry
                  spread=FALSE,
                  diagonal="histogram",
                  main="Scatter Plot Matrix via car package")

#gclus::cpairs rearrange vars so that higher correlation pairs are closer to 
#diagonal 
cor(mtcars[c("mpg","wt","disp","drat")])

library(gclus)
#absolute values of the correlations among variables
mydata<- mtcars[c(1,3,5,6)]
mydata.corr<- abs(cor(mydata))
#get colors for the plot. Given a symmetric matrix (a corr matrix in this case)
#dmat.color() return a matrix of colors
mycolors<- dmat.color(mydata.corr)
#order.single() sorts obj so that similar obj pairs are adjacent
myorder<- order.single(mydata.corr)
cpairs(mydata,myorder,panel.colors=mycolors,
       #add a small space btw cells of the matrix
       gap=.5,
       main="Variables Ordered and Colored by Correlation")

# HIGH-DENSITY SCATTER PLOTS
#-------------------------------------------------------------------------------
set.seed(1234)
n<- 10000
c1<- matrix(rnorm(n,mean=0,sd=.5),ncol=2)
c2<- matrix(rnorm(n,mean=3,sd=2),ncol=2)
mydata<-rbind(c1,c2)
mydata<-as.data.frame(mydata)
names(mydata)<-c("x","y")
#standard scatter plot :))
with(mydata,plot(x,y,pch=19,main="Scatter plot with 10,000 observations"))

#smoothed color density representations of the scatter plot
with(mydata,smoothScatter(x,y,main="Scatterplot colored by Smoothed Densities"))

#hexbin::hexbin() provides bivariate binning into hexgonal cells
library(hexbin)
with(mydata, {
        bin<- hexbin(x,y,xbins=50)
        plot(bin,main="Hexagonal Binning with 10,000 observations")
})

#IDPmisc::iplot() 
library(IDPmisc)
with(mydata,
     iplot(x,y,main="Image Scatter plot with Color indication Density"))
#?smoothScatter and?IDPmis::ipairs can be used to create readable scatter plot
#matrices for large dataset as well

#3D SCATTER PLOT
#-------------------------------------------------------------------------------
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg,
              pch=16,
              #highlighting to enhance the impression of depth
              highlight.3d=TRUE,
              #vertical lines connecting dots to the horiz plane
              type="h",
              main="Basic 3D Scatter Plot")
s3d<- scatterplot3d(wt,disp,mpg,
                    pch=16,
                    #highlighting to enhance the impression of depth
                    highlight.3d=TRUE,
                    #vertical lines connecting dots to the horiz plane
                    type="h",
                    main="Basic 3D Scatter Plot")
fit<- lm(mpg~wt+disp)
s3d$plane3d(fit)

#Spinning 3D Scatter plots
library(rgl)
attach(mtcars)
plot3d(wt,disp,mpg,col="red",size=5)

#Rcmdr :The scatter3d() function can include a variety of regression surfaces, such as linear,
#quadratic, smooth, and additive. The linear surface depicted is the default. Additionally,
#there are options for interactively identifying points.
library(Rcmdr)
attach(mtcars)
scatter3d(wt,disp,mpg)
#------------------------------------------------------------------------------------------------------------
# BUBBLE PLOTS
#------------------------------------------------------------------------------------------------------------
#Engine displacement as the buble size
r<- sqrt(disp/pi)
#inches control the size of the circle (by default the largest circle is 1 inch)
symbols(wt,mpg,circle=r,inches=0.30,fg="white",bg="lightblue")
main="Bubble Plot with point size proportional to displacement",
ylab="Miles Per Gallon",
xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=0.6)

#-------------------------------------------------------------------------------------------------------------
# LINE CHART
#-------------------------------------------------------------------------------------------------------------
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
t1 <- subset(Orange, Tree==1)
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference (mm)",
     main="Orange Tree 1 Growth")
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference (mm)",
     main="Orange Tree 1 Growth",
     type="b")
par(opar)

#There’s an important difference between the p lot() and l ines() functions. The
#plot() function will create a new graph when invoked. The lines() function adds
#information to an existing graph but can’t produce a graph on its own.

Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)
plot(xrange, yrange,
     #doesn't produce any line in this graph
     type="n",
     xlab="Age (days)",
     ylab="Circumference (mm)"
)
colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18, 18+ntrees, 1)
for (i in 1:ntrees) {
  tree <- subset(Orange, Tree==i)
  lines(tree$age, tree$circumference,
        type="b",
        lwd=2,
        lty=linetype[i],
        col=colors[i],
        pch=plotchar[i]
  )
}
title("Tree Growth", "example of line plot")
legend(xrange[1], yrange[2],
       1:ntrees,
       cex=0.8,
       col=colors,
       pch=plotchar,
       lty=linetype,
       title="Tree"
)

#----------------------------------------------------------------------------------
# CORRELOGRAMS
#_---------------------------------------------------------------------------------
#See how hard it is to judge correlations among multiple variables
options(ditgits=2)
cor(mtcars)
#By default, a blue color and hashing that goes from lower left to upper
#right represents a positive correlation between the two variables that meet at that cell.
#Conversely, a red color and hashing that goes from the upper left to the lower right represents
#a negative correlation. The darker and more saturated the color, the greater the
#magnitude of the correlation. Weak correlations, near zero, will appear washed out. In
#the current graph, the rows and columns have been reordered (using principal components
#analysis) to cluster variables together that have similar correlation patterns.
library(corrgram)
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel= panel.txt,
         main="Correlogram of mtcars intercorrelations")

corrgram(mtcars,order=TRUE,lower.panel=panel.ellipse,
         upper.panel=panel.pts,text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="correlogram of mtcars using scatter plot and ellipses")

corrgram(mtcars,lower.panel=panel.shade,
         upper.panel=NULL,text.panel=panel.txt,
         main="correlogram of mtcars (unsorted)")

#? col.corrgram not used
col.corrgram <- function(ncol){
        colorRampPalette(c("darkgoldenrod4", "burlywood1",
                           "darkkhaki", "darkgreen"))(ncol)}
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="A Corregram (or Horse) of a Different Color")
#-----------------------------------------------------------------------------------
# MOSAIC PLOTS
#-----------------------------------------------------------------------------------
ftable(Titanic)
library(vcd)
mosaic(Titanic,shade=TRUE,legend=TRUE)
#more control over selection of variables and their placements
mosaic(~Class+Sex+Age+Survived,data=Titanic,shade=TRUE,legend=TRUE) 
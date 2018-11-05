#######################################
#######################################
###                                 ###
### Plotting examples to guide your ###
### advanced plotting               ###
###                                 ###
### Data Visualization in R         ###
###                                 ###
#######################################
#######################################


#################################################################################################### 
####################################################################################################
# Begin base package

### Multi-dimensional scatter plot in base package ###

# First look at a single scatter plot
data(airquality)
summary(airquality)
plot(airquality$Temp, airquality$Ozone,
     xlab="Temperature", 
     ylab="Ozone",
     main="Air quality?")

# Now look at all of the scatter plots
plot(airquality)

# Effectively subset the scatter plot
pairs(airquality[, 2:4], main = "Paired scatter plot", lower.panel = NULL)


### Multi-dimensional scatter plot with univariate data on diag using base package ###

# Create function to make histogram with density superimposed
panel.hist <- function(x, ...)  {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  par(new=TRUE)                    # Do not start new plot
  hist(x, prob=TRUE, axes=FALSE, xlab="", ylab="", 
       main="", col="lightgray")
  lines(density(x, na.rm=TRUE))    # Add density curve
}

pairs(~ Temp + Wind + Solar.R, data=airquality, 
      lower.panel = panel.smooth, upper.panel = NULL, 
      diag.panel = panel.hist)

plot(airquality$Temp, airquality$Ozone, xlab = "Temperature", ylab = "Ozone", main = "Air quality?")



### Bar plots in base package ###
m <- matrix(c(23, 20, 12, 5, 6, 9), ncol = 3)  # Input data
relfrq <- prop.table(m, margin = 1)            # Calc rel. freqs
relfrq

# Stacked barplot
barplot(relfrq, beside = FALSE, col = c("green", "red"),
        names = c("Group1", "Group2", "Group3"))

legend("top", inset = -0.2, legend= c("Cheerful", "Dismal"),fill = c("green", "red"), horiz=TRUE,xpd=TRUE)


m <- matrix(c(23, 20, 12, 5, 6, 9), ncol = 3)  # Input data
relfrq <- prop.table(m, margin = 1)            # Calc rel. freqs

# Stacked relative barplot with labels added
par(mfrow = c(1, 1))
barplot(t(relfrq), names = c("Cheerful", "Dismal"), 
        horiz = FALSE, main = "An alternative presentation- stacked by emotion", 
        col = c("red", "green", "blue"),  beside = F)
legend("bottom", inset = -0.4, legend= c("Group 1", "Group 2", "Group 3"), fill = c("red", "green", "blue") ,horiz = T, xpd=TRUE)

# Q: What's the result if "beside" is "TRUE" above?


m <- matrix(c(23, 20, 12, 5, 6, 9), ncol = 2)  # Input data
relfrq <- prop.table(m, margin=1)            # Calc rel. freqs
relfrq


# Make juxtaposed barplots
par(mfrow=c(1, 2))

# Side by side bar plot vs stacked bar plot
barplot(relfrq,  beside = TRUE, col = c("green", "red", "gray"),
        names = c("Group1", "Group2"))
legend("top", inset = -0.3, legend= c("Cheerful", "Dismal", "Neutral"), fill = c("green", "red", "gray") ,horiz = F, xpd=TRUE)

# Stacked barplot
barplot(relfrq, beside = FALSE, col = c("green", "red", "gray"),
        names = c("Group1", "Group2"))
legend("top", inset = -0.3, legend= c("Cheerful", "Dismal", "Neutral"), fill = c("green", "red", "gray") ,horiz = F, xpd=TRUE)


### Histograms with varying bin width and density function overlay in base package ###
data(LakeHuron)

# Juxtaposed histograms
par(mfrow=c(1, 2))

# Regularly spaced bins
hist(LakeHuron, col = "darkred", xlab = "Level of Lake Huron (in feet)", freq = FALSE)
lines(density(LakeHuron))


# Irregularly spaced bins
# Use the breaks option to specify the number of bins
# regardless of the size of the dataset
hist(LakeHuron, xlab = "Level of Lake Huron (in feet)",
     freq = FALSE, breaks = c(574, 577, 578, 581, 585), 
     col = "darkgreen")
lines(density(LakeHuron))


### Other functionality of base plot function ###
par(mfrow=c(1, 1))

# Use of other chart elements
# Create plot with no x axis and no x axis label
x <- 1:6
plot(x, sin(x), xaxt="n", xlab="", yaxt="n",ylab = "", 
     pch=1:6, col = c("darkred", "darkblue", "darkgray", "orange",
                      "black", "darkgreen"), 
     lwd = 3, cex = 2, main = "Example of multiple chart elements")

axis(1, labels = FALSE)
axis(2, labels = F)
labels <- c("Are", "you", "master", "of", "your", "domain?")

# Add labels at default tick marks
text(x, y=par("usr")[3] - .1, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)

# End base package
#################################################################################################### 
####################################################################################################

#################################################################################################### 
####################################################################################################
# Begin advanced pacakges

# Uncomment below to install
# install.packages("plotrix")
library(plotrix)

### Pyramid plot with plotrix ###

men <- c(334700, 357834, 328545, 367201, 411689, 
         359363, 336841, 178422, 72296, 9552, 139, 0)
women <- c(318662, 340357, 320462, 365180, 401521, 
           357405, 345884, 208057, 117843, 27914, 761, 0)
groups <- paste(seq(0, 110, 10), "-", seq(9, 119, 10), sep="")
groups                             # Show group labels
men <- 100*men/sum(men)            # Normalize men
women <- 100*women/sum(women)      # and women
pyramid.plot(men, women, labels = groups,
             lxcol = "blue", rxcol = "blue", main = "Pyramid plot example")

# Uncomment below to install
# install.packages("scatterplot3d")

### 3D Scatterplot with scatterplot3d ###

library(scatterplot3d)
data(trees)
head(trees)
attach(trees)
scatterplot3d(Girth, Height, Volume)           # Scatter plot
scatterplot3d(Girth, Height, Volume, type = "h", # Add the vertical lines
              color = grey(31:1/40))

# Order trees according to volume and then 
# plot curve from lowest to highest volume
o <- order(trees$Volume)  
scatterplot3d(Girth[o], Height[o], Volume[o], type="l", 
              scale.y = 3, angle = 75, main = "")

# Recreate basic scatterplot then add a best fit plane to it (calculated with lm)
myplot <- scatterplot3d(Girth, Height, Volume, type = "h", 
                        angle=60)
# Fit a model
model <- lm(Volume ~ Girth + Height)  
# Add fitted plane
myplot$plane3d(model)                  
detach(trees)



### Confidence intervale estimates with plotCI from plotrix ###

# Plot confidence intervale estimates with CI
data(OrchardSprays)
fit <- lm(decrease ~ treatment - 1, data=OrchardSprays)
summary(fit)
par(mfrow=c(1,1))

# Pulls out coefficient information from summary
plotCI(coef(summary(fit))[,1], 
       uiw=qt(.975, df=56)*coef(summary(fit))[,2],
       ylab="Estimates and 95% confidence intervals", 
       xaxt="n",col="blue",main="Coeff estimates with CI")
axis(1, 1:length(rownames(coef(summary(fit)))), 
     rownames(coef(summary(fit))))

# Uncomment below to install
# install.packages("RColorBrewer")
### Use of palette and Color Brewer ###

# Use of palette and ColorBrewer palettes to adjust plot color pattern
# Show the current palette
palette()             
palette(c("red", "#1A334D", "black", rgb(.1, .8, 0)))
palette()

# Use the default palette
palette("default")    
library(RColorBrewer)
head(brewer.pal.info, n=10)
brewer.pal(n=5, name="Spectral") # 5 colors from Spectral
plot(1:5, col=brewer.pal(n=5, name="Spectral"), pch=16, cex=2)
# 8 colors from Accent
brewer.pal(n=8, name="Accent")        
plot(1:8, col=brewer.pal(n=8, name="Accent"), pch=16, cex=2)

# Set colors locally for just one function call
plot(1:8, col=brewer.pal(n=8, name="Reds"), pch=16, cex=2)

# Set global palette
palette(brewer.pal(n=8, name="Greens")) 

# Plot using colors
plot(1:8, col=1:8, pch=16, cex=3.5,main="Example of ColorBrewer for palette design")      

# Custom palette design
palette(c("darkred", "#1A334D", "black", rgb(1, .8, 0),rgb(0,0,1),rgb(0,1,0),rgb(1,0,0),rgb(.5,.5,.5)))
# Plot using colors
plot(1:8, col=1:8, pch=16, cex=4,main="Example of palette design")       

# End advanced packages
#################################################################################################### 
####################################################################################################


####################################################################################################
####################################################################################################
# Begin ggplot and qplot

# Uncomment below to install
# install.packages("ggplot2")
# install.pacakages("readxl")

### Pokemon data, bar plot colored by mean total points, continuous variable, code similar to class ###
library(readxl)
library(ggplot2)
poke <- as.data.frame(read_excel("Pokemon.xlsx"))

# Create a dataframe that has type of pokemon, mean total points for that type and count of each type
poke.total.mean <- tapply(poke$Total, poke$Type, mean)
poke.total <- as.data.frame(poke.total.mean)
poke.total <- cbind(Row.Names = rownames(poke.total), poke.total)
rownames(poke.total) <- NULL
freq <- as.data.frame(table(poke$Type))
poke.total.freq <- cbind(poke.total, freq)
poke.total.freq[,3] <- NULL
colnames(poke.total.freq) <- c("Type", "Mean_Total_Points", "Frequency")

# Set up some formatting
xtheme <- theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))

# Note the syntax, ggplot(data, aes(x = , y = , fill = )) + geom_bar() + 
# ggplot uses addition symbol
ggplot(data = poke.total.freq, aes(x = Type, y = Frequency, fill = Mean_Total_Points), 
       ylab = "Frequency of Pokemon Type") + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xtheme



### Pokemon data, bar plot colored by Attack Rating, discrete variable, code from class ###

# Set up all of the ratings in a matrix
count.freq.poke <- table(poke$Type)
poke.df <- as.data.frame(count.freq.poke)
colnames(poke.df) <- c("TYPE", "FREQ")

poke.means.attack <- tapply(poke$Attack, poke$Type, mean)
mu.attack <- mean(poke$Attack)
sd.all.attack <- sqrt((1/dim(poke)[1])*sum((poke$Attack-mu.attack)^2))

Z.attack <- (poke.means.attack - mu.attack)/(sd.all.attack/sqrt(poke.df[,2]))
poke.df[,3] <- Z.attack
p.attack <- 2*pnorm(-abs(Z.attack))
poke.df[,4] <- p.attack
colnames(poke.df) <- c("TYPE", "FREQ", "Z_SCORE", "P_VALUE")

poke.df[,5] <- poke.means.attack
poke.df[,6] <- NA
for(i in 1:dim(poke.df)[1]) {
  if(poke.df[i,4] < 0.001 & poke.df[i,3] > 0) {
    poke.df[i,6] <- "ATTACK+++"
  } else if (poke.df[i,4] >= 0.001 & poke.df[i,4] < 0.050 & poke.df[i,3] > 0) {
    poke.df[i,6] <- "ATTACK++"
  } else if (poke.df[i,4] >= 0.050 & poke.df[i,4] < 1.000 & poke.df[i,3] > 0) {
    poke.df[i,6] <- "ATTACK+"
  } else if (poke.df[i,4] == 1.000) {
    poke.df[i,6] <- "ATTACK_0"
  } else if (poke.df[i,4] >= 0.050 & poke.df[i,4] < 1.000 & poke.df[i,3] < 0) {
    poke.df[i,6] <- "ATTACK-"  
  } else if (poke.df[i,4] >= 0.001 & poke.df[i,4] < 0.050 & poke.df[i,3] < 0) {
    poke.df[i,6] <- "ATTACK--"
  } else if (poke.df[i,4] < 0.001 & poke.df[i,3] < 0) {
    poke.df[i,6] <- "ATTACK---"
  } else {
    poke.df[i,6] <- NA
  }
}
poke.df <- poke.df[ order(-poke.df[,3]),]
colnames(poke.df) <- c("TYPE", "FREQ", "Z_SCORE", "P_VALUE", "MEAN_ATTACK", "ATTACK_VALUE")

poke.df
poke.means.attack.df <- as.data.frame(poke.means.attack)
poke.means.attack.df <- poke.means.attack.df[order(-poke.means.attack.df[,1]),]
poke.df <- cbind(poke.means.attack.df, poke.df)
poke.df <- cbind(Row.Names = rownames(poke.df), poke.df)
rownames(poke.df) <- NULL

poke.df$ATTACK_VALUE <- factor(poke.df$ATTACK_VALUE, levels = c("ATTACK+++", "ATTACK++", "ATTACK+", "ATTACK-", "ATTACK--", "ATTACK---"))

# Plot with this ATTACK_VALUE as the fill
ggplot(data = poke.df, aes(x = TYPE, y = FREQ, fill = ATTACK_VALUE),ylab = "Frequency of Pokemon Type") + 
  geom_bar( stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xtheme



### ggplot bar plot, many variations using car data ###

summary(mpg)
xtheme <- theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14,face = "bold"))
c <- ggplot(mpg, aes(factor(manufacturer))) + xtheme
c + geom_bar(width = 0.5) + coord_flip() 
c + geom_bar(width = 0.5, color = "blue", fill = "yellow") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggplot(mpg, aes(factor(manufacturer), fill = factor(class))) + geom_bar() + coord_flip() + xtheme 
ggplot(mpg, aes(class)) + geom_bar() + facet_wrap( ~ manufacturer) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xtheme 
ggplot(mpg, aes(cyl)) + geom_bar(width = .9) + facet_grid(. ~ class)+ 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xtheme 


mhwy <- tapply(mpg$hwy, mpg$manufacturer, mean)
mhwy <- as.data.frame(mhwy)
mhwy <- cbind(manufacturer = rownames(mhwy), mhwy)
rownames(mhwy) <- NULL
ggplot(mhwy, aes(x = factor(manufacturer), y = mhwy)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xtheme 

# Understand these point plots on your own

ggplot(mpg, aes(hwy, cty)) + geom_point(aes(color = class), size = 4) + theme_bw() + xtheme
ggplot(mpg, aes(hwy, cty)) + geom_jitter(aes(color = class), size = 3) + theme_bw() + xtheme
ggplot(mpg, aes(hwy, cty)) + geom_jitter(aes(shape = factor(cyl), color = factor(cyl), size = 2)) + theme_bw() + xtheme
ggplot(mpg, aes(year, hwy)) + geom_jitter(aes(color = factor(cyl), shape = factor(fl), size = 2)) + theme_bw() + xtheme

### Beyond the bar plot with ggplot and qplot using Pokemon data ###
### Pokemon Histogram ###
# qplot is a simple version of ggplot, with less functionality, but more ease of use
qplot(Total, data = poke, geom = "histogram", xlab = "Total points", ylab = "Frequency of total points", main = "Total Pokemon points", binwidth = 25) + xtheme


### Pokemon scatter plot, attack vs. defense with best fit line, colored by total points ###

ggplot(data = poke, aes(Defense, Attack)) + geom_point(aes(color = Total, fill = Total), size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme

### Easy, nice boxplot using qplot by Pokemon type ###

qplot(Type, Speed, data = poke, geom = "boxplot", ylab = "Speed Points", xlab = "Pokemon Type") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xtheme

### Nice panelized histogram of special defense points by Pokemon type ###

ggplot(poke, aes(`Special Defense`)) + geom_histogram(binwidth = 10) + facet_wrap( ~ Type) + theme_bw() + xtheme

### Nice panelized histogram of special defense points by Pokemon type, adds density line ###

ggplot(poke, aes(`Special Defense`)) + geom_histogram(aes(y = ..density..), binwidth = 10) + geom_density() + facet_wrap( ~ Type) + theme_bw() + xtheme

# Understand this advanced pokemon plot

# This creates the 35040 unique rows needed to plot the matrix of scatter plots
Expand <- function(data) {
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  all
}

poke.points <- poke[, 4:9]
poke.points2 <- Expand(poke.points)
# makes the density plots on the diagonals
makeDensities <- function(data) {
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], 
               x = data[, i])
  }))
  densities
}
dens <- makeDensities(poke.points)

# still just adding more arguments after ggplot()
ggplot(poke.points2, aes(x = x, y = y)) + geom_point(alpha = I(1/10), shape = I(20), size = I(1)) + facet_grid(xvar ~ yvar, scales = "free")+  stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), data = dens, position = "identity", color = "grey20",geom = "line") + xtheme

# End ggplot and qplot packages
#################################################################################################### 
####################################################################################################
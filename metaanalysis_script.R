#This script was used to the data analysis in the paper: 
#"Domesticated, genetically engineered and wild plants relatives exhibit unintended phenotypic differences: a comparative meta-analysis profiling of rice, canola, maize, pumpkin and sunflower.  Terán, Alejandra H.; Wegier, Ana; Benítez, Mariana; Lira, Rafael; and Escalante, Ana*”
#*Corresponding author: aescalante@iecologia.unam.mx

#Normality of the data
shapiro.test(x)

#Outliers detection --- Cooks distance as a multivariate detection method
mod <- lm(x ~ ., data=data)
cooksd <- cooks.distance(mod)

#Plot with cutoff line
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#Influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(data[influential, ])  # influential observations

#Generalized Linear Model (GLM). The family was chosen according to the distribution of each subset
model1 <-glm(value~type, data=data, family = x)

#Pairwise comparisons of the GLM
mymodel = (glht(model1, mcp(type="Tukey")))

#Linear Discriminant Analysis
library(MASS)
model.1 <- lda(as.factor(type)~.,data=data)

#Project data on linear discriminants
model.1.values <- predict(model.1, data[,-1])

#Multivariate Analysis of Variance (MANOVA) as follow-up test
model <- manova(cbind(var1, var2, var2) ~ category, data = data)
summary(model)
#Univariate analysis for pairwise comparisons 
summary.aov(model)

#Plot the results
library(ggplot2)
p <- qplot(data=data.frame(model.1.values$x), main = "tittle", x=LD1, y=LD2, colour=data$type) + stat_ellipse() + labs(x="LD1 (%)", y="LD2 (%)") + geom_point() + geom_text(aes(label=data), size=4)
p
p <- p + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
p
p <- p + theme(legend.position="none")
p
p <- p + coord_flip()
p
print(p)

#Visualizing data with SpiderChart. Only the mean values was used to plot the chart.
library(fmsb)
colors_border=c( rgb(0.169,0.169,0.169,1), rgb(0.169,0.169,0.169,1),rgb(0.169,0.169,0.169,1), rgb(0.169,0.169,0.169,1))
colors_in=c( rgb(0.27,0.55,0,.5), rgb(0.93,0.57,0.13,.5), rgb(0.93,0.23,0.23,.5))

radarchart(data, axistype= , 
           #custom polygon
           pcol=colors_border, pfcol=colors_in , plwd=1 , plty=1, pty= 16,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8)

legend(x=1.5, y=1, legend = rownames(spider4[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

#The analysis was done in RStudio version 1.0.136 on Intel Mac OS X 10_12_1 software.

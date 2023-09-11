library(ggplot2)
library(ggcorrplot)
library(GGally)
library(dplyr)
library(corrplot)
library(ggridges)
library(R.utils)
library(ICSNP)
library(energy)
library(ggpubr)
library(forcats)
library(MASS)


setwd("D:\\Pablo\\Documents\\Universidad\\MASTER\\MultivariateDA")
load("forestfires.RData")

# forestfires$Severity <- forestfires$severity
forestfires$Severity <- capitalize(forestfires$severity)
# forestfires
head(forestfires)
dim(forestfires)
summary(forestfires)
sum(is.na(forestfires))
mod <- forestfires[forestfires$Severity == "Moderate",] 
sev <- forestfires[forestfires$Severity == "Severe",] 
non <- forestfires[forestfires$Severity == "None",] 
summary(mod)
summary(sev)
summary(non)



plot(forestfires)


############ DENSITIES

temp.dens <- ggplot(forestfires, aes(x = temp, fill = Severity)) +
  geom_density(alpha=0.8, lwd = 0.9) +
  # facet_grid(Severity~ .)+
  # facet_grid(~factor(Severity, levels=c("None", "Moderate", "Severe")))
  facet_grid(fct_relevel(Severity,"None", "Moderate", "Severe")~.)+
  scale_fill_manual(values = c("#FFCC00", "#33CC66","#CC0000"))+
  theme_minimal()+
  labs(x = "Temperature (ºC)", y = "Density", title = "Temperature")+
  # ylab("Density")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size=14, face="bold"),
        # axis.title.y = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(size = 11),
        legend.position = "none")

temp.dens
rh.dens <- ggplot(forestfires) +
  geom_density(aes(x = RH,fill = severity), alpha=0.8, lwd = 0.9) +
  facet_grid(fct_relevel(Severity,"None", "Moderate", "Severe")~.)+
  scale_fill_manual(values = c("#FFCC00", "#33CC66", "#CC0000"))+
  theme_minimal()+
  labs(x = "Relative humidity (%)", y = "Density", title = "Relative humidity")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold"),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(size = 11, face = "bold"),
        legend.position = "none")

wind.dens <- ggplot(forestfires) +
  geom_density(aes(x = wind, fill = severity), alpha=0.8, lwd = 0.9) +
  facet_grid(fct_relevel(Severity,"None", "Moderate", "Severe")~.)+
  scale_fill_manual(values = c("#FFCC00", "#33CC66", "#CC0000"))+
  theme_minimal()+
  labs(x = "Wind speed (km/h)", y = "Density", title = "Wind speed")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold"),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(size = 11, face = "bold"),
        legend.position = "none")

rain.dens <- ggplot(forestfires) +
  geom_density(aes(x = rain,fill = severity), alpha=0.5, lwd = 0.9) +
  # stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5)+
  facet_grid(severity~ .)+
  scale_fill_manual(values = c("#FFCC00", "#33CC66", "#CC0000"))+
  # scale_fill_brewer(palette = "Paired") +
  # scale_color_brewer(palette = "Paired") +
  theme_minimal()+
  labs(x = "Rainfall", y = "Density", title = "Rainfall by forest fire damage")+
  ylab("Density")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),strip.text = element_text(size = 15),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 11, face = "bold"),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(size = 11, face = "bold"),
        legend.position = "none")


densities <- ggarrange(temp.dens, rh.dens, wind.dens, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(densities, top = text_grob("Weather conditions by forest fire damage", 
                                           face = "bold", size = 18))





############SCATTERPLOTS

forestfires$Severity <- factor(forestfires$Severity, levels = c("None", "Moderate", "Severe"))

temp.rh <- ggplot(forestfires, aes(x = temp, y = RH, color = Severity)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("#33CC66","#FFCC00",  "#CC0000")) + 
  theme_minimal()+
  labs(x = "Temperature (ºC)", y = "Relative Humidity (%)", 
       title = "Temperature against Humidity") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14), 
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(size = 11))+
  xlim(0, 35) 

temp.wind <- ggplot(forestfires, aes(x = temp, y = wind, color = Severity)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("#33CC66","#FFCC00",  "#CC0000")) + 
  theme_minimal()+
  labs(x = "Temperature (ºC)", y = expression("Wind speed"~(mm/m^2)), 
       title = "Temperature against Wind Speed") +
  theme(strip.text = element_text(size = 15),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14, face="bold"), 
    axis.text.x = element_text(size = 11, face="bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.title = element_text(colour="black", size=14, face="bold"),
    legend.text = element_text(size = 11, face = "bold")) +
  xlim(0, 35) + ylim(0, 10)

rh.wind <- ggplot(forestfires, aes(x = RH, y = wind, color = Severity)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("#33CC66","#FFCC00",  "#CC0000")) + 
  theme_minimal() +
  labs(x = "Relative Humidity (%)", y = expression("Wind speed"~(mm/m^2)), 
       title = "Humidity against Wind Speed") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, face="bold"), 
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(size = 11, face = "bold")) +
  ylim(0, 10)


scatters <- ggarrange(temp.rh, temp.wind, rh.wind, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
scatters# annotate_figure(scatters, top = text_grob("Weather conditions by forest fire damage", 
                                           # face = "bold", size = 18))

# ggplot(mod, aes(x = RH, y = wind)) +   geom_point(size = 2)  
# ggplot(sev, aes(x = temp, y = wind)) +   geom_point(size = 2)  
# ggplot(sev, aes(x = temp, y = RH)) +   geom_point(size = 2)  





################BOXPLOTS
par(mfrow = c(1,4))
namess <- c(NA, "Temperature", "Relative Humidity", "Wind", "Rain")

for (i in c(2,3,4,5)){
  
  out <- boxplot.stats(forestfires[, i])$out
  out_ind <- which(forestfires[, i] %in% c(out))
  boxplot(forestfires[, i], ylab = namess[i])
  # mtext(paste("Outliers: ", paste(out, collapse = ", ")))
  mtext(namess[i])
}

for (i in c(2,3,4,5)){
  hist(forestfires[, i], main = colnames(forestfires)[i])
  
}

for (i in c(2,3,4,5)){
  ggplot(forestfires) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("cyl")
}

######################### Outlier detection

lower.temp <- quantile(forestfires$temp, 0.025)
lower.rh <- quantile(forestfires$RH, 0.025)
lower.wind <- quantile(forestfires$wind, 0.025)

higher.temp <- quantile(forestfires$temp, 0.975)
higher.rh <- quantile(forestfires$RH, 0.975)
higher.wind <- quantile(forestfires$wind, 0.975)

temp.outlier <- which(forestfires$temp < lower.temp | forestfires$temp > higher.temp)
rh.outlier <- which(forestfires$RH < lower.rh | forestfires$RH > higher.rh)
wind.outlier <- which(forestfires$wind < lower.wind | forestfires$wind > higher.wind)

forestfires[temp.outlier,]
forestfires[rh.outlier,]
forestfires[wind.outlier,]


forestfires[which(forestfires$temp < lower.temp),]
forestfires[which(forestfires$temp > higher.temp),]
forestfires[which(forestfires$RH < lower.rh),]
forestfires[which(forestfires$RH > higher.rh),]
forestfires[which(forestfires$wind < lower.wind),]
forestfires[which(forestfires$wind > higher.wind),]



################# CORRELATION MATRIX


all.cor <- ggcorrplot(cor(forestfires[, c(2,3,4,5)]), lab = TRUE,
          legend.title = "Correlation", 
          title = "Corelation between features", 
          digits = 3,
          outline.color = "black", 
          ggtheme = ggplot2::theme_minimal,
          colors = c("red", "white", "#33CC66"),
          tl.cex = 15) +
  scale_x_discrete(labels = c("Temperature (ºC)", 
                              "Relative Humidity (%)",
                              "Wind Speed (km/h)", 
                              expression(Rain~(mm/m^2)))) +
  scale_y_discrete(labels = c("Temperature (ºC)", 
                              "Relative Humidity (%)",
                              "Wind Speed (km/h)", 
                              expression(Rain~(mm/m^2)))) +
  theme(legend.title = element_text(size=14, face="bold"),
        legend.text=element_text(size=12),
        title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(face="bold", color="black", size=15, angle=45),
        axis.text.y = element_text(face="bold", color="black", size=15))

all.cor
###### SEVERE

sev.cor <- ggcorrplot(cor(sev[, c(2,3,4,5)]), lab = TRUE,
           legend.title = "Correlation", 
           title = "Severe forest fires", 
           digits = 3,
           outline.color = "black", 
           ggtheme = ggplot2::theme_minimal,
           colors = c("darkolivegreen2", "white", "steelblue"),
           tl.cex = 15) +
  
scale_x_discrete(labels = c("Temperature (ºC)", 
                            "Relative Humidity (%)",
                            "Wind Speed (km/h)", 
                            expression(Rain~(mm/m^2)))) +
scale_y_discrete(labels = c("Temperature (ºC)", 
                            "Relative Humidity (%)",
                            "Wind Speed (km/h)", 
                            expression(Rain~(mm/m^2)))) +
theme(legend.title = element_text(size=14, face="bold"),
      legend.text=element_text(size=12),
      title = element_text(size = 15, face = "bold"),
      axis.text.x = element_text(face="bold", color="black", size=12, angle=45),
      axis.text.y = element_text(face="bold", color="black", size=12))

###### MODERATE

mod.cor <- ggcorrplot(cor(mod[, c(2,3,4,5)]), lab = TRUE,
           legend.title = "Correlation", 
           title = "Moderate forest fires", 
           digits = 3,
           outline.color = "black", 
           ggtheme = ggplot2::theme_minimal,
           colors = c("darkolivegreen2", "white", "steelblue"),
           tl.cex = 15) +
  
scale_x_discrete(labels = c("Temperature (ºC)", 
                            "Relative Humidity (%)",
                            "Wind Speed (km/h)", 
                            expression(Rain~(mm/m^2)))) +
scale_y_discrete(labels = c("Temperature (ºC)", 
                            "Relative Humidity (%)",
                            "Wind Speed (km/h)", 
                            expression(Rain~(mm/m^2)))) +
theme(legend.title = element_text(size=14, face="bold"),
      legend.text=element_text(size=12),
      title = element_text(size = 15, face = "bold"),
      axis.text.x = element_text(face="bold", color="black", size=12, angle=45),
      axis.text.y = element_text(face="bold", color="black", size=12))

###### NO DAMAGE

non.cor <- ggcorrplot(cor(non[, c(2,3,4,5)]), lab = TRUE,
           legend.title = "Correlation", 
           title = "No forest damage", 
           digits = 3,
           outline.color = "black", 
           ggtheme = ggplot2::theme_minimal,
           colors = c("darkolivegreen2", "white", "steelblue"),
           tl.cex = 15) +
  
scale_x_discrete(labels = c("Temperature (ºC)", 
                            "Relative Humidity (%)",
                            "Wind Speed (km/h)", 
                            expression(Rain~(mm/m^2)))) +
scale_y_discrete(labels = c("Temperature (ºC)", 
                            "Relative Humidity (%)",
                            "Wind Speed (km/h)", 
                            expression(Rain~(mm/m^2)))) +
theme(legend.title = element_text(size=14, face="bold"),
      legend.text=element_text(size=12),
      title = element_text(size = 15, face = "bold"),
      axis.text.x = element_text(face="bold", color="black", size=12, angle=45),
      axis.text.y = element_text(face="bold", color="black", size=12))







###########
############
##########
#########
############
############
#############
##########

M <- cor(forestfires[, c(2,3,4,5)])
rownames(M) <- c("temp", "RH","wind","rain")
colnames(M) <- c("temp", "RH","wind","rain")

ggcorr(M, label = TRUE,label_round = 3)



ggcorrplot::ggcorrplot(signif(cor(forestfires[, c(2,3,4,5)]),3), lab = TRUE,
                       legend.title = "Correlation", 
                       title = "Correlation matrix", digits = 3,
                       outline.color = "black", 
                       ggtheme = ggplot2::theme_minimal,
                       colors = c("darkolivegreen2", "white", "steelblue"),
                       tl.cex = 15)









#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
################## TUTORIALS ############################

# TUTORIAL 2

# Cor, cov, means, sd
# If high correlation, one var might be omitted in subsequent regression analysis - not really

cor(forestfires[, c(2,3,4,5)])
cov(forestfires[, c(2,3,4,5)])
colMeans(forestfires[, c(2,3,4,5)])
for (i in c(2,3,4,5)){
  print(sd(forestfires[, i]))
}

# Per severity
cor(sev[, c(2,3,4,5)])
cov(sev[, c(2,3,4,5)])
colMeans(sev[, c(2,3,4,5)])
for (i in c(2,3,4,5)){
  print(sd(sev[, i]))
}

cor(mod[, c(2,3,4,5)])
cov(mod[, c(2,3,4,5)])
colMeans(mod[, c(2,3,4,5)])
for (i in c(2,3,4,5)){
  print(sd(mod))
}

cor(non[, c(2,3,4,5)])
cov(non[, c(2,3,4,5)])
colMeans(non[, c(2,3,4,5)])
for (i in c(2,3,4,5)){
  print(sd(non[, i]))
}


# Produce boxplots, histograms, scatterplots for data. Symmetric boxplots, histograms = normality
par(mfrow=c(1,4)) # reset to default plot window (1x1)
titles = c(NA, "Temperature", "Relative Humidity", "Wind", "Rain")
for (i in c(2,3,4,5)){
  hist(forestfires[, i], main = titles[i], xlab = titles[i])
}


# Q-Q plots to informally assess the univariate normality of each variable - 
for (i in c(2,3,4,5)){
  qqnorm(forestfires[, i], main = titles[i])
  qqline(forestfires[, i])
}



# Test multivariate normality based on 1000 bootstrap samples
set.seed(1234)
mvnorm.etest(forestfires[, c(2,3,4,5)], R = 1000) #The null hypothesis of the test is normality - reject
mvnorm.etest(forestfires[, c(2,3,4)], R = 1000) #The null hypothesis of the test is normality - reject
mvnorm.etest(forestfires[, c(3)], R = 1000) #The null hypothesis of the test is normality - reject


  # Multivariate Inference
HotellingsT2(forestfires[, c(2,3,4,5)], test="f") # Using the F-distribution (same as previous?)





# TUTORIAL 3 - PCA ANALYSIS
set.seed(1)

#use 85% of dataset as training set and 15% as test set
sample <- sample(c(TRUE, FALSE), nrow(forestfires), replace=TRUE, prob=c(0.85,0.15))
train  <- forestfires[sample, ]
test   <- forestfires[!sample, ]






# PCA analysis by basing on SVD decomposition
pca <- prcomp(forestfires[, -c(1, 6,7)], scale.=T) # PCA on the correlation matrix, scale = TRUE
pca.train <- prcomp(train[, -c(1, 6,7)], scale.=T) 
# Importance of components
summary(pca)# percent explained variance
summary(pca.train)
# Loadings + INTERPRET THEM
pca$rotation
pca.train$rotation

# Scores
scores <- predict(pca)[,1:3] # only first 3 components
scores <- predict(pca.train)[,1:3] # only first 3 components

# Scree-plot
plot(pca,type="line")
plot(pca.train,type="line")

# Draw biplot + INTERPRET
par(mfrow = c(1,1))
biplot(pca, expand = 2.8, ylim = c(-1, 0.2), xlim = c(-0.3, 0.3))
abline(h=0,col="grey") # Draw reference lines
abline(v=0,col="grey")



# Only 3 components
pca2 <- prcomp(train[, -c(1, 5,6,7)], scale.=T) # PCA on the correlation matrix, scale = TRUE
summary(pca2)# percent explained variance
pca2$rotation

scores2 <- predict(pca2)[,1:2] 
biplot(pca2)




test.sc <- scale(test[, -c(1,6,7)], center=TRUE, scale=FALSE)



pred <- predict(pca, newdata=test[,-c(1,6,7)])[,1:3]
pred






# Predicting using PCA regression - NEED LOGISTIC REGRESSION WITH 3 VARIABLES
# pcr <- lm(as.factor(forestfires[,6]) ~ scores)
# summary(pcr)
plot(nir[,1],pcr$fitted.values,xlab="Observed",ylab="Fitted")
abline(0,1) # Add reference line

plot(pca$x[,1:3],
     xlab=paste0("PC ", pc[1], " (", expl.var[pc[1]], "%)"), 
     ylab=paste0("PC ", pc[2], " (", expl.var[pc[2]], "%)")
)





# TUTORIAL 4
# Compare group means 
forest.manova <- manova(cbind(temp, RH, wind, rain) ~ severity, data=forestfires)
summary(forest.manova,test = "Hotelling-Lawley") #extracr manova table - NOT SIGNIFICANT

# Individual ANOVAs from the MANOVA result
summary.aov(forest.manova) # NONE ARE SIGINFICANT


# Linear discriminant analysis
forest.lda <- lda(severity ~ temp + wind + RH + rain,data=train)
forest.lda 

forest.lda2 <- lda(severity ~ temp + wind + RH, data=train)
forest.lda2


# Plots - cannot distinguish classes
plot(forest.lda, dimen = 2, pch = 1, fill = factor(forestfires$severity))
plot(forest.lda,type = "density")

plot(forest.lda2, dimen = 2)
plot(forest.lda2,type = "density")

# Training set predictions
head(predict(forest.lda)$x) # Discriminant scores
head(predict(forest.lda)$posterior) # Posterior probabilities for the groups
head(predict(forest.lda)$class) # allocated group

head(predict(forest.lda2)$x) # Discriminant scores
head(predict(forest.lda2)$posterior) # Posterior probabilities for the groups
head(predict(forest.lda2)$class) # allocated group

# percentage good predictions
sum(predict(forest.lda)$class == train$severity)/dim(train)[1]
sum(predict(forest.lda2)$class == train$severity)/dim(train)[1]


# Confusion tables
pred.gr <- c("Predict moderate","Predict none", "Predict severe")
table(train$severity, factor(predict(forest.lda)$class,labels=pred.gr))
table(train$severity, factor(predict(forest.lda2)$class,labels=pred.gr[-3]))

# Proportion correct by group
diag(prop.table(table(train$severity, factor(predict(forest.lda)$class,labels=pred.gr)), 1))
diag(prop.table(table(train$severity, factor(predict(forest.lda2)$class,labels=pred.gr[-3])), 1))
# Overall accuracy
sum(diag(prop.table(table(train$severity, factor(predict(forest.lda)$class,labels=pred.gr)))))
sum(diag(prop.table(table(train$severity, factor(predict(forest.lda2)$class,labels=pred.gr[-3]))))) # Overall accuracy



# Leave one out cross validation
forest.lda.cv <- lda(severity ~ temp + wind + RH + rain, data =forestfires,CV=T)
table(forestfires$severity, factor(forest.lda.cv$class,labels=pred.gr))

forest.lda.cv2 <- lda(severity ~ temp + wind + RH, data=forestfires,CV=T)
table(forestfires$severity, factor(forest.lda.cv2$class,labels=pred.gr[-3]))

# Class accuracy
diag(prop.table(table(forestfires$severity, forest.lda.cv$class), 1))
diag(prop.table(table(forestfires$severity, forest.lda.cv2$class), 1))

# Overall accuracy
sum(diag(prop.table(table(forestfires$severity, forest.lda.cv$class))))
sum(diag(prop.table(table(forestfires$severity, forest.lda.cv2$class))))



cor(cbind(train[,-c(1,6,7)],predict(forest.lda)$x)) #bind the original (measurement) variables with the extracted discriminant scores 
cor(cbind(train[,-c(1,6,7)],predict(forest.lda2)$x)) #bind the original (measurement) variables with the extracted discriminant scores 



# Classify new observations
# Classification of a new observation
new <- matrix(c(27, 20, 15, 0), ncol = 4) # New data
new <- as.data.frame(new) # Create data.frame ...
names(new) <- names(forestfires[,-c(1,6,7)]) # ... with the same variable names
predict(forest.lda,newdata=new) # Provides allocated group, posterior prob and discriminant score
predict(forest.lda2,newdata=new) # Provides allocated group, posterior prob and discriminant score
predict(forest.lda.cv,newdata=new) # Provides allocated group, posterior prob and discriminant score
predict(forest.lda.cv2,newdata=new)

predict(topred,newdata=new) # Provides allocated group, posterior prob and discriminant score

new <- matrix(c(27, 20, 15, 0), ncol = 4) # New data
names(new) <- names(forestfires[,-c(1,6,7)]) # ... with the same variable names

predict(topred,newdata=new) # Provides allocated group, posterior prob and discriminant score


lda.wind.pred <- predict(forest.lda, newdata = test)
lda.nowind.pred <- predict(forest.lda2, newdata = test)

sum(lda.wind.pred$class == test$severity)/dim(test)[1]
sum(lda.nowind.pred$class == test$severity)/dim(test)[1]
table(test$severity, factor(predict(forest.lda2, newdata = test)$class,labels=pred.gr[-3]))
  diag(prop.table(table(test$severity, lda.nowind.pred$class), 1))
  
x <- predict(topred, newdata = test)
sum(x$class == test$severity)/dim(test)[1]
diag(prop.table(table(test$severity, x$class), 1))



# Proportion correct by group
diag(prop.table(table(test$severity, factor(lda.wind.pred$class,labels=pred.gr)), 1))
diag(prop.table(table(test$severity, factor(lda.nowind.pred$class,labels=pred.gr[-3])), 1))
# Overall accuracy
# sum(diag(prop.table(table(test$severity, factor(lda.wind.pred$class),labels=pred.gr))))
# sum(diag(prop.table(table(test$severity, factor(lda.nowind.pred$class,labels=pred.gr[-3]))))) # Overall accuracy




# Quadratic discriminant analysis (QDA)

forest.qda <- qda(severity ~ temp + RH + wind + rain, data = forestfires,CV=T) # Using cross-validation
forest.qda2 <- qda(severity ~ temp + RH + wind, data = forestfires,CV=T) # Using cross-validation

topred <- qda(severity ~ temp + RH + wind, data = train) # Using cross-validation

forest.qda
cm.qda <- table(forestfires$severity,forest.qda$class) #Confusion table
cm.qda2 <- table(forestfires$severity,forest.qda2$class) #Confusion table
cm.qda
cm.qda2

# Accuracy by grouup
diag(prop.table(cm.qda,1))
diag(prop.table(cm.qda2,1))
# Overall
sum(diag(prop.table(cm.qda)))
sum(diag(prop.table(cm.qda2)))

##### TUTORIAL 5
library(fpc)
d <- dist(forestfires, method = "euclidean")
hc <- hclust(forestfires, method="ward.D")
# Cluster centroids (standarised data)
hc3 <- cutree(forestfires, k=3) # Cluster assignments
st <- by(temp2,hc3,colMeans) # Mean vector by cluster
st <- matrix(unlist(st), nrow = 12)
colnames(st) <- c("Cluster 1","Cluster 2","Cluster 3")
st <- as.data.frame(st,row.names=levels(temp.long$Month))
print(st)


















library(data.table)
library(corrplot)
library(ggplot2)
library(grid)
library(h2o)
library(Rmisc)
library(hexbin)
library(GGally)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

dt <- data.table::fread("train.csv", sep=',', header=TRUE)
summary(dt)
dt_na <- na.omit(dt)
fixed_dt <- dt_na
head(fixed_dt, 10)

fixed_dt$Ref_5x5_10th[which(fixed_dt$Ref_5x5_10th < 0)] <- NA
fixed_dt$Ref_5x5_50th[which(fixed_dt$Ref_5x5_50th < 0)] <- NA
fixed_dt$Ref_5x5_90th[which(fixed_dt$Ref_5x5_90th < 0)] <- NA
fixed_dt$RefComposite[which(fixed_dt$RefComposite < 0)] <- NA
fixed_dt$RefComposite_5x5_10th[which(fixed_dt$RefComposite_5x5_10th < 0)] <- NA
fixed_dt$RefComposite_5x5_50th[which(fixed_dt$RefComposite_5x5_50th < 0)] <- NA
fixed_dt$RefComposite_5x5_90th[which(fixed_dt$RefComposite_5x5_90th < 0)] <- NA
fixed_dt$Ref[which(fixed_dt$Ref < 0)] <- NA
fixed_dt$RhoHV[which(fixed_dt$RhoHV > 1)] <- NA
fixed_dt$RhoHV[which(fixed_dt$RhoHV_5x5_10th > 1)] <- NA
fixed_dt$RhoHV[which(fixed_dt$RhoHV_5x5_50th > 1)] <- NA
fixed_dt$RhoHV[which(fixed_dt$RhoHV_5x5_90th > 1)] <- NA
fixed_dt$Expected[which(fixed_dt$Expected >= 70)] <- NA
fixed_dt$logExpected <- log1p(fixed_dt$Expected)
fixed_dt <- na.omit(fixed_dt)

mcor <- cor(fixed_dt, use="complete")
corrplot(mcor, type="upper", tl.col="black", tl.srt=45)

g_ref <- ggplot(fixed_dt, aes(x = Ref, colour = "Ref"))+
  geom_density(size = 1, adjust = 2)+
  geom_hline(yintercept=0, colour="white", size=1)
g_ref + stat_function(fun = dnorm, aes(colour = "stand norm"), size = 1, args = list(mean = mean(fixed_dt$Ref, na.rm = TRUE), sd = sd(fixed_dt$Ref, na.rm = TRUE)))+
  scale_colour_manual("Line", values = c("red", "blue"), breaks=c("stand norm", "Ref"))+
  ggtitle("Ref and normal dist")+
  theme(text = element_text(size=20))

g_kdp <- ggplot(fixed_dt, aes(x = Kdp, colour = "Kdp"))+
  geom_density(size = 1, adjust = 3)
g_kdp + stat_function(fun = dnorm, aes(colour = "stand norm"), size = 1, args = list(mean = mean(fixed_dt$Kdp, na.rm = TRUE), sd = sd(fixed_dt$Kdp, na.rm = TRUE)))+
  scale_colour_manual("Line", values = c("red", "blue"), breaks=c("stand norm", "Kdp"))+
  xlim(-40, 40)+
  ggtitle("Kdp and normal dist")+
  theme(text = element_text(size=20))+
  geom_hline(yintercept=0, colour="white", size=0.8)

g_zdr <- ggplot(fixed_dt, aes(x = Zdr, colour = "Zdr"))+
  geom_density(size = 1, adjust = 3)
g_zdr + stat_function(fun = dnorm, aes(colour = "stand norm"), size = 1, args = list(mean = mean(fixed_dt$Zdr, na.rm = TRUE), sd = sd(fixed_dt$Zdr, na.rm = TRUE)))+
  scale_colour_manual("Line", values = c("red", "blue"), breaks=c("stand norm", "Zdr"))+
  xlim(-20,20)+
  ggtitle("Zdr and normal dist")+
  theme(text = element_text(size=20))+
  geom_hline(yintercept=0, colour="white", size=0.8)


g_rc <- ggplot(fixed_dt, aes(x = RefComposite, colour = "RefComp"))+
  geom_density(size = 1, adjust = 3)
g_rc + stat_function(fun = dnorm, aes(colour = "stand norm"), size = 1, args = list(mean = mean(fixed_dt$RefComposite, na.rm = TRUE), sd = sd(fixed_dt$RefComposite, na.rm = TRUE)))+
  scale_colour_manual("Line", values = c("red", "blue"), breaks=c("stand norm", "RefComp"))+
  ggtitle("RefComposite and normal dist")+
  theme(text = element_text(size=20))+
  geom_hline(yintercept=0, colour="white", size=0.8)

ggplot(fixed_dt, aes(x = RhoHV, colour = "RhoHV"))+
  geom_density(size = 1, adjust = 5)+
  geom_vline(xintercept=1, colour="black", size=1)+
  xlim(0.5,1.1)+
  ggtitle("RhoHV")+
  theme(text = element_text(size=20))

g1 <-  ggplot(fixed_dt, aes(x = Ref)) + geom_density()  
g2 <-  ggplot(fixed_dt, aes(x = Ref_5x5_10th)) + geom_density()  
g3 <-  ggplot(fixed_dt, aes(x = Ref_5x5_50th)) + geom_density()  
g4 <-  ggplot(fixed_dt, aes(x = Ref_5x5_90th)) + geom_density()  
g5 <-  ggplot(fixed_dt, aes(x = RefComposite)) + geom_density()  
g6 <-  ggplot(fixed_dt, aes(x = RefComposite_5x5_10th)) + geom_density()  
g7 <-  ggplot(fixed_dt, aes(x = RefComposite_5x5_50th)) + geom_density()  
g8 <-  ggplot(fixed_dt, aes(x = RefComposite_5x5_90th)) + geom_density()  
multiplot(g1, g2, g3, g4, g5, g6, g7, g8, cols=4)
?multiplot

ggplot(fixed_dt, aes(x = Expected, colour = "Expected"))+
  geom_density(size = 1, adjust = 3)+
  ggtitle("Expected dist")+
  theme(text = element_text(size=20))

ggplot(fixed_dt, aes(x = logExpected, colour = "logExpected"))+
  geom_density(size = 1, adjust = 3)+
  ggtitle("logExpected dist")+
  theme(text = element_text(size=20))

sample <- fixed_dt[sample(nrow(fixed_dt), 10000), ]

ggpairs(sample, columns = c(4:11))

ggpairs(sample, columns = c(12:15), title = 'RhoHV kind')

ggpairs(sample, columns = c(16:19), title = 'Zdr kind')

ggpairs(sample, columns = c(20:23), title = 'Kdp kind')

bh1 <- ggplot(sample, aes(Ref, Expected))
bh1 + stat_binhex(binwidth = c(1.5, 1.5), colour="white")+
  theme(text = element_text(size=20))

bh2 <- ggplot(sample, aes(Ref, logExpected))
bh2 + stat_binhex(binwidth = c(1, 0.07), colour="white")+
  theme(text = element_text(size=20))

bh3 <- ggplot(sample, aes(Ref_5x5_90th, logExpected))
bh3 + stat_binhex(binwidth = c(1.5, 0.1), colour="white")+
  theme(text = element_text(size=20))

conn <- h2o.init()
h2o_dt <- as.h2o(fixed_dt)

dt_split <- h2o.splitFrame(h2o_dt, ratios = 0.8, seed = -1)
train <- dt_split[[1]]
test <- dt_split[[2]]

glm1 = h2o.glm(y = "Expected", x = c(1:23),
                       training_frame = train, family = "gaussian")
perf <- h2o.performance(glm1, test)
r2_glm1 <- h2o.r2(perf)
h2o.varimp(glm1)
varimp <- h2o.varimp(glm1)
p1 <- ggplot(data = varimp, aes(x = names, y = coefficients))
p1 <- p1 + geom_bar(stat = "identity")
p1 <- p1 + coord_flip()
p1 + theme(text = element_text(size=20))

glm2 = h2o.glm(y = "logExpected", x = c(1:23),
                       training_frame = train, family = "gaussian")
perf <- h2o.performance(glm2, test)
r2_glm2 <- h2o.r2(perf)
varimp <- h2o.varimp(glm2)
p2 <- ggplot(data = varimp, aes(x = names, y = coefficients))
p2 <- p2 + geom_bar(stat = "identity")
p2 <- p2 + coord_flip()
p2 + theme(text = element_text(size=20))

glm3 = h2o.glm(y = "logExpected", x = c('Ref_5x5_90th'),
               training_frame = train, family = "gaussian")
print(glm3)
perf <- h2o.performance(glm3, test)
r2_glm3 <- h2o.r2(perf)
h2o.varimp(glm3)
varimp <- h2o.varimp(glm3)
p3 <- ggplot(data = varimp, aes(x = names, y = coefficients))
p3 <- p3 + geom_bar(stat = "identity")
p3 <- p3 + coord_flip()
p3 + theme(text = element_text(size=20))

glm4 = h2o.glm(y = "Expected", x = c(1:23),
               training_frame = train, family = "gaussian", lambda = 1)
perf <- h2o.performance(glm4, test)
r2_glm4 <- h2o.r2(perf)
varimp <- h2o.varimp(glm4)
p4 <- ggplot(data = varimp, aes(x = names, y = coefficients))
p4 <- p4 + geom_bar(stat = "identity")
p4 <- p4 + coord_flip()
p4 + theme(text = element_text(size=20))

rf1 = h2o.randomForest(y = "Expected", x = c(1:23),
                       training_frame = train)

perf <- h2o.performance(rf1, test)
r2_rf1 <- h2o.r2(perf)
varimp <- h2o.varimp(rf1)
p5 <- ggplot(data = varimp, aes(x = variable, y = scaled_importance))
p5 <- p5 + geom_bar(stat = "identity")
p5 <- p5 + coord_flip()
p5 + theme(text = element_text(size=20))

rf2 = h2o.randomForest(y = "logExpected", x = c(1:23), training_frame = train)
perf <- h2o.performance(rf2, test)
r2_rf2 <- h2o.r2(perf)
varimp <- h2o.varimp(rf2)
p6 <- ggplot(data = varimp, aes(x = variable, y = scaled_importance))
p6 <- p6 + geom_bar(stat = "identity")
p6 <- p6 + coord_flip()
p6 + theme(text = element_text(size=20))

rf3 = h2o.randomForest(y = "logExpected", x = c(3, 4, 6, 7, 8, 10, 11),
                       training_frame = train)
print(rf3)
perf <- h2o.performance(rf3, test)
r2_rf3 <- h2o.r2(perf)
h2o.varimp(rf3)
varimp <- h2o.varimp(rf3)
p7 <- ggplot(data = varimp, aes(x = variable, y = scaled_importance))
p7 <- p7 + geom_bar(stat = "identity")
p7 <- p7 + coord_flip()
p7 + theme(text = element_text(size=20))

rf4 = h2o.randomForest(y = "logExpected", x = c(7),
                       training_frame = train)
perf <- h2o.performance(rf4, test)
r2_rf4 <- h2o.r2(perf)
varimp <- h2o.varimp(rf4)
p8 <- ggplot(data = varimp, aes(x = variable, y = scaled_importance))
p8 <- p8 + geom_bar(stat = "identity")
p8 <- p8 + coord_flip()
p8 + theme(text = element_text(size=20))


dtr1 <- rpart(logExpected ~ minutes_past + Ref + Ref_5x5_10th + Ref_5x5_50th + Ref_5x5_90th + radardist_km+
               RefComposite + RefComposite_5x5_10th + RefComposite_5x5_50th +
               RefComposite_5x5_90th + RhoHV + RhoHV_5x5_10th + RhoHV_5x5_50th + 
               RhoHV_5x5_90th + Zdr + Zdr_5x5_10th + Zdr_5x5_50th + Zdr_5x5_90th +
               Kdp + Kdp_5x5_10th + Kdp_5x5_50th + Kdp_5x5_90th, data=sample, method= 'anova', control = rpart.control(minsplit=20, cp=0.001))
plotcp(dtr1)
rsq.rpart(dtr1)
fancyRpartPlot(dtr1)
plot(dtr1, uniform=TRUE, main="Regression Tree")
text(dtr1, use.n=TRUE, all=TRUE, cex=.5)
fancyRpartPlot(dtr2)
post(dtr2, file = "C:/Users/Aspire/Downloads/tree.ps", 
     title = "Regression Tree")
predictions <- predict(dtr, fixed_dt[,1:23])
mse <- mean((fixed_dt$Expected - predictions)^2, na.rm = TRUE)
print(mse)
dtr1_2 <- rpart(logExpected ~ minutes_past + Ref + Ref_5x5_10th + Ref_5x5_50th + Ref_5x5_90th + radardist_km+
                RefComposite + RefComposite_5x5_10th + RefComposite_5x5_50th +
                RefComposite_5x5_90th + RhoHV + RhoHV_5x5_10th + RhoHV_5x5_50th + 
                RhoHV_5x5_90th + Zdr + Zdr_5x5_10th + Zdr_5x5_50th + Zdr_5x5_90th +
                Kdp + Kdp_5x5_10th + Kdp_5x5_50th + Kdp_5x5_90th, data=sample, method= 'anova', control = rpart.control(minsplit=20, cp=0.003))
plotcp(dtr1_2)
rsq.rpart(dtr1_2)
fancyRpartPlot(dtr1_2)
predictions <- predict(dtr1_2, fixed_dt[,1:23])
mse <- mean((fixed_dt$Expected - predictions)^2, na.rm = TRUE)
print(mse)
dtr2 <- rpart(Expected ~ Ref_5x5_50th + Ref_5x5_90th, data=fixed_dt, method= 'anova', control = rpart.control(minsplit=20, cp=0.0001))
printcp(dtr2)
summary(dtr2)
plotcp(dtr2)
rsq.rpart(dtr2)
plot(dtr2, uniform=TRUE, main="Regression Tree")
text(dtr2, use.n=TRUE, all=TRUE, cex=.8)
fancyRpartPlot(dtr2)
post(dtr2, file = "C:/Users/Aspire/Downloads/tree.ps", 
     title = "Regression Tree")
predictions <- predict(dtr2, fixed_dt[,1:23])
rmse <- mean((fixed_dt$Expected - predictions)^2, na.rm = TRUE)
print(rmse)

dtr3 <- rpart(logExpected ~ Ref_5x5_50th + Ref_5x5_90th, data=fixed_dt, method= 'anova', control = rpart.control(minsplit=20, cp=0.0001))
printcp(dtr3)
summary(dtr3)
plotcp(dtr3)
rsq.rpart(dtr3)
plot(dtr3, uniform=TRUE, main="Regression Tree", margin=0.05)
text(dtr3, use.n=TRUE, all=TRUE, cex=.55)



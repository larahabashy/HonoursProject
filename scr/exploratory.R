#set working directory
setwd("~/Desktop/HP")

# Load packages
library(pacman)
p_load(
  tidyverse, skimr, feather, magrittr, lubridate,
  microbenchmark, tictoc, furrr,
  tidytext,
  xts, zoo, imputeTS,
  scales, dygraphs, plotly, htmlwidgets, viridis, ggrepel, gridExtra, ggthemes,
  tsibble
)
#devtools::install_github("ncordon/imbalance")

#read data
data <- read.csv("data.csv")

#format
data_copy <- data
str(data)
data$id <- NULL
data$diagnosis <- as.factor(data$diagnosis)

if (! require ("imbalance" )){
  install.packages ("imbalance")
  library ( imbalance )
}
imbalanceRatio(dataset = data, classAttr = 'diagnosis') #0.5938375
na.omit(data)

#convert diagnosis column to binary. M=1, B=0
#data$diagnosis.bin <- ifelse(data$diagnosis == 'M',1,0)

#proportions
prop.table(table(data$diagnosis)) #62.74165% B 37.25835% M

#Data Partition
set.seed (123)
ind <- sample(2,nrow(data), replace =TRUE , prob =c(2/3,1/3))
training = data [ind ==1,]
testing = data [ind ==2,]

prop.table(table(training$diagnosis)) #65.53525% B 34.46475% M 
prop.table(table(testing$diagnosis))  #56.98925% B 43.01075% M 

#Statified data partion
library(caret)
index <- createDataPartition(data$diagnosis, p = .7, list = FALSE)
train <- data[ index,]
test  <- data[-index,]

prop.table(table(train$diagnosis)) #62.65664% B 37.34336% M
prop.table(table(test$diagnosis))  #62.94118% B 37.05882% M 
train.copy <- train #fixed copy

#subsets
#the first "subset" is the full model of 30 features
s1 <- data
s1_train <- train
s1_test <- test
s1_train_scaled <- s1_train %>% mutate_at(c(2,31), funs(c(scale(.))))

#the second subset consists of 16 uncorrelated features
s2 <- data %>% dplyr::select(-c(perimeter_mean,radius_mean,perimeter_worst,
                         radius_worst, perimeter_se,radius_se,compactness_mean,
                         compactness_worst,compactness_se,concave.points_mean,
                         concave.points_worst,concave.points_se,fractal_dimension_se,
                         area_worst))

#standardize data
library(dplyr)
s2_scaled <- s2 %>% mutate_at(c(2,17), funs(c(scale(.))))
s2_split <- createDataPartition(s2$diagnosis, p = .7, list = FALSE)
#s2_split <- createDataPartition(s2_scaled$diagnosis, p = .7, list = FALSE)
s2_train <- s2[s2_split,]
s2_test <- s2[-s2_split,]

#correlation stuff
if (! require ("FSinR" )){
  install.packages ("FSinR")
  library ( FSinR )
}
if (! require ("tidyverse" )){
  install.packages ("tidyverse")
  library ( tidyverse )
}

best_features <- selectKBest(s2,'diagnosis',roughsetConsistency,10)
best_features$featuresSelected
best_features$bestFeatures
best_features$valuePerFeature
#best features are area_mean, concavity_mean, texture_se, area_se, smoothness_se, concavity_se, texture_worst, concavity_worst, symmetry_worst, fractal_dimension_worst
#value per feature: 0.9824253 0.9824253 0.9771529 0.9753954 0.9753954 0.9718805 0.9384886 0.9103691 0.9103691 0.8910369

#the third subset consists of top 10 best predictive features selected from s2
s3 <- s2 %>% dplyr::select(c(diagnosis, best_features$featuresSelected))
s3_scaled <- s3 %>% mutate_at(c(2,11), funs(c(scale(.))))
set.seed(123)
s3_split <- createDataPartition(s3$diagnosis, p = .7, list = FALSE)
#s3_split <- createDataPartition(s3_scaled$diagnosis, p = .7, list = FALSE)
s3_train <- s3[s3_split,]
s3_test <- s3[-s3_split,]

#third subset: select best features from full dataset
best_features_full <-selectKBest(s1,'diagnosis',roughsetConsistency,17)
best_features_full$valuePerFeature
best_features_full$featuresSelected
#we look at the selectbestK function attribute "value per feature"to get a rough estimate of the number of features to include in the modeling.
#the top 15 features values:
#first we look at the value per feature for all the featues. We see the value dropping to 93% around the top 18th - 20th valuable feature
#so it makes sense to consider the top 17 features - with the 17th feature approximately 97% value.
s4 <- data %>% dplyr::select(c(diagnosis, best_features_full$featuresSelected))
s4_scaled <- s4 %>% mutate_at(c(2,18), funs(c(scale(.))))
set.seed(123)
s4_split <- createDataPartition(s4$diagnosis, p = .7, list = FALSE)
#s4_split <- createDataPartition(s3_scaled$diagnosis, p = .7, list = FALSE)
s4_train <- s4[s4_split,]
s4_test <- s4[-s4_split,]


#----------------------------Random Forest------------------------
#Variable Importance using Random Forests 
if (! require ("randomForest" )){
  install.packages ("randomForest")
  library ( randomForest )
}
#perform training
rf <- randomForest(diagnosis ~ ., data=train, ntree=50, mtry=2, importance=TRUE)
#50 is optimal number of trees
rf

#method 1
varImpPlot(rf) 

# Validation set assessment - looking at confusion matrix
prediction_for_table <- predict(rf_classifier_1,test)
head(prediction_for_table)
head(testing$diagnosis) #only one misclassification

library(caret)
confusionMatrix(prediction_for_table, test$diagnosis)

#Error
plot(rf, main = "Error x Trees")


library(colorspace)
hist(treesize(rf),
     main= "Number of nodes for the trees",
     col=rainbow_hcl(6))
importance(rf)
varImpPlot(rf, sort = TRUE, scale = TRUE, main="Variable Importance",col=rainbow_hcl(10))
partialPlot(rf,train,area_mean,"M")

#construct subset s5
s5 <- s1 %>% dplyr::select(c(diagnosis, concave.points_mean, radius_worst, area_worst, concavity_mean,
                        area_mean, perimeter_worst, concave.points_worst, texture_worst,
                        area_se, perimeter_se, perimeter_mean, radius_se))
                        
s5_scaled <- s5 %>% mutate_at(c(2,13), funs(c(scale(.))))
set.seed(123)
s5_split <- createDataPartition(s5$diagnosis, p = .7, list = FALSE)
#s5_split <- createDataPartition(s5_scaled$diagnosis, p = .7, list = FALSE)
s5_train <- s5[s5_split,]
s5_test <- s5[-s5_split,]


#method 2
if (! require ("VSURF")){
  install.packages ("VSURF")
  library (VSURF)
}
rf_vsruf <- VSURF(diagnosis ~ ., data=train, ntree=50, mtry=2)  #long run-time
print(rf_vsruf$varselect.pred)
summary(rf_vsruf$varselect.pred)
plot(rf_vsruf)

s6 <- s1 %>% dplyr::select(c(diagnosis, perimeter_mean, area_mean, compactness_mean, 
                        concavity_mean, concave.points_mean, radius_worst, 
                        texture_worst, perimeter_worst, concavity_worst, 
                        concave.points_worst))
s6_scaled <- s6 %>% mutate_at(c(2,11), funs(c(scale(.))))
set.seed(123)
s6_split <- createDataPartition(s6$diagnosis, p = .7, list = FALSE)
#s6_split <- createDataPartition(s5_scaled$diagnosis, p = .7, list = FALSE)
s6_train <- s6[s6_split,]
s6_test <- s6[-s6_split,]
#results are consistent with s4 and s6 with radius mean and size features are strong predictive (area) 
#convaity and concave points are commin
#- fractal_dimension_worst (which is not in S2 because its a correlated feature)



#-----------------------------------------PCA-------------------------------
##Principal Component Analysis

if (! require ("colorspace" )){
  install.packages (" colorspace ")
  library ( colorspace )
}
if (! require ("ggfortify" )){
  install.packages (" ggfortify ")
  library ( ggfortify )
}
if (! require ("factoextra")){
  install.packages ("factoextra")
  library (factoextra)
}
#Normalize Data
cont.train <- train
cont.train$diagnosis <- NULL
#scale cont variables
cont.scaled <- scale(cont.train, scale = TRUE, center= TRUE)
str(cont.train)

#PCA on normalized Data
pc <- prcomp(cont.scaled)
summary(pc)
plot(pc, main= "PC Screeplot", col=rainbow_hcl(12))
screeplot(pc,main = "PCA Screeplot",  ylab = "Principal Components", type="lines",col=rainbow_hcl(7))

a_plot <- autoplot(pc, data = train, colour = 'diagnosis')
a_plot

fviz_eig(pc, barfill = rainbow_hcl(10), barcolor = rainbow_hcl(10), linecolor = "red", ggtheme = theme_minimal())

fviz_pca_ind(pc,
             col.ind = "red", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pc, repel = TRUE,
                title = "VA",
                col.var = "#2E9FDF", # Variables color
                col.ind = "white"  # Individuals color
)

library(ggplot2)
ggbiplot(pc,ellipse=TRUE,  labels=rownames(train), groups=as.factor(diagnosis))

library(devtools)
install_github("vqv/ggbiplot")

#Eigenvalues
eig.val <- get_eigenvalue(pc)
eig.val

#create a matrix of the first 10 principal components
pc.matrix<-matrix(c(pc$rotation[,1],
                    pc$rotation[,2],
                    pc$rotation[,3],
                    pc$rotation[,4],
                    pc$rotation[,5],
                    pc$rotation[,6],
                    pc$rotation[,7],
                    pc$rotation[,8],
                    pc$rotation[,9],
                  pc$rotation[,10]),ncol=10)


#PC1 - PC10 (10 x 30 matrix)
pc.matrix.abs<-abs(pc.matrix) #to see largest value regardless of sign
pc.matrix.abs

pc.matrix.abs<-round(pc.matrix.abs) 
pc.matrix.abs
#PC1 dominated by variable 6
#PC2 dominated by variable 2 and 4
#PC3 dominated by variable 3
#PC4 dominated by variable 2 and 22
#PC8 dominated by variable 12
#PC10- dominated by variable 9


#histogram with scaled data
pairs(cont.scaled, upper.panel = panel.cor, diag.panel = panel.hist)

#create matrix to see weights of variables on PC's then use this to eliminate variables & redo PCA 
random<-NULL 
for(i in 1:10) { 
  random<-c(random,which.max(pc.matrix.abs[,i])) 
  pc.matrix.abs[which.max(pc.matrix.abs[,i]),i] <- 0 
  random<-c(random,which.max(pc.matrix.abs[,i])) }
random.max <- matrix(random,nrow = 10,ncol = 2, byrow = TRUE) 
random.max 
var.freq<-as.data.frame(table(unlist(random.max))) 
var.freq




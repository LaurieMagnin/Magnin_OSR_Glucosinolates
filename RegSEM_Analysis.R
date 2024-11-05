library(lavaan)
library(semPlot)
library(car)
library(regsem)
library(ggplot2)
library(ggpubr)
library(tidyr)

data_2023 <- readRDS("~/working/4_Glucosinolates_2022/3-Script/Article_Gluco/data_2023.rds")
head(data_2023)

#Standardisation of numerical variables
data_2023$Fresh.weight <- log(data_2023$Fresh.weight)
data_2023$Glucoraphanin <- log(data_2023$Glucoraphanin+1)
data_2023$Butyl.GS <- log(data_2023$Butyl.GS+1)

standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return( z) 
} 

data_2023[7:11] <- apply(data_2023[7:11], 2, standardize)
data_2023[14:27] <- apply(data_2023[14:27], 2, standardize)


varTable(data_2023)#Check that variables are properly characterized

mymodel<-' 
        Larvae.NB ~ Variety2 + Variety3+ + Cropping.sys + Fresh.weight + Stem.H +
                 Collar.D + Leaves.NB + Dry.weight + Glucoraphanin 
          + Glucobrassicanapin + Glucobrassicin+ Progoitrin.isomer + Butyl.GS 
          + Gluconapin +Glucoalyssin +Gluconapoleiferin +Neoglucobrassicin
          + Methoxyglucobrassicin +Gluconasturtiin

        Dry.weight ~ Variety2 + Variety3 + Cropping.sys
        Fresh.weight ~ Variety2 + Variety3 + Cropping.sys
        Stem.H ~ Variety2 + Variety3 + Cropping.sys
        Collar.D ~ Variety2 + Variety3 + Cropping.sys
        Leaves.NB ~ Variety2 + Variety3 + Cropping.sys
        Progoitrin.isomer~ Variety2 + Variety3 + Cropping.sys
        Butyl.GS~   Variety2 + Variety3 + Cropping.sys
        Glucoalyssin~ Variety2 + Variety3+ Cropping.sys
        Glucoraphanin ~  Variety2 + Variety3 + Cropping.sys 
        Glucobrassicanapin~ Variety2 + Variety3 + Cropping.sys 
        Glucobrassicin~  Variety2 + Variety3 + Cropping.sys
        Butyl.GS~   Variety2 + Variety3 + Cropping.sys
        Gluconapoleiferin~   Variety2 + Variety3 + Cropping.sys
        Gluconapin~   Variety2 + Variety3 + Cropping.sys
        Methoxyglucobrassicin~   Variety2 + Variety3 + Cropping.sys
        Neoglucobrassicin~   Variety2 + Variety3 + Cropping.sys
        Gluconasturtiin~   Variety2 + Variety3 + Cropping.sys
        '

fit.path<-sem(mymodel,data=data_2023)

reg.out <- cv_regsem(fit.path,type="lasso")

round(reg.out$fits,2)

plot(reg.out,show.minimum="BIC")

summary(reg.out)

nrep <-600

estimates <- list()

for(isample in 1:nrep){
  
  data_boot <- data_2023[sample(1:nrow(data_2023),  replace = TRUE),]
  
  fit.path<-sem(mymodel,data=data_boot)
  
  final_model <- regsem(fit.path, lambda=0.07, type="lasso", gradFun="ram")
  
  estimates[[isample]] <- final_model$coefficients
}

library(data.table)

table_est <- data.frame(rbindlist(estimates))

head(table_est)

table_est <- subset(table_est, table_est$Variety2....Larvae.NB > -100) # Remove possible unconverged models
table_est <- subset(table_est, table_est$Variety2....Larvae.NB < 100) # Remove possible unconverged models
table_est <- subset(table_est, Cropping.sys....Larvae.NB < 100) # Remove possible unconverged models

q_est<- list()

for (ivariable in 1:ncol(table_est)) {
  variable <- colnames(table_est)[ivariable]
  q_est[[variable]] <- quantile(table_est[,ivariable], probs=c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)) 
}


q_est_df <- do.call(rbind, q_est)

# Assign column names for easier interpretation
colnames(q_est_df) <- c("Min", "2.5%", "25%", "Median", "75%", "97.5%", "Max")

# Add a column for variable names
q_est_df <- data.frame(Variable = rownames(q_est_df), q_est_df)
rownames(q_est_df) <- NULL

# View the result
print(q_est_df)


saveRDS(q_est_df, "~/working/4_Glucosinolates_2022/3-Script/Article_Gluco/Quantil_estimates.rds")

data_2023 <- read.csv("~/working/4_Glucosinolates_2022/2-Data/2024_EVA_Berlese_Gluco.csv", sep=";")
head(data_2023)

col_names <- colnames(data_2023)

data_2023 <- setNames(data_2023, c("X.","bloc","Variety","Cropping.sys", "Echantillon", "Modality", "Stem.H", 
                                   "Collar.D", "Leaves.NB", "Fresh.weight", "Dry.weight", "Larvae.NB", "Sample.ID",
                                   "Glucoraphanin", "Progoitrin.isomer", "Glucoalyssin","Gluconapoleiferin","Gluconapin","Butyl.GS", "Glucobrassicanapin",
                                   "Glucobrassicin", "Gluconasturtiin","Methoxyglucobrassicin","Neoglucobrassicin",
                                   "Unknown.GLS..C16H20N2O11S2.", "Total.GS"))

head(data_2023)

# Creating data set for SEM ####

data_2023<- subset(data_2023, data_2023$X. != "68") # Remove outlier
data_2023<- subset(data_2023, data_2023$X. != "2") #abnormal total gluco 
data_2023<- subset(data_2023, data_2023$X. != "18") #abnormal total gluco
data_2023<- subset(data_2023, data_2023$X. != "42") #abnormal total gluco

data_2023$larva.poid <- data_2023$Larvae.NB/data_2023$Fresh.weight


# Change varieties order to put Mambo(2) as the first Variety to make the intercept with it

data_2023$Variety <- ifelse(data_2023$Variety =="1", "2", ifelse(data_2023$Variety =="2", "1", "3"))
data_2023$Variety <- as.factor(data_2023$Variety )


# Transformation of the Cropping.sys variable into dummy

data_2023$Cropping.sys <- ifelse(data_2023$Cropping.sys=="mono", "0", "1")
data_2023$Cropping.sys <- as.factor(data_2023$Cropping.sys)

# Transformation of the Variety categorical factor into 2 (k-1) dummy variables

res <- model.matrix(~Variety-1, data = data_2023)
head(res[, -1])
data_2023<- cbind(data_2023, res)

data_2023$Variety2 <- as.numeric(data_2023$Variety2)
data_2023$Variety3 <- as.numeric(data_2023$Variety3)

saveRDS(data_2023, "~/working/4_Glucosinolates_2022/3-Script/Article_Gluco/data_2023.rds")


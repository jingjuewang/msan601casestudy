library(dplyr)

# load the housing.txt data
houseData <- read.table(FILENAME, header = TRUE,sep = ",", dec = ".")

# replace NA in housing data
replaceNA <- function(col, newVal){
  if(class(col) == "integer"){
    c <- which(is.na(col))
    col <- replace(col, c, newVal)
  }
  chr_col <- as.character(col)
  c <- which(is.na(chr_col))
  replaced_col <- replace(chr_col, c, newVal)
  col <- as.factor(replaced_col)
  return(col)
}

#Replace NA's for appropriate columns
houseData$MiscFeature <- replaceNA(houseData$MiscFeature,"None")
houseData$Fence <- replaceNA(houseData$Fence,"None")
houseData$PoolQC <- replaceNA(houseData$PoolQC,"None")
houseData$Fence <- replaceNA(houseData$Fence,"None")
houseData$LotFrontage <- replaceNA(houseData$LotFrontage,0)
houseData$BsmtQual <- replaceNA(houseData$BsmtQual,"None")
houseData$BsmtCond <- replaceNA(houseData$BsmtCond,"None")
houseData$BsmtExposure <- replaceNA(houseData$BsmtExposure,"None")
houseData$BsmtFinType1 <- replaceNA(houseData$BsmtFinType1,"None")
houseData$BsmtFinType2 <- replaceNA(houseData$BsmtFinType2,"None")
houseData$MiscFeature <- replaceNA(houseData$MiscFeature,"None")
houseData$FireplaceQu <- replaceNA(houseData$FireplaceQu,"None")
houseData$GarageType <- replaceNA(houseData$GarageType, "None")
houseData$GarageFinish <- replaceNA(houseData$GarageFinish, "None")
houseData$GarageQual <- replaceNA(houseData$GarageQual, "None")
houseData$GarageCond <- replaceNA(houseData$GarageCond, "None")
houseData$Electrical <- replaceNA(houseData$Electrical, 'None')
houseData$Alley <- replaceNA(houseData$Alley,"None")

#Remove rows for MasVnrArea/MasVnrType
houseData <- houseData[-which(is.na(houseData$MasVnrArea)), ]

#Remove columns Id and GarageYrBlt
houseData <- select(houseData, -c(Id, GarageYrBlt))

#Percentage of NA for each col should be 0
sapply(houseData, function(x) round(sum(is.na(x))/nrow(houseData)*100, 2))

#Make sure this returns 0.
sum(is.na(houseData))
       
# standardize numeric variables
cat_var <- names(houseData)[which(sapply(houseData, is.factor))]
numeric_var <- names(houseData)[which(sapply(houseData, is.numeric))]
housing_categoric <- houseData[, cat_var]
housing_numeric <- scale(houseData[, numeric_var])

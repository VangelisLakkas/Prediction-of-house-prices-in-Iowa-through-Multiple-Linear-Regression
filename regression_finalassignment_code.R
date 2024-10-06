require(corrplot)
library(sjPlot)
require(sjPlot)
library(ggplot2)
require(glmnet)
library(dplyr)
require(dplyr)
library(car)
require(car)
library(moments)
require(moments)
library(MASS)
require(MASS)
library(nortest)
require(nortest)
library(Metrics)


TEST_DF <- read.csv(file = 'ames_iowa_housing_test.csv', header = TRUE, sep = ";")


houses_df <- read.csv(file = 'ames_iowa_housing_07.csv', header = TRUE, sep = ";")
houses_df_num <-subset.data.frame(houses_df,select = c('Lot.Frontage','Lot.Area', 'Year.Built','Year.Remod.Add',
                                                       'Mas.Vnr.Area','BsmtFin.SF.1','BsmtFin.SF.2','Bsmt.Unf.SF','Total.Bsmt.SF','X1st.Flr.SF','X2nd.Flr.SF','Low.Qual.Fin.SF',
                                                       'Gr.Liv.Area','Bsmt.Full.Bath','Bsmt.Half.Bath','Full.Bath','Half.Bath','Bedroom.AbvGr','Kitchen.AbvGr','TotRms.AbvGrd','Fireplaces','Garage.Yr.Blt','Garage.Cars','Garage.Area','Wood.Deck.SF',
                                                       'Open.Porch.SF','Enclosed.Porch','X3Ssn.Porch','Screen.Porch','Pool.Area','Misc.Val','Mo.Sold','Yr.Sold','SalePrice'))

cor_matrix <- cor(houses_df_num, use = "complete.obs")
cor_matrix
corrplot(cor_matrix, col = c(4,6))
corrplot(cor(houses_df_num), col = c(4,6), method = "number", type = 'lower', number.font = 8)
is.na(houses_df_num$BsmtFin.SF.1)


sjp.corr(houses_df_num,
         corr.method = "pearson",
         show.values = TRUE,
         sort.corr = TRUE,
         decimals = 2)

c <-  sum(is.na(houses_df_num$Garage.Area)) ##lot frontage nas=241, mas.nvr.area nas=14, bsmt.fin.sf nas=1,
##bsmt full/half bath nas=1, Garage.Yr.Blt nas =82, Garage.Cars nas=1,  Garage.Area nas=1 
c
originalrows <- nrow(houses_df_num)
originalrows
problematic_row <- which(is.na(houses_df_num$BsmtFin.SF.1) | is.na(houses_df_num$BsmtFin.SF.2) | is.na(houses_df_num$Bsmt.Unf.SF))
problematic_row
houses_df_num <- houses_df_num[-320,]
prob_row_garage <- which(is.na(houses_df_num$Garage.Cars))
prob_row_garage
houses_df_num <- houses_df_num[-1355,]
nrow(houses_df_num)
houses_df <-houses_df[c(-320,-1355),-1]

# Calculate correlations
correlations <- cor(houses_df_num)
# Extract correlations with "SalePrice"
saleprice_correlations <- correlations[,"SalePrice"]

# Print the result
print(saleprice_correlations) ##correlations with NAs


# Create a heatmap
ggplot(data = plot_data, aes(x = variable, y = 1, fill = correlation)) +
  geom_tile() +
  labs(x = "Variable", y = "", fill = "Correlation with SalePrice") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "yellow", high = "purple")


houses_df$PID <- factor(houses_df$PID)
is.factor(houses_df$PID)
length(levels(houses_df$PID))
houses_df$MS.SubClass <- factor(houses_df$MS.SubClass)
levels(houses_df$MS.SubClass)
is.factor(houses_df$MS.SubClass)

houses_df$MS.Zoning <- as.factor(houses_df$MS.Zoning)
levels(houses_df$MS.Zoning)

houses_df$Street <- factor(houses_df$Street)
houses_df$Alley[houses_df$Alley %in% c(NA)] <- "NA"
houses_df$Alley <- factor(houses_df$Alley)
levels(houses_df$Alley)
# Check the distribution of levels in the factor variable
table(houses_df$Alley)
sum(is.na(houses_df$Alley))

houses_df$Lot.Shape <- factor(houses_df$Lot.Shape, levels = c('IR3','IR2','IR1','Reg'), ordered = TRUE)
levels(houses_df$Lot.Shape)
is.ordered(houses_df$Utilities)
houses_df$Land.Contour <- as.factor(houses_df$Land.Contour)
houses_df$Utilities <- factor(houses_df$Utilities, levels = c('NoSeWa','NoSewr','AllPub'), ordered=TRUE)
houses_df$Lot.Config <- as.factor(houses_df$Lot.Config)
houses_df$Neighborhood <- factor(houses_df$Neighborhood, levels = c("Blmngtn", "Blueste", "BrDale",  "BrkSide", "ClearCr", "CollgCr", "Crawfor", "Edwards", "Gilbert", "Greens", 
                                                                    "GrnHill", "IDOTRR", "Landmrk", "MeadowV", "Mitchel", "NAmes",  "NoRidge", "NPkVill", "NridgHt", "NWAmes",  "OldTown",
                                                                    "Sawyer",  "SawyerW", "Somerst", "StoneBr", "SWISU",  "Timber",  "Veenker"))

length(levels(houses_df$Neighborhood))
houses_df$Condition.1 <- as.factor(houses_df$Condition.1)
houses_df$Condition.2 <- as.factor(houses_df$Condition.2)
levels(houses_df$Condition.2)
houses_df$Bldg.Type <- as.factor(houses_df$Bldg.Type)
houses_df$House.Style <- as.factor(houses_df$House.Style)


houses_df$Land.Slope <- factor(houses_df$Land.Slope, levels = c('Gtl','Mod','Sev'), ordered = TRUE)
houses_df$Overall.Qual <- factor(houses_df$Overall.Qual, levels = c(1:10), ordered = TRUE)
houses_df$Overall.Cond <- factor(houses_df$Overall.Cond, levels = c(1:10), ordered = TRUE)
houses_df$Roof.Style <- factor(houses_df$Roof.Style)
houses_df$Roof.Matl <- factor(houses_df$Roof.Matl, levels = c('NA','ClyTile','CompShg','Metal','Roll','Tar&Grv','WdShake','WdShgnl'))
houses_df$Exterior.1st <- factor(houses_df$Exterior.1st, levels = c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock",  "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco","VinylSd", "Wd Sdng", "WdShing"))
houses_df$Exterior.2nd <- factor(houses_df$Exterior.2nd, levels = c("None","AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock",  "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco","VinylSd", "Wd Sdng", "WdShing"))


houses_df$Mas.Vnr.Type <- factor(houses_df$Mas.Vnr.Type, levels = c("BrkCmn",  "BrkFace", "CBlock", "None", "Stone" ))
houses_df$Exter.Qual <-factor(houses_df$Exter.Qual, levels = c('Po', 'Fa', 'TA', 'Gd', 'Ex'), ordered = TRUE)
houses_df$Exter.Cond <-factor(houses_df$Exter.Cond, levels = c('Po', 'Fa', 'TA', 'Gd', 'Ex'), ordered = TRUE)
houses_df$Foundation <- factor(houses_df$Foundation)
levels(houses_df$Foundation)

houses_df$Bsmt.Qual[houses_df$Bsmt.Qual %in% c(NA)] <- "NA"
houses_df$Bsmt.Cond[houses_df$Bsmt.Cond %in% c(NA)] <- "NA"
houses_df$Bsmt.Exposure[houses_df$Bsmt.Exposure %in% c(NA)] <- "NA"
houses_df$BsmtFin.Type.1[houses_df$BsmtFin.Type.1 %in% c(NA)] <- "NA"
houses_df$BsmtFin.Type.2[houses_df$BsmtFin.Type.2%in% c(NA)] <- "NA"



houses_df$Bsmt.Cond <- factor(houses_df$Bsmt.Cond, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
houses_df$Bsmt.Qual<- factor(houses_df$Bsmt.Qual, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
levels(houses_df$Bsmt.Cond)
table(houses_df$BsmtFin.Type.2, useNA = 'always')
houses_df$Bsmt.Exposure <- factor(houses_df$Bsmt.Exposure, levels = c('NA','No','Mn','Av','Gd'),ordered = TRUE)
houses_df$BsmtFin.Type.1 <- factor(houses_df$BsmtFin.Type.1, levels = c('NA','Unf','LwQ','Rec','BLQ','ALQ','GLQ'),ordered = TRUE)
houses_df$BsmtFin.Type.2 <- factor(houses_df$BsmtFin.Type.2, levels = c('NA','Unf','LwQ','Rec','BLQ','ALQ','GLQ'),ordered = TRUE)
is.ordered(houses_df$BsmtFin.Type.2)

houses_df$Heating <-factor(houses_df$Heating)
levels(houses_df$Heating)
houses_df$Heating.QC <- factor(houses_df$Heating.QC, levels = c("Po","Fa", "TA","Gd","Ex"), ordered = TRUE)
houses_df$Central.Air<-factor(houses_df$Central.Air)


houses_df$Electrical <- factor(houses_df$Electrical, levels = c("Mix", "FuseP","FuseF","FuseA", "SBrkr"),ordered = TRUE)
houses_df$Kitchen.Qual<-factor(houses_df$Kitchen.Qual, levels = c("Po","Fa", "TA","Gd","Ex"), ordered = TRUE)
houses_df$Functional <- factor(houses_df$Functional, levels = c("Sal","Sev","Maj2", "Maj1","Mod", "Min2", "Min1","Typ"),ordered=TRUE)


houses_df$Fireplace.Qu[houses_df$Fireplace.Qu%in% c(NA)] <- "NA"
houses_df$Garage.Type[houses_df$Garage.Type%in% c(NA)] <- "NA"
houses_df$Garage.Finish[houses_df$Garage.Finish%in% c(NA)] <- "NA"
houses_df$Garage.Qual[houses_df$Garage.Qual%in% c(NA)] <- "NA"
houses_df$Garage.Cond[houses_df$Garage.Cond%in% c(NA)] <- "NA"
houses_df$Pool.QC[houses_df$Pool.QC%in% c(NA)] <- "NA"
houses_df$Fence[houses_df$Fence%in% c(NA)] <- "NA"
houses_df$Misc.Feature[houses_df$Misc.Feature%in% c(NA)] <- "NA"

houses_df$Fireplace.Qu <- factor(houses_df$Fireplace.Qu, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
houses_df$Garage.Type <- factor(houses_df$Garage.Type)
houses_df$Garage.Finish<-factor(houses_df$Garage.Finish, levels = c( "NA","Unf","RFn","Fin"), ordered = TRUE)
houses_df$Garage.Qual<-factor(houses_df$Garage.Qual, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
houses_df$Garage.Cond<-factor(houses_df$Garage.Cond, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
houses_df$Paved.Drive<-factor(houses_df$Paved.Drive, ordered = TRUE)
houses_df$Pool.QC<- factor(houses_df$Pool.QC, levels =c('NA', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
houses_df$Fence <- factor(houses_df$Fence, levels=c("NA","MnWw","GdWo","MnPrv","GdPrv"),ordered = TRUE)
table(houses_df$Misc.Feature)
houses_df$Misc.Feature<-factor(houses_df$Misc.Feature,levels=c("NA","TenC","Shed","Othr","Gar2","Elev"))
houses_df$Sale.Type<-factor(houses_df$Sale.Type)
houses_df$Sale.Condition<-factor(houses_df$Sale.Condition)
levels(houses_df$Sale.Condition)


##Filling in NA values in numeric variables
missing_rows <- is.na(houses_df$Lot.Frontage)
houses_df$Lot.Frontage[missing_rows] <- round(houses_df$Lot.Area[missing_rows] * 0.009)
sum(is.na(houses_df$Lot.Frontage))

missing_rows_grg_year <- is.na(houses_df$Garage.Yr.Blt)
houses_df$Garage.Yr.Blt[missing_rows_grg_year] <- houses_df$Year.Built[missing_rows_grg_year]
sum(is.na(houses_df$Garage.Yr.Blt))

sum(is.na(houses_df$Mas.Vnr.Area))
missing_rows_mas.vnr.area <- is.na(houses_df$Mas.Vnr.Area)
houses_df$Mas.Vnr.Area[missing_rows_mas.vnr.area] <- 0
sum(is.na(houses_df))
col_with_na <- colnames(houses_df)[colSums(is.na(houses_df)) > 0]
print(col_with_na)
sum(is.na(houses_df))
sum(is.na(houses_df$Garage.Area))
rows_with_na_garage <- which(is.na(houses_df$Garage.Cars) | is.na(houses_df$Garage.Area))
rows_with_na_garage
houses_df <- houses_df[-rows_with_na_garage, ]

##Filling in NA values in categorical variables
missing_rows_vnr.type <- is.na(houses_df$Mas.Vnr.Type)
houses_df$Mas.Vnr.Type[missing_rows_vnr.type] <- "None"
table(houses_df$Mas.Vnr.Type, useNA = "always")
houses_df <-houses_df[,c(-1,-2)]
houses_df$Roof.Matl[houses_df$Roof.Matl%in% c(NA)] <- "NA"
table(houses_df$Exterior.2nd,useNA = "always")
houses_df$Exterior.2nd[houses_df$Exterior.2nd%in% c(NA)] <- "None"

sum(is.na(houses_df))

table(round(houses_df$Lot.Frontage/houses_df$Lot.Area, digits = 3))

##Second numeric subset
houses_df_num2 <- houses_df %>%
  select_if(is.numeric)
cor_matrix2 <- cor(houses_df_num2)
cor_matrix2
saleprice_correlations2 <- cor_matrix2[,"SalePrice"]
print(saleprice_correlations2) ##correlations without NAs
par(mfrow=c(2,2))
corrplot(cor_matrix, col = c(4,6))
corrplot(cor_matrix2, col = c(4,6))
par(mfrow=c(1,1))


full_model_demo <-lm(houses_df$SalePrice~.,data = houses_df)
summary(full_model_demo)


L1 <- model.matrix(full_model_demo)[,-1]
lasso2 <- glmnet(L1, houses_df$SalePrice)
plot(lasso, xvar = "lambda", label = T)
#Use cross validation to find a reasonable value for lambda 
lasso3 <- cv.glmnet(L1, houses_df$SalePrice, alpha = 1)
lasso3$lambda
summary(lasso3$lambda)
lasso3$lambda.min
lasso3$lambda.1se
plot(lasso3)
coef(lasso3, s = "lambda.min")
coef(lasso3, s = "lambda.1se")

lassof <- coef(lasso3, s ="lambda.min")
lassof


plot(lasso1$glmnet.fit, xvar = "lambda", label = T)
abline(v=log(c(lasso3$lambda.min, lasso1$lambda.1se)), lty =2)

plot(lasso1$glmnet.fit, xvar = "lambda", label = T)
abline(v=log(c(lasso1, lasso1$lambda.1se)), lty =2)


# Identify complete cases
complete_rows <- complete.cases(houses_df$SalePrice, houses_df[, colnames(houses_df) != "SalePrice"])
houses_df_subset <- houses_df[complete_rows, ]


nullmodel <- lm(houses_df$SalePrice~1, data = houses_df)
summary(nullmodel)
step(nullmodel, scope=list(lower=nullmodel,upper=full_model_demo) ,direction = 'forward')
summary(step(constantmodel, direction = 'forward'))

sum(is.na(houses_df$Mas.Vnr.Area))
mean(na.exclude(houses_df$Mas.Vnr.Area));median(na.exclude(houses_df$Mas.Vnr.Area))
round(houses_df$Lot.Frontage/houses_df$Lot.Area, digits = 3)
table(round(houses_df$Lot.Frontage/houses_df$Lot.Area, digits = 3))




model2 <- lm(houses_df$SalePrice~houses_df$MS.SubClass+houses_df$MS.Zoning+houses_df$Lot.Area+houses_df$Land.Contour+houses_df$Utilities
             +houses_df$Lot.Config+houses_df$Neighborhood+houses_df$Condition.1+houses_df$Condition.2
             +houses_df$Bldg.Type+houses_df$Overall.Qual+houses_df$Overall.Cond+houses_df$Year.Built+houses_df$Year.Remod.Add+houses_df$Roof.Style
             +houses_df$Roof.Matl+houses_df$Exterior.1st+houses_df$Exterior.2nd+houses_df$Mas.Vnr.Area+houses_df$Exter.Qual+houses_df$Foundation+houses_df$Bsmt.Qual
             +houses_df$Bsmt.Exposure+houses_df$BsmtFin.SF.1+houses_df$BsmtFin.SF.2+houses_df$Total.Bsmt.SF+houses_df$Heating+houses_df$Heating.QC
             +houses_df$Low.Qual.Fin.SF+houses_df$Gr.Liv.Area+houses_df$Bsmt.Full.Bath+houses_df$Full.Bath+houses_df$Half.Bath
             +houses_df$Bedroom.AbvGr+houses_df$Kitchen.AbvGr+houses_df$Kitchen.Qual+houses_df$Functional+houses_df$Fireplaces
             +houses_df$Garage.Cars+houses_df$Garage.Area+houses_df$Garage.Qual+houses_df$Garage.Cond
            +houses_df$Wood.Deck.SF+houses_df$Open.Porch.SF+houses_df$Screen.Porch+houses_df$Pool.QC
             +houses_df$Fence+houses_df$Misc.Feature+houses_df$Sale.Type+houses_df$Sale.Condition, data = houses_df)
summary(model2)
step(model2, direction = 'backward')
summary(step(model2, direction = 'backward'))

model3 <- lm(formula = houses_df$SalePrice ~ houses_df$Lot.Area + houses_df$Land.Contour + 
               houses_df$Neighborhood + houses_df$Condition.1 + houses_df$Condition.2 + 
               houses_df$Bldg.Type + houses_df$Overall.Qual + houses_df$Overall.Cond + 
               houses_df$Year.Built + houses_df$Year.Remod.Add + houses_df$Roof.Matl + 
               houses_df$Exterior.1st + houses_df$Mas.Vnr.Area + houses_df$Exter.Qual + 
               houses_df$Bsmt.Qual + houses_df$Bsmt.Exposure + houses_df$BsmtFin.SF.1 + 
               houses_df$BsmtFin.SF.2 + houses_df$Total.Bsmt.SF + houses_df$Low.Qual.Fin.SF + 
               houses_df$Gr.Liv.Area + houses_df$Bsmt.Full.Bath + houses_df$Full.Bath + 
               houses_df$Bedroom.AbvGr + houses_df$Kitchen.AbvGr + houses_df$Kitchen.Qual + 
               houses_df$Functional + houses_df$Fireplaces + houses_df$Garage.Cars + 
               houses_df$Garage.Area + houses_df$Garage.Qual + houses_df$Wood.Deck.SF + 
               houses_df$Open.Porch.SF + houses_df$Screen.Porch + houses_df$Pool.QC + 
               houses_df$Misc.Feature + houses_df$Sale.Type + houses_df$Sale.Condition, 
             data = houses_df)
summary(model3) ##neighborhood statistically insignificant except 7-8 levels, Condition1 st. insignificant except 'Norm' level
##condition2 st.insignificant except 'PosA' level, Bldg.Type significant, Overall.Qual. insignificant,Overall.Cond. significant at 4-5 levels
##Roof.matl significant, exterior1st significant at 2 levels,Exter.Qual signif. at Q level, Bsmt.Qual. insignificant,
##Bsmt.exposure insignificant, kitchen.Qual signif. at all levels,Functional signif. at 2-3 levels, Garage.Qual. insignif.
##Open.Porch.SF MARGINally insignif., Pool.QC insignif., Misc.Feature significant only at 1 level, Sale.Type signif. at 3 levels,
##Sale.Condition signif. at 3 levels
#step(model2, direction = 'both')
#summary(step(model2, direction = 'both'))
model4 <- lm(formula = houses_df$SalePrice ~ houses_df$Lot.Area + houses_df$Land.Contour + 
               houses_df$Neighborhood + houses_df$Condition.1 + houses_df$Condition.2 + 
               houses_df$Bldg.Type + houses_df$Overall.Cond + 
               houses_df$Year.Built + houses_df$Year.Remod.Add + houses_df$Roof.Matl + 
               houses_df$Exterior.1st + houses_df$Mas.Vnr.Area + houses_df$Exter.Qual  + houses_df$BsmtFin.SF.1 + 
               houses_df$BsmtFin.SF.2 + houses_df$Total.Bsmt.SF + houses_df$Low.Qual.Fin.SF + 
               houses_df$Gr.Liv.Area + houses_df$Bsmt.Full.Bath + houses_df$Full.Bath + 
               houses_df$Bedroom.AbvGr + houses_df$Kitchen.AbvGr + houses_df$Kitchen.Qual + 
               houses_df$Functional + houses_df$Fireplaces + houses_df$Garage.Cars + 
               houses_df$Garage.Area + houses_df$Wood.Deck.SF + 
               houses_df$Screen.Porch + houses_df$Sale.Type + houses_df$Sale.Condition, 
             data = houses_df)
summary(model4)
step(model4, direction = 'backward')
summary(step(model4, direction = 'backward'))

model5 <- lm(formula = houses_df$SalePrice ~ houses_df$Lot.Area + houses_df$Land.Contour + 
     houses_df$Neighborhood + houses_df$Bldg.Type + 
     houses_df$Overall.Cond + houses_df$Year.Built + houses_df$Year.Remod.Add + 
     houses_df$Roof.Matl + houses_df$Exterior.1st + houses_df$Mas.Vnr.Area + 
     houses_df$Exter.Qual + houses_df$BsmtFin.SF.1 + houses_df$BsmtFin.SF.2 + 
     houses_df$Total.Bsmt.SF + houses_df$Low.Qual.Fin.SF + houses_df$Gr.Liv.Area + 
     houses_df$Bsmt.Full.Bath + houses_df$Full.Bath + houses_df$Bedroom.AbvGr + 
     houses_df$Kitchen.AbvGr + houses_df$Kitchen.Qual + houses_df$Functional + 
     houses_df$Fireplaces + houses_df$Garage.Cars + houses_df$Garage.Area + 
     houses_df$Wood.Deck.SF + houses_df$Screen.Porch + houses_df$Sale.Type + 
     houses_df$Sale.Condition, data = houses_df)
summary(model5)



vifres <- vif(model5) #Checking for multicollinearity: Neighborhood, Bldg.Type, Exterior.1st, 
#Exter.Qual, Kitchen.Qual, Sale.Type,Sale.Condition may lead to a problem, 
#Exter.Qual, Kitchen.Qual have values close to 10 and the others have values way over 10
vifres

#removing Neighborhood Exterior.1st Sale.Type due to multicolinearity
model6 <- lm(formula = houses_df$SalePrice ~ houses_df$Lot.Area + houses_df$Land.Contour + houses_df$Bldg.Type + 
               houses_df$Overall.Cond + houses_df$Year.Built + houses_df$Year.Remod.Add + 
               houses_df$Roof.Matl + houses_df$Mas.Vnr.Area + 
               houses_df$Exter.Qual + houses_df$BsmtFin.SF.1 + houses_df$BsmtFin.SF.2 + 
               houses_df$Total.Bsmt.SF + houses_df$Low.Qual.Fin.SF + houses_df$Gr.Liv.Area + 
               houses_df$Bsmt.Full.Bath + houses_df$Full.Bath + houses_df$Bedroom.AbvGr + 
               houses_df$Kitchen.AbvGr + houses_df$Kitchen.Qual + houses_df$Functional + 
               houses_df$Fireplaces + houses_df$Garage.Cars + houses_df$Garage.Area + 
               houses_df$Wood.Deck.SF + houses_df$Screen.Porch + houses_df$Sale.Condition, data = houses_df)
summary(model6)
vifres_mod6 <- vif(model6)
vifres_mod6

#in model 6 garage cars, functional seem to be insignificant try to remove them
model7 <- lm(formula = houses_df$SalePrice ~ houses_df$Lot.Area + houses_df$Land.Contour + houses_df$Bldg.Type + 
               houses_df$Overall.Cond + houses_df$Year.Built + houses_df$Year.Remod.Add + 
               houses_df$Roof.Matl + houses_df$Mas.Vnr.Area + houses_df$Exter.Qual + houses_df$BsmtFin.SF.1 + 
               houses_df$BsmtFin.SF.2 + houses_df$Total.Bsmt.SF + houses_df$Low.Qual.Fin.SF + houses_df$Gr.Liv.Area + 
               houses_df$Bsmt.Full.Bath + houses_df$Full.Bath + houses_df$Bedroom.AbvGr + 
               houses_df$Kitchen.AbvGr + houses_df$Kitchen.Qual + houses_df$Fireplaces + houses_df$Garage.Area + 
               houses_df$Wood.Deck.SF + houses_df$Screen.Porch + houses_df$Sale.Condition, data = houses_df)
summary(model7) #perhaps kitchen quality is insignificant
vifres_mod7<-vif(model7)
vifres_mod7 #multicolinearity seems ok


L_mod7 <- model.matrix(model7)[,-1]
lasso_m7 <- glmnet(L_mod7, houses_df$SalePrice)
plot(lasso_m7, xvar = "lambda", label = T)
#Use cross validation to find a reasonable value for lambda 
lasso_mod7 <- cv.glmnet(L_mod7, houses_df$SalePrice, alpha = 1)
lasso_mod7$lambda
summary(lasso_mod7$lambda)
lasso_mod7$lambda.min
lasso_mod7$lambda.1se
plot(lasso_mod7)
coef(lasso_mod7, s = "lambda.min")
coef(lasso_mod7, s = "lambda.1se")

lasso_finm7 <- coef(lasso_mod7, s = "lambda.1se")
lasso_finm7

step(model7, direction = 'backward')
summary(step(model7, direction = 'backward'))

##model bsed on lasso with 1se
model8 <- lm(formula = houses_df$SalePrice ~ houses_df$Lot.Area + houses_df$Year.Built + houses_df$Year.Remod.Add + 
               houses_df$Roof.Matl + houses_df$Mas.Vnr.Area + houses_df$Exter.Qual + houses_df$BsmtFin.SF.1 + 
               houses_df$Total.Bsmt.SF + houses_df$Kitchen.Qual + houses_df$Gr.Liv.Area + houses_df$Fireplaces + houses_df$Garage.Area, data = houses_df)
summary(model8)
vif(model8)
#removing kitchen quality
model9 <- lm(formula = houses_df$SalePrice ~ houses_df$Lot.Area + houses_df$Year.Built + houses_df$Year.Remod.Add + 
                         houses_df$Roof.Matl + houses_df$Mas.Vnr.Area + houses_df$Exter.Qual + houses_df$BsmtFin.SF.1 + 
                         houses_df$Total.Bsmt.SF + houses_df$Gr.Liv.Area + houses_df$Fireplaces + houses_df$Garage.Area, data = houses_df)
summary(model9)
vif(model8)
step(model9, direction = 'backward')
summary(step(model9, direction = 'backward'))
##Checking assumptions for model 9
qqnorm(rstudent(model9));qqline(rstudent(model9)) ##normality rejected
residualPlot(model9, type='rstudent') ##linearity rejected
plot(fitted(model9),rstudent(model9)^2);abline(h=4,col=2,lty=2) ##homoscedasticity rejected
plot(rstudent(model9), type = "l")
lillie.test((rstudent(model9)))
shapiro.test((rstudent(model9)))
residualPlots(model9, plot=F)

yhat.quantiles_model9<-
  cut(fitted(model9), breaks=4)
leveneTest(rstudent(model9)~yhat.quantiles_model9) ##homoscedasticity rejected

## subsetting the dataset in order to build a model with centered to 0 covariates
## I want to do that because the intercept is statistically significant but it is negative(=-1.319e+06)
## and thus it doesn't have a reasonable interpretation

houses_df_num3 <- subset.data.frame(houses_df,select = c('Lot.Area', 'Year.Built','Year.Remod.Add',
                                                         'Mas.Vnr.Area','BsmtFin.SF.1','Total.Bsmt.SF',
                                                         'Gr.Liv.Area','Garage.Area','Wood.Deck.SF','Fireplaces'
                                                         ,'SalePrice'))



houses_df_numsubset <- as.data.frame(scale(houses_df_num3, center = TRUE, scale = F))

houses_df_numsubset$SalePrice<- houses_df$SalePrice
sapply(houses_df_numsubset,mean)
sapply(houses_df_numsubset,sd)
round(sapply(houses_df_numsubset,mean),6)
round(sapply(houses_df_numsubset,sd),2)

houses_df_numsubset$roof.matl <- houses_df$Roof.Matl
houses_df_numsubset$ext.qual <- houses_df$Exter.Qual
houses_df_numsubset$Bldg.Type <- houses_df$Bldg.Type
houses_df_numsubset$Kitchen.Qual <- houses_df$Kitchen.Qual


model9_centered_numfac2 <- lm(SalePrice~.,data =houses_df_numsubset) #after adding factors to the subset
summary(model9)
summary(model9_centered_numfac2)


plot(model9_numandfactors, which = 2)
qqnorm(rstudent(model9_centered_numfac2));qqline(rstudent(model9_centered_numfac2))


residualPlot(model8, type='rstudent')
residualPlot(model9_centered_numfac2, type='rstudent')

plot(model9_centered_numfac2, which = 3)
plot(fitted(model9_numandfactors),rstudent(model9_numandfactors)^2);abline(h=4,col=2,lty=2)
plot(fitted(model8),rstudent(model8)^2);abline(h=4,col=2,lty=2)
plot(fitted(log_centered_model),rstudent(log_centered_model)^2);abline(h=4,col=2,lty=2)



ncvTest(model9_centered_numfac2)
ncvTest(model_transformed)

plot(rstudent(model9), type = "l")
plot(rstudent(model8), type = "l")
plot(rstudent(model9_centered_numfac2), type = "l")

vif(model9_centered_numfac2)
outlierTest(model9_centered_numfac2)



par(mfrow=c(1,1))
plot(lm(log(SalePrice)~.-Lot.Area-Fireplaces-Mas.Vnr.Area,data=houses_df_numsubset),2, main='log of price')
plot(log_centered_model,2)

plot(lm(log(SalePrice)~houses_df$Lot.Area + houses_df$Year.Built + houses_df$Year.Remod.Add + 
         houses_df$Roof.Matl + houses_df$Mas.Vnr.Area + houses_df$Exter.Qual + houses_df$BsmtFin.SF.1 + 
         houses_df$Total.Bsmt.SF + houses_df$Gr.Liv.Area + houses_df$Fireplaces + houses_df$Garage.Area, data = houses_df), 2)


residualPlot(lm(log(SalePrice)~.,data=houses_df_numsubset), type='rstudent')
plot(log_centered_model, type="l")
log_centered_model <- lm(log(SalePrice)~.,data=houses_df_numsubset)
summary(log_centered_model)
plot( lm(log(SalePrice)~.,data=houses_df_numsubset), 2)
plot(log_centered_model,2)
plot(log_centered_model,3)
ncvTest(log_centered_model)
residualPlot(log_centered_model,type='rstudent')
outlierTest(log_centered_model)





#searching for leverage points
hatvalues(log_centered_model)
lev3<-hatvalues(log_centered_model)
plot(lev3, ylim=range(c(0,3.5*2/10, lev3)), main='Data with leverage')
abline( h=c(2,3)*2/10, col=2:3, lty=2:3 )

##subtracting the leverage points
levpoints <- c(1458,895,1397,1023)
houses_df_no_leverage <- houses_df_numsubset[-c(1449,1390,894),]
houses_df_no_leverage <- houses_df_numsubset[-c(1449,1390,894),]

which(houses_df_numsubset$SalePrice==13100)
which(houses_df_numsubset$SalePrice==277000)
which(houses_df_numsubset$SalePrice==292500)
which(houses_df_numsubset$SalePrice==34900)
which(houses_df_numsubset$SalePrice==375000)


model_no_leverage <- lm(log(SalePrice) ~ .-SalePrice_transformed, data = houses_df_no_leverage)
summary(model_no_leverage)
qqnorm(rstudent(model_no_leverage));qqline(rstudent(model_no_leverage))
plot(model_no_leverage,4)
outlierTest(model_no_leverage)

plot(log_centered_model,4)
plot(log_centered_model,5)

critical.value <- 4/(nrow(houses_df_numsubset)-length(log_centered_model$coef))



residualPlot(lm(log(SalePrice)~.-SalePrice_transformed-Year.Remod.Add-Fireplaces-ext.qual-Total.Bsmt.SF+poly(Lot.Area,2)
   +poly(Gr.Liv.Area,2)+poly(Garage.Area,2),data = houses_df_numsubset), type='rstudent')


plot(rstudent(log_centered_model), type='l')
plot(rstudent(log_pol_model), type='l')
plot(rstudent(model_no_leverage), type = 'l') 
plot(rstudent(lm(log(SalePrice)~.-Year.Remod.Add-Fireplaces-ext.qual+poly(Lot.Area,3)
                 +poly(Gr.Liv.Area,3)+poly(Garage.Area,3),data=houses_df_numsubset)), type = 'l')


plot(fitted(log_centered_model),rstudent(log_centered_model)^2);abline(h=4,col=2,lty=2)
plot(fitted(log_pol_model),rstudent(log_pol_model)^2);abline(h=4,col=2,lty=2)
plot(fitted(model_no_leverage),rstudent(model_no_leverage)^2);abline(h=4,col=2,lty=2)
plot(fitted(lm(log(SalePrice)~.-Year.Remod.Add-Fireplaces-ext.qual+poly(Lot.Area,3)
               +poly(Gr.Liv.Area,3)+poly(Garage.Area,3),data=houses_df_numsubset)),rstudent(lm(log(SalePrice)~.-Year.Remod.Add-Fireplaces-ext.qual+poly(Lot.Area,3)+poly(Gr.Liv.Area,3)+poly(Garage.Area,3),data=houses_df_numsubset))^2);abline(h=4,col=2,lty=2)




ncvTest(lm(log(SalePrice)~.-SalePrice_transformed-Year.Remod.Add-Fireplaces-ext.qual-Total.Bsmt.SF+poly(Lot.Area,2)
           +poly(Gr.Liv.Area,2)+poly(Garage.Area,2),data=houses_df_numsubset))
ncvTest(lm(log(SalePrice)~.-SalePrice_transformed-Year.Remod.Add-Fireplaces-ext.qual-Total.Bsmt.SF+poly(Lot.Area,2)
           +poly(Gr.Liv.Area,2)+poly(Garage.Area,2),data=houses_df_numsubset))


plot(log_pol_model, 3)
plot(model_no_leverage,3)
plot(lm(log(SalePrice)~.-Year.Remod.Add-Fireplaces-ext.qual+poly(Lot.Area,3)
        +poly(Gr.Liv.Area,3)+poly(Garage.Area,3),data=houses_df_numsubset),3)



houses_df_numsubset<-houses_df_numsubset[,-14]



##Trying log and polynomial transform together

robust_model1 <- lm(log(SalePrice)~1/Year.Built+poly(Lot.Area,2)
                    +poly(Gr.Liv.Area,2)+poly(Garage.Area,2)+roof.matl+BsmtFin.SF.1+1/Total.Bsmt.SF+1/Wood.Deck.SF,data=houses_df_numsubset)


robust_model2 <- lm(log(SalePrice)~log(Year.Built)+poly(Lot.Area,2)
                    +poly(Gr.Liv.Area,2)+poly(Garage.Area,2)+roof.matl+log(BsmtFin.SF.1)+1/Total.Bsmt.SF+1/Wood.Deck.SF,data=houses_df_numsubset)

rm3 <- lm(log(SalePrice)~log(Year.Built)+poly(Lot.Area,2)
                           +poly(Gr.Liv.Area,2)+poly(Garage.Area,2)+1/roof.matl+log(BsmtFin.SF.1)+1/Total.Bsmt.SF+1/Wood.Deck.SF,data=houses_df_numsubset)

rm4 <- lm(log(SalePrice)~log(Year.Built)+poly(Lot.Area,8)
          +log(Gr.Liv.Area)+poly(Garage.Area,8)+1/roof.matl+log(BsmtFin.SF.1)+1/Total.Bsmt.SF+1/Wood.Deck.SF,data=houses_df_numsubset)

rm5 <- lm(log(SalePrice)~log(Year.Built)+poly(Lot.Area,10)
          +poly(Garage.Area,10)+poly(roof.matl,5)+log(BsmtFin.SF.1)+poly(Total.Bsmt.SF,8)+poly(Wood.Deck.SF,8),data=houses_df_numsubset)

rm6 <- lm(log(SalePrice)~log(Year.Built)+poly(Lot.Area,6)
          +poly(Garage.Area,5)+poly(roof.matl,4)+log(BsmtFin.SF.1)+poly(Total.Bsmt.SF,5)+poly(Wood.Deck.SF,4),data=houses_df_numsubset)

rm7 <- lm(log(SalePrice)~log(Year.Built)+Lot.Area +poly(Lot.Area,6)+Garage.Area+poly(Garage.Area,4)+roof.matl+log(BsmtFin.SF.1)
          +Total.Bsmt.SF+poly(Total.Bsmt.SF,6)+Wood.Deck.SF, data=houses_df_numsubset)

rm7_signif <- lm(log(SalePrice)~log(Year.Built)+Lot.Area +poly(Lot.Area,6)+Garage.Area+BsmtFin.SF.1
                 +Total.Bsmt.SF+poly(Total.Bsmt.SF,6)+Wood.Deck.SF, data=houses_df_numsubset)


vif(lm(log(SalePrice)~log(Year.Built)+Lot.Area +Garage.Area+roof.matl+log(BsmtFin.SF.1)
       +Total.Bsmt.SF+Wood.Deck.SF, data=houses_df_numsubset))
f2 <- lm(log(SalePrice,base = 10)~Year.Built +Garage.Area+poly(Garage.Area,3)+Exter.Qual+ ##FINAL model
         +log(Gr.Liv.Area,base = 10), data=houses_df)

summary(f2)
levels(houses_df$Exter.Qual)

f3 <- lm(log(SalePrice)~log(Year.Remod.Add+1) +poly(Lot.Area,3)+log(Fireplaces+1)
           +log(Gr.Liv.Area+1)+Wood.Deck.SF+Kitch, data=houses_df)

summary(f2)
residualPlot(f2, type='rstudent')
qqnorm(rstudent(f2));qqline(rstudent(f2))
qqnorm(rstudent(f3));qqline(rstudent(f3))
lillie.test(rstudent(f2))

plot(fitted(robust_model2),rstudent(robust_model2)^2);abline(h=4,col=2,lty=2)
plot(fitted(rm4),rstudent(rm4)^2);abline(h=4,col=2,lty=2)
plot(fitted(rm4),rstudent(rm4)^2);abline(h=4,col=2,lty=2)
ncvTest(rm4) ## constant variance assumption satisfied
plot(fitted(rm5),rstudent(rm5)^2);abline(h=4,col=2,lty=2)
plot(fitted(rm5),rstudent(rm5)^2);abline(h=4,col=2,lty=2)
plot(fitted(rm7),rstudent(rm7)^2);abline(h=4,col=2,lty=2) ##homoscedasticity satisfied
plot(fitted(rm7_signif),rstudent(rm7_signif)^2);abline(h=4,col=2,lty=2) ##homoscedasticity satisfied
plot(fitted(f2),rstudent(f2)^2);abline(h=4,col=2,lty=2) ##homoscedasticity satisfied

yhat.quantiles<-
  cut(fitted(f2), breaks=4);
yhat.quantiles2<-
  cut(fitted(rm7_signif), breaks=4)


leveneTest(rstudent(f2)~yhat.quantiles)## reject the null hypothesis for homoscedasticity
leveneTest(rstudent(rm7_signif)~yhat.quantiles2)
leveneTest(rstudent(final_model)~yhat.quantiles3)

ncvTest(final_model)
ncvTest(rm7) ##do not reject the null hypothesis for homoscedasticity
ncvTest(rm7_signif) ##do not reject the null hypothesis for homoscedasticity
ncvTest(f2) ## reject the null hypothesis for homoscedasticity


qqnorm(rstudent(rm4));qqline(rstudent(rm4))
qqnorm(rstudent(rm5));qqline(rstudent(rm5))
qqnorm(rstudent(rm6));qqline(rstudent(rm6))
qqnorm(rstudent(rm7));qqline(rstudent(rm7))
qqnorm(rstudent(rm7_signif));qqline(rstudent(rm7_signif))
qqnorm(rstudent(f2));qqline(rstudent(f2)) ##reject normality


residualPlot(rm5, type='rstudent')
residualPlot(rm6, type='rstudent')
residualPlot(rm7, type='rstudent')
residualPlot(rm7_signif, type='rstudent')
residualPlot(final_model, type='rstudent')
residualPlot(f2, type='rstudent', main="Linearity for the transformed model") ##linearity satisfied



residualPlots(rm7, plot=F, conf.level = 0.95) ##linearity ok

residualPlots(f2, plot=F)

qqnorm(rstudent(robust_model1));qqline(rstudent(robust_model1))
lillie.test(rstudent(robust_model2))
lillie.test(rstudent(rm3))
lillie.test(rstudent(rm5))
lillie.test(rstudent(rm6))
lillie.test(rstudent(rm7_signif))##  normality assumption satisfied
lillie.test(rstudent(final_model))
lillie.test(rstudent(f2))


summary(rm5)
summary(rm6)
summary(rm7)
summary(rm7_signif)
summary(final_model)




##Descriptive for numeric variables
options(scipen = 999)
hist(houses_df$SalePrice, col = 'lightblue', main ='Histogram of Sale Price', xlab="SalePrice", border = 'black', labels = TRUE)
mean(houses_df$SalePrice);median(houses_df$SalePrice)
summary(houses_df$SalePrice)
skewness(houses_df$SalePrice);kurtosis(houses_df$SalePrice)

hist(houses_df$Lot.Area, main ='Lot Size in Square Feet', xlab="Lot Size", border = 'black', labels = TRUE,col = 'skyblue')
mean(houses_df$Lot.Area);median(houses_df$Lot.Area)

hist(houses_df$Year.Built, main ='Year of Construction Histogram', xlab="Year Built", border = 'black', labels = TRUE,col = 'skyblue')
median(houses_df$Year.Built);mean(houses_df$Year.Built)
summary(houses_df$Year.Built)
hist(houses_df$Year.Remod.Add)
median(houses_df$Year.Remod.Add);mean(houses_df$Year.Remod.Add)
summary(houses_df$Year.Remod.Add)

hist(houses_df$Garage.Cars)
mean(houses_df$Garage.Cars);median(houses_df$Garage.Cars)
hist(houses_df$Garage.Area, main ='Garage Area Histogram', xlab="Garage Area", border = 'black', col = 'skyblue')
mean(houses_df$Garage.Area);median(houses_df$Garage.Area)

hist(houses_df$Gr.Liv.Area, main ='Above ground living area in square feet', xlab="Gr.Liv.Area", border = 'black', col = 'skyblue')
summary(houses_df$Gr.Liv.Area)

hist(houses_df$Fireplaces)
hist(houses_df_numsubset$Fireplaces)
hist(houses_df$Wood.Deck.SF)
hist(houses_df_numsubset$Wood.Deck.SF)
hist(houses_df$BsmtFin.SF.1)
hist(houses_df_numsubset$BsmtFin.SF.1)
hist(houses_df$Total.Bsmt.SF)
hist(houses_df_numsubset$Total.Bsmt.SF)



hist(houses_df$Low.Qual.Fin.SF)
summary(houses_df$Low.Qual.Fin.SF)
hist((houses_df$X1st.Flr.SF))
summary(houses_df$X1st.Flr.SF)
hist(houses_df$X2nd.Flr.SF)
summary(houses_df$X2nd.Flr.SF)


n <-nrow(houses_df)
table(houses_df$Roof.Matl)
barplot(table(houses_df$Kitchen.Qual)/n, horiz=T, las=1, ylim=c(0,8), cex.names=1.3,col = 1:5, main = 'Kitchen Quality bar plot')
barplot(table(houses_df$Overall.Qual)/n, horiz=T, las=1, ylim=c(0,12), cex.names=1.3, col = 1:10)
barplot(table(houses_df$Exter.Qual)/n, horiz=T, las=1, ylim=c(0,8), cex.names=1.3,col = 1:5, main = 'External Quality bar plot') 

boxplot(houses_df$SalePrice~houses_df$Kitchen.Qual, ylab = "Sale Price", xlab = "Kitchen Quality", main="Price-Kitchen Quality Boxplot", col="lightgreen")
boxplot(houses_df$SalePrice~houses_df$Exter.Qual,  ylab = "Sale Price", xlab = "Exterior Quality", main="Price-Exterior Quality Boxplot", col="lightgreen")
boxplot(houses_df$SalePrice~houses_df$Roof.Matl)



shapiro.test(aov(houses_df$SalePrice~houses_df$Utilities)$res)
shapiro.test(aov(houses_df$SalePrice~houses_df$Kitchen.Qual)$res)
shapiro.test(aov(houses_df$SalePrice~houses_df$Exter.Qual)$res)
shapiro.test(aov(houses_df$SalePrice~houses_df$Exter.Cond)$res)


lillie.test(aov(houses_df$SalePrice~houses_df$Utilities)$res)
lillie.test(aov(houses_df$SalePrice~houses_df$Kitchen.Qual)$res)
lillie.test(aov(houses_df$SalePrice~houses_df$Exter.Qual)$res)
lillie.test(aov(houses_df$SalePrice~houses_df$Exter.Cond)$res)

kruskal.test(houses_df$SalePrice~houses_df$Kitchen.Qual)
TukeyHSD(aov(houses_df$SalePrice~houses_df$Kitchen.Qual))


kruskal.test(houses_df$SalePrice~houses_df$Exter.Qual)
TukeyHSD(aov(houses_df$SalePrice~houses_df$Exter.Qual))
##Cleaning the TEST Dataset
columns_with_na_TESTDF <- colSums(is.na(TEST_DF)) > 0
print(names(columns_with_na_TESTDF)[columns_with_na_TESTDF])
sum(is.na(TEST_DF)==TRUE)
TEST_DF <- TEST_DF[,c(-1,-2,-3)]
TEST_DF$MS.SubClass <- factor(TEST_DF$MS.SubClass)
TEST_DF$MS.Zoning <- as.factor(TEST_DF$MS.Zoning)
missing_rowstest_lotfront <- is.na(TEST_DF$Lot.Frontage)
TEST_DF$Lot.Frontage[missing_rowstest_lotfront] <- round(TEST_DF$Lot.Area[missing_rowstest_lotfront] * 0.009)

TEST_DF$Street <- factor(TEST_DF$Street)
TEST_DF$Alley[TEST_DF$Alley %in% c(NA)] <- "NA"
TEST_DF$Alley <- factor(TEST_DF$Alley)

TEST_DF$Lot.Shape <- factor(TEST_DF$Lot.Shape, levels = c('IR3','IR2','IR1','Reg'), ordered = TRUE)
levels(TEST_DF$Lot.Shape)
is.ordered(TEST_DF$Utilities)
TEST_DF$Land.Contour <- as.factor(TEST_DF$Land.Contour)
TEST_DF$Utilities <- factor(TEST_DF$Utilities, levels = c('NoSeWa','NoSewr','AllPub'), ordered=TRUE)
TEST_DF$Lot.Config <- as.factor(TEST_DF$Lot.Config)
TEST_DF$Neighborhood <- factor(TEST_DF$Neighborhood, levels = c("Blmngtn", "Blueste", "BrDale",  "BrkSide", "ClearCr", "CollgCr", "Crawfor", "Edwards", "Gilbert", "Greens", 
                                                                    "GrnHill", "IDOTRR", "Landmrk", "MeadowV", "Mitchel", "NAmes",  "NoRidge", "NPkVill", "NridgHt", "NWAmes",  "OldTown",
                                                                    "Sawyer",  "SawyerW", "Somerst", "StoneBr", "SWISU",  "Timber",  "Veenker"))

TEST_DF$Condition.1 <- as.factor(TEST_DF$Condition.1)
TEST_DF$Condition.2 <- as.factor(TEST_DF$Condition.2)
levels(houses_df$Condition.2)
TEST_DF$Bldg.Type <- as.factor(TEST_DF$Bldg.Type)
TEST_DF$House.Style <- as.factor(TEST_DF$House.Style)

TEST_DF$Land.Slope <- factor(TEST_DF$Land.Slope, levels = c('Gtl','Mod','Sev'), ordered = TRUE)
TEST_DF$Overall.Qual <- factor(TEST_DF$Overall.Qual, levels = c(1:10), ordered = TRUE)
TEST_DF$Overall.Cond <- factor(TEST_DF$Overall.Cond, levels = c(1:10), ordered = TRUE)
TEST_DF$Roof.Style <- factor(TEST_DF$Roof.Style)
TEST_DF$Roof.Matl <- factor(TEST_DF$Roof.Matl, levels = c('NA','ClyTile','CompShg','Metal','Roll','Tar&Grv','WdShake','WdShgnl'))
TEST_DF$Exterior.1st <- factor(TEST_DF$Exterior.1st, levels = c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock",  "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco","VinylSd", "Wd Sdng", "WdShing"))
TEST_DF$Exterior.2nd <- factor(TEST_DF$Exterior.2nd, levels = c("None","AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock",  "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco","VinylSd", "Wd Sdng", "WdShing"))

TEST_DF$Mas.Vnr.Type <- factor(TEST_DF$Mas.Vnr.Type, levels = c("BrkCmn",  "BrkFace", "CBlock", "None", "Stone" ))
TEST_DF$Exter.Qual <-factor(TEST_DF$Exter.Qual, levels = c('Po', 'Fa', 'TA', 'Gd', 'Ex'), ordered = TRUE)
TEST_DF$Exter.Cond <-factor(TEST_DF$Exter.Cond, levels = c('Po', 'Fa', 'TA', 'Gd', 'Ex'), ordered = TRUE)
TEST_DF$Foundation <- factor(TEST_DF$Foundation)

TEST_DF$Bsmt.Qual[TEST_DF$Bsmt.Qual %in% c(NA)] <- "NA"
TEST_DF$Bsmt.Cond[TEST_DF$Bsmt.Cond %in% c(NA)] <- "NA"
TEST_DF$Bsmt.Exposure[TEST_DF$Bsmt.Exposure %in% c(NA)] <- "NA"
TEST_DF$BsmtFin.Type.1[TEST_DF$BsmtFin.Type.1 %in% c(NA)] <- "NA"
TEST_DF$BsmtFin.Type.2[TEST_DF$BsmtFin.Type.2%in% c(NA)] <- "NA"

TEST_DF$Bsmt.Cond <- factor(TEST_DF$Bsmt.Cond, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
TEST_DF$Bsmt.Qual<- factor(TEST_DF$Bsmt.Qual, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
levels(houses_df$Bsmt.Cond)
TEST_DF$Bsmt.Exposure <- factor(TEST_DF$Bsmt.Exposure, levels = c('NA','No','Mn','Av','Gd'),ordered = TRUE)
TEST_DF$BsmtFin.Type.1 <- factor(TEST_DF$BsmtFin.Type.1, levels = c('NA','Unf','LwQ','Rec','BLQ','ALQ','GLQ'),ordered = TRUE)
TEST_DF$BsmtFin.Type.2 <- factor(TEST_DF$BsmtFin.Type.2, levels = c('NA','Unf','LwQ','Rec','BLQ','ALQ','GLQ'),ordered = TRUE)


TEST_DF$Heating <-factor(TEST_DF$Heating)
levels(houses_df$Heating)
TEST_DF$Heating.QC <- factor(TEST_DF$Heating.QC, levels = c("Po","Fa", "TA","Gd","Ex"), ordered = TRUE)
TEST_DF$Central.Air<-factor(TEST_DF$Central.Air)


TEST_DF$Electrical <- factor(TEST_DF$Electrical, levels = c("Mix", "FuseP","FuseF","FuseA", "SBrkr"),ordered = TRUE)
TEST_DF$Kitchen.Qual<-factor(TEST_DF$Kitchen.Qual, levels = c("Po","Fa", "TA","Gd","Ex"), ordered = TRUE)
TEST_DF$Functional <- factor(TEST_DF$Functional, levels = c("Sal","Sev","Maj2", "Maj1","Mod", "Min2", "Min1","Typ"),ordered=TRUE)

TEST_DF$Fireplace.Qu[TEST_DF$Fireplace.Qu%in% c(NA)] <- "NA"
TEST_DF$Garage.Type[TEST_DF$Garage.Type%in% c(NA)] <- "NA"
TEST_DF$Garage.Finish[TEST_DF$Garage.Finish%in% c(NA)] <- "NA"
TEST_DF$Garage.Qual[TEST_DF$Garage.Qual%in% c(NA)] <- "NA"
TEST_DF$Garage.Cond[TEST_DF$Garage.Cond%in% c(NA)] <- "NA"
TEST_DF$Pool.QC[TEST_DF$Pool.QC%in% c(NA)] <- "NA"
TEST_DF$Fence[TEST_DF$Fence%in% c(NA)] <- "NA"
TEST_DF$Misc.Feature[TEST_DF$Misc.Feature%in% c(NA)] <- "NA"


TEST_DF$Fireplace.Qu <- factor(TEST_DF$Fireplace.Qu, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
TEST_DF$Garage.Type <- factor(TEST_DF$Garage.Type)
TEST_DF$Garage.Finish<-factor(TEST_DF$Garage.Finish, levels = c( "NA","Unf","RFn","Fin"), ordered = TRUE)
TEST_DF$Garage.Qual<-factor(TEST_DF$Garage.Qual, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
TEST_DF$Garage.Cond<-factor(TEST_DF$Garage.Cond, levels =c('NA','Po', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
TEST_DF$Paved.Drive<-factor(TEST_DF$Paved.Drive, ordered = TRUE)
TEST_DF$Pool.QC<- factor(TEST_DF$Pool.QC, levels =c('NA', 'Fa', 'TA', 'Gd', 'Ex'),ordered = TRUE)
TEST_DF$Fence <- factor(TEST_DF$Fence, levels=c("NA","MnWw","GdWo","MnPrv","GdPrv"),ordered = TRUE)
table(TEST_DF$Misc.Feature)
TEST_DF$Misc.Feature<-factor(TEST_DF$Misc.Feature,levels=c("NA","TenC","Shed","Othr","Gar2","Elev"))
TEST_DF$Sale.Type<-factor(TEST_DF$Sale.Type)
TEST_DF$Sale.Condition<-factor(TEST_DF$Sale.Condition)
levels(houses_df$Sale.Condition)

testmissing_rows_grg_year <- is.na(TEST_DF$Garage.Yr.Blt)
TEST_DF$Garage.Yr.Blt[testmissing_rows_grg_year] <- TEST_DF$Year.Built[testmissing_rows_grg_year]
sum(is.na(TEST_DF$Garage.Yr.Blt))

testmissing_rows_mas.vnr.area <- is.na(TEST_DF$Mas.Vnr.Area)
TEST_DF$Mas.Vnr.Area[testmissing_rows_mas.vnr.area] <- 0

testmissing_rows_vnr.type <- is.na(TEST_DF$Mas.Vnr.Type)
TEST_DF$Mas.Vnr.Type[testmissing_rows_vnr.type] <- "None"
table(TEST_DF$Mas.Vnr.Type, useNA = "always")
TEST_DF$Roof.Matl[TEST_DF$Roof.Matl%in% c(NA)] <- "NA"
table(TEST_DF$Exterior.2nd,useNA = "always")
TEST_DF$Exterior.2nd[TEST_DF$Exterior.2nd%in% c(NA)] <- "None"


##Building subset with centered covariates also for the test dataset

houses_df_num_test <- subset.data.frame(TEST_DF,select = c('Lot.Area', 'Year.Built','Year.Remod.Add',
                                                         'Mas.Vnr.Area','BsmtFin.SF.1','Total.Bsmt.SF',
                                                         'Gr.Liv.Area','Garage.Area','Wood.Deck.SF','Fireplaces'
                                                         ,'SalePrice'))


houses_df_numcentered_test <- as.data.frame(scale(houses_df_num_test, center = TRUE, scale = F))

houses_df_numcentered_test$SalePrice<- TEST_DF$SalePrice
sapply(houses_df_numcentered_test,mean)
sapply(houses_df_numcentered_test,sd)
round(sapply(houses_df_numcentered_test,mean),6)
round(sapply(houses_df_numcentered_test,sd),2)

houses_df_numcentered_test$roof.matl <- TEST_DF$Roof.Matl
houses_df_numcentered_test$ext.qual <- TEST_DF$Exter.Qual
houses_df_numcentered_test$Bldg.Type <- TEST_DF$Bldg.Type
houses_df_numcentered_test$Kitchen.Qual <- TEST_DF$Kitchen.Qual

#calculating RMSE
houses_df_numsubset$roof.matl <- factor(houses_df_numsubset$roof.matl, levels = levels(final_model$roof.matl))

train_predictions <- predict(f2,houses_df)
res <- log(houses_df$SalePrice)-train_predictions
rmse <- sqrt(mean(res^2))
rmse
cvrmse <- rmse/mean(log(houses_df$SalePrice))
cvrmse
test_predictions <-predict(f2,TEST_DF)
res1 <- log(TEST_DF$SalePrice)-test_predictions
rmse1 <- sqrt(mean(res1^2))
rmse1
cvrmse1 <- rmse1/mean(log(TEST_DF$SalePrice))
cvrmse1

 summary(lm(log(SalePrice,base = 10)~log(Year.Built, base=10)+log(Lot.Area, base = 10) +Garage.Area+poly(Garage.Area,6)+Exter.Qual+Fireplaces+poly(Fireplaces,3)
         +log(Gr.Liv.Area,base = 10), data=TEST_DF))
 

 ##leave-one-out method
 
cross_model <- cv.glmnet(L1, houses_df$SalePrice,alpha=1, nfolds = nrow(houses_df))
coef(cross_model,s="lambda.min") 
loocv_model <- lm(log(SalePrice)~MS.SubClass+Lot.Area+MS.Zoning+Utilities+Land.Slope+Neighborhood+Condition.1+Bldg.Type+Condition.2+Overall.Qual+Overall.Cond+Year.Built+Year.Remod.Add+Roof.Style+Roof.Matl+Exterior.1st+Mas.Vnr.Type+Mas.Vnr.Area+Exter.Qual+Bsmt.Qual+Bsmt.Exposure+BsmtFin.SF.1+BsmtFin.SF.2+Total.Bsmt.SF+Heating+Low.Qual.Fin.SF+Gr.Liv.Area+Bsmt.Full.Bath+Full.Bath+Bedroom.AbvGr+Kitchen.AbvGr+Kitchen.Qual+Functional+Fireplace.Qu+Fireplaces+Garage.Cars+Garage.Area+Garage.Cond+Wood.Deck.SF+Open.Porch.SF+Screen.Porch+Pool.QC+Sale.Type+Sale.Condition, data = houses_df)
summary(loocv_model)
qqnorm(rstudent(loocv_model));qqline(rstudent(loocv_model)) ##normality is worse comparing to my model
residualPlot(loocv_model, type='rstudent') ##linearity is not satisfied unlike my model
loocv_train_predictions <- predict(loocv_model,houses_df)
loocv_res <- log(houses_df$SalePrice)-loocv_train_predictions
loocv_rmse <- sqrt(mean(loocv_res^2))
loocv_rmse ##worse rmse on the training dataset
loocv_cvrmse <- loocv_rmse/mean(log(houses_df$SalePrice))
loocv_cvrmse

loocv_test_predictions <-predict(loocv_model,TEST_DF)
res1 <- log(TEST_DF$SalePrice)-test_predictions
rmse1 <- sqrt(mean(res1^2))
loocv_rmse
cvrmse1 <- rmse1/mean(log(TEST_DF$SalePrice))
cvrmse1


step(f2, direction = 'backward')
summary(step(f2, direction = 'backward'))

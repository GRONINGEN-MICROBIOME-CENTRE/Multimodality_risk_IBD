library(dplyr)
library(readxl)

#### create LLD diet file ----
    # sec_c_ffq_selrename was used as input, already made previously by selecting and rename variables (no script available)
diet = read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/sec_c_ffq_selrename.csv") 
    # select LLD participants
LLD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLD_casecontrol.csv")
LLDdiet=merge(diet[,-1],LLD[,-1], by="project_pseudo_id")
LLDdiet=na.omit(LLDdiet)

write.csv(LLDdiet, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDdiet_june5.csv")

#### create 1000IBD diet file and merge with LLD ----
    # load in
UMCGdiet=read_xlsx("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/Diet_Final_Metadata_IBD_V46_clean.xlsx")
UMCGdiet=na.omit(UMCGdiet)
    # load in and select columns LLD
LLDdiet=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDdiet_june5.csv")
LLDdiet=LLDdiet[,-c(1,60)]

    # select same columns as LLD for UMCG
names(LLDdiet)[names(LLDdiet) == "project_pseudo_id"] <- "ID"    
names(UMCGdiet)[names(UMCGdiet) == "UMCGIBDResearchID"] <- "ID"    
UMCGdiet$IBD <- 1
list=colnames(LLDdiet)
UMCGdiet=UMCGdiet[ , names(UMCGdiet) %in% list]

    # change variable to numeric
vars=colnames(UMCGdiet[,-c(1,59)])
UMCGdiet=UMCGdiet %>% 
  mutate_at(vars(vars), as.numeric)

    # make order of columns same and rbind
UMCGdiet=UMCGdiet[,order(colnames(UMCGdiet))]
LLDdiet=LLDdiet[,order(colnames(LLDdiet))]

UMCGLLDdiet=rbind(UMCGdiet, LLDdiet)

    # filter SUMOFKCAL <800, >5000 or NA
UMCGLLDdiet=subset(UMCGLLDdiet,UMCGLLDdiet$SUMOFKCAL>=800 & UMCGLLDdiet$SUMOFKCAL<=5000 & UMCGLLDdiet$SUMOFKCAL != "NA")

write.csv(UMCGLLDdiet, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/UMCGLLDdiet.csv")

#### calculate MDS ----
    # read df
UMCGLLDdiet=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/UMCGLLDdiet.csv")

    #1 Vegetables, "group_vegetables" includes; "vegetables_cooked_nobutter, vegetables_stirfried, vegetables_cooked_butter"
UMCGLLDdiet$veg.total <- UMCGLLDdiet$vegetables_cooked_nobutter + UMCGLLDdiet$vegetables_stirfried + UMCGLLDdiet$vegetables_cooked_butter
x<-median(UMCGLLDdiet$veg.total)          
UMCGLLDdiet$veg.points <- ifelse(UMCGLLDdiet$veg.total>= x, 1, 0)

    #2 Legumes, only 1 column legumes in database; "legumes" 
UMCGLLDdiet$leg.total<- UMCGLLDdiet$legumes
x<- median(UMCGLLDdiet$leg.total)
UMCGLLDdiet$leg.points <- ifelse(UMCGLLDdiet$leg.total>= x, 1, 0)

    #3 Fruits, "fruitjuice","fruit" 
    # don't use the already existing "group_fruits" -> contains applesauce. 
    # consider/ compare outcomes when leaving out fruitjuice as this contains a lot of sugar. Original score includes fruit juice though 
UMCGLLDdiet$fruit.total<-UMCGLLDdiet$fruit+UMCGLLDdiet$fruitjuice  
x<-median(UMCGLLDdiet$fruit.total)  
UMCGLLDdiet$fruit.points<- ifelse(UMCGLLDdiet$fruit.total>= x, 1, 0)

    #4 Nuts 
    #nut.total<-UMCGLLDdiet$group_nuts
UMCGLLDdiet$nut.total <- UMCGLLDdiet$nut_d + UMCGLLDdiet$snack_nut + UMCGLLDdiet$peanutbutter
x<-median(UMCGLLDdiet$nut.total)
UMCGLLDdiet$nut.points<-ifelse(UMCGLLDdiet$nut.total>= x, 1, 0)

    #5 Whole grains 
UMCGLLDdiet$whgrains.total<-UMCGLLDdiet$bread
x<-median(UMCGLLDdiet$whgrains.total)
UMCGLLDdiet$whgrains.points<-ifelse(UMCGLLDdiet$whgrains.total>= x, 1, 0)

    #6 fish, group_fish contains herring_salted, fish_white_fried, fish_lean, fish_fatty, fish_other, fish_prepared_fat
UMCGLLDdiet$fish.total <- UMCGLLDdiet$herring_salted + UMCGLLDdiet$fish_white_fried + UMCGLLDdiet$fish_lean + UMCGLLDdiet$fish_fatty + UMCGLLDdiet$fish_other + UMCGLLDdiet$fish_prepared_fat
x<-median(UMCGLLDdiet$fish.total)
UMCGLLDdiet$fish.points<-ifelse(UMCGLLDdiet$fish.total>= x, 1, 0)

    #7 red and processed meats; containing: sausage_smoked, meats_other_cold_cuts, meat, beef_lean, beef_fat
    #pork_lean, pork_fat, pork_processed, meats_fat, snack_meats
UMCGLLDdiet$meat.total<- UMCGLLDdiet$sausage_smoked + UMCGLLDdiet$meat + UMCGLLDdiet$meats_fat+ UMCGLLDdiet$meats_other+ UMCGLLDdiet$beef_fat+ UMCGLLDdiet$beef_lean + UMCGLLDdiet$pork_fat + UMCGLLDdiet$pork_lean + UMCGLLDdiet$pork_processed + UMCGLLDdiet$snack_meats
x<-median(UMCGLLDdiet$meat.total)
UMCGLLDdiet$meat.points<-ifelse(UMCGLLDdiet$meat.total>= x, 0, 1)

    #8 alcohol; group_alcohol consist of beer, wine_red, wine_white, wine_fort, spirits, other_alc_drinks
    #  1 point if 10-20 g/d (1-2 glasses a day),  1 glas alcohol is 10 g / day 
UMCGLLDdiet$group_alcohol <- UMCGLLDdiet$beer + UMCGLLDdiet$wine_red + UMCGLLDdiet$wine_white + UMCGLLDdiet$wine_fort + UMCGLLDdiet$spirits + UMCGLLDdiet$other_alc_drinks
UMCGLLDdiet$alcohol.points<-ifelse((UMCGLLDdiet$group_alcohol >=10 & UMCGLLDdiet$group_alcohol <= 20), 1, 0 ) 

    #9 M/S ratio no data 

    #add the points together [max amount of 8 points, min amount of 0 points]
UMCGLLDdiet$aMED <- UMCGLLDdiet$veg.points + UMCGLLDdiet$leg.points + UMCGLLDdiet$fruit.points + UMCGLLDdiet$nut.points + 
  UMCGLLDdiet$whgrains.points + UMCGLLDdiet$fish.points + UMCGLLDdiet$meat.points + UMCGLLDdiet$alcohol.points

    # make figure and run test
UMCGLLDdiet$IBD[UMCGLLDdiet$IBD==1]<-"IBD"
UMCGLLDdiet$IBD[UMCGLLDdiet$IBD==0]<-"Non-IBD"

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/MDS_plot2.pdf")
library(ggplot2)
ggplot(UMCGLLDdiet, aes(x = IBD, y = aMED)) +
  geom_boxplot()+geom_violin(alpha=0.7)
dev.off()

median(UMCGLLDdiet$aMED[UMCGLLDdiet$IBD=='IBD'], na.rm=T)
median(UMCGLLDdiet$aMED[UMCGLLDdiet$IBD=='Non-IBD'], na.rm=T)

wilcox.test(aMED~IBD,data=UMCGLLDdiet)

#### calculate LLDS ----
    # read df
UMCGLLDdiet=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/UMCGLLDdiet.csv")

#Calculate food intake in gram per 1000 calories -> divide the g/day food intake by calories, dived by 1000 
UMCGLLDdiet$Kcal <- UMCGLLDdiet$SUMOFKCAL/1000

#Positive Groups 

#N.B.: "group vegetables", "group nuts" etc. from Wageningen already included all types of vegetables/nuts
#If you did not receive these group items yet, then you can just add individual vegetable items to form this category (vegetables_stirfried + vegetables_cooked)

#Vegetables
UMCGLLDdiet$vegetables <- (UMCGLLDdiet$vegetables_cooked_nobutter + UMCGLLDdiet$vegetables_stirfried + UMCGLLDdiet$vegetables_cooked_butter) / UMCGLLDdiet$Kcal
UMCGLLDdiet$vegetables.points <- ntile (UMCGLLDdiet$vegetables, 5)-1

#Whole Fruit  
UMCGLLDdiet$wholefruit <- UMCGLLDdiet$fruit / UMCGLLDdiet$Kcal 
UMCGLLDdiet$fruit.points <- ntile (UMCGLLDdiet$wholefruit, 5)-1

#Whole Grains  
UMCGLLDdiet$wholegrains <- UMCGLLDdiet$bread / UMCGLLDdiet$Kcal 
UMCGLLDdiet$wholegrains.points <- ntile (UMCGLLDdiet$wholegrains, 5)-1

#Legumes and Nuts 
UMCGLLDdiet$legnut <- (UMCGLLDdiet$legumes + UMCGLLDdiet$nut_d + UMCGLLDdiet$snack_nut + UMCGLLDdiet$peanutbutter) / UMCGLLDdiet$Kcal 
UMCGLLDdiet$legnuts.points <- ntile (UMCGLLDdiet$legnut,5)-1

#Fish
UMCGLLDdiet$fish <- (UMCGLLDdiet$herring_salted + UMCGLLDdiet$fish_white_fried + UMCGLLDdiet$fish_lean + UMCGLLDdiet$fish_fatty + UMCGLLDdiet$fish_other + UMCGLLDdiet$fish_prepared_fat) / UMCGLLDdiet$Kcal 
UMCGLLDdiet$fish.points <- ntile (UMCGLLDdiet$fish, 5)-1

#Oils and soft margarines
UMCGLLDdiet$oilsmargarines<-UMCGLLDdiet$margarine_lfb + UMCGLLDdiet$saladdressing_f + UMCGLLDdiet$saladdressing_w + UMCGLLDdiet$mayonaise + UMCGLLDdiet$mayonaise_snack
UMCGLLDdiet$oilsmargarines <- UMCGLLDdiet$oilsmargarines / UMCGLLDdiet$Kcal 
UMCGLLDdiet$oilsmargarines.points <- ntile (UMCGLLDdiet$oilsmargarines, 5)-1

#Unsweetened Dairy
UMCGLLDdiet$unsweetdairy <- UMCGLLDdiet$milk_whole + UMCGLLDdiet$milk_semiskimmed + UMCGLLDdiet$milk_skimmed + UMCGLLDdiet$buttermilk + 
  UMCGLLDdiet$yoghurt_drink_other + UMCGLLDdiet$yoghurt_ff + UMCGLLDdiet$yoghurt_lf + UMCGLLDdiet$milk_coffee
UMCGLLDdiet$unsweetdairy <- UMCGLLDdiet$unsweetdairy / UMCGLLDdiet$Kcal 
UMCGLLDdiet$unsweetdairy.points <- ntile (UMCGLLDdiet$unsweetdairy, 5)-1

#Coffee 
#UMCGLLDdiet$group_coffee
UMCGLLDdiet$coffee <- UMCGLLDdiet$group_coffee / UMCGLLDdiet$Kcal 
UMCGLLDdiet$coffee.points <- ntile (UMCGLLDdiet$coffee, 5)-1

#Tea
#UMCGLLDdiet$group_tea
UMCGLLDdiet$tea <- UMCGLLDdiet$group_tea / UMCGLLDdiet$Kcal 
UMCGLLDdiet$tea.points <- ntile (UMCGLLDdiet$tea, 5)-1

#Negative groups 

#red and processed meat
#sausage_smoked_cold_cuts, meats_other_cold_cuts, meat, beef_lean, beef_fat, pork_lean, pork_fat, pork_processed, meats_fat snack_meats
UMCGLLDdiet$redprocmeat <- UMCGLLDdiet$sausage_smoked + UMCGLLDdiet$meat + UMCGLLDdiet$meats_fat+ UMCGLLDdiet$meats_other+ UMCGLLDdiet$beef_fat+ UMCGLLDdiet$beef_lean + 
  UMCGLLDdiet$pork_fat + UMCGLLDdiet$pork_lean + UMCGLLDdiet$pork_processed + UMCGLLDdiet$snack_meats
UMCGLLDdiet$redprocmeat <- UMCGLLDdiet$redprocmeat / UMCGLLDdiet$Kcal 
UMCGLLDdiet$redprocmeat.points <- ntile(desc(UMCGLLDdiet$redprocmeat), 5) -1

#butter and hard margarines
UMCGLLDdiet$buttermar <- UMCGLLDdiet$butter_b + UMCGLLDdiet$butter_ob + UMCGLLDdiet$sandwichspread + UMCGLLDdiet$gravy 
UMCGLLDdiet$buttermar <- UMCGLLDdiet$buttermar / UMCGLLDdiet$Kcal 
UMCGLLDdiet$buttermar.points <- ntile(desc(UMCGLLDdiet$buttermar), 5)-1

#sugar-sweetened beverages
UMCGLLDdiet$sugarsweetbev <- UMCGLLDdiet$softdrink_sugar + UMCGLLDdiet$breakfast_drink + UMCGLLDdiet$chocolatemilk + UMCGLLDdiet$fruitjuice + UMCGLLDdiet$beer_af + UMCGLLDdiet$yoghurt_drink_added_sugar 
UMCGLLDdiet$sugarsweetbev <- UMCGLLDdiet$sugarsweetbev / UMCGLLDdiet$Kcal 
UMCGLLDdiet$sugarsweetbev.points <- ntile(desc(UMCGLLDdiet$sugarsweetbev), 5)-1

#total points 
UMCGLLDdiet$totalpoints <- UMCGLLDdiet$vegetables.points + UMCGLLDdiet$fruit.points + UMCGLLDdiet$wholegrains.points + UMCGLLDdiet$legnuts.points + 
  UMCGLLDdiet$fish.points + UMCGLLDdiet$oilsmargarines.points + UMCGLLDdiet$unsweetdairy.points + UMCGLLDdiet$coffee.points + UMCGLLDdiet$tea.points + 
  UMCGLLDdiet$redprocmeat.points + UMCGLLDdiet$buttermar.points + UMCGLLDdiet$sugarsweetbev.points

summary(UMCGLLDdiet$totalpoints) # min 5 points, max 41 points, median and mean 24 points. 

    # perform testing
UMCGLLDdiet$IBD[UMCGLLDdiet$IBD==1]<-"IBD"
UMCGLLDdiet$IBD[UMCGLLDdiet$IBD==0]<-"Non-IBD"

wilcox.test(totalpoints~IBD,data=UMCGLLDdiet)


pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDS_plot.pdf")
library(ggplot2)
ggplot(UMCGLLDdiet, aes(x = IBD, y = totalpoints)) +
  geom_boxplot()+geom_violin(alpha=0.7)
dev.off()

median(UMCGLLDdiet$aMED[UMCGLLDdiet$IBD=='IBD'], na.rm=T)
median(UMCGLLDdiet$aMED[UMCGLLDdiet$IBD=='Non-IBD'], na.rm=T)
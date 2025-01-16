library(dplyr)
library(ggplot2)

#### calculate MDS ----
    # read df
meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta=meta[,c(3,17,26:82)]
meta_clean=na.omit(meta)
meta_clean=subset(meta_clean,meta_clean$diet_SUMOFKCAL>=800 & meta_clean$diet_SUMOFKCAL<=5000)

    #1 Vegetables, "group_vegetables" includes; "vegetables_cooked_nobutter, vegetables_stirfried, vegetables_cooked_butter"
meta_clean$veg.total <- meta_clean$diet_vegetables_cooked_nobutter + meta_clean$diet_vegetables_stirfried + meta_clean$diet_vegetables_cooked_butter
x<-median(meta_clean$veg.total)          
meta_clean$veg.points <- ifelse(meta_clean$veg.total>= x, 1, 0)

    #2 Legumes, only 1 column legumes in database; "legumes" 
meta_clean$leg.total<- meta_clean$diet_legumes
x<- median(meta_clean$leg.total)
meta_clean$leg.points <- ifelse(meta_clean$leg.total>= x, 1, 0)

    #3 Fruits, "fruitjuice","fruit" 
    # don't use the already existing "group_fruits" -> contains applesauce. 
    # consider/ compare outcomes when leaving out fruitjuice as this contains a lot of sugar. Original score includes fruit juice though 
meta_clean$fruit.total<-meta_clean$diet_fruit+meta_clean$diet_fruitjuice  
x<-median(meta_clean$fruit.total)  
meta_clean$fruit.points<- ifelse(meta_clean$fruit.total>= x, 1, 0)

    #4 Nuts 
    #nut.total<-meta_clean$group_nuts
meta_clean$nut.total <- meta_clean$diet_nut_d + meta_clean$diet_snack_nut + meta_clean$diet_peanutbutter
x<-median(meta_clean$nut.total)
meta_clean$nut.points<-ifelse(meta_clean$nut.total>= x, 1, 0)

    #5 Whole grains 
meta_clean$whgrains.total<-meta_clean$diet_bread
x<-median(meta_clean$whgrains.total)
meta_clean$whgrains.points<-ifelse(meta_clean$whgrains.total>= x, 1, 0)

    #6 fish, group_fish contains herring_salted, fish_white_fried, fish_lean, fish_fatty, fish_other, fish_prepared_fat
meta_clean$fish.total <- meta_clean$diet_herring_salted + meta_clean$diet_fish_white_fried + meta_clean$diet_fish_lean + meta_clean$diet_fish_fatty + meta_clean$diet_fish_other + meta_clean$diet_fish_prepared_fat
x<-median(meta_clean$fish.total)
meta_clean$fish.points<-ifelse(meta_clean$fish.total>= x, 1, 0)

    #7 red and processed meats; containing: sausage_smoked, meats_other_cold_cuts, meat, beef_lean, beef_fat
    #pork_lean, pork_fat, pork_processed, meats_fat, snack_meats
meta_clean$meat.total<- meta_clean$diet_sausage_smoked + meta_clean$diet_meat + meta_clean$diet_meats_fat+ meta_clean$diet_meats_other+ meta_clean$diet_beef_fat+ meta_clean$diet_beef_lean + meta_clean$diet_pork_fat + meta_clean$diet_pork_lean + meta_clean$diet_pork_processed + meta_clean$diet_snack_meats
x<-median(meta_clean$meat.total)
meta_clean$meat.points<-ifelse(meta_clean$meat.total>= x, 0, 1)

    #8 alcohol; group_alcohol consist of beer, wine_red, wine_white, wine_fort, spirits, other_alc_drinks
    #  1 point if 10-20 g/d (1-2 glasses a day),  1 glas alcohol is 10 g / day 
meta_clean$group_alcohol <- meta_clean$diet_beer + meta_clean$diet_wine_red + meta_clean$diet_wine_white + meta_clean$diet_wine_fort + meta_clean$diet_spirits + meta_clean$diet_other_alc_drinks
meta_clean$alcohol.points<-ifelse((meta_clean$group_alcohol >=10 & meta_clean$group_alcohol <= 20), 1, 0 ) 

    #9 M/S ratio no data 

    #add the points together [max amount of 8 points, min amount of 0 points]
meta_clean$diet_aMED <- meta_clean$veg.points + meta_clean$leg.points + meta_clean$fruit.points + meta_clean$nut.points + 
  meta_clean$whgrains.points + meta_clean$fish.points + meta_clean$meat.points + meta_clean$alcohol.points

    # make figure and run test
meta_clean$base_IBDconf[meta_clean$base_IBDconf==1]<-"IBD"
meta_clean$base_IBDconf[meta_clean$base_IBDconf==0]<-"Non-IBD"

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/MDS_plot.pdf")
ggplot(meta_clean, aes(x=base_IBDconf, y=diet_aMED, fill=base_IBDconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Mediterranean diet score",x="IBD", y = "aMED")+
  theme_classic()
dev.off()

median(meta_clean$diet_aMED[meta_clean$base_IBDconf=='IBD'])
median(meta_clean$diet_aMED[meta_clean$base_IBDconf=='Non-IBD'])
table(meta_clean$base_IBDconf)

wilcox.test(diet_aMED~base_IBDconf,data=meta_clean)

#### calculate LLDS ----
    # read df
meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta=meta[,c(3,17,26:82)]
meta_clean=na.omit(meta)
meta_clean=subset(meta_clean,meta_clean$diet_SUMOFKCAL>=800 & meta_clean$diet_SUMOFKCAL<=5000)

#Calculate food intake in gram per 1000 calories -> divide the g/day food intake by calories, dived by 1000 
meta_clean$Kcal <- meta_clean$diet_SUMOFKCAL/1000

#Positive Groups 

#N.B.: "group vegetables", "group nuts" etc. from Wageningen already included all types of vegetables/nuts
#If you did not receive these group items yet, then you can just add individual vegetable items to form this category (vegetables_stirfried + vegetables_cooked)

#Vegetables
meta_clean$vegetables <- (meta_clean$diet_vegetables_cooked_nobutter + meta_clean$diet_vegetables_stirfried + meta_clean$diet_vegetables_cooked_butter) / meta_clean$Kcal
meta_clean$vegetables.points <- ntile (meta_clean$vegetables, 5)-1

#Whole Fruit  
meta_clean$wholefruit <- meta_clean$diet_fruit / meta_clean$Kcal 
meta_clean$fruit.points <- ntile (meta_clean$wholefruit, 5)-1

#Whole Grains  
meta_clean$wholegrains <- meta_clean$diet_bread / meta_clean$Kcal 
meta_clean$wholegrains.points <- ntile (meta_clean$wholegrains, 5)-1

#Legumes and Nuts 
meta_clean$legnut <- (meta_clean$diet_legumes + meta_clean$diet_nut_d + meta_clean$diet_snack_nut + meta_clean$diet_peanutbutter) / meta_clean$Kcal 
meta_clean$legnuts.points <- ntile (meta_clean$legnut,5)-1

#Fish
meta_clean$fish <- (meta_clean$diet_herring_salted + meta_clean$diet_fish_white_fried + meta_clean$diet_fish_lean + meta_clean$diet_fish_fatty + meta_clean$diet_fish_other + meta_clean$diet_fish_prepared_fat) / meta_clean$Kcal 
meta_clean$fish.points <- ntile (meta_clean$fish, 5)-1

#Oils and soft margarines
meta_clean$oilsmargarines<-meta_clean$diet_margarine_lfb + meta_clean$diet_saladdressing_f + meta_clean$diet_saladdressing_w + meta_clean$diet_mayonaise + meta_clean$diet_mayonaise_snack
meta_clean$oilsmargarines <- meta_clean$oilsmargarines / meta_clean$Kcal 
meta_clean$oilsmargarines.points <- ntile (meta_clean$oilsmargarines, 5)-1

#Unsweetened Dairy
meta_clean$unsweetdairy <- meta_clean$diet_milk_whole + meta_clean$diet_milk_semiskimmed + meta_clean$diet_milk_skimmed + meta_clean$diet_buttermilk + 
  meta_clean$diet_yoghurt_drink_other + meta_clean$diet_yoghurt_ff + meta_clean$diet_yoghurt_lf + meta_clean$diet_milk_coffee
meta_clean$unsweetdairy <- meta_clean$unsweetdairy / meta_clean$Kcal 
meta_clean$unsweetdairy.points <- ntile (meta_clean$unsweetdairy, 5)-1

#Coffee 
#meta_clean$group_coffee
meta_clean$coffee <- meta_clean$diet_group_coffee / meta_clean$Kcal 
meta_clean$coffee.points <- ntile (meta_clean$coffee, 5)-1

#Tea
#meta_clean$group_tea
meta_clean$tea <- meta_clean$diet_group_tea / meta_clean$Kcal 
meta_clean$tea.points <- ntile (meta_clean$tea, 5)-1

#Negative groups 

#red and processed meat
#sausage_smoked_cold_cuts, meats_other_cold_cuts, meat, beef_lean, beef_fat, pork_lean, pork_fat, pork_processed, meats_fat snack_meats
meta_clean$redprocmeat <- meta_clean$diet_sausage_smoked + meta_clean$diet_meat + meta_clean$diet_meats_fat+ meta_clean$diet_meats_other+ meta_clean$diet_beef_fat+ meta_clean$diet_beef_lean + 
  meta_clean$diet_pork_fat + meta_clean$diet_pork_lean + meta_clean$diet_pork_processed + meta_clean$diet_snack_meats
meta_clean$redprocmeat <- meta_clean$redprocmeat / meta_clean$Kcal 
meta_clean$redprocmeat.points <- ntile(desc(meta_clean$redprocmeat), 5) -1

#butter and hard margarines
meta_clean$buttermar <- meta_clean$diet_butter_b + meta_clean$diet_butter_ob + meta_clean$diet_sandwichspread + meta_clean$diet_gravy 
meta_clean$buttermar <- meta_clean$buttermar / meta_clean$Kcal 
meta_clean$buttermar.points <- ntile(desc(meta_clean$buttermar), 5)-1

#sugar-sweetened beverages
meta_clean$sugarsweetbev <- meta_clean$diet_softdrink_sugar + meta_clean$diet_breakfast_drink + meta_clean$diet_chocolatemilk + meta_clean$diet_fruitjuice + meta_clean$diet_beer_af + meta_clean$diet_yoghurt_drink_added_sugar 
meta_clean$sugarsweetbev <- meta_clean$sugarsweetbev / meta_clean$Kcal 
meta_clean$sugarsweetbev.points <- ntile(desc(meta_clean$sugarsweetbev), 5)-1

#total points 
meta_clean$diet_LLDS <- meta_clean$vegetables.points + meta_clean$fruit.points + meta_clean$wholegrains.points + meta_clean$legnuts.points + 
  meta_clean$fish.points + meta_clean$oilsmargarines.points + meta_clean$unsweetdairy.points + meta_clean$coffee.points + meta_clean$tea.points + 
  meta_clean$redprocmeat.points + meta_clean$buttermar.points + meta_clean$sugarsweetbev.points

summary(meta_clean$diet_LLDS) # min 4 points, max 41 points, median and mean 24 points. 

    # perform testing
meta_clean$base_IBDconf[meta_clean$base_IBDconf==1]<-"IBD"
meta_clean$base_IBDconf[meta_clean$base_IBDconf==0]<-"Non-IBD"

wilcox.test(diet_LLDS~base_IBDconf,data=meta_clean)

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDS_plot.pdf")
ggplot(meta_clean, aes(x=base_IBDconf, y=diet_LLDS, fill=base_IBDconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Lifelines diet score",x="IBD", y = "LLDS")+
  theme_classic()
dev.off()

table(meta_clean$base_IBDconf)

#### merge and save score columns ----
meta_clean=meta_clean[c("A_ID_LLDUMCGIBD","diet_LLDS","diet_aMED")]
#write.csv(meta_clean,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_diet.csv",row.names=F)


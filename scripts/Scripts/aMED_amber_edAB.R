####################### ALTERNATE MEDITERRANEAN DIET SCORE (aMED, Fung et al.) ##########################################################################################################################

#The aMED score was based on the Mediterranean diet scale of Trichopoulou et al. The original score was based on the 
# intake of 9 items: vegetables, legumes, fruit and nuts, dairy, cereals, meat and meat products, fish, alcohol, 
# and the ratio of monounsaturated to saturated fat. Intakes above the median of the study subjects received 1 point;
# all other intakes received 0 points. Meat and dairy product consumption less than the median received 1 point. 

#Fung et al. modified the original scale by excluding potatoe-products from the vegetable group, separating fruit 
# and nuts into 2 groups, eliminating the dairy group, including whole-grain products only, including only 
# red and processed meats for the meat group, and assigning 1 point for moderate alcohol intake between 5 and 15 g/d.  
# These modifications were based on dietary patterns and eating behaviors that have been consistently associated 
# with lower risks of chronic disease in clinical and epidemiologic studies. 
# Possible scores on the aMED ranged from 0 to 9. 
# To consider: 
# exlude juice from favorable group of fruit (high in sugar) 
# sex-specific allowance for alcohol (5-25 g for females and 10-50 g for males) 
# https://watermark.silverchair.com/znu00705000163.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAswwggLIBgkqhkiG9w0BBwagggK5MIICtQIBADCCAq4GCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMmRrjW3Dh092cep5lAgEQgIICf5PZg5ILLP7gqVwm8J9PhkaR48u1tnI1opMsjpVEq2rh1hRJ7jnIbM8E1HY1D-GO4zh79atE0PzvkeMSH7cXQU6gEEKncqaGAWS1jz3Ox8t3NOfL4h2PUyj3912EoGKuUghdEYumFEvlGfHvJLKXt4FuYu5i9SZR2NOyJazzQWXDhgQcmDkou_1cFUD_HWQTwdYgGOQU38bYetDTnPvCJ18OA_Io2WnSnYPstu12B3Wdih6rD0X1o6b6oa0GyLzNNPYlpSyZcS4F4gQ7i6LPUeD52YCdXdTA9oKnSk0kb2VDXlos6ZejRaOyAnpsNgFCu4DQfF5Rs7g0uzNpAHTrobDReyrUAS7_7jvudQ-XKQZTPAZygfCx4A561tiJAOfcQNoR43gceSWGdKhRvVhXwnfrnVAV0VVhN_CNlcExDLXgYo7bPnnMHEBcroc0I4BdLikDdSW8H1oKeKU_K0w40Uo2hyqo2jtqXuTNV00MIU3eEKnElQKgNjAn3Txx5jNMWmZzYKXNsBobjtjYsfAN4MjVUNf21vrP6NEblPVSove8_LMVno7KuGauJbEob9G8fVJx0KVLJfCxWkLVlOaqGy1eS6nbnQJpylqVifWCqWHNxTS5HeW3gf-OPUlA7N2DiJdo28YVxv_iy3syKcxRa3I5SXqF3Mct3U9lq6Hq_3nYykEnrbpRLyd8rXQ8JrMPxe34SJXpAk-UArRsAhlHbCrzgUVGUxdtTpYXYVEVtNgphr5HeQbKgqWl6azfYVfI-wZ58MGNS_atYRE6TiX76UkA1j5DcMmojLKBLgONYQijbFxLcU2GYEZQNOmNclA9xebcfiPtyo8i1A7j7t33qA
# https://academic.oup.com/view-large/111576177 

################ SCRIPT FOR TRAIN SET  ########################################################################
## Food groups should be in the dataset, otherwise see food grouping table Laura 

library(tidyverse)
library(dplyr)
phenosdiet1 = read.delim(file = "Data/phenos_IBD_clean_v2.txt")

#ffq data, phenosdiet2 #rows is samples, column is foodgroups   
MDS <- phenosdiet1 %>%
  select(1, starts_with("diet_"))

#remove rows with only NA and row 177 because of no filled in FFQs
MDS <- MDS[rowSums(is.na(MDS[,-c(1,98)])) != ncol(MDS) -2, ]
MDS <- MDS[-177, ]
#Positive consumption above median gets 1 points, below median 0 points. 

#1 Vegetables, "group_vegetables" includes; "vegetables_cooked_nobutter, vegetables_stirfried, vegetables_cooked_butter"
veg.total <- MDS$diet_vegetables_cooked_nobutter.Res + MDS$diet_vegetables_stirfried.Res + MDS$diet_vegetables_cooked_butter.Res

#veg.total<-MDS$group_vegetables
x<-median(veg.total)                    
MDS$veg.points <- ifelse(veg.total>= x, 1, 0)

#2 Legumes, only 1 column legumes in database; "legumes" 
leg.total<- MDS$diet_legumes.Res
x<- median(leg.total)
MDS$leg.points <- ifelse(leg.total>= x, 1, 0)

#3 Fruits, "fruitjuice","fruit" 
# don't use the already existing "group_fruits" -> contains applesauce. 
# consider/ compare outcomes when leaving out fruitjuice as this contains a lot of sugar. Original score includes fruit juice though 
fruit.total<-MDS$diet_fruit.Res+MDS$diet_fruitjuice.Res  
x<-median(fruit.total)  
MDS$fruit.points<- ifelse(fruit.total>= x, 1, 0)

#4 Nuts 
#nut.total<-MDS$group_nuts
nut.total <- MDS$diet_nut_d.Res + MDS$diet_snack_nut.Res + MDS$diet_peanutbutter.Res
x<-median(nut.total)
MDS$nut.points<-ifelse(nut.total>= x, 1, 0)

#5 Whole grains 
whgrains.total<-MDS$diet_bread.Res 
#--> limitation FFQ LL and IBD T1: no differentiation whole grain/ white. LLDS-score might work better! 
x<-median(whgrains.total)
MDS$whgrains.points<-ifelse(whgrains.total>= x, 1, 0)

#6 fish, group_fish contains herring_salted, fish_white_fried, fish_lean, fish_fatty, fish_other, fish_prepared_fat
#fish.total<-MDS$group_fish
fish.total <- MDS$diet_herring_salted.Res + MDS$diet_fish_white_fried.Res + MDS$diet_fish_lean.Res + MDS$diet_fish_fatty.Res + MDS$diet_fish_other.Res + MDS$diet_fish_prepared_fat.Res
x<-median(fish.total)
MDS$fish.points<-ifelse(fish.total>= x, 1, 0)

#7 red and processed meats; containing: sausage_smoked, meats_other_cold_cuts, meat, beef_lean, beef_fat
#pork_lean, pork_fat, pork_processed, meats_fat, snack_meats
meat.total<- MDS$diet_sausage_smoked.Res + MDS$diet_meat.Res + MDS$diet_meats_fat.Res+ MDS$diet_beef_fat.Res+ MDS$diet_beef_lean.Res + 
  MDS$diet_pork_fat.Res + MDS$diet_pork_lean.Res + MDS$diet_pork_processed.Res + MDS$diet_snack_meats.Res 
#--> adaption from origianl script: no distinction in cold cuts or not (no distinction made in current dataset)
x<-median(meat.total)
MDS$meat.points<-ifelse(meat.total>= x, 0, 1)

#8 alcohol; group_alcohol consist of beer, wine_red, wine_white, wine_fort, spirits, other_alc_drinks
#  1 point if 10-20 g/d (1-2 glasses a day),  1 glas alcohol is 10 g / day 
#summary(MDS$group_alcohol)
MDS$group_alcohol <- MDS$diet_beer.Res + MDS$diet_wine_red.Res + MDS$diet_wine_white.Res + MDS$diet_wine_fort.Res + MDS$diet_spirits.Res + MDS$diet_other_alc_drinks.Res
MDS$alcohol.points<-ifelse((MDS$group_alcohol >=10 & MDS$group_alcohol <= 20), 1, 0 ) 

#9 M/S ratio no data 

#add the points together [max amount of 8 points, min amount of 0 points]
MDS$aMED <- MDS$veg.points + MDS$leg.points + MDS$fruit.points + MDS$nut.points + 
  MDS$whgrains.points + MDS$fish.points + MDS$meat.points + MDS$alcohol.points

########################################### LifeLines Diet Score (LLDS) ####################################################### 

# A food-based score reflecting relative dietary quality in the Lifelines population 
# Vinke et al. 2018, doi: 10.1038/s41430-018-0205-z

# Input: Intake of food groups expressed in gram per 1000 kcal
# Score consisting of 12 categories 
# Positive categories (9): vegetables, fruit, whole grain products, legumes & nuts, fish, oils & soft margarines,
#                          unsweetened dairy, coffee & tea
# Negative categroies (3): red & processed meat, butter & hard margarines, sugar-sweetened beverages
# Each category divided into quintiles-> score ranging from 0-4 per category -> total score of max 48 points (=high diet quality)

LLDS <- MDS

#Vegetables
LLDS$vegetables <- LLDS$diet_group_vegetables.Res 
LLDS$vegetables.points <- ntile (LLDS$vegetables, 5)-1

#Whole Fruit  
LLDS$wholefruit <- LLDS$fruit / Kcal 
LLDS$fruit.points <- ntile (LLDS$wholefruit, 5)-1

#Whole Grains  
LLDS$wholegrains <- LLDS$bread / Kcal 
LLDS$wholegrains.points <- ntile (LLDS$wholegrains, 5)-1

#Legumes and Nuts 
LLDS$legnut<- LLDS$legumes + LLDS$group_nuts 
LLDS$legnut <- LLDS$legnut / Kcal 
LLDS$legnuts.points <- ntile (LLDS$legnut,5)-1

#Fish
LLDS$fish <- LLDS$group_fish / Kcal 
LLDS$fish.points <- ntile (LLDS$fish, 5)-1

#Oils and soft margarines
LLDS$oilsmargarines<-LLDS$margarine_lfb + LLDS$saladdressing_f + LLDS$saladdressing_w + LLDS$mayonaise + LLDS$mayonaise_snack
LLDS$oilsmargarines <- LLDS$oilsmargarines / Kcal 
LLDS$oilsmargarines.points <- ntile (LLDS$oilsmargarines, 5)-1

#Unsweetened Dairy
LLDS$unsweetdairy <- LLDS$milk_whole + LLDS$milk_semiskimmed + LLDS$milk_skimmed + LLDS$buttermilk + 
  LLDS$yoghurt_drink_other + LLDS$yoghurt_ff + LLDS$yoghurt_lf + LLDS$milk_coffee
LLDS$unsweetdairy <- LLDS$unsweetdairy / Kcal 
LLDS$unsweetdairy.points <- ntile (LLDS$unsweetdairy, 5)-1

#Coffee 
#LLDS$group_coffee
LLDS$coffee <- LLDS$group_coffee / Kcal 
LLDS$coffee.points <- ntile (LLDS$coffee, 5)-1

#Tea
#LLDS$group_tea
LLDS$tea <- LLDS$group_tea / Kcal 
LLDS$tea.points <- ntile (LLDS$tea, 5)-1

#Negative groups 

#red and processed meat
#sausage_smoked_cold_cuts, meats_other_cold_cuts, meat, beef_lean, beef_fat, pork_lean, pork_fat, pork_processed, meats_fat snack_meats
LLDS$redprocmeat <- LLDS$sausage_smoked + LLDS$meat + LLDS$meats_fat+ LLDS$meats_other+ LLDS$beef_fat+ LLDS$beef_lean + 
  LLDS$pork_fat + LLDS$pork_lean + LLDS$pork_processed + LLDS$snack_meats
LLDS$redprocmeat <- LLDS$redprocmeat / Kcal 
LLDS$redprocmeat.points <- ntile(desc(LLDS$redprocmeat), 5) -1

#butter and hard margarines
LLDS$buttermar <- LLDS$butter_b + LLDS$butter_ob + LLDS$sandwichspread + LLDS$gravy 
LLDS$buttermar <- LLDS$buttermar / Kcal 
LLDS$buttermar.points <- ntile(desc(LLDS$buttermar), 5)-1

#sugar-sweetened beverages
LLDS$sugarsweetbev <- LLDS$softdrink_sugar + LLDS$breakfast_drink + LLDS$chocolatemilk
LLDS$fruitjuice + LLDS$beer_af + LLDS$yoghurt_drink_added_sugar 
LLDS$sugarsweetbev <- LLDS$sugarsweetbev / Kcal 
LLDS$sugarsweetbev.points <- ntile(desc(LLDS$sugarsweetbev), 5)-1

#total points 
LLDS$totalpoints <- LLDS$vegetables.points + LLDS$fruit.points + LLDS$wholegrains.points + LLDS$legnuts.points + 
  LLDS$fish.points + LLDS$oilsmargarines.points + LLDS$unsweetdairy.points + LLDS$coffee.points + LLDS$tea.points + 
  LLDS$redprocmeat.points + LLDS$buttermar.points + LLDS$sugarsweetbev.points




#write df
MDS %>% select(AA_ID, aMED) -> To_write
write_tsv(To_write,"TABLES/aMED_score.tsv")

#ggplot(MDS,aes(x=aMED)) + geom_histogram() + theme_bw() -> PLOT
#ggsave(filename="Histogram_amed.pdf", plot=PLOT)



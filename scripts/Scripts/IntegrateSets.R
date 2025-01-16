# Script to integrate datasets
  # 26-09-2023

library(haven)
library(readxl)
library(openxlsx)
library(dplyr)

# Prepare env dataset
GIEQ2 <- read_sav("Data/GIEQ2.sav")
GIEQ1 <- read_excel("Data/GIEQ1.xlsx")
  # change labels in GIEQ2
key <- read_excel("Data/GIEQ2_exc.xlsx", sheet = 3)
GIEQ2 <- merge(GIEQ2, key, by.x = "STNR",by.y = "OldSTNR", all.x = T)
GIEQ2$NewSTNR <- ifelse(is.na(GIEQ2$NewSTNR), GIEQ2$STNR, GIEQ2$NewSTNR)
GIEQ2[is.na(GIEQ2$NewSTNR),]
  # merge datasets
GIEQ <- merge(GIEQ1, GIEQ2[, 4:ncol(GIEQ2)], by.x = "GIEQID",by.y = "NewSTNR")
#write.xlsx(GIEQ, "Data/GIEQfull.xlsx")

# Prepare diet dataset
diet_clean <- read.table("Data/phenos_IBD_clean_v2.txt", header = TRUE, sep = "\t")
diet_raw <-  read_excel("Data/Final_Metadata_IBD_V46.xlsx")
diet_raw <- diet_raw[-c(1,2,3),]
diet_full <- merge(diet_clean,diet_raw, by.x = "AA_ID", by.y ="IBDFECIDTotal10032017")

cols_of_interest <- c("DietQuestionnaireCompleted.","AgeAtFecalSampling","UMCGIBDResearchIDorLLDeepID", "AA_ID","UMCGNoFromZIC","group_vegetables", "fruit", "bread","legumes","group_nuts","group_fish","margarine_lfb", 
                      "saladdressing_f", "saladdressing_w", "mayonaise", "mayonaise_snack","milk_whole","milk_semiskimmed", "milk_skimmed","buttermilk",
                      "yoghurt_drink_other","yoghurt_ff", "yoghurt_lf", "milk_coffee","group_coffee","group_tea",
                      "sausage_smoked" ,"meat" ,"meats_fat","meats_other","beef_fat","beef_lean",
                      "pork_fat" ,"pork_lean" ,"pork_processed" ,"snack_meats","butter_b" ,"butter_ob" ,"sandwichspread" ,"gravy", "softdrink_sugar" ,"breakfast_drink" ,"chocolatemilk",
                      "fruitjuice" ,"beer_af" ,"yoghurt_drink_added_sugar", "vegetables_cooked_nobutter", "vegetables_stirfried", "vegetables_cooked_butter",
                      "nut_d", "snack_nut", "peanutbutter","herring_salted","fish_white_fried","fish_lean","fish_fatty","fish_other", "fish_prepared_fat",
                      "sausage_smoked" ,"meat" ,"meats_fat","beef_fat","beef_lean" ,"pork_fat" ,"pork_lean" ,"pork_processed" ,"snack_meats", 
                      "beer" ,"wine_red" ,"wine_white" ,"wine_fort" ,"spirits" ,"other_alc_drinks", 
                      
                      "diet_group_vegetables.Res", "diet_fruit.Res","diet_bread.Res","diet_legumes.Res","diet_group_nuts.Res","diet_group_fish.Res","diet_margarine_lfb.Res", 
                      "diet_saladdressing_f.Res", "diet_saladdressing_w.Res", "diet_mayonaise.Res", "diet_mayonaise_snack.Res","diet_milk_whole.Res","diet_milk_semiskimmed.Res", "diet_milk_skimmed.Res","diet_buttermilk.Res",
                      "diet_yoghurt_drink_other.Res","diet_yoghurt_ff.Res", "diet_yoghurt_lf.Res", "diet_milk_coffee.Res","diet_group_coffee.Res","diet_group_tea.Res",
                      "diet_sausage_smoked.Res" ,"diet_meat.Res" ,"diet_meats_fat.Res","diet_meats_other.Res","diet_beef_fat.Res","diet_beef_lean.Res",
                      "diet_pork_fat.Res" ,"diet_pork_lean.Res" ,"diet_pork_processed.Res" ,"diet_snack_meats.Res","diet_butter_b.Res" ,"diet_butter_ob.Res" ,"diet_sandwichspread.Res" ,"diet_gravy.Res", "diet_softdrink_sugar.Res" ,"diet_breakfast_drink.Res" ,"diet_chocolatemilk.Res",
                      "diet_fruitjuice.Res" ,"diet_beer_af.Res" ,"diet_yoghurt_drink_added_sugar.Res","diet_vegetables_cooked_nobutter.Res", "diet_vegetables_stirfried.Res", "diet_vegetables_cooked_butter.Res",
                      "diet_nut_d.Res", "diet_snack_nut.Res", "diet_peanutbutter.Res", "diet_herring_salted.Res","diet_fish_white_fried.Res","diet_fish_lean.Res","diet_fish_fatty.Res","diet_fish_other.Res", "diet_fish_prepared_fat.Res",
                      "diet_sausage_smoked.Res" ,"diet_meat.Res" ,"diet_meats_fat.Res","diet_beef_fat.Res","diet_beef_lean.Res" ,"diet_pork_fat.Res" ,"diet_pork_lean.Res" ,"diet_pork_processed.Res" ,"diet_snack_meats.Res", 
                      "diet_beer.Res" ,"diet_wine_red.Res" ,"diet_wine_white.Res" ,"diet_wine_fort.Res" ,"diet_spirits.Res" ,"diet_other_alc_drinks.Res", "SUMOFKCAL")

diet_final <- diet_full[cols_of_interest]

# Integrate diet and env
GIEQdiet <- merge(GIEQ, diet_final, by.x="UMCGID",by.y="UMCGNoFromZIC")
key <- read_excel("Data/Key.xlsx")
key <- key[,c(2,4,5,6,8)]
GIEQdiet <- merge(GIEQdiet, key, by.x="UMCGID",by.y="UMCG ID")
GIEQdiet <- filter(GIEQdiet, GIEQdiet$`In GSA?` == "ja")
#write.xlsx(GIEQdiet, "Data/GIEQdiet.xlsx")

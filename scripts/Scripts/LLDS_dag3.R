######################################
# Script to calculate LLDS in DAG3   #
# Date: 19-09-2024                   #
######################################

library(dplyr)

#### determine quintiles in 1000IBD LLD cohort ---- 
meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta=meta[,c("diet_vegetables_cooked_nobutter", "diet_vegetables_stirfried", "diet_vegetables_cooked_butter","diet_SUMOFKCAL","diet_fruit","diet_bread",
             "diet_legumes","diet_nut_d" ,"diet_snack_nut", "diet_peanutbutter","diet_herring_salted", "diet_fish_white_fried", "diet_fish_lean", 
             "diet_fish_fatty", "diet_fish_other","diet_margarine_lfb", "diet_fish_prepared_fat","diet_saladdressing_f","diet_saladdressing_w","diet_mayonaise","diet_mayonaise_snack",
             "diet_milk_whole", "diet_milk_semiskimmed", "diet_milk_skimmed", "diet_buttermilk","diet_yoghurt_drink_other", "diet_yoghurt_ff", "diet_yoghurt_lf","diet_milk_coffee",
             "diet_group_coffee","diet_group_tea","diet_sausage_smoked","diet_meat", "diet_meats_fat","diet_meats_other","diet_beef_fat","diet_beef_lean",
             "diet_pork_fat","diet_pork_lean","diet_pork_processed","diet_snack_meats","diet_butter_b", "diet_butter_ob","diet_sandwichspread","diet_gravy",
              "diet_softdrink_sugar", "diet_breakfast_drink", "diet_chocolatemilk","diet_fruitjuice","diet_beer_af","diet_yoghurt_drink_added_sugar","base_IBDconf","A_ID_LLDUMCGIBD")]
meta_clean=na.omit(meta)
meta_clean=subset(meta_clean,meta_clean$diet_SUMOFKCAL>=800 & meta_clean$diet_SUMOFKCAL<=5000)
meta_clean$Kcal <- meta_clean$diet_SUMOFKCAL/1000

meta_clean$vegetables <- (meta_clean$diet_vegetables_cooked_nobutter + meta_clean$diet_vegetables_stirfried + meta_clean$diet_vegetables_cooked_butter) / meta_clean$Kcal
veg.q01=quantile(meta_clean$vegetables, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
veg.q12=quantile(meta_clean$vegetables, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
veg.q23=quantile(meta_clean$vegetables, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
veg.q34=quantile(meta_clean$vegetables, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$vegetables.points <- ntile (meta_clean$vegetables, 5)-1
meta_clean$points.veg.check=ifelse(meta_clean$vegetables<=veg.q01,0,
                                   ifelse(meta_clean$vegetables>veg.q01&meta_clean$vegetables<=veg.q12,1,
                                          ifelse(meta_clean$vegetables>veg.q12&meta_clean$vegetables<=veg.q23,2,
                                                 ifelse(meta_clean$vegetables>veg.q23&meta_clean$vegetables<=veg.q34,3,4))))
table(meta_clean$points.veg.chec,meta_clean$vegetables.points) ## check if its the same as the original code

meta_clean$wholefruit <- meta_clean$diet_fruit / meta_clean$Kcal 
wf.q01=quantile(meta_clean$wholefruit, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
wf.q12=quantile(meta_clean$wholefruit, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
wf.q23=quantile(meta_clean$wholefruit, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
wf.q34=quantile(meta_clean$wholefruit, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$wholegrains <- meta_clean$diet_bread / meta_clean$Kcal 
wg.q01=quantile(meta_clean$wholegrains, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
wg.q12=quantile(meta_clean$wholegrains, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
wg.q23=quantile(meta_clean$wholegrains, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
wg.q34=quantile(meta_clean$wholegrains, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$legnut <- (meta_clean$diet_legumes + meta_clean$diet_nut_d + meta_clean$diet_snack_nut + meta_clean$diet_peanutbutter) / meta_clean$Kcal 
ln.q01=quantile(meta_clean$legnut, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
ln.q12=quantile(meta_clean$legnut, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
ln.q23=quantile(meta_clean$legnut, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
ln.q34=quantile(meta_clean$legnut, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$fish <- (meta_clean$diet_herring_salted + meta_clean$diet_fish_white_fried + meta_clean$diet_fish_lean + meta_clean$diet_fish_fatty + meta_clean$diet_fish_other + meta_clean$diet_fish_prepared_fat) / meta_clean$Kcal 
fish.q01=quantile(meta_clean$fish, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
fish.q12=quantile(meta_clean$fish, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
fish.q23=quantile(meta_clean$fish, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
fish.q34=quantile(meta_clean$fish, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$oilsmargarines<-meta_clean$diet_margarine_lfb + meta_clean$diet_saladdressing_f + meta_clean$diet_saladdressing_w + meta_clean$diet_mayonaise + meta_clean$diet_mayonaise_snack
meta_clean$oilsmargarines <- meta_clean$oilsmargarines / meta_clean$Kcal 
om.q01=quantile(meta_clean$oilsmargarines, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
om.q12=quantile(meta_clean$oilsmargarines, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
om.q23=quantile(meta_clean$oilsmargarines, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
om.q34=quantile(meta_clean$oilsmargarines, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$unsweetdairy <- meta_clean$diet_milk_whole + meta_clean$diet_milk_semiskimmed + meta_clean$diet_milk_skimmed + meta_clean$diet_buttermilk + 
  meta_clean$diet_yoghurt_drink_other + meta_clean$diet_yoghurt_ff + meta_clean$diet_yoghurt_lf + meta_clean$diet_milk_coffee
meta_clean$unsweetdairy <- meta_clean$unsweetdairy / meta_clean$Kcal 
ud.q01=quantile(meta_clean$unsweetdairy, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
ud.q12=quantile(meta_clean$unsweetdairy, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
ud.q23=quantile(meta_clean$unsweetdairy, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
ud.q34=quantile(meta_clean$unsweetdairy, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$coffee <- meta_clean$diet_group_coffee / meta_clean$Kcal 
cof.q01=quantile(meta_clean$coffee, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
cof.q12=quantile(meta_clean$coffee, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
cof.q23=quantile(meta_clean$coffee, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
cof.q34=quantile(meta_clean$coffee, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$tea <- meta_clean$diet_group_tea / meta_clean$Kcal 
tea.q01=quantile(meta_clean$tea, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
tea.q12=quantile(meta_clean$tea, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
tea.q23=quantile(meta_clean$tea, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
tea.q34=quantile(meta_clean$tea, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$redprocmeat <- meta_clean$diet_sausage_smoked + meta_clean$diet_meat + meta_clean$diet_meats_fat+ meta_clean$diet_meats_other+ meta_clean$diet_beef_fat+ meta_clean$diet_beef_lean + 
  meta_clean$diet_pork_fat + meta_clean$diet_pork_lean + meta_clean$diet_pork_processed + meta_clean$diet_snack_meats
meta_clean$redprocmeat <- meta_clean$redprocmeat / meta_clean$Kcal
rpm.q01=quantile(meta_clean$redprocmeat, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
rpm.q12=quantile(meta_clean$redprocmeat, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
rpm.q23=quantile(meta_clean$redprocmeat, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
rpm.q34=quantile(meta_clean$redprocmeat, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$buttermar <- meta_clean$diet_butter_b + meta_clean$diet_butter_ob + meta_clean$diet_sandwichspread + meta_clean$diet_gravy 
meta_clean$buttermar <- meta_clean$buttermar / meta_clean$Kcal 
bm.q01=quantile(meta_clean$buttermar, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
bm.q12=quantile(meta_clean$buttermar, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
bm.q23=quantile(meta_clean$buttermar, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
bm.q34=quantile(meta_clean$buttermar, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

meta_clean$sugarsweetbev <- meta_clean$diet_softdrink_sugar + meta_clean$diet_breakfast_drink + meta_clean$diet_chocolatemilk + meta_clean$diet_fruitjuice + meta_clean$diet_beer_af + meta_clean$diet_yoghurt_drink_added_sugar 
meta_clean$sugarsweetbev <- meta_clean$sugarsweetbev / meta_clean$Kcal 
ssb.q01=quantile(meta_clean$sugarsweetbev, prob=c(.2,.4,.6,.8), type=1,names=F)[1]
ssb.q12=quantile(meta_clean$sugarsweetbev, prob=c(.2,.4,.6,.8), type=1,names=F)[2]
ssb.q23=quantile(meta_clean$sugarsweetbev, prob=c(.2,.4,.6,.8), type=1,names=F)[3]
ssb.q34=quantile(meta_clean$sugarsweetbev, prob=c(.2,.4,.6,.8), type=1,names=F)[4]

rm(meta,meta_clean)
#### calculate score in DAG3 ---- 
dag=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull.csv")
dag=dag[,c("diet_vegetables_cooked_nobutter", "diet_vegetables_stirfried", "diet_vegetables_cooked_butter","diet_SUMOFKCAL","diet_fruit","diet_bread",
             "diet_legumes","diet_nut_d" ,"diet_snack_nut", "diet_peanutbutter","diet_herring_salted", "diet_fish_white_fried", "diet_fish_lean", 
             "diet_fish_fatty", "diet_fish_other","diet_margarine_lfb", "diet_fish_prepared_fat","diet_saladdressing_f","diet_saladdressing_w","diet_mayonaise","diet_mayonaise_snack",
             "diet_milk_whole", "diet_milk_semiskimmed", "diet_milk_skimmed", "diet_buttermilk","diet_yoghurt_drink_other", "diet_yoghurt_ff", "diet_yoghurt_lf","diet_milk_coffee",
             "diet_group_coffee","diet_group_tea","diet_sausage_smoked","diet_meat", "diet_meats_fat","diet_meats_other","diet_beef_fat","diet_beef_lean",
             "diet_pork_fat","diet_pork_lean","diet_pork_processed","diet_snack_meats","diet_butter_b", "diet_butter_ob","diet_sandwichspread","diet_gravy",
             "diet_softdrink_sugar", "diet_breakfast_drink", "diet_chocolatemilk","diet_fruitjuice","diet_beer_af","diet_yoghurt_drink_added_sugar","A_project_pseudo_id")]
dag_clean=na.omit(dag)
dag_clean=subset(dag_clean,dag_clean$diet_SUMOFKCAL>=800 & dag_clean$diet_SUMOFKCAL<=5000)
dag_clean$Kcal <- dag_clean$diet_SUMOFKCAL/1000

dag_clean$vegetables <- (dag_clean$diet_vegetables_cooked_nobutter + dag_clean$diet_vegetables_stirfried + dag_clean$diet_vegetables_cooked_butter) / dag_clean$Kcal
dag_clean$vegetables.points=ifelse(dag_clean$vegetables<=veg.q01,0,
                                   ifelse(dag_clean$vegetables>veg.q01&dag_clean$vegetables<=veg.q12,1,
                                          ifelse(dag_clean$vegetables>veg.q12&dag_clean$vegetables<=veg.q23,2,
                                                 ifelse(dag_clean$vegetables>veg.q23&dag_clean$vegetables<=veg.q34,3,4))))

dag_clean$wholefruit <- dag_clean$diet_fruit / dag_clean$Kcal 
dag_clean$fruit.points=ifelse(dag_clean$wholefruit<=wf.q01,0,
                                   ifelse(dag_clean$wholefruit>wf.q01&dag_clean$wholefruit<=wf.q12,1,
                                          ifelse(dag_clean$wholefruit>wf.q12&dag_clean$wholefruit<=wf.q23,2,
                                                 ifelse(dag_clean$wholefruit>wf.q23&dag_clean$wholefruit<=wf.q34,3,4))))

dag_clean$wholegrains <- dag_clean$diet_bread / dag_clean$Kcal 
dag_clean$wholegrains.points=ifelse(dag_clean$wholegrains<=wg.q01,0,
                                   ifelse(dag_clean$wholegrains>wg.q01&dag_clean$wholegrains<=wg.q12,1,
                                          ifelse(dag_clean$wholegrains>wg.q12&dag_clean$wholegrains<=wg.q23,2,
                                                 ifelse(dag_clean$wholegrains>wg.q23&dag_clean$wholegrains<=wg.q34,3,4))))

dag_clean$legnut <- (dag_clean$diet_legumes + dag_clean$diet_nut_d + dag_clean$diet_snack_nut + dag_clean$diet_peanutbutter) / dag_clean$Kcal 
dag_clean$legnuts.points=ifelse(dag_clean$legnut<=ln.q01,0,
                                    ifelse(dag_clean$legnut>ln.q01&dag_clean$legnut<=ln.q12,1,
                                           ifelse(dag_clean$legnut>ln.q12&dag_clean$legnut<=ln.q23,2,
                                                  ifelse(dag_clean$legnut>ln.q23&dag_clean$legnut<=ln.q34,3,4))))

dag_clean$fish <- (dag_clean$diet_herring_salted + dag_clean$diet_fish_white_fried + dag_clean$diet_fish_lean + dag_clean$diet_fish_fatty + dag_clean$diet_fish_other + dag_clean$diet_fish_prepared_fat) / dag_clean$Kcal 
dag_clean$fish.points=ifelse(dag_clean$fish<=fish.q01,0,
                               ifelse(dag_clean$fish>fish.q01&dag_clean$fish<=fish.q12,1,
                                      ifelse(dag_clean$fish>fish.q12&dag_clean$fish<=fish.q23,2,
                                             ifelse(dag_clean$fish>fish.q23&dag_clean$fish<=fish.q34,3,4))))

dag_clean$oilsmargarines<-dag_clean$diet_margarine_lfb + dag_clean$diet_saladdressing_f + dag_clean$diet_saladdressing_w + dag_clean$diet_mayonaise + dag_clean$diet_mayonaise_snack
dag_clean$oilsmargarines <- dag_clean$oilsmargarines / dag_clean$Kcal 
dag_clean$oilsmargarines.points=ifelse(dag_clean$oilsmargarines<=om.q01,0,
                             ifelse(dag_clean$oilsmargarines>om.q01&dag_clean$oilsmargarines<=om.q12,1,
                                    ifelse(dag_clean$oilsmargarines>om.q12&dag_clean$oilsmargarines<=om.q23,2,
                                           ifelse(dag_clean$oilsmargarines>om.q23&dag_clean$oilsmargarines<=om.q34,3,4))))

dag_clean$unsweetdairy <- dag_clean$diet_milk_whole + dag_clean$diet_milk_semiskimmed + dag_clean$diet_milk_skimmed + dag_clean$diet_buttermilk + 
  dag_clean$diet_yoghurt_drink_other + dag_clean$diet_yoghurt_ff + dag_clean$diet_yoghurt_lf + dag_clean$diet_milk_coffee
dag_clean$unsweetdairy <- dag_clean$unsweetdairy / dag_clean$Kcal 
dag_clean$unsweetdairy.points=ifelse(dag_clean$unsweetdairy<=ud.q01,0,
                                       ifelse(dag_clean$unsweetdairy>ud.q01&dag_clean$unsweetdairy<=ud.q12,1,
                                              ifelse(dag_clean$unsweetdairy>ud.q12&dag_clean$unsweetdairy<=ud.q23,2,
                                                     ifelse(dag_clean$unsweetdairy>ud.q23&dag_clean$unsweetdairy<=ud.q34,3,4))))

dag_clean$coffee <- dag_clean$diet_group_coffee / dag_clean$Kcal 
dag_clean$coffee.points=ifelse(dag_clean$coffee<=cof.q01,0,
                                     ifelse(dag_clean$coffee>cof.q01&dag_clean$coffee<=cof.q12,1,
                                            ifelse(dag_clean$coffee>cof.q12&dag_clean$coffee<=cof.q23,2,
                                                   ifelse(dag_clean$coffee>cof.q23&dag_clean$coffee<=cof.q34,3,4))))

dag_clean$tea <- dag_clean$diet_group_tea / dag_clean$Kcal 
dag_clean$tea.points=ifelse(dag_clean$tea<=tea.q01,0,
                               ifelse(dag_clean$tea>tea.q01&dag_clean$tea<=tea.q12,1,
                                      ifelse(dag_clean$tea>tea.q12&dag_clean$tea<=tea.q23,2,
                                             ifelse(dag_clean$tea>tea.q23&dag_clean$tea<=tea.q34,3,4))))

dag_clean$redprocmeat <- dag_clean$diet_sausage_smoked + dag_clean$diet_meat + dag_clean$diet_meats_fat+ dag_clean$diet_meats_other+ dag_clean$diet_beef_fat+ dag_clean$diet_beef_lean + 
  dag_clean$diet_pork_fat + dag_clean$diet_pork_lean + dag_clean$diet_pork_processed + dag_clean$diet_snack_meats
dag_clean$redprocmeat <- dag_clean$redprocmeat / dag_clean$Kcal
dag_clean$redprocmeat.points=ifelse(dag_clean$redprocmeat<=rpm.q01,0,
                            ifelse(dag_clean$redprocmeat>rpm.q01&dag_clean$redprocmeat<=rpm.q12,1,
                                   ifelse(dag_clean$redprocmeat>rpm.q12&dag_clean$redprocmeat<=rpm.q23,2,
                                          ifelse(dag_clean$redprocmeat>rpm.q23&dag_clean$redprocmeat<=rpm.q34,3,4))))

dag_clean$buttermar <- dag_clean$diet_butter_b + dag_clean$diet_butter_ob + dag_clean$diet_sandwichspread + dag_clean$diet_gravy 
dag_clean$buttermar <- dag_clean$buttermar / dag_clean$Kcal 
dag_clean$buttermar.points=ifelse(dag_clean$buttermar<=bm.q01,0,
                                    ifelse(dag_clean$buttermar>bm.q01&dag_clean$buttermar<=bm.q12,1,
                                           ifelse(dag_clean$buttermar>bm.q12&dag_clean$buttermar<=bm.q23,2,
                                                  ifelse(dag_clean$buttermar>bm.q23&dag_clean$buttermar<=bm.q34,3,4))))

dag_clean$sugarsweetbev <- dag_clean$diet_softdrink_sugar + dag_clean$diet_breakfast_drink + dag_clean$diet_chocolatemilk + dag_clean$diet_fruitjuice + dag_clean$diet_beer_af + dag_clean$diet_yoghurt_drink_added_sugar 
dag_clean$sugarsweetbev <- dag_clean$sugarsweetbev / dag_clean$Kcal 
dag_clean$sugarsweetbev.points=ifelse(dag_clean$sugarsweetbev<=ssb.q01,0,
                                  ifelse(dag_clean$sugarsweetbev>ssb.q01&dag_clean$sugarsweetbev<=ssb.q12,1,
                                         ifelse(dag_clean$sugarsweetbev>ssb.q12&dag_clean$sugarsweetbev<=ssb.q23,2,
                                                ifelse(dag_clean$sugarsweetbev>ssb.q23&dag_clean$sugarsweetbev<=ssb.q34,3,4))))

dag_clean$diet_LLDS = dag_clean$vegetables.points + dag_clean$fruit.points + dag_clean$wholegrains.points + dag_clean$legnuts.points + dag_clean$fish.points + dag_clean$oilsmargarines.points + dag_clean$unsweetdairy.points + dag_clean$coffee.points + dag_clean$tea.points + dag_clean$redprocmeat.points + dag_clean$buttermar.points + dag_clean$sugarsweetbev.points

#### env score in DAG3 ---- 
dag=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull.csv")
dag$envscore_u15_sel_weigh=log(0.74)*dag$env_breastfeeding_binomial + log(0.76)*dag$env_petsu15 + log(1.35)*dag$env_urbanicitychild + log(1.31)*dag$env_HCP

    # recategorize based on 1 = high risk, 0 = low risk
dag$env_breastfeeding_binomial <- ifelse(dag$env_breastfeeding_binomial == 1, 0, 1)
dag$env_petsu15 <- ifelse(dag$env_petsu15 == 1, 0, 1)

dag$envscore_u15_sel_unweigh=dag$env_breastfeeding_binomial + dag$env_petsu15 + dag$env_urbanicitychild + dag$env_HCP
dag3_envscores=dag[c("A_project_pseudo_id","envscore_u15_sel_weigh","envscore_u15_sel_unweigh")]

#write.csv(dag3_envscores,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_envscores.csv",row.names=F)
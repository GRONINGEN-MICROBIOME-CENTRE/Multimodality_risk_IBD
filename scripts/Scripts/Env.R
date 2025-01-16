library(dplyr)
library(readxl)

#### create LLD environmental file ----
    # read dataframes
env1=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_1_results.csv")
env2=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_2_results.csv")

    # select columns of interest
env1=env1[,c(1,3,4,5,6,28,29)]
env2=env2[,c(1,10,12,16,23,297)]

    # merge with LLD
LLD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLD_casecontrol.csv")
env=merge(env1, env2, by="project_pseudo_id")
LLDenv=merge(LLD,env,by="project_pseudo_id")
write.csv(LLDenv, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLD_env.csv")

#### create DAG3 environmental file ----
    # read dataframes
env1=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_2_results.csv")
env2=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/2a_q_1_results.csv")
env3=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/DAG3_dataset_order_202404/results/dag3_q_1_results.csv")
    
    # select columns of interest  
env1=env1[,c(1,10,12,16,23,297)] 
env2=env2[,c(1:6,32,33)]
env3=env3[,c(1,57:61)]

    # merge with DAG3
env=merge(env1, env2, by="project_pseudo_id", all=T)
env=merge(env, env3, by="project_pseudo_id", all=T)
DAG3=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/DAG3_casecontrol.csv")
DAG3env=merge(env, DAG3, by="project_pseudo_id", all.y=T)
DAG3env=DAG3env[,-19] # remove "X" column
write.csv(DAG3env, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/DAG3env.csv")

#### add urbanicity based on neigbourhood code, combine pets, and make breastfeeding binomial, add HCP ----
    # read df
LLDenv=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLD_env_with_menu.csv")
kwb12=read_xls("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/kwb-2012.xls")
kwb12=kwb12[,c(2,11)]

    # merge neigbourhood code with urbanicity
LLDenv$neighbourhood_at_sampling_2012 <- sub("^BU", "", LLDenv$neighbourhood_at_sampling_2012)
LLDenv=merge(LLDenv,kwb12,by.x="neighbourhood_at_sampling_2012", by.y="GWB_CODE12", all.x=T)

    # find missing rows and manually substitute urbanicity
LLDenv[is.na(LLDenv$STED),c(3,7,18)] 

LLDenv[1415,23]<-"1"
LLDenv[1416,23]<-"4"
LLDenv[1417,23]<-"2"
LLDenv[1418,23]<-"4"
LLDenv[1419,23]<-"1"
LLDenv[1420,23]<-"4"
LLDenv[1421,23]<-"4"
LLDenv[1422,23]<-"1" #2013
LLDenv[1423,23]<-"3" #2011

    # change urbanicity categories
LLDenv$urbanicity <- ifelse(LLDenv$STED %in% 1:3, 1, 0)

    # combine pets variables into one
cols_to_modify <- c("guineapig_presence_adu_q_1", "cat_presence_adu_q_1", "dog_presence_adu_q_1", "bird_presence_adu_q_1")
LLDenv[cols_to_modify] <- lapply(LLDenv[cols_to_modify], function(x) replace(x, x == "$6", NA))
LLDenv$pets=ifelse(LLDenv$guineapig_presence_adu_q_1 == 1 | LLDenv$cat_presence_adu_q_1 == 1 | LLDenv$dog_presence_adu_q_1 == 1 | LLDenv$bird_presence_adu_q_1 == 1, 1, 0)

    # make breastfeeding binominal
LLDenv$breastfeeding_duration_adu_q_1 <- ifelse(LLDenv$breastfeeding_duration_adu_q_1 == 9 | LLDenv$breastfeeding_duration_adu_q_1 == "$6", NA, LLDenv$breastfeeding_duration_adu_q_1)
LLDenv$breastfeeding_binomial <- ifelse(is.na(LLDenv$breastfeeding_duration_adu_q_1), NA, ifelse(LLDenv$breastfeeding_duration_adu_q_1 == 1, 0, 1))

    # make bedpartner
LLDenv$inhouse_partner_adu_q_1 <- ifelse(LLDenv$inhouse_partner_adu_q_1 == "$6", NA, LLDenv$inhouse_partner_adu_q_1) 
LLDenv$bedpartner <- ifelse(is.na(LLDenv$inhouse_partner_adu_q_1), NA, ifelse(LLDenv$inhouse_partner_adu_q_1 == 1, 1, 0))

    # make housemates
LLDenv$inhouse_alone_adu_q_1 <- ifelse(LLDenv$inhouse_alone_adu_q_1 == "$6", NA, LLDenv$inhouse_alone_adu_q_1) 
LLDenv$housemates<- ifelse(is.na(LLDenv$inhouse_alone_adu_q_1), NA, ifelse(LLDenv$inhouse_alone_adu_q_1 == 1, 0, 1))

    # remove redundant columns and save
write.csv(LLDenv, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDenv_may31.csv")

    # add HCP use (was forgotten)
LLDenv=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDenv_may31.csv")
env1=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_1_results.csv")
env1=env1[,c(1,24)]
LLDenv=merge(LLDenv,env1,by="project_pseudo_id")
LLDenv$HCP <- ifelse(LLDenv$hormonal_contraception_adu_q_1 == "$6" & LLDenv$gender == "FEMALE", NA, ifelse(LLDenv$hormonal_contraception_adu_q_1 == 1, 1, 0)) 

write.csv(LLDenv, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDenv_june5.csv")

#### create 1000IBD environmental file ----
    # load in dataframe
GIEQ=read_xlsx("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQfull.xlsx")
GIEQ=GIEQ[,c(1,7:10,12,27)]

    # redefine housemates
GIEQ$housemates= ifelse(is.na(GIEQ$Housemates), NA, ifelse(GIEQ$Housemates== 0, 0, 1))

    # redefine urbanicity
GIEQ$urbanicity=ifelse(is.na(GIEQ$Woonplaats), NA, ifelse(GIEQ$Woonplaats== 0, 0, 1))

    # redefine breastfeeding_binomial
GIEQ$BIRTH151 = ifelse(GIEQ$BIRTH151 == 9, NA, GIEQ$BIRTH151)
GIEQ$breastfeeding_binomial = ifelse(is.na(GIEQ$BIRTH151), NA, ifelse(GIEQ$BIRTH151 == 1, 0, 1))

    # rewrite pets and bedpartner
names(GIEQ)[names(GIEQ) == "PetsNow"] <- "pets_all"
names(GIEQ)[names(GIEQ) == "Bedpartner"] <- "bedpartner"

    # add pets big 4
big4=read_xlsx("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQextra.xlsx", sheet = 2)
big4=big4[,c(1,14:25)]
big4=as.data.frame(big4[rowSums(is.na(big4[,-1])) != ncol(big4[,-1]), ]) #remove individuals with all NA
big4$pets_4 <- ifelse(big4$Now_Dxcat == 1 | big4$Now_Dxdog == 1 | big4$Now_Dxbird == 1 | big4$Now_Dxcavia == 1, 1, 0)
big4$pets_4[is.na(big4$pets_4)] <- 0

GIEQ=merge(GIEQ, big4[,c(1,2:5,14)], by = "GIEQID", all.x = T)

    # add sex and diagnosis
sex=read_xlsx("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQextra.xlsx", sheet = 1)
GIEQ=merge(GIEQ, sex[,-2], by = "GIEQID")

    # make HCP
  # replace NA in men with 0
GIEQ$HCP = ifelse(is.na(GIEQ$FEM81) & GIEQ$Gender == "Male", 0, ifelse(GIEQ$FEM81 == 1 | GIEQ$FEM81 == 0, GIEQ$FEM81, NA))

    # save
write.csv(GIEQ, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQenv_june25.csv")

#### combine LLD and GIEQ ----
    # read dataframes
GIEQ=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQenv_june25.csv")
LLD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDenv_june5.csv")

    # add case definition to GIEQ and select columns of interest
GIEQ$IBD <- "1"
GIEQ=GIEQ[,c(2,5,9:11,16,19,20)]
LLD=LLD[,c(2,8,26:30,32)]
names(GIEQ)[names(GIEQ) == "pets_4"] <- "pets"
LLD=LLD[,c("project_pseudo_id",  "bedpartner", "housemates", "urbanicity", "breastfeeding_binomial","pets", "HCP","IBD")]
names(GIEQ)[names(GIEQ) == "GIEQID"] <- "ID"
names(LLD)[names(LLD) == "project_pseudo_id"] <- "ID"

    # bind dataframes
GIEQLLD=rbind(GIEQ, LLD)

# save
write.csv(GIEQLLD, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQLLD_big4.csv")

#### perform environment tests ----
GIEQLLD<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQLLD_big4.csv")

GIEQLLD$envscore=log(0.74)*GIEQLLD$breastfeeding_binomial + log(0.76)*GIEQLLD$pets + log(0.53)*GIEQLLD$housemates + log(0.66)*GIEQLLD$bedpartner + log(1.35)*GIEQLLD$urbanicity + log(1.31)*GIEQLLD$HCP

GIEQLLD$IBD[GIEQLLD$IBD==1]<-"IBD"
GIEQLLD$IBD[GIEQLLD$IBD==0]<-"Non-IBD"

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_big4.pdf")
library(ggplot2)
ggplot(GIEQLLD, aes(x = IBD, y = envscore)) +
  geom_boxplot()+geom_violin(alpha=0.7)
dev.off()

median(GIEQLLD$envscore[GIEQLLD$IBD=='IBD'], na.rm=T)
median(GIEQLLD$envscore[GIEQLLD$IBD=='Non-IBD'], na.rm=T)
table(GIEQLLD[!is.na(GIEQLLD$envscore),9])

wilcox.test(envscore~IBD,data=GIEQLLD)

#### separate UC/CD table prep ----
    # add metadata to LLD df
LLD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLDenv_june5.csv")
meta_1a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_1_results.csv")
LLDCDUC=merge(LLD[,-c(1,3,5,6)],meta_1a[,c(1,17,20,21,36)],by="project_pseudo_id")
meta_1a2=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_2_results.csv")
LLDCDUC=merge(LLDCDUC, meta_1a2[,c(1,11,17,323,324)], by="project_pseudo_id")
meta_1b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1b_q_1_results.csv")
LLDCDUC=merge(LLDCDUC, meta_1b[,c(1,8,9,17,18)], by="project_pseudo_id", all.x=T)
rm(meta_1b, meta_1a2, meta_1a,LLD)
    # create UC and CD variables LLD (excluding people who said have both)
LLDCDUC$UC <- ifelse(LLDCDUC$crohns_presence_adu_q_1 == 1, NA, ifelse(LLDCDUC$colitisulcerosa_presence_adu_q_1 == 1, 1, 0))
LLDCDUC$CD <- ifelse(LLDCDUC$colitisulcerosa_presence_adu_q_1 == 1, NA, ifelse(LLDCDUC$crohns_presence_adu_q_1 == 1, 1, 0))
    # create and rename variables of interest LLD
LLDCDUC$currentsmoking=ifelse(LLDCDUC$current_smoker_adu_c_2 == "$6", NA, LLDCDUC$current_smoker_adu_c_2)
LLDCDUC$totalactivityscore = ifelse(LLDCDUC$squashsum_reliability_adu_c_2 == 2 | LLDCDUC$squashsum_mvpascore_adu_c_1 == 0, NA,LLDCDUC$squashsum_mvpascore_adu_c_1)
LLDCDUC$sisters=ifelse(LLDCDUC$sisters_number_fam_q_1_a == 1, NA, ifelse(LLDCDUC$sisters_number_fam_q_1 == "$6", 0, LLDCDUC$sisters_number_fam_q_1))
LLDCDUC$brothers=ifelse(LLDCDUC$brothers_number_adu_q_1_a == 1, NA, ifelse(LLDCDUC$brothers_number_adu_q_1 == "$6", 0, LLDCDUC$brothers_number_adu_q_1))
LLDCDUC$siblings <- as.numeric(LLDCDUC$brothers) + as.numeric(LLDCDUC$sisters)
LLDCDUC$siblingstwoormore <- ifelse(LLDCDUC$siblings >= 2, 1, 0)
LLDCDUC$Csection=ifelse(LLDCDUC$birth_type_adu_q_1 == 4, NA, ifelse(LLDCDUC$birth_type_adu_q_1 == 3, 1,ifelse(LLDCDUC$birth_type_adu_q_1 == 1 | LLDCDUC$birth_type_adu_q_1 == 2, 0, NA)))
LLDCDUC$appendectomy=ifelse(LLDCDUC$appendectomy_lifetime_adu_q_1 == "$6", 0, LLDCDUC$appendectomy_lifetime_adu_q_1)
LLDCDUC$tonsillectomy=ifelse(LLDCDUC$tonsillectomy_lifetime_adu_q_1 == "$6", 0, LLDCDUC$tonsillectomy_lifetime_adu_q_1)
names(LLDCDUC)[names(LLDCDUC) == "project_pseudo_id"] <- "ID"
    # select cols of interest LLD
LLDCDUC=LLDCDUC[,c(1,7,22:26,28,41:44,48:51)]

    # add metadata to GIEQ df
GIEQ=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQenv_june25.csv")
extra=read_xlsx("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/GIEQfull.xlsx")
GIEQCDUC=merge(GIEQ[,-1], extra[,c(1:3,14:17,11,18,19)], by = "GIEQID")
rm(extra,GIEQ)
    # create UC and CD variables GIEQ
GIEQCDUC$UC=ifelse(GIEQCDUC$Diagnosis == "UC", 1, NA)
GIEQCDUC$CD=ifelse(GIEQCDUC$Diagnosis == "CD", 1, NA)
    # create and rename variables of interest LLD
GIEQCDUC$currentsmoking=ifelse(GIEQCDUC$Smokingstatus == 1, 0, ifelse(GIEQCDUC$Smokingstatus==2, 1, GIEQCDUC$Smokingstatus))
GIEQCDUC$totalactivityscore = ifelse(GIEQCDUC$PA_totalactivityscore == 0, NA,GIEQCDUC$PA_totalactivityscore)
GIEQCDUC$brothers = ifelse(GIEQCDUC$SIBSB1 == 0 & is.na(GIEQCDUC$BROTHERS1), 0, GIEQCDUC$BROTHERS1)
GIEQCDUC$sisters = ifelse(GIEQCDUC$SIBSZ1 == 0 & is.na(GIEQCDUC$SISTERS1), 0, GIEQCDUC$SISTERS1)
GIEQCDUC$siblings <- as.numeric(GIEQCDUC$brothers) + as.numeric(GIEQCDUC$sisters)
GIEQCDUC$siblingstwoormore <- ifelse(GIEQCDUC$siblings >= 2, 1, 0)
GIEQCDUC$Csection=ifelse(GIEQCDUC$BIRTH141 == 3, 1,ifelse(GIEQCDUC$BIRTH141 == 1 | GIEQCDUC$BIRTH141 == 2, 0, NA))
GIEQCDUC$appendectomy=ifelse(GIEQCDUC$HEALTH1261 == 2, NA, GIEQCDUC$HEALTH1261)
GIEQCDUC$tonsillectomy=ifelse(GIEQCDUC$HEALTH1251 == 2, NA, GIEQCDUC$HEALTH1251)
names(GIEQCDUC)[names(GIEQCDUC) == "pets_4"] <- "pets"
names(GIEQCDUC)[names(GIEQCDUC) == "Gender"] <- "gender"
names(GIEQCDUC)[names(GIEQCDUC) == "GIEQID"] <- "ID"

GIEQCDUC=GIEQCDUC[,c(1,4,8:10,15,16,18,28:31,35:38)]

    # combine GIEQ and LLD
GIEQCDUC=GIEQCDUC[,order(colnames(GIEQCDUC))]
LLDCDUC=LLDCDUC[,order(colnames(LLDCDUC))]
EnvCDUC=rbind(GIEQCDUC, LLDCDUC)

write.csv(EnvCDUC, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/EnvCDUC_june26.csv")

#### separate UC/CD calculations ----
EnvCDUC=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/EnvCDUC_june26.csv")

    # CD
    #Filter CD and CD controls
EnvCD=filter(EnvCDUC, !is.na(EnvCDUC$CD))
    # make activity categorical
median=median(EnvCD$totalactivityscore, na.rm = T)
EnvCD$activitycat=ifelse(EnvCD$totalactivityscore >= median, 1, 0)

    # calculate score, run test and plot
EnvCD$CDenvscore=log(1.76)*EnvCD$currentsmoking+log(0.63)*EnvCD$activitycat+log(0.71)*EnvCD$breastfeeding_binomial + log(0.77)*EnvCD$pets + log(0.49)*EnvCD$housemates +log(0.54)*EnvCD$bedpartner + log(0.93)*EnvCD$siblingstwoormore+ log(1.42)*EnvCD$urbanicity + log(1.38)*EnvCD$Csection + log(1.61)*EnvCD$appendectomy + log(1.37)*EnvCD$tonsillectomy+ log(1.25)*EnvCD$HCP

EnvCD$CD[EnvCD$CD==1]<-"CD"
EnvCD$CD[EnvCD$CD==0]<-"NoCD"

median(EnvCD$CDenvscore[EnvCD$CD=="CD"], na.rm=T)
median(EnvCD$CDenvscore[EnvCD$CD=='NoCD'], na.rm=T)
table(EnvCD[!is.na(EnvCD$CDenvscore),5])

wilcox.test(CDenvscore~CD,data=EnvCD)

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_CD.pdf")
library(ggplot2)
ggplot(EnvCD, aes(x = CD, y = CDenvscore)) +
  geom_boxplot()+geom_violin(alpha=0.7)
dev.off()

    # UC
    #Filter UC and UC controls
EnvUC=filter(EnvCDUC, !is.na(EnvCDUC$UC))
    #Calculate score, run test and plot
EnvUC$UCenvscore=log(0.58)*EnvUC$currentsmoking+log(0.78)*EnvUC$breastfeeding_binomial + log(0.75)*EnvUC$pets +log(0.53)*EnvUC$bedpartner + log(1.17)*EnvUC$urbanicity + log(0.39)*EnvUC$appendectomy + log(1.28)*EnvUC$HCP

EnvUC$UC[EnvUC$UC==1]<-"UC"
EnvUC$UC[EnvUC$UC==0]<-"NoUC"
table(EnvUC[!is.na(EnvUC$UCenvscore),16])

wilcox.test(UCenvscore~UC,data=EnvUC)

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_UC.pdf")
library(ggplot2)
ggplot(EnvUC, aes(x = UC, y = UCenvscore)) +
  geom_boxplot()+geom_violin(alpha=0.7)
dev.off()

#### junk ----
df <- data.frame(
  col1 = c(1, 1, 2, 2, NA, NA, 2),
  col2 = c(2, 2, 1, 2, 1, 2, 2),
  col3 = c(2, 1, 2, 2, 2, 2, 2),
  col4 = c(2, 2, 2, 1, 1, 2, 2)
)

ifelse(df$col1 == 1 | df$col2 == 1 | df$col3 == 1 | df$col4 == 1, 1, 0)
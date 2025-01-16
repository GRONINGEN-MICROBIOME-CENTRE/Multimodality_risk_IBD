library(dplyr)

train=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
train=train[,-1]
gen=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/genetics_GIEQ_ids.csv")
gen <- gen[, ! names(gen) == "Diagnosis", drop = F]

# add missing participants
gen$IBDcohort <- 1
names(gen)[names(gen) == "V2"] <- "A_ID_LLDUMCGIBD"
names(gen)[names(gen) == "GIEQID"] <- "A_GIEQID"
gen_noNA=filter(gen,!is.na(gen$A_GIEQID))
train_add=merge(train,gen_noNA,by="A_GIEQID",all=T)
nrow(train)
#[1] 2562
nrow(train_add)
#[1] 2562 #GIEQ metadata added, no rows extra
train_add$A_ID_LLDUMCGIBD=ifelse(is.na(train_add$A_ID_LLDUMCGIBD.x),train_add$A_ID_LLDUMCGIBD.y,train_add$A_ID_LLDUMCGIBD.x)
gen_NA=filter(gen,is.na(gen$A_GIEQID))
train_adds=merge(train_add,gen_NA[,-1],by="A_ID_LLDUMCGIBD",all=T)
nrow(train_adds)
#[1] 2908 #all genetics added

# combine added columns
colnames(train_adds)
train_adds$Gender=ifelse(is.na(train_adds$Gender.x),train_adds$Gender.y,train_adds$Gender.x)
train_adds$base_sex=ifelse(is.na(train_adds$base_sex),train_adds$Gender,train_adds$base_sex)
train_adds$Age_GIEQ=ifelse(is.na(train_adds$Age_GIEQ.x),train_adds$Age_GIEQ.y,train_adds$Age_GIEQ.x)
train_adds$base_AgeEnvironment=ifelse(is.na(train_adds$base_AgeEnvironment),train_adds$Age_GIEQ,train_adds$base_AgeEnvironment)
train_adds$IBDcohort=ifelse(is.na(train_adds$IBDcohort.x),train_adds$IBDcohort.y,train_adds$IBDcohort.x)
train_adds$base_IBD=ifelse(is.na(train_adds$base_IBD),train_adds$IBDcohort,train_adds$base_IBD)
train_adds$base_IBDconf=ifelse(is.na(train_adds$base_IBDconf),train_adds$IBDcohort,train_adds$base_IBDconf)
train_adds$base_sex[train_adds$base_sex =="Female"] <- "FEMALE"
train_adds$base_sex[train_adds$base_sex =="Male"] <- "MALE"

# add in (new) scores
prs=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/prs_scores.csv")
colnames(prs)=c("score_prs_2015","Diagnosis","research_id")
final=merge(train_adds,prs[,-2],by.x="A_ID_LLDUMCGIBD",by.y="research_id",all.x=T)

microbiome=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/Microbiome_scores_log.csv")
names(microbiome)[names(microbiome) == "IBD_score"] <- "score_microbiome_log"
final=merge(final,microbiome[,-c(1,3)],by.x="A_ID_LLDUMCGIBD",by.y="umcg_id",all.x=T)

# add in additional MGS metadata
library(readxl)
MGS=read_xlsx("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/AdditionalMetadataMGS.xlsx")
MGS=MGS[,c(1,2,10,11,12,13)]
colnames=colnames(MGS)
colnames[2:6]<- paste0("base_", colnames[2:6])
colnames(MGS) <- colnames
names(MGS)[names(MGS) == "base_BMI"] <- "base_BMIatFecalSampling"
final=merge(final,MGS,by.x="A_ID_LLDUMCGIBD",by.y="UMCGIBDResearchID",all.x=T)

# add lifelines BMI
llBMI=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/BMI_dataset_order_202409/results/1a_v_1_results.csv")
llBMI$heightmeters=llBMI$bodylength_cm_all_m_1/100
llBMI$base_BMIatFecalSampling=llBMI$bodyweight_kg_all_m_1/(llBMI$heightmeters*llBMI$heightmeters)
final=merge(final,llBMI[,c(1,12)],by.x="A_project_pseudo_id",by.y="project_pseudo_id",all.x=T)
final$base_BMIatFecalSampling=ifelse(is.na(final$base_BMIatFecalSampling.x),final$base_BMIatFecalSampling.y,final$base_BMIatFecalSampling.x)

# rename and remove columns and reorder
names(final)[names(final) == "microbiome_shannon"] <- "score_microbiome_shannon"
names(final)[names(final) == "envscore_u15_sel_unweigh"] <- "score_env_u15_sel_unweigh"
names(final)[names(final) == "envscore_u15_sel_weigh"] <- "score_env_u15_sel_weigh"
names(final)[names(final) == "diet_aMED"] <- "score_diet_aMED"
names(final)[names(final) == "diet_LLDS"] <- "score_diet_LLDS"

final <- select(final, -c("microbiome_score","PRS_score","IBDcohort","Gender.x","Age_GIEQ.x","IBDcohort.x",
                          "Gender.y","Age_GIEQ.y","IBDcohort.y","Gender","Age_GIEQ","A_ID_LLDUMCGIBD.y",
                          "base_BMIatFecalSampling.y","base_BMIatFecalSampling.x","A_ID_LLDUMCGIBD.x"))

final=final[,order(colnames(final))]

# remove GIEQID with uncertain matching
final2=filter(final,!final$A_GIEQID %in% c('10017', '10018')) #IDs known to A. Bangma
nrow(final2)
#[1] 2906

# put NA in microbiome scores for "not OK for MGS'
final2$base_OKforMetagenomicAnalysis[is.na(final2$base_OKforMetagenomicAnalysis)] <- "yes"
final2$score_microbiome_log=ifelse(final2$base_OKforMetagenomicAnalysis == "no",NA,final2$score_microbiome_log)
final2$score_microbiome_shannon=ifelse(final2$base_OKforMetagenomicAnalysis == "no",NA,final2$score_microbiome_shannon)
#write.csv(final2,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull_v2.csv",row.names=F)


library(dplyr)

meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull.csv")

# merge in LLDS
LLDS=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_LLDS.csv")
names(LLDS)[names(LLDS) == "diet_LLDS"] <- "score_diet_LLDS"
meta=merge(meta,LLDS[,-1],by="A_project_pseudo_id",all.x=T)

# merge in env
env=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_envscores.csv")
names(env)[names(env) == "envscore_u15_sel_weigh"] <- "score_env_u15_sel_weigh"
names(env)[names(env) == "envscore_u15_sel_unweigh"] <- "score_env_u15_sel_unweigh"
meta=merge(meta,env,by="A_project_pseudo_id")

# merge in microbiome
micro=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/DAG3_microbiome_scores_log.csv")
names(micro)[names(micro) == "IBD_score"] <- "score_microbiome_log"
meta=merge(meta,micro,by.x="A_DAG3_sampleID",by.y="DAG3_ids",all.x=T)
meta=merge(meta,micro,by.x="A_DAG3_sampleID_additional",by.y="DAG3_ids",all.x=T)
meta=merge(meta,micro,by.x="A_DAG3_sampleID_duplicate",by.y="DAG3_ids",all.x=T)
meta$score_microbiome_log=ifelse(is.na(meta$score_microbiome_log.x), meta$score_microbiome_log,meta$score_microbiome_log.x)
meta$score_microbiome_log=ifelse(is.na(meta$score_microbiome_log), meta$score_microbiome_log.y,meta$score_microbiome_log)
meta <- select(meta, -c("score_microbiome_log.x","score_microbiome_log.y"))

# merge in genetics
gen=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/DAG3_PRS_2015.csv")
link=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/linkage_files_202404/gsa_linkage_file_OV23_00813.csv")
genlink=merge(gen,link[,-2],by.x="research_id",by.y="UGLI_ID")
colnames(genlink)=c("A_UGLI_ID","score_prs_2015","A_project_pseudo_id")
meta=merge(meta,genlink,by="A_project_pseudo_id",all.x=T)

# add BMI
llBMI=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/BMI_dataset_order_202409/results/2a_v_1_results.csv")
llBMI$heightmeters=llBMI$bodylength_cm_all_m_1/100
llBMI$base_BMIatFecalSampling=llBMI$bodyweight_kg_all_m_1/(llBMI$heightmeters*llBMI$heightmeters)
meta=merge(meta,llBMI[,c(1,12)],by.x="A_project_pseudo_id",by.y="project_pseudo_id",all.x=T)

meta=meta[,order(colnames(meta))]
#write.csv(meta,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull_v2.csv",row.names=F)

# merge in age at fecal sampling
  # microbiome score was changed between v2 and v3
meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull_v3.csv")
age=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/2a_q_1_results.csv")
age2=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/DAG3_dataset_order_202404/results/dag3_d_1_results.csv")
age3=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/2a_q_2_results.csv")
age <- select(age, c("project_pseudo_id","age"))
age2 <- select(age2, c("project_pseudo_id","age"))
age3 <- select(age3, c("project_pseudo_id","age"))
names(age)<- c("A_project_pseudo_id","base_AgeFecal_2a1")
names(age2)<- c("A_project_pseudo_id","base_AgeFecal_dag3")
names(age3)<- c("A_project_pseudo_id","base_AgeFecal_2a2")
meta2=merge(meta,age,by="A_project_pseudo_id",all.x=T)
meta2=merge(meta2,age2,by="A_project_pseudo_id",all.x=T)
meta2=merge(meta2,age3,by="A_project_pseudo_id",all.x=T)
meta2$base_AgeFecal <- ifelse(is.na(meta2$base_AgeFecal_dag3),meta2$base_AgeFecal_2a1,meta2$base_AgeFecal_dag3)
meta2$base_AgeFecal <- ifelse(is.na(meta2$base_AgeFecal),meta2$base_AgeFecal_2a2,meta2$base_AgeFecal)
meta2 <- select(meta2, -c("base_AgeFecal_dag3","base_AgeFecal_2a1","base_AgeFecal_2a2"))

meta2=meta2[,order(colnames(meta2))]

write.csv(meta2,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull_v4.csv",row.names=F)

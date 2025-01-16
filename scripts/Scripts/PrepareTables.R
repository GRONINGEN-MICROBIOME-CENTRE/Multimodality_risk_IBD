library(dplyr)

#### create LLD case control ----
    # load in dataframes
LLD=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/linkage_files_202404/deep_linkage_file_v2_OV23_00813.csv")
meta_1a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_1_results.csv")
meta_2a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/2a_q_1_results.csv")
meta_1b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/1b_q_1_results.csv")
meta_1c=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/1c_q_1_results.csv")
meta_3a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/3a_q_1_results.csv")
meta_3b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/3b_q_1_results.csv")

nrow(meta_1a)
#[1] 8942
nrow(meta_1b)
#[1] 8221
nrow(meta_1c)
#[1] 6863
nrow(meta_2a)
#[1] 7635
nrow(meta_3a)
#[1] 5601
nrow(meta_3b)
#[1] 2204

    # identify individuals that said developed IBD after 1a
meta_1b$IBD_1b=ifelse(meta_1b$colitisulcerosa_followup_adu_q_1 == 1 | meta_1b$crohns_followup_adu_q_1 == 1, 1, 0)
meta_1c$IBD_1c=ifelse(meta_1c$colitisulcerosa_followup_adu_q_1 == 1 | meta_1c$crohns_followup_adu_q_1 == 1, 1, 0)
meta_2a$IBD_2a=ifelse(meta_2a$colitisulcerosa_followup_adu_q_1 == 1 | meta_2a$crohns_followup_adu_q_1 == 1, 1, 0)
meta_3a$IBD_3a=ifelse(meta_3a$colitisulcerosa_followup_adu_q_1 == 1 | meta_3a$crohns_followup_adu_q_1 == 1, 1, 0)
meta_3b$IBD_3b=ifelse(meta_3b$colitisulcerosa_followup_adu_q_1 == 1 | meta_3b$crohns_followup_adu_q_1 == 1, 1, 0)

meta=Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "project_pseudo_id", all = TRUE),
            list(meta_1b[,c(1,9)], meta_1c[,c(1,9)], meta_2a[,c(1,36)],meta_3a[,c(1,9)],meta_3b[,c(1,9)]))
meta[is.na(meta)] <- 999
meta$IBD_followup=ifelse(meta$IBD_1b == 1 | meta$IBD_1c == 1 | meta$IBD_2a == 1| meta$IBD_3a == 1|meta$IBD_3b == 1, 1, 0)

    # indentify individuals that said to have IBD at 1a
meta_1a=meta_1a[,c(1,20,21)]
meta_1a$IBD_LLDbaseline=ifelse(meta_1a$colitisulcerosa_presence_adu_q_1 == 1 | meta_1a$crohns_presence_adu_q_1 == 1, 1,ifelse(meta_1a$colitisulcerosa_presence_adu_q_1==""|meta_1a$crohns_presence_adu_q_1=="","excl", 0)) # exclude individuals that did not fill in questionnaire

    # merge dataframes
LLD_meta=merge(LLD,meta,by="project_pseudo_id",all.x = TRUE)
LLD_full=merge(LLD_meta,meta_1a,by="project_pseudo_id")

    # create final IBD case control status and exclude individuals that did not have IBD at 1a and developed IBD after 1a
LLD_full[is.na(LLD_full)] <- 999
LLD_full$IBD=ifelse(LLD_full$IBD_LLDbaseline == 1, 1, ifelse(LLD_full$IBD_followup == "1" & LLD_full$IBD_LLDbaseline != "1" | LLD_full$IBD_LLDbaseline == "excl" , "excl", 0))
LLD_full=LLD_full[,c(1,2,12)]
LLD_full=LLD_full[LLD_full$IBD != "excl",]

write.csv(LLD_full, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLD_casecontrol.csv")


#### create DAG3 case control ----
    # load in dataframes
DAG3=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/linkage_files_202404/dag3_linkage_file_OV23_00813.csv")
DAG3=DAG3 %>% rename(project_pseudo_id=PROJECT_PSEUDO_ID)
meta_1a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_1_results.csv")
meta_2a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/2a_q_1_results.csv")
meta_1b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/1b_q_1_results.csv")
meta_1c=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/1c_q_1_results.csv")
meta_3a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/3a_q_1_results.csv")
meta_3b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/3b_q_1_results.csv")

    # identify individuals that said developed IBD after 2a
meta_3a$IBD_3a=ifelse(meta_3a$colitisulcerosa_followup_adu_q_1 == 1 | meta_3a$crohns_followup_adu_q_1 == 1, 1, 0)
meta_3b$IBD_3b=ifelse(meta_3b$colitisulcerosa_followup_adu_q_1 == 1 | meta_3b$crohns_followup_adu_q_1 == 1, 1, 0)

meta=merge(meta_3a[,c(1,9)],meta_3b[,c(1,9)], by = "project_pseudo_id", all = TRUE)
meta[is.na(meta)] <- 999
meta$IBD_followup=ifelse(meta$IBD_3a == 1|meta$IBD_3b == 1, 1, 0)

    # exclude individuals that did not fill in 1a questionnaire (no missing in 1b, 1c, 2a)
    # create IBD development at each timepoint
meta_1a=meta_1a[meta_1a$colitisulcerosa_presence_adu_q_1!=""|meta_1a$crohns_presence_adu_q_1!="",]
meta_1a$IBD_1a=ifelse(meta_1a$colitisulcerosa_presence_adu_q_1 == 1 | meta_1a$crohns_presence_adu_q_1 == 1, 1, 0) 
meta_1b$IBD_1b=ifelse(meta_1b$colitisulcerosa_followup_adu_q_1 == 1 | meta_1b$crohns_followup_adu_q_1 == 1, 1, 0)
meta_1c$IBD_1c=ifelse(meta_1c$colitisulcerosa_followup_adu_q_1 == 1 | meta_1c$crohns_followup_adu_q_1 == 1, 1, 0)
meta_2a$IBD_2a=ifelse(meta_2a$colitisulcerosa_followup_adu_q_1 == 1 | meta_2a$crohns_followup_adu_q_1 == 1, 1, 0)

    # merge dataframes to get df containing DAG3 individuals with data in 1a and 2a
DAG3_metabefore=merge(DAG3,meta_1a[,c(1,37)],by="project_pseudo_id")
DAG3_metabefore=merge(DAG3_metabefore,meta_2a[,c(1,36)],by="project_pseudo_id")
DAG3_metabefore=Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "project_pseudo_id", all.x = T),
            list(DAG3_metabefore, meta_1b[,c(1,9)], meta_1c[,c(1,9)]))
DAG3_metabefore[is.na(DAG3_metabefore)] <- 999

    # create IBD_baseline
DAG3_metabefore$IBD_baseline=ifelse(DAG3_metabefore$IBD_1a == 1 | DAG3_metabefore$IBD_1b == 1 |DAG3_metabefore$IBD_1c == 1 |DAG3_metabefore$IBD_2a == 1, 1, 0)

    # merge df's and create IBD final case/control with IBD_baseline and IBD_followup
DAG3_full=merge(DAG3_metabefore, meta[,c(1,4)], by="project_pseudo_id", all.x = T)
DAG3_full[is.na(DAG3_full)] <- 999
DAG3_full$IBD=ifelse(DAG3_full$IBD_baseline == 1, 1, ifelse(DAG3_full$IBD_followup == "1" & DAG3_full$IBD_baseline != "1", "excl", 0))

    # filter out LLD individuals
LLD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/LLD_casecontrol.csv")
DAG3_full=anti_join(DAG3_full,LLD, by="project_pseudo_id")
DAG3_full=DAG3_full[DAG3_full$IBD != "excl",]
DAG3_full=DAG3_full[,c(1:6,13)]

write.csv(DAG3_full, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/DAG3_casecontrol.csv")

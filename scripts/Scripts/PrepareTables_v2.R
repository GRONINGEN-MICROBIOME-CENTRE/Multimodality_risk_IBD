library(dplyr)

#### create LLD meta ----
    # load in dataframes
LLD=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/linkage_files_202404/deep_linkage_file_v2_OV23_00813.csv")
meta_1a1=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_1_results.csv")
meta_1a2=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_2_results.csv")
meta_2a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/2a_q_1_results.csv")
meta_1b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/1b_q_1_results.csv")
meta_1c=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/1c_q_1_results.csv")
meta_3a=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/3a_q_1_results.csv")
meta_3b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202405/results/3b_q_1_results.csv")

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
meta[meta == 999] <- NA

rm(meta_1b, meta_1c, meta_2a, meta_3a, meta_3b)
    # prep 1a1 and identify individuals that said to have IBD at 1a
meta_1a1=meta_1a1[,c(1,3:5,17,20:21,24,28,29,36)]
meta_1a1$IBD1a=ifelse(meta_1a1$colitisulcerosa_presence_adu_q_1 == 1 | meta_1a1$crohns_presence_adu_q_1 == 1, 1, ifelse(meta_1a1$colitisulcerosa_presence_adu_q_1==""|meta_1a1$crohns_presence_adu_q_1=="",NA,0)) 
names(meta_1a1)[names(meta_1a1) == "date"] <- "date_1a1"
names(meta_1a1)[names(meta_1a1) == "age"] <- "age_1a1"

    # prep 1a2
meta_1a2=meta_1a2[,c(1,3,4,7:17,20:23,294:298,323,324)]
names(meta_1a2)[names(meta_1a2) == "date"] <- "date_1a2"
names(meta_1a2)[names(meta_1a2) == "age"] <- "age_1a2"

    # prep 1b
meta_1b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1b_q_1_results.csv")
meta_1b=meta_1b[,c(1,3,8,9,17,18)]
names(meta_1b)[names(meta_1b) == "date"] <- "date_1b"

    # merge dataframes and add category
LLD_meta=merge(LLD,meta,by="project_pseudo_id",all.x = TRUE)
LLD_full=merge(LLD_meta,meta_1a1,by="project_pseudo_id", all.x=T)
LLD_full=merge(LLD_full,meta_1a2,by="project_pseudo_id", all.x=T)
LLD_full=merge(LLD_full,meta_1b,by="project_pseudo_id", all.x=T)
colnames=colnames(LLD_full)
colnames[1:2] <- paste0("A_", colnames[1:2])
colnames[c(3:11,13,14,19:21,44)] <- paste0("base_", colnames[c(3:11,13,14,19:21,44)])
colnames[c(12,15:18,22:43,45:48)]<- paste0("env_", colnames[c(12,15:18,22:43,45:48)])
colnames(LLD_full) <- colnames

    # add diet data and add category
diet = read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/sec_c_ffq_selrename.csv") 
diet=diet[,-1]
colnames=colnames(diet)
colnames[1]<- paste0("A_", colnames[1])
colnames[-1] <- paste0("diet_", colnames[-1])
colnames(diet) <- colnames
diet <- na.omit(diet)
LLD_full=merge(LLD_full, diet, by="A_project_pseudo_id", all.x=T)
rm(diet,LLD_meta, meta_1a1,meta_1a2,meta_1b)

    # create final IBD case control status and exclude individuals that did not have IBD at 1a and developed IBD after 1a
LLD_full[is.na(LLD_full)] <- 999
LLD_full$base_IBD=ifelse(LLD_full$base_IBD1a == 1, 1, ifelse(LLD_full$base_IBD_followup == "1" & LLD_full$base_IBD1a != "1" | LLD_full$base_IBD1a == 999, NA, 0))
LLD_full$base_UC <- ifelse(LLD_full$base_crohns_presence_adu_q_1 == 1 |LLD_full$base_colitisulcerosa_presence_adu_q_1=="" | LLD_full$base_colitisulcerosa_presence_adu_q_1==999| (LLD_full$base_IBD_followup == "1" & LLD_full$base_colitisulcerosa_presence_adu_q_1 != "1"), NA, ifelse(LLD_full$base_colitisulcerosa_presence_adu_q_1 == 1, 1, 0))
LLD_full$base_CD <- ifelse(LLD_full$base_colitisulcerosa_presence_adu_q_1 == 1|LLD_full$base_crohns_presence_adu_q_1 =="" | LLD_full$base_colitisulcerosa_presence_adu_q_1==999| (LLD_full$base_IBD_followup == "1" & LLD_full$base_crohns_presence_adu_q_1 != "1"), NA, ifelse(LLD_full$base_crohns_presence_adu_q_1 == 1, 1, 0))
LLD_full[LLD_full == 999] <- NA

    # link with palga 
palga=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/OV23_00813_palga_ibd_from_ov22_00601_20240719.csv")
colnames=colnames(palga)
colnames[1]<- paste0("A_", colnames[1])
colnames[2:5]<- paste0("base_palga_", colnames[2:5])
colnames(palga) <- colnames
LLD_full=merge(LLD_full,palga[-6], by="A_project_pseudo_id", all.x=T)

    # change categories to proper form
cols_to_modify <- c("env_hometown_childhood_adu_q_1","env_guineapig_childhood_adu_q_1_a", "env_guineapig_childhood_adu_q_1_b","env_guineapig_childhood_adu_q_1_c",
                    "env_bird_childhood_adu_q_1_a","env_bird_childhood_adu_q_1_b","env_bird_childhood_adu_q_1_c","env_cat_childhood_adu_q_1_a","env_cat_childhood_adu_q_1_b","env_cat_childhood_adu_q_1_c",
                    "env_dog_childhood_adu_q_1_a","env_dog_childhood_adu_q_1_b","env_dog_childhood_adu_q_1_c","env_breastfeeding_duration_adu_q_1","env_inhouse_partner_adu_q_1", "env_inhouse_alone_adu_q_1",
                    "env_hormonal_contraception_adu_q_1","env_current_smoker_adu_c_2","env_squashsum_mvpascore_adu_c_1","env_birth_type_adu_q_1")
LLD_full[cols_to_modify] <- lapply(LLD_full[cols_to_modify], function(x) replace(x, x == "$6", NA))

LLD_full$env_urbanicitychild <- ifelse(LLD_full$env_hometown_childhood_adu_q_1 %in% 3:5, 1, ifelse(LLD_full$env_hometown_childhood_adu_q_1 == 6 | is.na(LLD_full$env_hometown_childhood_adu_q_1), NA, 0))
LLD_full$env_petsu5 <- ifelse(LLD_full$env_guineapig_childhood_adu_q_1_a == 1| LLD_full$env_guineapig_childhood_adu_q_1_b ==1 | 
                                LLD_full$env_bird_childhood_adu_q_1_a == 1 | LLD_full$env_bird_childhood_adu_q_1_b ==1 |
                                LLD_full$env_cat_childhood_adu_q_1_a == 1 | LLD_full$env_cat_childhood_adu_q_1_b == 1 | 
                                LLD_full$env_dog_childhood_adu_q_1_a == 1 | LLD_full$env_dog_childhood_adu_q_1_b ==1 , 1,
                              ifelse(LLD_full$env_guineapig_childhood_adu_q_1_a == 3| LLD_full$env_guineapig_childhood_adu_q_1_b ==3 | 
                                       LLD_full$env_bird_childhood_adu_q_1_a == 3 | LLD_full$env_bird_childhood_adu_q_1_b ==3 | 
                                       LLD_full$env_cat_childhood_adu_q_1_a == 3 | LLD_full$env_cat_childhood_adu_q_1_b == 3 | 
                                       LLD_full$env_dog_childhood_adu_q_1_a == 3 | LLD_full$env_dog_childhood_adu_q_1_b ==3 | 
                                       is.na(LLD_full$env_guineapig_childhood_adu_q_1_a) | is.na(LLD_full$env_guineapig_childhood_adu_q_1_b) |  
                                       is.na(LLD_full$env_bird_childhood_adu_q_1_a) | is.na(LLD_full$env_bird_childhood_adu_q_1_b) | 
                                       is.na(LLD_full$env_cat_childhood_adu_q_1_a) | is.na(LLD_full$env_cat_childhood_adu_q_1_b) | 
                                       is.na(LLD_full$env_dog_childhood_adu_q_1_a) | is.na(LLD_full$env_dog_childhood_adu_q_1_b), NA, 0))
LLD_full$env_petsu15 <- ifelse(LLD_full$env_guineapig_childhood_adu_q_1_a == 1| LLD_full$env_guineapig_childhood_adu_q_1_b ==1 | LLD_full$env_guineapig_childhood_adu_q_1_c == 1 | 
           LLD_full$env_bird_childhood_adu_q_1_a == 1 | LLD_full$env_bird_childhood_adu_q_1_b ==1 | LLD_full$env_bird_childhood_adu_q_1_c == 1 |
           LLD_full$env_cat_childhood_adu_q_1_a == 1 | LLD_full$env_cat_childhood_adu_q_1_b == 1 | LLD_full$env_cat_childhood_adu_q_1_c == 1 |
           LLD_full$env_dog_childhood_adu_q_1_a == 1 | LLD_full$env_dog_childhood_adu_q_1_b ==1 | LLD_full$env_dog_childhood_adu_q_1_c == 1, 1, 
         ifelse(LLD_full$env_guineapig_childhood_adu_q_1_a == 3| LLD_full$env_guineapig_childhood_adu_q_1_b ==3 | LLD_full$env_guineapig_childhood_adu_q_1_c == 3 | 
                  LLD_full$env_bird_childhood_adu_q_1_a == 3 | LLD_full$env_bird_childhood_adu_q_1_b ==3 | LLD_full$env_bird_childhood_adu_q_1_c == 3 |
                  LLD_full$env_cat_childhood_adu_q_1_a == 3 | LLD_full$env_cat_childhood_adu_q_1_b == 3 | LLD_full$env_cat_childhood_adu_q_1_c == 3 |
                  LLD_full$env_dog_childhood_adu_q_1_a == 3 | LLD_full$env_dog_childhood_adu_q_1_b ==3 | LLD_full$env_dog_childhood_adu_q_1_c == 3 |
                  is.na(LLD_full$env_guineapig_childhood_adu_q_1_a) | is.na(LLD_full$env_guineapig_childhood_adu_q_1_b) | is.na(LLD_full$env_guineapig_childhood_adu_q_1_c) |
                  is.na(LLD_full$env_bird_childhood_adu_q_1_a) | is.na(LLD_full$env_bird_childhood_adu_q_1_b) | is.na(LLD_full$env_bird_childhood_adu_q_1_c) |
                  is.na(LLD_full$env_cat_childhood_adu_q_1_a) | is.na(LLD_full$env_cat_childhood_adu_q_1_b) | is.na(LLD_full$env_cat_childhood_adu_q_1_c) |
                  is.na(LLD_full$env_dog_childhood_adu_q_1_a) | is.na(LLD_full$env_dog_childhood_adu_q_1_b) | is.na(LLD_full$env_dog_childhood_adu_q_1_c), NA, 0))
LLD_full$env_breastfeeding_binomial <- ifelse(is.na(LLD_full$env_breastfeeding_duration_adu_q_1)|LLD_full$env_breastfeeding_duration_adu_q_1 == 9, NA, ifelse(LLD_full$env_breastfeeding_duration_adu_q_1 == 1, 0, 1))
LLD_full$env_bedpartner <- ifelse(is.na(LLD_full$env_inhouse_partner_adu_q_1)|LLD_full$env_inhouse_partner_adu_q_1=="", NA, ifelse(LLD_full$env_inhouse_partner_adu_q_1 == 1, 1, 0))
LLD_full$env_housemates<- ifelse(is.na(LLD_full$env_inhouse_alone_adu_q_1)|LLD_full$env_inhouse_alone_adu_q_1=="", NA, ifelse(LLD_full$env_inhouse_alone_adu_q_1 == 1, 0, 1))
LLD_full$env_HCP <- ifelse(LLD_full$base_gender == "MALE" | LLD_full$env_hormonal_contraception_adu_q_1 ==2, 0, ifelse(is.na(LLD_full$env_hormonal_contraception_adu_q_1) | LLD_full$env_hormonal_contraception_adu_q_1 == "", NA, 1))
LLD_full$env_currentsmoking=LLD_full$env_current_smoker_adu_c_2
LLD_full$env_totalactivityscore = ifelse(LLD_full$env_squashsum_reliability_adu_c_2 == 2 | LLD_full$env_squashsum_mvpascore_adu_c_1 == 0, NA,LLD_full$env_squashsum_mvpascore_adu_c_1)
LLD_full$env_sisters=ifelse(LLD_full$env_sisters_number_fam_q_1_a == 1, NA, ifelse(LLD_full$env_sisters_number_fam_q_1 == "$6", 0, LLD_full$env_sisters_number_fam_q_1))
LLD_full$env_brothers=ifelse(LLD_full$env_brothers_number_adu_q_1_a == 1, NA, ifelse(LLD_full$env_brothers_number_adu_q_1 == "$6", 0, LLD_full$env_brothers_number_adu_q_1))
LLD_full$env_siblings <- as.numeric(LLD_full$env_brothers) + as.numeric(LLD_full$env_sisters)
LLD_full$env_siblings_twoormore <- ifelse(LLD_full$env_siblings >= 2, 1, 0)
LLD_full$env_Csection=ifelse(LLD_full$env_birth_type_adu_q_1 == 4, NA, ifelse(LLD_full$env_birth_type_adu_q_1 == 3, 1,0))
LLD_full$env_appendectomy=ifelse(LLD_full$env_appendectomy_lifetime_adu_q_1 == "", NA, ifelse(LLD_full$env_appendectomy_lifetime_adu_q_1 =="$6", 0, 1))
LLD_full$env_tonsillectomy=ifelse(LLD_full$env_tonsillectomy_lifetime_adu_q_1 == "", NA, ifelse(LLD_full$env_tonsillectomy_lifetime_adu_q_1 == "$6", 0, 1))

    # alphabetically order colnames
LLD_full=LLD_full[,order(colnames(LLD_full))]

#write.csv(LLD_full, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/LLD_metafull.csv")

#### create 1000IBD meta ----
IBD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/GIEQdietfull.csv")
IBD=IBD[,-c(1,145:153)]

    # add categories
colnames=colnames(IBD)
colnames[c(1,3,90)]<- paste0("A_", colnames[c(1,3,90)])
colnames[c(4:35,39:86)] <- paste0("env_", colnames[c(4:35,39:86)])
colnames[c(36:38,87:89,150,151)] <- paste0("base_", colnames[c(36:38,87:89,150,151)])
colnames[c(2,91:149)] <- paste0("diet_", colnames[c(2,91:149)])
colnames(IBD) <- colnames

    # change categories to proper form
IBD$env_housemates= ifelse(IBD$env_Housemates== 0, 0, 1)
IBD$env_urbanicitychild= ifelse(IBD$env_Livingarea_all== 6, NA, ifelse(IBD$env_Livingarea_all %in% 3:5, 1, 0))
cols_to_modify <- c("env_Child_cat", "env_Child_dog","env_Child_bird","env_Child_cavia", "env_FEM81")
IBD[cols_to_modify] <- lapply(IBD[cols_to_modify], function(x) replace(x, x == 2, NA))
IBD$env_catu5= ifelse(IBD$env_Child_cat == 0, 0, ifelse(is.na(IBD$env_Child_cat1.4) & is.na(IBD$env_Child_cat1st) & IBD$env_Child_dog5.15 == 1, 0, ifelse(IBD$env_Child_cat1st == 1 | IBD$env_Child_cat1.4 == 1, 1, 0)))
IBD$env_dogu5= ifelse(IBD$env_Child_dog == 0, 0, ifelse(is.na(IBD$env_Child_dog1.4) & is.na(IBD$env_Child_dog1st) & IBD$env_Child_dog5.15 == 1, 0, ifelse(IBD$env_Child_dog1st == 1 | IBD$env_Child_dog1.4 == 1, 1, 0)))
IBD$env_birdu5= ifelse(IBD$env_Child_bird == 0, 0, ifelse(is.na(IBD$env_Child_bird1.4) & is.na(IBD$env_Child_bird1st) & IBD$env_Child_bird5.15 == 1, 0, ifelse(IBD$env_Child_bird1st == 1 | IBD$env_Child_bird1.4 == 1, 1, 0)))
IBD$env_guineapigu5= ifelse(IBD$env_Child_cavia == 0, 0, ifelse(is.na(IBD$env_Child_cavia1.4) & is.na(IBD$env_Child_cavia1st) & IBD$env_Child_cavia5.15 == 1, 0, ifelse(IBD$env_Child_cavia1st == 1 | IBD$env_Child_cavia1.4 == 1, 1, 0)))
IBD$env_petsu5=ifelse(IBD$env_guineapigu5 == 1| IBD$env_birdu5 == 1| IBD$env_dogu5==1|IBD$env_catu5 == 1, 1, 0)
IBD$env_petsu15=ifelse(IBD$env_Child_cat == 0 & IBD$env_Child_dog == 0 & IBD$env_Child_bird == 0 & IBD$env_Child_cavia == 0, 0, 
                       ifelse(IBD$env_Child_dog5.15 == 1|IBD$env_Child_cat1st == 1 | IBD$env_Child_cat1.4 == 1 | 
                        IBD$env_Child_dog5.15 == 1 | IBD$env_Child_dog1st == 1 | IBD$env_Child_dog1.4 == 1 | 
                         IBD$env_Child_bird5.15 == 1 | IBD$env_Child_bird1st == 1 | IBD$env_Child_bird1.4 == 1 | 
                          IBD$env_Child_cavia5.15 == 1 | IBD$env_Child_cavia1st == 1 | IBD$env_Child_cavia1.4 == 1, 1, 0))  
IBD$env_breastfeeding_binomial = ifelse(IBD$env_BIRTH151 == 9, NA, ifelse(IBD$env_BIRTH151 == 1, 0, 1))
names(IBD)[names(IBD) == "env_Bedpartner"] <- "env_bedpartner"
IBD$env_housemates = ifelse(IBD$env_Housemates== 0, 0, 1)
IBD$env_HCP = ifelse(is.na(IBD$env_FEM81) & IBD$base_Gender == "Male", 0, ifelse(IBD$base_Gender == "Female", IBD$env_FEM81, NA)) #1 male using HCP excluded
IBD$env_currentsmoking=ifelse(IBD$env_Smokingstatus == 1, 0, ifelse(IBD$env_Smokingstatus==2, 1, IBD$env_Smokingstatus))
IBD$env_totalactivityscore = ifelse(IBD$env_PA_totalactivityscore == 0, NA,IBD$env_PA_totalactivityscore)
IBD$env_brothers = ifelse(IBD$env_SIBSB1 == 0 & is.na(IBD$env_BROTHERS1), 0, IBD$env_BROTHERS1)
IBD$env_sisters = ifelse(IBD$env_SIBSZ1 == 0 & is.na(IBD$env_SISTERS1), 0, IBD$env_SISTERS1)
IBD$env_siblings <- as.numeric(IBD$env_brothers) + as.numeric(IBD$env_sisters)
IBD$env_siblings_twoormore <- ifelse(IBD$env_siblings >= 2, 1, 0)
IBD$env_Csection=ifelse(IBD$env_BIRTH141 == 3, 1,ifelse(IBD$env_BIRTH141 == 1 | IBD$env_BIRTH141 == 2, 0, NA))
IBD$env_appendectomy=ifelse(IBD$env_HEALTH1261 == 2, NA, IBD$env_HEALTH1261)
IBD$env_tonsillectomy=ifelse(IBD$env_HEALTH1251 == 2, NA, IBD$env_HEALTH1251)
IBD$base_UC=ifelse(IBD$base_Diagnosis == "UC", 1, NA)
IBD$base_CD=ifelse(IBD$base_Diagnosis == "CD", 1, NA)

    # reorder columns and save
IBD=IBD[,order(colnames(IBD))]
#write.csv(IBD, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/IBD_metafull.csv")

#### combine 1000IBD and LLD into train_meta ----
    # load dfs
IBD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/IBD_metafull.csv")
LLD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/LLD_metafull.csv")

    # create same columns
LLD$A_GIEQID <- NA
LLD$A_IBDFECIDTotal10032017 <- NA
names(LLD)[names(LLD) == "A_LLDEEP_ID"] <- "A_ID_LLDUMCGIBD"
names(LLD)[names(LLD) == "base_gender"] <- "base_sex"
LLD$base_AgeFecalDiet <- LLD$base_age_1a2
LLD$base_AgeEnvironment <- LLD$base_age_1a1
LLD$base_DietQuestionnaireCompleted. <- "yes"
LLD$base_GIEQ <- NA
LLD$base_Year_birth <- NA
LLD$base_IBDconf <- ifelse(LLD$base_IBD == 1 & LLD$base_palga_IBD != "IBD", NA,LLD$base_IBD)
LLD$base_CDconf=ifelse(LLD$base_CD == 1 & LLD$base_IBDconf != 1, NA, LLD$base_CD)
LLD$base_UCconf=ifelse(LLD$base_UC == 1 & LLD$base_IBDconf != 1, NA, LLD$base_UC)


names(IBD)[names(IBD) == "A_UMCGIBDResearchID"] <- "A_ID_LLDUMCGIBD"
IBD$A_project_pseudo_id <- NA
IBD$base_AgeEnvironment <- NA
IBD$base_AgeFecalDiet <- IBD$base_AgeAtFecalSampling
IBD$base_IBD <- 1
IBD$base_date_1a1 <- NA
IBD$base_date_1a2 <- NA
IBD$base_date_1b <- NA
IBD$base_IBD_followup <- NA
IBD$base_IBD1a <- NA
IBD$base_palga_Certainty_Idmatch <- NA
IBD$base_palga_IBD <- NA
IBD$base_palga_Pref_Dx <- NA
IBD$base_palga_Year_Dx <- NA
IBD$base_sex = ifelse(is.na(IBD$base_Sex), IBD$base_Gender, IBD$base_Sex)
IBD$base_sex[IBD$base_sex %in% c("female", "Female")] <- "FEMALE"
IBD$base_sex[IBD$base_sex %in% c("male", "Male")] <- "MALE"
IBD$base_IBDconf <- 1
IBD$base_CDconf <- IBD$base_CD
IBD$base_UCconf <- IBD$base_UC

    # select columns and bind
LLD <- select(LLD, -c("X", "env_hometown_childhood_adu_q_1","env_guineapig_childhood_adu_q_1_a", "env_guineapig_childhood_adu_q_1_b","env_guineapig_childhood_adu_q_1_c",
                      "env_bird_childhood_adu_q_1_a","env_bird_childhood_adu_q_1_b","env_bird_childhood_adu_q_1_c","env_cat_childhood_adu_q_1_a","env_cat_childhood_adu_q_1_b","env_cat_childhood_adu_q_1_c",
                      "env_dog_childhood_adu_q_1_a","env_dog_childhood_adu_q_1_b","env_dog_childhood_adu_q_1_c","env_breastfeeding_duration_adu_q_1","env_inhouse_partner_adu_q_1", "env_inhouse_alone_adu_q_1",
                      "env_hormonal_contraception_adu_q_1","env_current_smoker_adu_c_2","env_squashsum_mvpascore_adu_c_1","env_squashsum_reliability_adu_c_2","env_birth_type_adu_q_1", "env_sisters_number_fam_q_1_a",
                      "env_sisters_number_fam_q_1","env_brothers_number_adu_q_1_a","env_brothers_number_adu_q_1","env_birth_type_adu_q_1","env_appendectomy_lifetime_adu_q_1","env_tonsillectomy_lifetime_adu_q_1",
                      "base_colitisulcerosa_presence_adu_q_1", "base_crohns_presence_adu_q_1","base_IBD_1b","base_IBD_1c","base_IBD_2a","base_IBD_3a","base_IBD_3b", "base_age_1a2","base_age_1a1",
                      "env_bird_presence_adu_q_1", "env_guineapig_presence_adu_q_1","env_dog_presence_adu_q_1","env_cat_presence_adu_q_1"))

list=colnames(LLD)
IBD=IBD[ , names(IBD) %in% list]

IBD=IBD[,order(colnames(IBD))]
LLD=LLD[,order(colnames(LLD))]

test_meta=rbind(LLD,IBD)
#write.csv(test_meta,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv",row.names=F)

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
meta$IBD_followup=ifelse(meta$IBD_3a == 1|meta$IBD_3b == 1, 1, 0)

    # create IBD development at each timepoint
meta_1a$IBD_1a=ifelse(meta_1a$colitisulcerosa_presence_adu_q_1 == 1 | meta_1a$crohns_presence_adu_q_1 == 1, 1, 0) 
meta_1b$IBD_1b=ifelse(meta_1b$colitisulcerosa_followup_adu_q_1 == 1 | meta_1b$crohns_followup_adu_q_1 == 1, 1, 0)
meta_1c$IBD_1c=ifelse(meta_1c$colitisulcerosa_followup_adu_q_1 == 1 | meta_1c$crohns_followup_adu_q_1 == 1, 1, 0)
meta_2a$IBD_2a=ifelse(meta_2a$colitisulcerosa_followup_adu_q_1 == 1 | meta_2a$crohns_followup_adu_q_1 == 1, 1, 0)

    # merge dataframes
DAG3meta=merge(DAG3,meta_1a[,c(1,37)],by="project_pseudo_id",all.x=T)
DAG3meta=merge(DAG3meta,meta_2a[,c(1,36)],by="project_pseudo_id",all.x=T)
DAG3meta=Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "project_pseudo_id", all.x = T),
            list(DAG3meta, meta_1b[,c(1,9)], meta_1c[,c(1,9)]))

    # create IBD_baseline
DAG3meta$IBD_baseline=ifelse(DAG3meta$IBD_1a == 1 | DAG3meta$IBD_1b == 1 |DAG3meta$IBD_1c == 1 |DAG3meta$IBD_2a == 1, 1, 0)

    # merge df's and create IBD final case/control with IBD_baseline and IBD_followup
DAG3_full=merge(DAG3meta, meta[,c(1,4)], by="project_pseudo_id", all.x = T)
DAG3_full[is.na(DAG3_full)] <- 999
DAG3_full$IBD=ifelse(DAG3_full$IBD_baseline == 1, 1, ifelse(DAG3_full$IBD_followup == 1 & DAG3_full$IBD_baseline != 1 | DAG3_full$IBD_baseline == 999, NA, 0))
DAG3_full[DAG3_full == 999] <- NA

    # filter out LLD individuals
LLD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/LLD_metafull.csv")
LLD=LLD[c("A_project_pseudo_id","base_IBD")]
LLD=na.omit(LLD)
DAG3_full=DAG3_full %>% rename(A_project_pseudo_id=project_pseudo_id)
DAG3_full=anti_join(DAG3_full,LLD,by="A_project_pseudo_id")

    # merge with palga
palga=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/OV23_00813_palga_ibd_from_ov22_00601_20240719.csv")
colnames=colnames(palga)
colnames[1]<- paste0("A_", colnames[1])
colnames[2:5]<- paste0("base_palga_", colnames[2:5])
colnames(palga) <- colnames
DAG3=merge(DAG3_full,palga[-6], by= "A_project_pseudo_id", all.x=T)
DAG3$base_IBDconf=ifelse(DAG3$IBD == 1 & DAG3$base_palga_IBD != "IBD", NA,DAG3$IBD)

rm(LLD,DAG3_full,DAG3meta,meta_1a,meta_1b,meta_1c,meta_2a,meta_3a,meta_3b,meta)
    # add metadata
# prep 1a1 and identify individuals that said to have IBD at 1a
meta_1a1=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_1_results.csv")
meta_1a1=meta_1a1[,c(1,3:5,17,24,28,29,36)]
names(meta_1a1)[names(meta_1a1) == "date"] <- "date_1a1"
names(meta_1a1)[names(meta_1a1) == "age"] <- "age_1a1"
# prep 1a2
meta_1a2=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1a_q_2_results.csv")
meta_1a2=meta_1a2[,c(1,3,4,7:17,20:23,294:298,323,324)]
names(meta_1a2)[names(meta_1a2) == "date"] <- "date_1a2"
names(meta_1a2)[names(meta_1a2) == "age"] <- "age_1a2"
# prep 1b
meta_1b=read.csv("/groups/umcg-lifelines/prm02/projects/ov23_0813/dataset_order_202404/results/1b_q_1_results.csv")
meta_1b=meta_1b[,c(1,3,8,9,17,18)]
names(meta_1b)[names(meta_1b) == "date"] <- "date_1b"

    # merge and add category
DAG3meta = merge(DAG3,meta_1a1, by.x = "A_project_pseudo_id", by.y="project_pseudo_id",all.x=T)
DAG3meta = merge(DAG3meta,meta_1a2, by.x = "A_project_pseudo_id", by.y="project_pseudo_id",all.x=T)
DAG3meta = merge(DAG3meta,meta_1b, by.x = "A_project_pseudo_id", by.y="project_pseudo_id",all.x=T)
colnames=colnames(DAG3meta)
colnames[2:6]<- paste0("A_", colnames[2:6])
colnames[c(7:13,19:21,27,28,51)] <- paste0("base_", colnames[c(7:13,19:21,27,28,51)])
colnames[c(22:26,29:50,52:55)]<- paste0("env_", colnames[c(22:26,29:50,52:55)])
colnames(DAG3meta) <- colnames

    # add diet data and add category
diet = read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/rawfiles/sec_c_ffq_selrename.csv") 
diet=diet[,-1]
colnames=colnames(diet)
colnames[1]<- paste0("A_", colnames[1])
colnames[-1] <- paste0("diet_", colnames[-1])
colnames(diet) <- colnames
diet <- na.omit(diet)
DAG3meta=merge(DAG3meta, diet, by="A_project_pseudo_id", all.x=T)

    # change categories to proper form
cols_to_modify <- c("env_hometown_childhood_adu_q_1","env_guineapig_childhood_adu_q_1_a", "env_guineapig_childhood_adu_q_1_b","env_guineapig_childhood_adu_q_1_c",
                    "env_bird_childhood_adu_q_1_a","env_bird_childhood_adu_q_1_b","env_bird_childhood_adu_q_1_c","env_cat_childhood_adu_q_1_a","env_cat_childhood_adu_q_1_b","env_cat_childhood_adu_q_1_c",
                    "env_dog_childhood_adu_q_1_a","env_dog_childhood_adu_q_1_b","env_dog_childhood_adu_q_1_c","env_breastfeeding_duration_adu_q_1","env_inhouse_partner_adu_q_1", "env_inhouse_alone_adu_q_1",
                    "env_hormonal_contraception_adu_q_1","env_current_smoker_adu_c_2","env_squashsum_mvpascore_adu_c_1","env_birth_type_adu_q_1")
DAG3meta[cols_to_modify] <- lapply(DAG3meta[cols_to_modify], function(x) replace(x, x == "$6", NA))

DAG3meta$env_urbanicitychild <- ifelse(DAG3meta$env_hometown_childhood_adu_q_1 %in% 3:5, 1, ifelse(DAG3meta$env_hometown_childhood_adu_q_1 == 6 | is.na(DAG3meta$env_hometown_childhood_adu_q_1), NA, 0))
DAG3meta$env_petsu5 <- ifelse(DAG3meta$env_guineapig_childhood_adu_q_1_a == 1| DAG3meta$env_guineapig_childhood_adu_q_1_b ==1 | 
                                DAG3meta$env_bird_childhood_adu_q_1_a == 1 | DAG3meta$env_bird_childhood_adu_q_1_b ==1 |
                                DAG3meta$env_cat_childhood_adu_q_1_a == 1 | DAG3meta$env_cat_childhood_adu_q_1_b == 1 | 
                                DAG3meta$env_dog_childhood_adu_q_1_a == 1 | DAG3meta$env_dog_childhood_adu_q_1_b ==1 , 1,
                              ifelse(DAG3meta$env_guineapig_childhood_adu_q_1_a == 3| DAG3meta$env_guineapig_childhood_adu_q_1_b ==3 | 
                                       DAG3meta$env_bird_childhood_adu_q_1_a == 3 | DAG3meta$env_bird_childhood_adu_q_1_b ==3 | 
                                       DAG3meta$env_cat_childhood_adu_q_1_a == 3 | DAG3meta$env_cat_childhood_adu_q_1_b == 3 | 
                                       DAG3meta$env_dog_childhood_adu_q_1_a == 3 | DAG3meta$env_dog_childhood_adu_q_1_b ==3 | 
                                       is.na(DAG3meta$env_guineapig_childhood_adu_q_1_a) | is.na(DAG3meta$env_guineapig_childhood_adu_q_1_b) |  
                                       is.na(DAG3meta$env_bird_childhood_adu_q_1_a) | is.na(DAG3meta$env_bird_childhood_adu_q_1_b) | 
                                       is.na(DAG3meta$env_cat_childhood_adu_q_1_a) | is.na(DAG3meta$env_cat_childhood_adu_q_1_b) | 
                                       is.na(DAG3meta$env_dog_childhood_adu_q_1_a) | is.na(DAG3meta$env_dog_childhood_adu_q_1_b), NA, 0))
DAG3meta$env_petsu15 <- ifelse(DAG3meta$env_guineapig_childhood_adu_q_1_a == 1| DAG3meta$env_guineapig_childhood_adu_q_1_b ==1 | DAG3meta$env_guineapig_childhood_adu_q_1_c == 1 | 
                                 DAG3meta$env_bird_childhood_adu_q_1_a == 1 | DAG3meta$env_bird_childhood_adu_q_1_b ==1 | DAG3meta$env_bird_childhood_adu_q_1_c == 1 |
                                 DAG3meta$env_cat_childhood_adu_q_1_a == 1 | DAG3meta$env_cat_childhood_adu_q_1_b == 1 | DAG3meta$env_cat_childhood_adu_q_1_c == 1 |
                                 DAG3meta$env_dog_childhood_adu_q_1_a == 1 | DAG3meta$env_dog_childhood_adu_q_1_b ==1 | DAG3meta$env_dog_childhood_adu_q_1_c == 1, 1, 
                               ifelse(DAG3meta$env_guineapig_childhood_adu_q_1_a == 3| DAG3meta$env_guineapig_childhood_adu_q_1_b ==3 | DAG3meta$env_guineapig_childhood_adu_q_1_c == 3 | 
                                        DAG3meta$env_bird_childhood_adu_q_1_a == 3 | DAG3meta$env_bird_childhood_adu_q_1_b ==3 | DAG3meta$env_bird_childhood_adu_q_1_c == 3 |
                                        DAG3meta$env_cat_childhood_adu_q_1_a == 3 | DAG3meta$env_cat_childhood_adu_q_1_b == 3 | DAG3meta$env_cat_childhood_adu_q_1_c == 3 |
                                        DAG3meta$env_dog_childhood_adu_q_1_a == 3 | DAG3meta$env_dog_childhood_adu_q_1_b ==3 | DAG3meta$env_dog_childhood_adu_q_1_c == 3 |
                                        is.na(DAG3meta$env_guineapig_childhood_adu_q_1_a) | is.na(DAG3meta$env_guineapig_childhood_adu_q_1_b) | is.na(DAG3meta$env_guineapig_childhood_adu_q_1_c) |
                                        is.na(DAG3meta$env_bird_childhood_adu_q_1_a) | is.na(DAG3meta$env_bird_childhood_adu_q_1_b) | is.na(DAG3meta$env_bird_childhood_adu_q_1_c) |
                                        is.na(DAG3meta$env_cat_childhood_adu_q_1_a) | is.na(DAG3meta$env_cat_childhood_adu_q_1_b) | is.na(DAG3meta$env_cat_childhood_adu_q_1_c) |
                                        is.na(DAG3meta$env_dog_childhood_adu_q_1_a) | is.na(DAG3meta$env_dog_childhood_adu_q_1_b) | is.na(DAG3meta$env_dog_childhood_adu_q_1_c), NA, 0))
DAG3meta$env_breastfeeding_binomial <- ifelse(is.na(DAG3meta$env_breastfeeding_duration_adu_q_1)|DAG3meta$env_breastfeeding_duration_adu_q_1 == 9, NA, ifelse(DAG3meta$env_breastfeeding_duration_adu_q_1 == 1, 0, 1))
DAG3meta$env_bedpartner <- ifelse(is.na(DAG3meta$env_inhouse_partner_adu_q_1)|DAG3meta$env_inhouse_partner_adu_q_1=="", NA, ifelse(DAG3meta$env_inhouse_partner_adu_q_1 == 1, 1, 0))
DAG3meta$env_housemates<- ifelse(is.na(DAG3meta$env_inhouse_alone_adu_q_1)|DAG3meta$env_inhouse_alone_adu_q_1=="", NA, ifelse(DAG3meta$env_inhouse_alone_adu_q_1 == 1, 0, 1))
DAG3meta$env_HCP <- ifelse(DAG3meta$base_gender == "MALE" | DAG3meta$env_hormonal_contraception_adu_q_1 ==2, 0, ifelse(is.na(DAG3meta$env_hormonal_contraception_adu_q_1) | DAG3meta$env_hormonal_contraception_adu_q_1 == "", NA, 1))
DAG3meta$env_currentsmoking=DAG3meta$env_current_smoker_adu_c_2
DAG3meta$env_totalactivityscore = ifelse(DAG3meta$env_squashsum_reliability_adu_c_2 == 2 | DAG3meta$env_squashsum_mvpascore_adu_c_1 == 0, NA,DAG3meta$env_squashsum_mvpascore_adu_c_1)
DAG3meta$env_sisters=ifelse(DAG3meta$env_sisters_number_fam_q_1_a == 1 | DAG3meta$env_sisters_number_fam_q_1 == 99, NA, ifelse(DAG3meta$env_sisters_number_fam_q_1 == "$6" | DAG3meta$env_sisters_number_fam_q_1 == "$7" , 0, DAG3meta$env_sisters_number_fam_q_1))
DAG3meta$env_brothers=ifelse(DAG3meta$env_brothers_number_adu_q_1_a == 1 | DAG3meta$env_brothers_number_adu_q_1 == 99, NA, ifelse(DAG3meta$env_brothers_number_adu_q_1 == "$6" | DAG3meta$env_brothers_number_adu_q_1 == "$7" , 0, DAG3meta$env_brothers_number_adu_q_1))
DAG3meta$env_siblings <- as.numeric(DAG3meta$env_brothers) + as.numeric(DAG3meta$env_sisters)
DAG3meta$env_siblings_twoormore <- ifelse(DAG3meta$env_siblings >= 2, 1, 0)
DAG3meta$env_Csection=ifelse(DAG3meta$env_birth_type_adu_q_1 == 4, NA, ifelse(DAG3meta$env_birth_type_adu_q_1 == 3, 1,0))
DAG3meta$env_appendectomy=ifelse(DAG3meta$env_appendectomy_lifetime_adu_q_1 == "", NA, ifelse(DAG3meta$env_appendectomy_lifetime_adu_q_1 =="$6", 0, 1))
DAG3meta$env_tonsillectomy=ifelse(DAG3meta$env_tonsillectomy_lifetime_adu_q_1 == "", NA, ifelse(DAG3meta$env_tonsillectomy_lifetime_adu_q_1 == "$6", 0, 1))

DAG3meta=DAG3meta[,order(colnames(DAG3meta))]

#write.csv(DAG3meta, "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull_all.csv")

DAG3meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull_all.csv")
DAG3meta$base_AgeFecal <- DAG3meta$base_age_2a
DAG3meta$base_AgeDietEnvironment <- DAG3meta$base_age_1a1
DAG3meta <- select(DAG3meta, -c("X", "env_hometown_childhood_adu_q_1","env_guineapig_childhood_adu_q_1_a", "env_guineapig_childhood_adu_q_1_b","env_guineapig_childhood_adu_q_1_c",
                      "env_bird_childhood_adu_q_1_a","env_bird_childhood_adu_q_1_b","env_bird_childhood_adu_q_1_c","env_cat_childhood_adu_q_1_a","env_cat_childhood_adu_q_1_b","env_cat_childhood_adu_q_1_c",
                      "env_dog_childhood_adu_q_1_a","env_dog_childhood_adu_q_1_b","env_dog_childhood_adu_q_1_c","env_breastfeeding_duration_adu_q_1","env_inhouse_partner_adu_q_1", "env_inhouse_alone_adu_q_1",
                      "env_hormonal_contraception_adu_q_1","env_current_smoker_adu_c_2","env_squashsum_mvpascore_adu_c_1","env_squashsum_reliability_adu_c_2","env_birth_type_adu_q_1", "env_sisters_number_fam_q_1_a",
                      "env_sisters_number_fam_q_1","env_brothers_number_adu_q_1_a","env_brothers_number_adu_q_1","env_birth_type_adu_q_1","env_appendectomy_lifetime_adu_q_1","env_tonsillectomy_lifetime_adu_q_1",
                      "base_IBD_1a","base_IBD_1b","base_IBD_1c","base_IBD_2a","env_bird_presence_adu_q_1", "env_guineapig_presence_adu_q_1","env_dog_presence_adu_q_1","env_cat_presence_adu_q_1", "base_age_1a1", "base_age_1a2"))
#write.csv(DAG3meta,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull.csv", row.names = F)

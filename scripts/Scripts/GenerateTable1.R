#####################################################################
# Script to generate table 1s - descriptive statistics cohorts      #
# Date: 26-09-2024                                                  #
#####################################################################

library(dplyr)
library(tidyr)
library(gtsummary)

#### table 1 - train cohort ----
meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull_v2.csv")
IBD=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/IBD_metafull.csv")

meta_fill=meta %>% drop_na(score_env_u15_sel_weigh,score_microbiome_log,score_prs_2015,score_diet_LLDS,base_IBDconf)

# merge in and make diagnosis
meta_fill=merge(meta_fill,IBD[,c(4,7)],by.x="A_ID_LLDUMCGIBD",by.y="A_UMCGIBDResearchID",all.x=T)
meta_fill$base_diagnosis=ifelse(is.na(meta_fill$base_Diagnosis) & meta_fill$base_UCconf == 1, "UC",ifelse(is.na(meta_fill$base_Diagnosis) & meta_fill$base_CDconf == 1, "CD", meta_fill$base_Diagnosis))

# prepare df
df_table1 <- meta_fill %>% select(A_ID_LLDUMCGIBD,base_AgeFecalDiet,base_diagnosis,
                                  base_BMIatFecalSampling,base_CDconf,base_UCconf,base_sex,base_IBDconf,
                                  score_diet_aMED,score_diet_LLDS,score_env_u15_sel_unweigh,score_env_u15_sel_weigh,
                                  score_microbiome_log,score_microbiome_shannon,score_prs_2015)

df_table1$base_sex <- as.factor(df_table1$base_sex)
df_table1$base_IBDconf <- as.factor(df_table1$base_IBDconf)
df_table1$base_AgeFecalDiet <- as.numeric(df_table1$base_AgeFecalDiet)

# make table 1
TABLE1 <- df_table1 %>% 
  select(base_IBDconf,base_sex,base_AgeFecalDiet,base_BMIatFecalSampling) %>%
  tbl_summary(
    by = base_IBDconf,
    missing = "no",
    statistic = list(all_continuous()  ~ "{median} ({IQR})",
                     all_categorical() ~ "{n}    ({p}%)")
  ) %>%
  add_p(
    test = list(all_continuous()  ~ "wilcox.test",
                all_categorical() ~ "chisq.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall(last = FALSE,
              col_label = "**All participants**<br>N = {N}")

# diagnosis was added later due to NA in controls
table(df_table1$base_IBDconf,df_table1$base_diagnosis)

# safe table as word doc
TABLE1 %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/table1_train_v2.docx")


#### table 3 - validation cohort ----
meta=read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/DAG3_metafull_v4.csv")
meta_fill=meta %>% drop_na(score_env_u15_sel_weigh,score_microbiome_log,score_prs_2015,score_diet_LLDS,base_IBD1a2a)

# prepare df
df_table1 <- meta_fill %>% select(A_project_pseudo_id,base_AgeFecal,
                                  base_BMIatFecalSampling,base_gender,base_IBD1a2a,
                                  score_diet_LLDS,score_env_u15_sel_weigh,
                                  score_microbiome_log,score_prs_2015)

df_table1$base_sex <- as.factor(df_table1$base_gender)
df_table1$base_IBD1a2a <- as.factor(df_table1$base_IBD1a2a)
df_table1$base_AgeFecal <- as.numeric(df_table1$base_AgeFecal)
df_table1$score_diet_LLDS <- as.numeric(df_table1$score_diet_LLDS)

# make table 1
TABLE1 <- df_table1 %>% 
  select(base_IBD1a2a,base_sex,base_BMIatFecalSampling,
         base_AgeFecal,score_env_u15_sel_weigh,score_diet_LLDS,score_microbiome_log,score_prs_2015) %>%
  tbl_summary(
    by = base_IBD1a2a,
    missing = "no",
    statistic = list(all_continuous()  ~ "{median} ({IQR})",
                     all_categorical() ~ "{n}    ({p}%)")
  ) %>%
  add_p(
    test = list(all_continuous()  ~ "wilcox.test",
                all_categorical() ~ "chisq.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall(last = FALSE,
              col_label = "**All participants**<br>N = {N}")

# manually change p-values of microbiome and prs from <0.0001 to real value
wilcox.test(score_microbiome_log ~ base_IBD1a2a, data=meta_fill)
wilcox.test(score_prs_2015 ~ base_IBD1a2a, data=meta_fill)

# safe table as word doc
TABLE1 %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/table1_validation_v2.docx")



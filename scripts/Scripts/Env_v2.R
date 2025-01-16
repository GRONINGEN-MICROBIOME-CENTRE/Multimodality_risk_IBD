library(dplyr)
library(ggplot2)
library(tidyr)

#### perform environment tests - weighted ----
    # calculate score
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta$envscore_u15=log(0.74)*meta$env_breastfeeding_binomial + log(0.76)*meta$env_petsu15 + log(0.53)*meta$env_housemates + 
  log(0.66)*meta$env_bedpartner + log(1.35)*meta$env_urbanicitychild + log(1.31)*meta$env_HCP

    # create form for plots and tests
meta$base_IBDconf[meta$base_IBDconf==1]<-"IBD"
meta$base_IBDconf[meta$base_IBDconf==0]<-"Non-IBD"
meta_clean <- meta %>% filter(!is.na(base_IBDconf))
meta_clean <- meta_clean %>% filter(!is.na(envscore_u15))

    # plot
pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_u15.pdf")
ggplot(meta_clean, aes(x=base_IBDconf, y=envscore_u15, fill=base_IBDconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Environmental score (pets u15)",x="IBD", y = "Score")+
  theme_classic()
dev.off()

    # test
median(meta_clean$envscore_u15[meta_clean$base_IBDconf=='IBD'])
median(meta_clean$envscore_u15[meta_clean$base_IBDconf=='Non-IBD'])
table(meta_clean$base_IBDconf)
wilcox.test(envscore_u15~base_IBDconf,data=meta_clean)

    # individual factors
meta_clean$eff_breast <- log(0.74)*meta_clean$env_breastfeeding_binomial
meta_clean$eff_petsu15 <- log(0.76)*meta_clean$env_petsu15
meta_clean$eff_house <- log(0.53)*meta_clean$env_housemates
meta_clean$eff_bed <- log(0.66)*meta_clean$env_bedpartner
meta_clean$eff_urban <- log(1.35)*meta_clean$env_urbanicitychild
meta_clean$eff_HCP <- log(1.31)*meta_clean$env_HCP

meta_long <- meta_clean %>%
  select(base_IBDconf, eff_breast,eff_petsu15,eff_house,eff_bed,eff_urban,eff_HCP) %>%  # Select relevant columns
  pivot_longer(cols = eff_breast:eff_HCP, names_to = "variable", values_to = "value")

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/effects.pdf")
ggplot(meta_long, aes(x = base_IBDconf, y = value, fill = base_IBDconf)) +
  stat_summary(fun = mean, geom = "bar") +  # Adjust bar spacing and width
  facet_wrap(~variable, scales = "free_y", nrow = 1) +  # Arrange all plots in one row
  theme_classic() +
  labs(x = "Group", y = "Mean Effect", title = "Mean effect by IBD Group") +
  coord_cartesian(ylim = c(-0.6, 0.2))  # Set the y-axis limits
dev.off()

chisq.test(table(meta_clean$base_IBDconf, meta_clean$env_breastfeeding_binomial))
chisq.test(table(meta_clean$base_IBDconf, meta_clean$env_petsu15))
chisq.test(table(meta_clean$base_IBDconf, meta_clean$env_housemates))
chisq.test(table(meta_clean$base_IBDconf, meta_clean$env_bedpartner))
chisq.test(table(meta_clean$base_IBDconf, meta_clean$env_urbanicitychild))
chisq.test(table(meta_clean$base_IBDconf, meta_clean$env_HCP))

    # only factors that make sense
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta$envscore_u15_sel=log(0.74)*meta$env_breastfeeding_binomial + log(0.76)*meta$env_petsu15 + log(1.35)*meta$env_urbanicitychild + log(1.31)*meta$env_HCP
meta$base_IBDconf[meta$base_IBDconf==1]<-"IBD"
meta$base_IBDconf[meta$base_IBDconf==0]<-"Non-IBD"
meta_clean <- meta %>% filter(!is.na(base_IBDconf))
meta_clean <- meta_clean %>% filter(!is.na(envscore_u15_sel))

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_u15_sel.pdf")
ggplot(meta_clean, aes(x=base_IBDconf, y=envscore_u15_sel, fill=base_IBDconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Environmental score (pets u15) - selected variables",x="IBD", y = "Score")+
  theme_classic()
dev.off()

median(meta_clean$envscore_u15_sel[meta_clean$base_IBDconf=='IBD'])
median(meta_clean$envscore_u15_sel[meta_clean$base_IBDconf=='Non-IBD'])
table(meta_clean$base_IBDconf)
wilcox.test(envscore_u15_sel~base_IBDconf,data=meta_clean)

#### CD calculation ----
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta$CDenvscore=log(1.76)*meta$env_currentsmoking+log(0.71)*meta$env_breastfeeding_binomial + 
  log(0.77)*meta$env_petsu15 + log(0.49)*meta$env_housemates +log(0.54)*meta$env_bedpartner + log(0.93)*meta$env_siblings_twoormore+ 
  log(1.42)*meta$env_urbanicitychild + log(1.38)*meta$env_Csection + log(1.61)*meta$env_appendectomy + log(1.37)*meta$env_tonsillectomy+ log(1.25)*meta$env_HCP
metaCD=filter(meta, !is.na(meta$base_CDconf))
metaCD=filter(metaCD,!is.na(metaCD$CDenvscore))
median=median(metaCD$env_totalactivityscore, na.rm = T)
metaCD$env_activitycat=ifelse(metaCD$env_totalactivityscore >= median, 1, 0)     # make activity categorical
metaCD$CDenvscore=log(1.76)*metaCD$env_currentsmoking+log(0.63)*metaCD$env_activitycat+log(0.71)*metaCD$env_breastfeeding_binomial + 
  log(0.77)*metaCD$env_petsu15 + log(0.49)*metaCD$env_housemates +log(0.54)*metaCD$env_bedpartner + log(0.93)*metaCD$env_siblings_twoormore+ 
  log(1.42)*metaCD$env_urbanicitychild + log(1.38)*metaCD$env_Csection + log(1.61)*metaCD$env_appendectomy + log(1.37)*metaCD$env_tonsillectomy+ log(1.25)*metaCD$env_HCP
metaCD=filter(metaCD,!is.na(metaCD$CDenvscore))

    # calculate score, run test and plot
metaCD$base_CDconf[metaCD$base_CDconf==1]<-"CD"
metaCD$base_CDconf[metaCD$base_CDconf==0]<-"NoCD"

median(metaCD$CDenvscore[metaCD$base_CDconf=="CD"])
median(metaCD$CDenvscore[metaCD$base_CDconf=='NoCD'])
table(metaCD$base_CDconf)

wilcox.test(CDenvscore~base_CDconf,data=metaCD)

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_CD.pdf")
ggplot(metaCD, aes(x=base_CDconf, y=CDenvscore, fill=base_CDconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Environmental score CD (pets u15)",x="CD", y = "Score")+
  theme_classic()
dev.off()

    # only variables that make sense 
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta$CDenvscore_sel=log(1.76)*meta$env_currentsmoking+log(0.71)*meta$env_breastfeeding_binomial + log(0.77)*meta$env_petsu15  + log(0.93)*meta$env_siblings_twoormore+ 
  log(1.42)*meta$env_urbanicitychild + log(1.38)*meta$env_Csection + log(1.61)*meta$env_appendectomy + log(1.37)*meta$env_tonsillectomy+ log(1.25)*meta$env_HCP
metaCD=filter(meta, !is.na(meta$base_CDconf))
metaCD=filter(metaCD,!is.na(metaCD$CDenvscore_sel))
median=median(metaCD$env_totalactivityscore, na.rm = T)
metaCD$env_activitycat=ifelse(metaCD$env_totalactivityscore >= median, 1, 0)     # make activity categorical
metaCD$CDenvscore_sel=log(1.76)*metaCD$env_currentsmoking+log(0.63)*metaCD$env_activitycat+log(0.71)*metaCD$env_breastfeeding_binomial + 
  log(0.77)*metaCD$env_petsu15 + log(0.93)*metaCD$env_siblings_twoormore+ log(1.42)*metaCD$env_urbanicitychild + 
  log(1.38)*metaCD$env_Csection + log(1.61)*metaCD$env_appendectomy + log(1.37)*metaCD$env_tonsillectomy+ log(1.25)*metaCD$env_HCP
metaCD=filter(metaCD,!is.na(metaCD$CDenvscore_sel))

# calculate score, run test and plot
metaCD$base_CDconf[metaCD$base_CDconf==1]<-"CD"
metaCD$base_CDconf[metaCD$base_CDconf==0]<-"NoCD"

median(metaCD$CDenvscore_sel[metaCD$base_CDconf=="CD"])
median(metaCD$CDenvscore_sel[metaCD$base_CDconf=='NoCD'])
table(metaCD$base_CDconf)

wilcox.test(CDenvscore_sel~base_CDconf,data=metaCD)

pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_CDsel.pdf")
ggplot(metaCD, aes(x=base_CDconf, y=CDenvscore_sel, fill=base_CDconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Environmental score CD (pets u15) - selected variables",x="CD", y = "Score")+
  theme_classic()
dev.off()

#### UC calculation ----
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta$UCenvscore=log(0.58)*meta$env_currentsmoking+log(0.78)*meta$env_breastfeeding_binomial + log(0.75)*meta$env_petsu15 +
  log(0.53)*meta$env_bedpartner + log(1.17)*meta$env_urbanicitychild + log(0.39)*meta$env_appendectomy + log(1.28)*meta$env_HCP

metaUC=filter(meta, !is.na(meta$base_UCconf))
metaUC=filter(metaUC,!is.na(metaUC$UCenvscore))

# calculate score, run test and plot
metaUC$base_UCconf[metaUC$base_UCconf==1]<-"UC"
metaUC$base_UCconf[metaUC$base_UCconf==0]<-"NoUC"

median(metaUC$UCenvscore[metaUC$base_UCconf=="UC"])
median(metaUC$UCenvscore[metaUC$base_UCconf=='NoUC'])
table(metaUC$base_UCconf)

wilcox.test(UCenvscore~base_UCconf,data=metaUC)

metaUC$base_UCconf=factor(metaUC$base_UCconf, levels = c("UC","NoUC"))
pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_UC.pdf")
ggplot(metaUC, aes(x=base_UCconf, y=UCenvscore, fill=base_UCconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Environmental score UC (pets u15)",x="UC", y = "Score")+
  theme_classic()
dev.off()

    # only variables that make sense
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta$UCenvscore_sel=log(0.58)*meta$env_currentsmoking+log(0.78)*meta$env_breastfeeding_binomial + log(0.75)*meta$env_petsu15 +
   log(1.17)*meta$env_urbanicitychild + log(0.39)*meta$env_appendectomy + log(1.28)*meta$env_HCP

metaUC=filter(meta, !is.na(meta$base_UCconf))
metaUC=filter(metaUC,!is.na(metaUC$UCenvscore_sel))

# calculate score, run test and plot
metaUC$base_UCconf[metaUC$base_UCconf==1]<-"UC"
metaUC$base_UCconf[metaUC$base_UCconf==0]<-"NoUC"

median(metaUC$UCenvscore_sel[metaUC$base_UCconf=="UC"])
median(metaUC$UCenvscore_sel[metaUC$base_UCconf=='NoUC'])
table(metaUC$base_UCconf)

wilcox.test(UCenvscore_sel~base_UCconf,data=metaUC)

metaUC$base_UCconf=factor(metaUC$base_UCconf, levels = c("UC","NoUC"))
pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_UCsel.pdf")
ggplot(metaUC, aes(x=base_UCconf, y=UCenvscore_sel, fill=base_UCconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Environmental score UC (pets u15) - selected variables",x="UC", y = "Score")+
  theme_classic()
dev.off()

#### merge and save ----
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")
meta$env_scoreu15=log(0.74)*meta$env_breastfeeding_binomial + log(0.76)*meta$env_petsu15 + log(0.53)*meta$env_housemates + 
  log(0.66)*meta$env_bedpartner + log(1.35)*meta$env_urbanicitychild + log(1.31)*meta$env_HCP
meta$env_scoreu15_sel=log(0.74)*meta$env_breastfeeding_binomial + log(0.76)*meta$env_petsu15 + log(1.35)*meta$env_urbanicitychild + log(1.31)*meta$env_HCP
meta$env_scoreu5=log(0.74)*meta$env_breastfeeding_binomial + log(0.76)*meta$env_petsu5 + log(1.35)*meta$env_urbanicitychild + log(1.31)*meta$env_HCP
meta=meta[c("A_GIEQID","A_ID_LLDUMCGIBD","base_IBDconf","env_scoreu15","env_scoreu15_sel","env_scoreu5")]
#write.csv(meta,"/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_env.csv",row.names=F)


#### unweighted score ----
# calculate score
meta<-read.csv("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/metadata/train_metafull.csv")

meta$env_breastfeeding_binomial <- ifelse(meta$env_breastfeeding_binomial == 1, 0, 1)
meta$env_petsu15 <- ifelse(meta$env_petsu15 == 1, 0, 1)
meta$env_housemates <- ifelse(meta$env_housemates == 1, 0, 1)
meta$env_bedpartner <- ifelse(meta$env_bedpartner == 1, 0, 1)

meta$envscore_u15_sel_unweigh=meta$env_breastfeeding_binomial + meta$env_petsu15 +
 meta$env_urbanicitychild + meta$env_HCP

meta$base_IBDconf[meta$base_IBDconf==1]<-"IBD"
meta$base_IBDconf[meta$base_IBDconf==0]<-"Non-IBD"
meta_clean <- meta %>% filter(!is.na(base_IBDconf))
meta_clean <- meta_clean %>% filter(!is.na(envscore_u15_sel_unweigh))

# plot
pdf("/groups/umcg-lifelines/tmp01/projects/ov23_0813/Amber/env_plot_unweighted_sel.pdf")
ggplot(meta_clean, aes(x=base_IBDconf, y=envscore_u15_sel_unweigh, fill=base_IBDconf)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Environmental score (pets u15) - unweighted selected",x="IBD", y = "Score")+
  theme_classic()
dev.off()

# test
median(meta_clean$envscore_u15_sel_unweigh[meta_clean$base_IBDconf=='IBD'])
median(meta_clean$envscore_u15_sel_unweigh[meta_clean$base_IBDconf=='Non-IBD'])
table(meta_clean$base_IBDconf)
wilcox.test(envscore_u15_sel_unweigh~base_IBDconf,data=meta_clean)


#### junk ----
df <- data.frame(
  col1 = c(1, 1, 2, 2, NA, NA, 2),
  col2 = c(2, 2, 1, 2, 1, 2, 2),
  col3 = c(2, 1, 2, 2, 2, 2, 2),
  col4 = c(2, 2, 2, 1, 1, 2, 2)
)

ifelse(df$col1 == 1 | df$col2 == 1 | df$col3 == 1 | df$col4 == 1, 1, 0)
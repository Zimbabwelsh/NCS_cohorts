library(readxl)
library(tidyverse)
library(ggplot2)
library(viridis)

setwd("C:/Users/gg9824/Dropbox/00ESRC Fellowship/NCS Cohorts/ARQ1 Paper 2 Graphics")

## Read in Prevalence data
df <- read_xlsx("2021.04.22_ARQ1_MH2 _OutcomeDescriptives (5).xlsx", sheet=3)
# Drop empty rows (drops if missing data for first three columns)
df <- df[complete.cases(df[, 1:2]),]

# rename columns to not include "%"
colnames <- c("cohort", "outcome", "measure", "timepoint", "month", "monthnum", "strat_by", "strat_group", "perc", "loci", "hici", "n")
colnames(df) <- colnames

# numeric cols have read in with characters due to NAs, so make numeric and convert to percentages.
df$perc <- as.numeric(df$perc) * 100
df$loci <- as.numeric(df$loci) * 100
df$hici <- as.numeric(df$hici) * 100

# Coerce variable labels to lower case for consistency
df$strat_by <- tolower(df$strat_by)
df$strat_group <- tolower(df$strat_group)

# Generate broad outcome list
outcomes <- unique(df$outcome)

# Generate cohort list
studylist <- unique(df$cohort)
agehet <- c("USOC", "ELSA", "GS", "TWINSUK","BiB")
agehom <- c("MCS", "ALSPAC", "NS", "BCS70", "NCDS", "NSHD")

# Generate distinct time period list
timelist <- unique(df$timepoint)

# Generate dataframe for solely "overall" estimates
totdf <- df %>% filter(strat_by == "overall")
##Note - Twins and GS don't have "overall" category - but can construct weighted means from male/female counts

# USOC has monthly estimates, so collapse to "timepoint" estimates
groupdf <- totdf %>% filter(outcome=="General"|outcome=="Depression") %>% 
  group_by(cohort,timepoint, monthnum, outcome) %>% 
  summarise(mean=mean(perc),
            loCI=mean(loci),
            hiCI=mean(hici))

### Generate "type" where age-heterogeneous == 1, else 0
groupdf <- groupdf %>% mutate(type = (case_when(
  cohort %in% agehet ~ 1,
  cohort %in% agehom ~ 0)
  ))

groupdf$type <- recode(groupdf$type, `1`="Age Heterogeneous", `0`="Age Homogeneous")
groupdf$type2 <- factor(groupdf$type, levels=c("Age Homogeneous", "Age Heterogeneous"))

### Generate X limits for geom_rect input
xmins <- c(4, 7, 11)
xmaxs <- c(6, 10, 15)

### Recode for age-het/age-hom studies


#################################################
# Generate desc figure for unstratified overall #
#################################################

#Reorder cohorts for display

studylist <- groupdf$cohort
orderlist <- c("USOC", "ELSA", "GS", "TWINSUK","BiB","MCS", "ALSPAC", "NS", "BCS70", "NCDS", "NSHD")
f1 <- factor(studylist, levels=orderlist)
groupdf <- groupdf[order(f1),]
groupdf$cohort <- factor(groupdf$cohort, levels=orderlist)

groupdf <- groupdf %>% mutate(row=case_when((cohort=="USOC" | cohort=="MCS")~1,
                                 (cohort=="ELSA" | cohort=="ALSPAC")~2,
                                 (cohort=="GS" | cohort== "NS")~3,
                                 (cohort=="TWINSUK" | cohort=="BCS70")~4,
                                 (cohort=="BiB" | cohort == "NCDS")~5, 
                                 (cohort =="NSHD"~6))
                   )


### Facet_wrap code:
age_strat_plot <- ggplot(groupdf,
                         aes(x= monthnum,
                             y=mean,
                             colour=cohort)) +
  geom_point (size=1.5) +
  geom_line(size=0.5, linetype=1) +
  ylab("Proportion over case threshold") +
  theme(axis.text.x=element_text(color = "black",
                                 size=8, angle=30, hjust=0.8)) +
  scale_x_continuous(breaks=c(0,3,6,9,12,15),
                     labels=c("0" = "Pre-Pandemic",
                              "3" = "March 2020",
                              "6"="June 2020",
                              "9"="September 2020",
                              "12"="December 2020",
                              "15"="March 2021")) +
  scale_fill_viridis()+
  #geom_label(cohort)+
  geom_ribbon(aes(ymin=loCI, ymax=hiCI),
              linetype=1, alpha=0.2) +
  theme(panel.border=element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  xlab("Pandemic Timepoint")+
  ylab("Case Prevalence (%)") +
  annotate("rect", xmin=xmins[1], xmax=xmaxs[1], ymin=-0.5, ymax=Inf, alpha=0.1, fill="purple")+
  annotate("rect", xmin=xmins[2], xmax=xmaxs[2], ymin=-0.5, ymax=Inf, alpha=0.1, fill="blue")+
  annotate("rect", xmin=xmins[3], xmax=xmaxs[3], ymin=-0.5, ymax=Inf, alpha=0.1, fill="green")+
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_colour_viridis_d(direction=-1) +
  facet_grid(row~type2) +
  geom_text(data=(groupdf %>% group_by(cohort) %>% top_n(1,monthnum)),
            label=unique(groupdf$cohort), y=5, size=2.5, colour="black", x = 14.5, hjust=1)

##Generate code for labels
ann.text <- data.frame(monthnum = c(5, 8.5, 13), lab = c("Period 1", "Period 2", "Period 3"),
                       cohort=c("MCS", "MCS", "MCS"),
                       type2 = c("Age Heterogeneous","Age Heterogeneous","Age Heterogeneous"),
                       row = c(6,6,6),
                       mean = c(22,22,22))

annot_age_plot <- age_strat_plot+  geom_text(
    data=ann.text,
    mapping = aes(x=monthnum, y=mean, label = lab),
    size=2.5,
    angle=45,
    colour="black"
  ) + facet_grid(row~factor(type2, level=c("Age Homogeneous", "Age Heterogeneous")))
 
annot_age_plot

ggsave("outcomeDescPlot.png", width=6, height=6, dpi=600)

################################################
# Generate dataframe for stratification by sex #
################################################

##Construct sex dataset
sexdf <- df %>% filter(strat_group== "male" | strat_group == "female")
sumsex <- sexdf %>% group_by(cohort, timepoint, monthnum , strat_group) %>% 
  summarise(mean=mean(perc),
            loCI=mean(loci),
            hiCI=mean(hici))

studylist <- sumsex$cohort
orderlist <- c("USOC", "ELSA", "GS", "TWINSUK","BiB","MCS", "ALSPAC", "NS", "BCS70", "NCDS", "NSHD")
f1 <- factor(studylist, levels=orderlist)
sumsex <- sumsex[order(f1),]
sumsex$cohort <- factor(sumsex$cohort, levels=orderlist)

### Generate faceting variables again
sumsex <- sumsex %>% mutate(row=case_when((cohort=="USOC" | cohort=="MCS")~1,
                                            (cohort=="ELSA" | cohort=="ALSPAC")~2,
                                            (cohort=="GS" | cohort== "NS")~3,
                                            (cohort=="TWINSUK" | cohort=="BCS70")~4,
                                            (cohort=="BiB" | cohort == "NCDS")~5, 
                                            (cohort =="NSHD"~6))
)
sumsex <- sumsex %>% mutate(type = (case_when(
  cohort %in% agehet ~ 1,
  cohort %in% agehom ~ 0)
))
sumsex$type <- recode(sumsex$type, `1`="Age Heterogeneous", `0`="Age Homogeneous")

colnames(sumsex) <- c("Cohort","timepoint","monthnum","Sex","mean","loCI","hiCI","row","type")


### Generate plot

sexplot <- ggplot(sumsex,
                         aes(x= monthnum,
                             y=mean,
                             colour=Cohort)) +
  geom_point (size=1.5) +
  geom_line(aes(linetype=Sex)) +
  ylab("Proportion over case threshold") +
  theme(axis.text.x=element_text(color = "black",
                                 size=8, angle=30, hjust=0.8)) +
  scale_x_continuous(breaks=c(0,3,6,9,12,15),
                     labels=c("0" = "Pre-Pandemic",
                              "3" = "March 2020",
                              "6"="June 2020",
                              "9"="September 2020",
                              "12"="December 2020",
                              "15"="March 2021")) +
  scale_fill_viridis()+
  #geom_label(cohort)+
  # geom_ribbon(aes(ymin=loCI, ymax=hiCI),
  #             linetype=1, alpha=0.2) +
  facet_grid(row~type) +
  theme(panel.border=element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  xlab("Pandemic Timepoint")+
  ylab("Case Prevalence (%)") +
  annotate("rect", xmin=xmins[1], xmax=xmaxs[1], ymin=-0.5, ymax=Inf, alpha=0.1, fill="purple")+
  annotate("rect", xmin=xmins[2], xmax=xmaxs[2], ymin=-0.5, ymax=Inf, alpha=0.1, fill="blue")+
  annotate("rect", xmin=xmins[3], xmax=xmaxs[3], ymin=-0.5, ymax=Inf, alpha=0.1, fill="green")+
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_colour_viridis_d(direction=-1)+
  geom_text(data=(groupdf %>% group_by(cohort) %>% top_n(1,monthnum)),
  label=unique(groupdf$cohort), y=5, size=2.5, colour="black", x = 14.5, hjust=1)

ann.text2 <- data.frame(monthnum = c(5, 8.5, 13), lab = c("Period 1", "Period 2", "Period 3"),
                       cohort=c("MCS", "MCS", "MCS"),
                       type = c("Age Heterogeneous","Age Heterogeneous","Age Heterogeneous"),
                       row = c(6,6,6),
                       mean = c(22,22,22))

annot_sex_plot <- sexplot+  geom_text(
  data=ann.text2,
  mapping = aes(x=monthnum, y=mean, label = lab),
  size=2.5,
  angle=45,
  colour="black"
) + facet_grid(row~factor(type, level=c("Age Homogeneous", "Age Heterogeneous")))
annot_sex_plot

ggsave("sexDescPlot.png", width=6, height=6, dpi=600)


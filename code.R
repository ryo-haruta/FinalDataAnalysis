###############################
## Data Management (with R)  ##
## FInal Data Project        ##
## Coder: Ryo Haruta         ##
###############################

#0. install and load packages
#install.packages("readxl")
library(readxl) #for readinv csv files
#install.packages("tidyverse")
library(tidyverse) # for making data tidy
#install.packages("stringr")
library(stringr) #for modifying strings
#install.packages("ggplot2")
library(ggplot2) # for plotting
#install.packages("ggrepel")
library(ggrepel) # for data label
#install.packages("stargazer")
library(stargazer) # for regression tables

#1.	Prepare data set for analysis (25%)
##data import
a_accused <- read_csv("data/a_wdb_accused.csv")
c_case <- read_csv("data/c_wdb_case.csv")
t_trial <- read_csv("data/t_wdb_trial.csv")
t_mentionedaswitch <- read_csv("data/t_wdb_mentionedaswitch.csv")
t_confession <- read_csv("data/t_wdb_confession.csv")
t_torture <- read_csv("data/t_wdb_torture.csv")

##sort and modify
a_accused <- a_accused %>% 
  arrange(accusedid) %>% 
  .[,c(1:7, 11, 18)]

c_case <- c_case %>% 
  arrange(caseid) %>% 
  .[,c(1:3, 5, 11, 18:47, 52:53, 58, 60:66, 70:79, 81:97)]

t_trial <- t_trial %>% 
  arrange(trialsystemid, trialid) %>% 
  filter(!is.na(verdict) | !is.na(sentence)) %>% 
  .[,c(1:5, 38:39)]

t_mentionedaswitch <- t_mentionedaswitch %>% 
  arrange(mentionsystemid, mentionid) %>% 
  .[,c(1:4, 7, 11)]
  
t_confession <- t_confession %>% 
  arrange(confessionsystemid, confessionid) %>% 
  .[,c(1:4)]

t_torture <- t_torture %>% 
  arrange(torturesystemid, tortureid) %>% 
  .[,c(1:5, 8)]

##check data
table(a_accused$accusedref)
hist(table(a_accused$accusedref))

table(c_case$accusedref)
hist(table(c_case$accusedref))

table(t_trial$caseref)
hist(table(t_trial$caseref))

###cut doubled
temp <- table(t_trial$caseref) == 2
temp <- temp[temp == TRUE]
temp <- t_trial %>% 
  filter(caseref %in% names(temp)) %>% 
  arrange(caseref)
t_trial <- t_trial %>% 
  filter(!(trialref %in% c("T/JO/966", "T/JO/1258", "T/LA/90", "T/LA/302", "T/LA/320", "T/LA/1012", "T/LA/1247")))
###end

table(t_trial$caseref)
hist(table(t_trial$caseref))

table(t_mentionedaswitch$trialref)
hist(table(t_mentionedaswitch$trialref))

table(t_confession$trialref)
hist(table(t_confession$trialref))

table(t_torture$trialref)
hist(table(t_torture$trialref))

##combine datasets
###trial level
####t_trial with t_mentionaswitch
temp <- table(t_mentionedaswitch$trialref)
t_mentionedaswitch_n <- tibble(trialref = names(temp), mentionedaswitch_n = temp, mentionedaswitch_e = TRUE) %>% 
  arrange(trialref)
trial_int <- left_join(t_trial, t_mentionedaswitch_n, by = "trialref")

temp <- table(t_mentionedaswitch$mentionedintrialofref)
t_mentionaswitch_n <- tibble(trialref = names(temp), mentionaswitch_n = temp, mentionaswitch_e = TRUE) %>% 
  arrange(trialref)
trial_int <- left_join(trial_int, t_mentionaswitch_n, by = "trialref")

####t_trial with t_confession
temp <- table(t_confession$trialref)
t_confession_n <- tibble(trialref = names(temp), confession_n = temp, confession_e = TRUE) %>% 
  arrange(trialref)
trial_int <- left_join(trial_int, t_confession_n, by = "trialref")

####t_trial with t_torture
temp <- table(t_torture$trialref)
t_torture_n <- tibble(trialref = names(temp), torture_n = temp, torture_e = TRUE) %>% 
  arrange(trialref)
trial_int <- left_join(trial_int, t_torture_n, by = "trialref")

####modify
unique(trial_int$verdict)
unique(trial_int$sentence)
trial_int$guilty <- trial_int$sentence %in% c("Execution", "Banishment", "Excommunicated", "Hang",
                                              "Put to the horn", "Declared Fugitive", "Branded",
                                              "Prison", "Public Humiliation") |
  trial_int$verdict %in% c("Guilty", "Half Guilty")
table(trial_int$guilty)

###whole
####trial_int with c_case
witch <- left_join(trial_int, c_case, by = "caseref")

####witch with a_accused
witch <- left_join(witch, a_accused, by = "accusedref") %>% 
  .[,c(-1:-4, -17:-19, -88:-89)]

####modify 
witch$mentionedaswitch_n <- replace(witch$mentionedaswitch_n, which(is.na(witch$mentionedaswitch_n)), 0)
witch$mentionedaswitch_e <- replace(witch$mentionedaswitch_e, which(is.na(witch$mentionedaswitch_e)), FALSE)

witch$mentionaswitch_n <- replace(witch$mentionaswitch_n, which(is.na(witch$mentionaswitch_n)), 0)
witch$mentionaswitch_e <- replace(witch$mentionaswitch_e, which(is.na(witch$mentionaswitch_e)), FALSE)

witch$confession_n <- replace(witch$confession_n, which(is.na(witch$confession_n)), 0)
witch$confession_e <- replace(witch$confession_e, which(is.na(witch$confession_e)), FALSE)

witch$torture_n <- replace(witch$torture_n, which(is.na(witch$torture_n)), 0)
witch$torture_e <- replace(witch$torture_e, which(is.na(witch$torture_e)), FALSE)

witch[129, 82] <- "Stewart"

####create initials and ends
witch$firstname_i <- toupper(str_sub(witch$firstname, 1, 1))
witch$firstname_e <- toupper(str_sub(witch$firstname, -1, -1))
witch$lastname_i <- toupper(str_sub(witch$lastname, 1, 1))
witch$lastname_e <- toupper(str_sub(witch$lastname, -1, -1))

######################
#2.	Perform exploratory data analysis and visualize interesting descriptive facts about data (25%)
##tables
table(witch$guilty)
table(witch$sex)
table(witch$res_county)

table(witch$guilty, witch$sex)
table(witch$guilty, witch$res_county)

##graphs
jpeg("01_gender.jpg", width = 700, height = 700)
ggplot(data = witch,
            mapping = aes(x = sex, fill = guilty)) + 
  geom_bar(position = "fill") +
  geom_text(stat='count', aes(label = ..count..), size = 4, position = position_fill(vjust = 0.5)) +
  scale_x_discrete(na.translate = FALSE) + 
  scale_fill_manual(values = c("steelblue2", "indianred1"), name = "", labels = c("Innocence", "Guilty")) +
  labs(x = "Gender",
       y = "Share")
dev.off()

jpeg("02_county.jpg", width = 700, height = 700)
ggplot(data = witch,
       mapping = aes(x = res_county, fill = guilty)) + 
  geom_bar(position = "fill") +
  geom_text(stat='count', aes(label = ..count..), size = 3, position = position_fill(vjust = 0.5)) +
  scale_x_discrete(na.translate = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) + 
  scale_fill_manual(values = c("steelblue2", "indianred1"), name = "", labels = c("Innocence", "Guilty")) +
  labs(x = "County",
       y = "Share")
dev.off()

###appendix
jpeg("a_01_firstname.jpg", width = 2000, height = 700)
ggplot(data = witch,
       mapping = aes(x = firstname, fill = guilty)) + 
  geom_bar(position = "fill") +
  geom_text(stat='count', aes(label = ..count..), size = 3, position = position_fill(vjust = 0.5)) +
  scale_x_discrete(na.translate = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) + 
  scale_fill_manual(values = c("steelblue2", "indianred1"), name = "", labels = c("Innocence", "Guilty")) +
  labs(x = "Firstname",
       y = "Share")
dev.off()

jpeg("a_02_lastname.jpg", width = 4000, height = 700)
ggplot(data = witch,
       mapping = aes(x = lastname, fill = guilty)) + 
  geom_bar(position = "fill") +
  geom_text(stat='count', aes(label = ..count..), size = 3, position = position_fill(vjust = 0.5)) +
  scale_x_discrete(na.translate = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) + 
  scale_fill_manual(values = c("steelblue2", "indianred1"), name = "", labels = c("Innocence", "Guilty")) +
  labs(x = "Lastname",
       y = "Share")
dev.off()

######################
#3.	Develop and specify ideas for analyzing data and run the appropriate statistical model (25%)
##relationship between guilty and variables
m1_1_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex,
              witch, family = binomial(link = logit))
m1_2_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + res_county,
              witch, family = binomial(link = logit))
m1_3_l <- glm(guilty ~ mentionedaswitch_n + confession_n + torture_n + sex,
              witch, family = binomial(link = logit))
m1_4_l <- glm(guilty ~ mentionedaswitch_n + confession_n + torture_n + sex + res_county,
              witch, family = binomial(link = logit))
stargazer(list(m1_1_l, m1_2_l, m1_3_l, m1_4_l),
          out = "m1_1_l.html",
          float = F, header = F, single.row = T, align = T, font.size = 'footnotesize',
          dep.var.labels = "Guilty",
          covariate.labels = c("Mentioned As Witch", "Confession", "Torture", "Nr. Mentioned As Witch",
                               "Nr. Confession", "Nr. Torture", "Male",
                               "Argyll", "Ayr", "Banff", "Berwick", "Bute", "Clackmannan", "Dumfries", "Dunbarton",
                               "Edinburgh", "Elgin", "Fife", "Forfar", "Haddington", "Inverness", "Kinross",
                               "Kirkcudbright", "Lanark", "Linlithgow", "Orkney", "Peebles", "Perth", "Renfrew",
                               "Ross", "Selkirk", "Shetland", "Stirling", "Wigtown"))

###check quadratic (appendix)
m1_3_2_l <- glm(guilty ~ mentionedaswitch_n + confession_n + I(confession_n^2) + 
                  torture_n + sex,
                witch, family = binomial(link = logit))
m1_4_2_l <- glm(guilty ~ mentionedaswitch_n + confession_n + I(confession_n^2) + 
                  torture_n + sex + res_county,
              witch, family = binomial(link = logit))

stargazer(list(m1_3_l, m1_3_2_l, m1_4_l, m1_4_2_l),
          out = "a_m1_2_l.html",
          float = F, header = F, single.row = T, align = T, font.size = 'footnotesize',
          dep.var.labels = "Guilty",
          covariate.labels = c("Nr. Mentioned As Witch", "Nr. Confession", "Nr. Confession ^ 2", "Nr. Torture", "Male",
                               "Argyll", "Ayr", "Banff", "Berwick", "Bute", "Clackmannan", "Dumfries", "Dunbarton",
                               "Edinburgh", "Elgin", "Fife", "Forfar", "Haddington", "Inverness", "Kinross",
                               "Kirkcudbright", "Lanark", "Linlithgow", "Orkney", "Peebles", "Perth", "Renfrew",
                               "Ross", "Selkirk", "Shetland", "Stirling", "Wigtown"))

##relationship between variables
m2_1_l <- glm(confession_e ~ torture_e,
              witch, family = binomial(link = logit))
m2_2_l <- glm(mentionedaswitch_e ~ confession_e,
              witch, family = binomial(link = logit))

stargazer(list(m2_1_l, m2_2_l),
          out = "a_m2_l.html",
          float = F, header = F, single.row = T, align = T, font.size = 'footnotesize',
          dep.var.labels = c("Confession", "Mentioned As Witch"),
          covariate.labels = c("Torture", "Confession"))

##relationship between guilty and names (appendix)
m3_1_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + firstname,
              witch, family = binomial(link = logit))
m3_2_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + lastname,
              witch, family = binomial(link = logit))

m3_3_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + m_firstname,
              witch, family = binomial(link = logit))
m3_4_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + m_surname,
              witch, family = binomial(link = logit))

stargazer(m3_1_l,
          out = "m3_1_l.html",
          float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

stargazer(m3_2_l,
         out = "a_m3_2_l.html",
         float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

stargazer(m3_3_l,
         out = "a_m3_3_l.html",
         float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

stargazer(m3_4_l,
         out = "a_m3_4_l.html",
         float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

##relationship between guilty and initials or ends (appendix)
m4_1_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + firstname_i,
              witch, family = binomial(link = logit))
m4_2_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + lastname_i,
              witch, family = binomial(link = logit))
m4_3_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + firstname_e,
              witch, family = binomial(link = logit))
m4_4_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex + lastname_e,
              witch, family = binomial(link = logit))

stargazer(m4_1_l,
         out = "a_m4_1_l.html",
         float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

stargazer(m4_2_l,
         out = "a_m4_2_l.html",
         float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

stargazer(m4_3_l,
         out = "a_m4_3_l.html",
         float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

stargazer(m4_4_l,
         out = "a_m4_4_l.html",
         float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

##relationship between guilty and features (appendix)
m5_l <- glm(guilty ~ mentionedaswitch_e + confession_e + torture_e + sex +
              unorthodoxrelpract_p + unorthodoxrelpract_s +
              consulting_p + consulting_s +
              demonic_p + demonic_s +
              demonic_possess_p + demonic_possess_s +
              fairies_p + fairies_s +
              folk_healing_p + folk_healing_s +
              maleficium_p + maleficium_s +
              midwifery_p + midwifery_s +
              implicatedbyanother_p + implicatedbyanother_s +
              neighbhd_dispute_p + neighbhd_dispute_s +
              politicalmotive_p + politicalmotive_s +
              propertymotive_p + propertymotive_s +
              refusedcharity_p + refusedcharity_s +
              treason_p + treason_s +
              other_p + other_s +
              whitemagic_p + whitemagic_s +
              witchesmeeting +
              devilpresent +
              maleficium +
              communalsex +
              devilworship +
              foodanddrink +
              dancing +
              singing +
              elphane_fairyland +
              food_drink +
              specificverbalformulae +
              specificritualacts +
              familiars +
              shape_changing +
              dreams_visions +
              unorthodoxreligiouspractice +
              sympatheticmagic +
              ridingdead +
              humanillness +
              humandeath +
              animalillness +
              animaldeath +
              femaleinfertility +
              maleimpotence +
              aggravatingdisease +
              transferringdisease +
              layingon +
              removalbewitchment +
              quarreling +
              cursing +
              poisoning +
              rechealer +
              healinghumans +
              healinganimals +
              midwifery,
              witch, family = binomial(link = logit))

stargazer(m5_l,
          out = "a_m5_l.html",
          float = F, header = F, single.row = T, align = T, font.size = 'footnotesize')

######################
#4.	Present and discuss results (25%)
##predictions for m1_1_l
df <- data.frame(mentionedaswitch_e = FALSE,
                 confession_e = c(FALSE, TRUE),
                 torture_e = FALSE,
                 sex = "Female")

preds <- predict(m1_1_l, newdata = df, type = 'response', se.fit = TRUE)

df$prediction <- preds$fit
df$lower <- preds$fit - 1.96 * preds$se.fit
df$upper <- preds$fit + 1.96 * preds$se.fit

jpeg("91_pred_m1_1_l.jpg", width = 700, height = 700)
ggplot(df, aes(x = confession_e)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = prediction), size = 5) +
  labs(x = "Confession",
       y = "Guilty Probability")
dev.off()

##predictions for m1_2_l
df <- data.frame(mentionedaswitch_e = FALSE,
                 confession_e = FALSE,
                 torture_e = FALSE,
                 sex = "Female",
                 res_county = sort(unique(witch$res_county)))

preds <- predict(m1_2_l, newdata = df, type = 'response', se.fit = TRUE)

df$prediction <- preds$fit
df$lower <- preds$fit - 1.96 * preds$se.fit
df$upper <- preds$fit + 1.96 * preds$se.fit

jpeg("92_pred_m1_2_l.jpg", width = 700, height = 700)
ggplot(df, aes(x = res_county)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = prediction), size = 5) +
  labs(x = "County",
       y = "Guilty Probability") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
dev.off()

##predictions for m1_3_l
df <- data.frame(mentionedaswitch_n = 0,
                 confession_n = min(witch$confession_n):max(witch$confession_n),
                 torture_n = 0,
                 sex = "Female")

preds <- predict(m1_3_l, newdata = df, type = 'response', se.fit = TRUE)

df$prediction <- preds$fit
df$lower <- preds$fit - 1.96 * preds$se.fit
df$upper <- preds$fit + 1.96 * preds$se.fit

jpeg("92_pred_m1_3_l.jpg", width = 700, height = 700)
ggplot(df, aes(x = confession_n)) +
  geom_line(aes(y = prediction)) + 
  theme_bw() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
  labs(x = "Number of Confessions",
       y = "Guilty Probability")
dev.off()

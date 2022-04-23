gss2020panel_r1a  -> df

table(df$year_1b,df$year_2)
table(df$year_1a,df$year_2)

recode_dum <- function(x) { ifelse(x==1,1,0)}

df %>% filter(year_1a==2016) -> df2 ; table(df2$racdif1_1a)

racdif1_2016 <- recode_dum(df$racdif1_1a)
racdif2_2016 <- recode_dum(df$racdif2_1a)
racdif3_2016 <- recode_dum(df$racdif3_1a)
racdif4_2016 <- recode_dum(df$racdif4_1a)

racdif_none_2016 <- racdif1_2016+racdif2_2016+racdif3_2016+racdif4_2016
table(racdif_none_2016)

racdif1_2018 <- recode_dum(df$racdif1_1b)
racdif2_2018 <- recode_dum(df$racdif2_1b)
racdif3_2018 <- recode_dum(df$racdif3_1b)
racdif4_2018 <- recode_dum(df$racdif4_1b)

racdif_none_2018 <- racdif1_2018+racdif2_2018+racdif3_2018+racdif4_2018
table(racdif_none_2018)

racdif1_2020 <- recode_dum(df$racdif1_2)
racdif2_2020 <- recode_dum(df$racdif2_2)
racdif3_2020 <- recode_dum(df$racdif3_2)
racdif4_2020 <- recode_dum(df$racdif4_2)

racdif_none_2020 <- racdif1_2020+racdif2_2020+racdif3_2020+racdif4_2020
table(racdif_none_2020)

as.data.frame.matrix(table(racdif_none_2018,racdif_none_2020)) -> mat
as.matrix(mat) -> mat


race_in_2016 <- ifelse(racdif2_2016 ==1 | racdif4_2016==1, 1,0)
race_struc_2016 <- ifelse(racdif1_2016 ==1 | racdif3_2016==1, 1,0)
race_none_2016 <- ifelse(race_in_2016 !=1 & race_struc_2016!=1,1,0)

racdif_2016 <- ifelse(race_struc_2016==1 & race_in_2016==0,"struc",
                      ifelse(race_struc_2016==1 & race_in_2016==1,"both",
                             ifelse(race_struc_2016==0 & race_in_2016==1,"ind","none")))


race_in_2018 <- ifelse(racdif2_2018 ==1 | racdif4_2018==1, 1,0)
race_struc_2018 <- ifelse(racdif1_2018 ==1 | racdif3_2018==1, 1,0)
race_none_2018 <- ifelse(race_in_2018 !=1 & race_struc_2018!=1,1,0)

racdif_2018 <- ifelse(race_struc_2018==1 & race_in_2018==0,"struc",
                      ifelse(race_struc_2018==1 & race_in_2018==1,"both",
                             ifelse(race_struc_2018==0 & race_in_2018==1,"ind","none")))
                      
race_in_2020 <- ifelse(racdif2_2020 ==1 | racdif4_2020==1, 1,0)
race_struc_2020 <- ifelse(racdif1_2020 ==1 | racdif3_2020==1, 1,0)
race_none_2020 <- ifelse(race_in_2020 !=1 & race_struc_2020!=1,1,0)

racdif_2020 <- ifelse(race_struc_2020==1 & race_in_2020==0,"struc",
                      ifelse(race_struc_2020==1 & race_in_2020==1,"both",
                             ifelse(race_struc_2020==0 & race_in_2020==1,"ind","none")))

table(racdif_2016,racdif_2020)
table(racdif_2018,racdif_2020)

racdif_to_struc <- ifelse(racdif_2016 %in% c("ind","none") & racdif_2020 %in% c("both","struc"),1,0)  
racdif_to_struc <- ifelse(racdif_2018 %in% c("ind","none") & racdif_2020 %in% c("both","struc"),1,racdif_to_struc)  
racdif_to_struc <- ifelse((is.na(racdif_2016) & is.na(racdif_2020)) | (is.na(racdif_2018) & is.na(racdif_2020)),NA, racdif_to_struc)
table(racdif_to_struc)

racdif_to_ind <- ifelse(racdif_2016 %in% c("struc","both") & racdif_2020 %in% c("ind","none"),1,0)  
racdif_to_ind <- ifelse(racdif_2018 %in% c("struc","both") & racdif_2020 %in% c("ind","none"),1,racdif_to_ind)  
racdif_to_ind <- ifelse((is.na(racdif_2016) & is.na(racdif_2020)) | (is.na(racdif_2018) & is.na(racdif_2020)),NA, racdif_to_ind)
table(racdif_to_ind)

racdif_to_none <- ifelse(racdif_2016 %in% c("struc","both","ind") & racdif_2020 %in% c("none"),1,0)  
racdif_to_none <- ifelse(racdif_2018 %in% c("struc","both","ind") & racdif_2020 %in% c("none"),1,racdif_to_none)  
racdif_to_none <- ifelse((is.na(racdif_2016) & is.na(racdif_2020)) | (is.na(racdif_2018) & is.na(racdif_2020)),NA, racdif_to_none)
table(racdif_to_none)

racdif_to_both <- ifelse(racdif_2016 %in% c("struc","none","ind") & racdif_2020 %in% c("both"),1,0)  
racdif_to_both <- ifelse(racdif_2018 %in% c("struc","none","ind") & racdif_2020 %in% c("both"),1,racdif_to_none)  
racdif_to_both <- ifelse((is.na(racdif_2016) & is.na(racdif_2020)) | (is.na(racdif_2018) & is.na(racdif_2020)),NA, racdif_to_both)
table(racdif_to_both)


summary(glm(racdif_to_none~as.factor(df$partyid_2), family=binomial))

summary(glm(racdif_to_ind~as.factor(df$partyid_2), family=binomial))
summary(glm(racdif_to_ind+racdif_to_none>0~as.factor(df$partyid_2)+df$year_flag, family=binomial))

summary(glm(racdif_to_struc~as.factor(df$partyid_2)+df$year_flag, family=binomial))

df$switch_dem <- ifelse((partyid_2 <3 & partyid_1a>2) |(partyid_2 <3 & partyid_1b>2),1,0)
df$year_flag <-  ifelse(is.na(year_1a),1,0)
table(df$year_flag)

df$race <-  ifelse(is.na(race_1a),race_1b,race_1a)


summary(glm(racdif_to_struc~df$partyid_2+df$year_flag+as.factor(df$degree_2)+(df$pres16_2==2)+as.factor(df$race), family=binomial))
summary(glm(racdif_to_ind~df$switch_dem*df$year_flag+as.factor(df$degree_2)+(df$pres16_2==2)+as.factor(df$race), family=binomial))
summary(glm(racdif_to_none~df$switch_dem+df$year_flag+as.factor(df$degree_2)+(df$pres16_2==2)+as.factor(df$race), family=binomial))
summary(glm(racdif_to_both~df$switch_dem+df$year_flag+as.factor(df$degree_2) +as.factor(df$protest3_1a)+as.factor(df$race), family=binomial))
summary(glm(racdif_to_none+racdif_to_ind+racdif_to_struc+racdif_to_both>=1~as.factor(df$partyid_2)+as.factor(df$race)+df$year_flag+(df$pres16_2==2), family=binomial))

summary(glm((racdif_2020=="struc" & racdif_to_struc == 0)~as.factor(df$partyid_2)+df$year_flag+as.factor(df$degree_2)+df$race, family=binomial))

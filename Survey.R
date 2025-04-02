pacman::p_load(haven, tidyverse, purrr, forcats, dplyr, survey, psych, gtsummary, kableExtra, car, gt, htmltools, ggplot2)

DCMS<-read_dta("participation_2021-22_annual_data_open.dta")

DCMS<-select(DCMS,c('WELLB1','AGESHORT'
                    ,'FINHARD' 
                    ,'CHERVIS12_001','CFREHER12_a'
                    ,'finalweight','MasterStratum2'))


#function to change "multi selected", "not answered", "not applicable" to NA; keeping "prefer not to say"
miss <- function(x) {
  x[x <= -2] <- NA
  x
}

#function to change variables into factors
fac <- function(x) as_factor(x)

#function to drop levels
d_lvl <- function(x) droplevels(x)

#stating which variables to impute for function
cols_to_impute <- c("WELLB1", "AGESHORT", "FINHARD", "CHERVIS12_001")

#removing "don't know"
DCMS$CFREHER12_a[DCMS$CFREHER12_a == 999] <- NA
DCMS$CFREHER12_a[DCMS$CFREHER12_a<=-4]<-NA

#changing missing responses to NA
DCMS[cols_to_impute] <- map(DCMS[cols_to_impute], miss)

#changing to factors
DCMS[cols_to_impute] <- map(DCMS[cols_to_impute], fac)
DCMS$CFREHER12_a<-fac(DCMS$CFREHER12_a)

#removing all rows with NA
DCMS <- DCMS[complete.cases(DCMS), ]

#dropping levels
DCMS[cols_to_impute] <- map(DCMS[cols_to_impute], d_lvl)
DCMS$CFREHER12_a<-d_lvl(DCMS$CFREHER12_a)

#renaming columns
DCMS<-DCMS%>%rename("Life Satisfaction"= WELLB1, "Age Group"= AGESHORT, "Financial Situation"=FINHARD,
                    "Visited in last 12 months"=CHERVIS12_001,
                    "Frequency of visit"=CFREHER12_a)

#descriptive table
smpl<-tbl_summary(DCMS, include = c("Life Satisfaction","Age Group","Financial Situation","Visited in last 12 months","Frequency of visit"))%>%bold_labels()%>%modify_header(label='Variables')%>%
  as_kable() %>%
  kable_styling(font_size = 12, bootstrap_options = c("striped", "condensed"))

smpl


#turning life satisfaction into numeric for regression model
DCMS$`Life Satisfaction` <- as.numeric(as.character(DCMS$`Life Satisfaction`))

#survey design object
dcms_svy <- svydesign(strata = ~MasterStratum2, id=~1, weights = ~finalweight,
                    nest = TRUE, data=DCMS)

#no interaction terms
model1 <- svyglm(`Life Satisfaction` ~ 
                   `Frequency of visit` + 
                   `Age Group` + `Financial Situation`, 
                 design = dcms_svy)

summary(model1)
vif(model1)

freq<-summary(model1)

model_df1 <- as.data.frame(coef(freq))
colnames(model_df1) <- c("Estimate", "Std.Error", "t value", "P value")
model_df1 %>%
  kable(digits = 3, format = "html", caption = "Survey-weighted Regression Results for frequency of visiting a historic town/city") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

#interaction terms
model2 <- svyglm(`Life Satisfaction` ~ 
                   `Age Group`*`Frequency of visit` +
                   `Financial Situation`*`Frequency of visit`,
                 design = dcms_svy)

summary(model2)
vif(model2)
vif(model2, type = "predictor")

freq2<-summary(model2)
model_df2 <- as.data.frame(coef(freq2))
colnames(model_df1) <- c("Estimate", "Std.Error", "t value", "P value")
model_df2 %>%
  kable(digits = 3, format = "html", caption = "Survey-weighted Regression Results for frequency of visiting a historic town/city") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

#calculate AIC
AIC(model1)
AIC(model2)
#model 2 performs better


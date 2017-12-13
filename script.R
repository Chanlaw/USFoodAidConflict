library("foreign")
library("DataCombine")
library("sandwich")
data <- read.dta("data.dta")

#create the lagged wheat_production variable
data <- slide(data, Var="US_wheat_production", GroupVar = "risocode", slideBy=-1)
attach(data)
lagged_wheat_prod <- as.matrix(data["US_wheat_production-1"])
options(na.action="na.exclude") #we need to do this to make 2SLS easier

# 
aid_by_year <- aggregate(wheat_aid ~ year, FUN=sum)
war_freq_by_year <- aggregate(any_war ~ year, FUN=mean)
wheat_prod_by_year <- aggregate(lagged_wheat_prod ~ year, FUN=mean)
df1 <- merge(war_freq_by_year, wheat_prod_by_year)
df1 <- merge(df1, aid_by_year)

plot(as.matrix(df1["US_wheat_production-1"]), df1$wheat_aid, xlab="Lagged US Wheat Production, (Metric Tons)", ylab="Total US Wheat Aid, (Metric Tons)")
text(as.matrix(df1["US_wheat_production-1"]), df1$wheat_aid, df1$year, cex=0.6, pos=3)
abline(lm(df1$wheat_aid ~ as.matrix(df1["US_wheat_production-1"])))

plot(as.matrix(df1["US_wheat_production-1"]), df1$any_war, xlab="Lagged US Wheat Production, (Metric Tons)", ylab="Frequency of War")
text(as.matrix(df1["US_wheat_production-1"]), df1$any_war, df1$year, cex=0.6, pos=4)
abline(lm(df1$any_war ~ as.matrix(df1["US_wheat_production-1"])))

# OLS Regressions
wheat_aid <- wheat_aid/1000
lagged_wheat_prod <- lagged_wheat_prod/1000
model1 <- lm(any_war ~ wheat_aid + risocode + wb_region*year)
model2 <- lm(any_war ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg)
model3 <- lm(any_war ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                        + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                        + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                        + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                        + all_Precip_may_faavg + all_Precip_jun_faavg
                        + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                        + all_Precip_nov_faavg + all_Precip_dec_faavg
                        + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                        + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                        + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                        + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                        + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg)
model4 <- lm(any_war ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
             + real_usmilaid + real_us_nonfoodaid_ecaid)
model5 <- lm(any_war ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
             + real_usmilaid + real_us_nonfoodaid_ecaid
             + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
model6 <- lm(intra_state ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
             + real_usmilaid + real_us_nonfoodaid_ecaid
             + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
model7 <- lm(inter_state ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
             + real_usmilaid + real_us_nonfoodaid_ecaid
             + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
plot(model5)

#First stage Regression for 2SLS
fs_model1 <- lm(wheat_aid ~ fadum_avg*lagged_wheat_prod + risocode + wb_region*year)
fs_model2 <- lm(wheat_aid ~ fadum_avg*lagged_wheat_prod + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg)
fs_model3 <- lm(wheat_aid ~ fadum_avg*lagged_wheat_prod + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg)
fs_model4 <- lm(wheat_aid ~ fadum_avg*lagged_wheat_prod + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
             + real_usmilaid + real_us_nonfoodaid_ecaid)
fs_model5 <- lm(wheat_aid ~ fadum_avg*lagged_wheat_prod + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
             + real_usmilaid + real_us_nonfoodaid_ecaid
             + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
summary(fs_model5)
plot(fs_model5)

#Second stage regression for 2SLS

ss_model1 <- lm(any_war ~ fadum_avg*fitted(fs_model1) + risocode + wb_region*year)
ss_model2 <- lm(any_war ~ fadum_avg*fitted(fs_model2) + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg)
ss_model3 <- lm(any_war ~ fadum_avg*fitted(fs_model3) + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                + all_Precip_may_faavg + all_Precip_jun_faavg
                + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                + all_Precip_nov_faavg + all_Precip_dec_faavg
                + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg)
ss_model4 <- lm(any_war ~ fadum_avg*fitted(fs_model3)+ risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                + all_Precip_may_faavg + all_Precip_jun_faavg
                + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                + all_Precip_nov_faavg + all_Precip_dec_faavg
                + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                + real_usmilaid + real_us_nonfoodaid_ecaid)
ss_model5 <- lm(any_war ~ fadum_avg*fitted(fs_model4) + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                + all_Precip_may_faavg + all_Precip_jun_faavg
                + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                + all_Precip_nov_faavg + all_Precip_dec_faavg
                + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                + real_usmilaid + real_us_nonfoodaid_ecaid
                + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
ss_model6 <- lm(intra_state ~ fadum_avg*fitted(fs_model5) + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                + all_Precip_may_faavg + all_Precip_jun_faavg
                + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                + all_Precip_nov_faavg + all_Precip_dec_faavg
                + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                + real_usmilaid + real_us_nonfoodaid_ecaid
                + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
ss_model7 <- lm(inter_state ~ fadum_avg*fitted(fs_model5) + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                + all_Precip_may_faavg + all_Precip_jun_faavg
                + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                + all_Precip_nov_faavg + all_Precip_dec_faavg
                + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                + real_usmilaid + real_us_nonfoodaid_ecaid
                + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)

summary(ss_model1)
summary(ss_model2)
summary(ss_model3)
summary(ss_model4)
summary(ss_model5)
summary(ss_model6)
summary(ss_model7)
plot(ss_model5)

#Breusch-Pagan test for Heteroscedasticity

resid5.2 <- resid(model5)^2
resid_model5 <- lm(resid5.2 ~  wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                  + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                  + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                  + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                  + all_Precip_may_faavg + all_Precip_jun_faavg
                  + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                  + all_Precip_nov_faavg + all_Precip_dec_faavg
                  + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                  + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                  + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                  + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                  + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                  + real_usmilaid + real_us_nonfoodaid_ecaid
                  + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
summary(resid_model5)
nR5.2 <- length(resid_model5$fitted.values) * summary(resid_model5)$r.squared
length(resid_model5$fitted.values)
nR5.2
1 - pchisq(nR5.2, length(resid_model5$fitted.values))

resid6.2 <- resid(model6)^2
resid_model6 <- lm(resid6.2 ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
             + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
             + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
             + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
             + all_Precip_may_faavg + all_Precip_jun_faavg
             + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
             + all_Precip_nov_faavg + all_Precip_dec_faavg
             + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
             + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
             + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
             + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
             + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
             + real_usmilaid + real_us_nonfoodaid_ecaid
             + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
nR6.2 <- length(resid_model6$fitted.values) * summary(resid_model6)$r.squared
length(resid_model6$fitted.values)
nR6.2
1 - pchisq(nR6.2, length(resid_model6$fitted.values))

ss_resid2 <- resid(ss_model5)^2
ss_resid_model <- lm(ss_resid2 ~ fadum_avg*fitted(fs_model4) + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                + all_Precip_may_faavg + all_Precip_jun_faavg
                + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                + all_Precip_nov_faavg + all_Precip_dec_faavg
                + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                + real_usmilaid + real_us_nonfoodaid_ecaid
                + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
summary(ss_resid_model)
nR2 <- length(ss_resid_model$fitted.values) * summary(ss_resid_model)$r.squared
1 - pchisq(nR2,length(ss_resid_model$fitted.values))

ss_resid6.2 <- resid(ss_model6)^2
ss_resid_model6 <- lm(ss_resid6.2 ~ fadum_avg*fitted(fs_model4) + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                     + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                     + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                     + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                     + all_Precip_may_faavg + all_Precip_jun_faavg
                     + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                     + all_Precip_nov_faavg + all_Precip_dec_faavg
                     + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                     + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                     + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                     + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                     + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                     + real_usmilaid + real_us_nonfoodaid_ecaid
                     + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
summary(ss_resid_model6)
nR2 <- length(ss_resid_model6$fitted.values) * summary(ss_resid_model6)$r.squared
length(ss_resid_model6$fitted.values)
1 - pchisq(nR2,length(ss_resid_model6$fitted.values))

#Haussmann Test for Endogeneity
model5Haussmann <- lm(any_war ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                      + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                      + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                      + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                      + all_Precip_may_faavg + all_Precip_jun_faavg
                      + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                      + all_Precip_nov_faavg + all_Precip_dec_faavg
                      + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                      + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                      + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                      + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                      + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                      + real_usmilaid + real_us_nonfoodaid_ecaid
                      + recipient_cereals_prod + cereal_pc_import_quantity_avg*year
                      + resid(fs_model5))
vcov_H <- vcov(model5Haussmann)["resid(fs_model5)", "resid(fs_model5)"]
beta_H <- coef(model5Haussmann)["resid(fs_model5)"]
F_stat <-t(beta_H)%*%solve(vcov_H)%*%beta_H
F_stat
1-pchisq(F_stat,1)

model6Haussmann <- lm(intra_state ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                      + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                      + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                      + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                      + all_Precip_may_faavg + all_Precip_jun_faavg
                      + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                      + all_Precip_nov_faavg + all_Precip_dec_faavg
                      + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                      + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                      + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                      + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                      + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                      + real_usmilaid + real_us_nonfoodaid_ecaid
                      + recipient_cereals_prod + cereal_pc_import_quantity_avg*year
                      + resid(fs_model5))
vcov_H <- vcov(model6Haussmann)["resid(fs_model5)", "resid(fs_model5)"]
beta_H <- coef(model6Haussmann)["resid(fs_model5)"]
F_stat <-t(beta_H)%*%solve(vcov_H)%*%beta_H
F_stat
1-pchisq(F_stat,1)

#Sagran test for validity of overidentification assumption
hat_resid <- resid(ss_model5)
model5Sargan <- lm(hat_resid ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                   + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                   + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                   + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                   + all_Precip_may_faavg + all_Precip_jun_faavg
                   + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                   + all_Precip_nov_faavg + all_Precip_dec_faavg
                   + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                   + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                   + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                   + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                   + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                   + real_usmilaid + real_us_nonfoodaid_ecaid
                   + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
summary(model5Sargan)
nR_u2<- length(fitted(model5Sargan)) * (summary(model5Sargan)$r.squared)
nR_u2
1-pchisq(nR_u2, length(fitted(model5Sargan))) 

hat_resid6 <- resid(ss_model6)
model6Sargan <- lm(hat_resid6 ~ wheat_aid + risocode + wb_region*year + USA_rgdpch*fadum_avg + US_president_democ*fadum_avg + oil_price_2011_USD*fadum_avg
                   + all_Precip_jan + all_Precip_feb + all_Precip_mar + all_Precip_apr + all_Precip_may + all_Precip_jun
                   + all_Precip_jul + all_Precip_aug + all_Precip_sep + all_Precip_oct + all_Precip_nov + all_Precip_dec
                   + all_Precip_jan_faavg + all_Precip_feb_faavg + all_Precip_mar_faavg + all_Precip_apr_faavg 
                   + all_Precip_may_faavg + all_Precip_jun_faavg
                   + all_Precip_jul_faavg + all_Precip_aug_faavg + all_Precip_sep_faavg + all_Precip_oct_faavg 
                   + all_Precip_nov_faavg + all_Precip_dec_faavg
                   + all_Temp_jan + all_Temp_feb + all_Temp_mar + all_Temp_apr + all_Temp_may + all_Temp_jun
                   + all_Temp_jul + all_Temp_aug + all_Temp_sep + all_Temp_oct + all_Temp_nov + all_Temp_dec
                   + all_Temp_jan_faavg + all_Temp_feb_faavg + all_Temp_mar_faavg + all_Temp_apr_faavg 
                   + all_Temp_may_faavg + all_Temp_jun_faavg + all_Temp_jul_faavg + all_Temp_aug_faavg
                   + all_Temp_sep_faavg + all_Temp_oct_faavg + all_Temp_nov_faavg + all_Temp_dec_faavg
                   + real_usmilaid + real_us_nonfoodaid_ecaid
                   + recipient_cereals_prod + cereal_pc_import_quantity_avg*year)
summary(model6Sargan)
nR_u2<- length(fitted(model6Sargan)) * (summary(model6Sargan)$r.squared)
nR_u2
1-pchisq(nR_u2, length(fitted(model6Sargan))) 
library(R.matlab)
library(mgcv)
library(gratia)

ReplicationFolder = '';
NetworkPropertise_Mat_Path = paste0(ReplicationFolder, '/NetworkPropertise.mat');
NetworkPropertise_Mat = readMat(NetworkPropertise_Mat_Path);
NetworkPropertise <- NetworkPropertise_Mat$NetworkPropertise;

Demographics <- as.data.frame(NetworkPropertise);
col_names <- paste0("NP_",1:col_num)
names(Demographics) <- col_names

Demographics$AgeYears <- NetworkPropertise_Mat$demo[,1];
Demographics$Sex_factor <- as.factor(NetworkPropertise_Mat$demo[,2]);
Demographics$MotionMeanRelRMS <- NetworkPropertise_Mat$demo[,3];
Demographics$TBV <- NetworkPropertise_Mat$demo[,4];
Demographics$SN <- NetworkPropertise_Mat$demo[,5];
print('###### Age effect of NetworkPropertise at multilevel ######');

for (i in 1:col_num)
{
  print(i)
  NetworkPropertise_tmp <- NetworkPropertise[, i];
  # Gam analysis was used for age effect
  NetworkPropertise_Gam <- gam(NetworkPropertise_tmp ~ s(AgeYears, k=4) + Sex_factor + MotionMeanRelRMS + TBV + DC, method = "REML", data = Demographics);
  NetworkPropertise_Gam2 <- gam(NetworkPropertise_tmp ~ Sex_factor + MotionMeanRelRMS + TBV + DC, method = "REML", data = Demographics);
  NetworkPropertise_Gam_Age[i, 1] <- summary(NetworkPropertise_Gam)$r.sq - summary(NetworkPropertise_Gam2)$r.sq
  NetworkPropertise_Gam_Age[i, 2] <- summary(NetworkPropertise_Gam)$s.table[, 4];
  NetworkPropertise_lm <- lm(NetworkPropertise_tmp ~ AgeYears + Sex_factor + MotionMeanRelRMS + TBV + DC, data = Demographics);
  NetworkPropertise_tmp_Partial <- lm(NetworkPropertise_tmp ~ Sex_factor + MotionMeanRelRMS + TBV + DC, data = Demographics)$residuals;
  Age_Partial <- lm(AgeYears ~ Sex_factor + MotionMeanRelRMS + TBV + DC, data = Demographics)$residuals;
  
  pred_df <- data.frame(AgeYears = c(min(Demographics$AgeYears), max(Demographics$AgeYears)))
  pred_df$Sex_factor <- as.factor("0")
  pred_df$MotionMeanRelRMS  <- median(Demographics$MotionMeanRelRMS)
  pred_df$TBV  <- median(Demographics$TBV)
  pred_df$SN  <- median(Demographics$SN)
  age_minmax <- predict.gam(afit, newdata = pred_df)
  total_change <- diff(age_minmax)
  deriv <- derivatives(NetworkPropertise_Gam, term = 'AgeYears', n = 10000, eps = 1e-4, order = 1,partial_match = TRUE)
  }




# Naama Leah Radonsky 213879224
# Ester Dray 326335247
git:https://github.com/naamaleah/BigData/tree/main/Week5_datingNLP

data.file <- 'Week5_datingNLP/okcupid_profiles.csv.gz'
source('Week5_datingNLP.r')

─── confusion matrix ───
          Reference
Prediction     f     m
         f   5790  4395
         m  18327 31434

               Accuracy : 0.621
                 95% CI : (0.6171, 0.6248)
    No Information Rate : 0.5977
    P-Value [Acc > NIR] : < 2.2e-16
                  Kappa : 0.1297
 Mcnemar's Test P-Value : < 2.2e-16
            Sensitivity : 0.24008
            Specificity : 0.87733
         Pos Pred Value : 0.56848
         Neg Pred Value : 0.63170
             Prevalence : 0.40231
         Detection Rate : 0.09659
   Detection Prevalence : 0.16990
      Balanced Accuracy : 0.55871
       'Positive' Class : f

─── training times ───
10-fold training 1
Time difference of 49.75443 mins

10-fold training 2
Time difference of 41.11271 mins

10-fold training 3
Time difference of 51.45643 mins

─── list of female words ───
[1] "like"   "love"   "life"   "peopl"  "time"   "thing"  "can"    "just"   "go"     "work"

─── list of male words ───
[1] "friend" "new"    "like"   "love"   "life"   "peopl"  "time"   "thing"  "can"    "just"

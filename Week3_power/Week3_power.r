# Week3_power.R
sink("Week3_power.out")
# --- שלב 1: קריאת נתונים ---
A <- read.delim("electricity/table.tsv")
A$DateTime <- as.POSIXct(A$megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y")
A$Date <- as.Date(A$DateTime)
A$Hour <- as.numeric(format(A$DateTime, "%H"))
A$Minute <- as.numeric(format(A$DateTime, "%M"))
summary(A)

# --- שלב 2: בניית fact table עבור התחנות PJM, ISNE, NYIS, FPL, CPLE ---
fact_pjm  <- data.frame(Date=A$Date, Time=A$DateTime, Hour=A$Hour, Minute=A$Minute, Location="PJM",  NetGen=A$Net.generation.9)
fact_isne <- data.frame(Date=A$Date, Time=A$DateTime, Hour=A$Hour, Minute=A$Minute, Location="ISNE", NetGen=A$Net.generation.5)
fact_nyis <- data.frame(Date=A$Date, Time=A$DateTime, Hour=A$Hour, Minute=A$Minute, Location="NYIS", NetGen=A$Net.generation.7)
fact_fpl  <- data.frame(Date=A$Date, Time=A$DateTime, Hour=A$Hour, Minute=A$Minute, Location="FPL",  NetGen=A$Net.generation.4)
fact_cple <- data.frame(Date=A$Date, Time=A$DateTime, Hour=A$Hour, Minute=A$Minute, Location="CPLE", NetGen=A$Net.generation.2)

# איחוד הכל
electric_fact <- rbind(fact_pjm, fact_isne, fact_nyis, fact_fpl, fact_cple)

# --- שלב 3: בניית קוביית נתונים מראש ---
electric_cube <- tapply(
  electric_fact$NetGen,
  list(
    Hour = electric_fact$Hour,
    Location = electric_fact$Location,
    Date = electric_fact$Date
  ),
  FUN = mean,
  na.rm = TRUE
)

# --- שלב 4: שאלה 1 – ממוצע יומי באמצעות הקוביה ---
days_of_interest <- as.character(seq(as.Date("2021-02-07"), as.Date("2021-02-14"), by = "day"))
daily_avg <- sapply(days_of_interest, function(day) {
  slice <- electric_cube[, , day]
  mean(slice, na.rm = TRUE)
})
daily_avg_df <- data.frame(Date = as.Date(days_of_interest), NetGen = daily_avg)

# --- שלב 5: שאלה 2 – רגרסיה מטריציונית לפי טווחי שעות ---
east_coast <- c("PJM", "NYIS", "ISNE", "FPL", "CPLE")

# טווח 1: 10:00–18:00
range1 <- subset(electric_fact,
                 Location %in% east_coast & Hour >= 10 & Hour < 18 & !is.na(NetGen))

# טווח 2: 20:00–03:00
range2 <- subset(electric_fact,
                 Location %in% east_coast &
                   (Hour >= 20 | Hour < 3) & !is.na(NetGen))

# רגרסיה מטריציונית טווח 1
U1 <- cbind(1, range1$Hour + range1$Minute / 60)
T1 <- range1$NetGen
beta1 <- solve(t(U1) %*% U1) %*% t(U1) %*% T1
fit1 <- U1 %*% beta1

# רגרסיה מטריציונית טווח 2
U2 <- cbind(1, range2$Hour + range2$Minute / 60)
T2 <- range2$NetGen
beta2 <- solve(t(U2) %*% U2) %*% t(U2) %*% T2
fit2 <- U2 %*% beta2

# --- שלב 6: שמירת משתנים לקובץ rdata ---
save(file = "Week3_power.rdata",
     electric_fact, electric_cube,
     daily_avg_df, range1, range2,
     beta1, beta2, fit1, fit2)

# --- שלב 7: גרפים ל־PDF ---
pdf("Week3_power.pdf")

# גרף שאלה 1 – ממוצע יומי
plot(daily_avg_df$Date, daily_avg_df$NetGen, type = "b", col = "blue",
     main = "Daily Avg Net Generation (7–14 Feb 2021)",
     xlab = "Date", ylab = "Net Generation")
abline(h = mean(daily_avg_df$NetGen), col = "red", lty = 2)

# --- גרף שני: 10:00–18:00 ---
range1$MinuteOfDay <- range1$Hour * 60 + range1$Minute
avg1 <- aggregate(NetGen ~ Location + MinuteOfDay, data = range1, FUN = mean)
library(reshape2)
avg_mat1 <- dcast(avg1, MinuteOfDay ~ Location, value.var = "NetGen")

# צירוף ממוצע כללי
avg_mat1$Overall <- rowMeans(avg_mat1[,-1], na.rm = TRUE)

# רגרסיה כוללת
U <- cbind(1, avg_mat1$MinuteOfDay)
T <- avg_mat1$Overall
beta <- solve(t(U) %*% U) %*% t(U) %*% T
trend <- U %*% beta

# גרף
colors <- c("CPLE" = "red", "FPL" = "orange", "ISNE" = "yellow",
            "NYIS" = "blue", "PJM" = "pink", "Overall" = "black", "Trend" = "purple")

plot(NA, xlim = range(avg_mat1$MinuteOfDay),
     ylim = range(avg_mat1[,-1], na.rm = TRUE),
     main = "East Coast Avg Demand\n10:00–18:00",
     xlab = "Minute of Day", ylab = "Avg Demand (MWh)")

for (loc in names(colors)[1:5]) {
  lines(avg_mat1$MinuteOfDay, avg_mat1[[loc]], col = colors[loc], lwd = 2)
}
lines(avg_mat1$MinuteOfDay, avg_mat1$Overall, col = "black", lwd = 2)
lines(avg_mat1$MinuteOfDay, trend, col = "brown", lwd = 2, lty = 2)

legend("topright", legend = c(names(colors)[1:5], "Overall Average", "Overall Trend"),
       col = colors, lty = c(rep(1, 5), 1, 2), lwd = 2, title = "Electric Demand by Region")

# --- גרף שלישי: 20:00–03:00 ---
range2$MinuteOfDay <- range2$Hour * 60 + range2$Minute
range2$MinuteOfDay[range2$Hour < 3] <- range2$MinuteOfDay[range2$Hour < 3] + 1440  # לשעות אחרי חצות

avg2 <- aggregate(NetGen ~ Location + MinuteOfDay, data = range2, FUN = mean)
avg_mat2 <- dcast(avg2, MinuteOfDay ~ Location, value.var = "NetGen")
avg_mat2$Overall <- rowMeans(avg_mat2[,-1], na.rm = TRUE)

U2 <- cbind(1, avg_mat2$MinuteOfDay)
T2 <- avg_mat2$Overall
beta2 <- solve(t(U2) %*% U2) %*% t(U2) %*% T2
trend2 <- U2 %*% beta2

plot(NA, xlim = range(avg_mat2$MinuteOfDay),
     ylim = range(avg_mat2[,-1], na.rm = TRUE),
     main = "East Coast Avg Demand\n20:00–03:00 ",
     xlab = "Minute of Day", ylab = "Avg Demand (MWh)")

for (loc in names(colors)[1:5]) {
  lines(avg_mat2$MinuteOfDay, avg_mat2[[loc]], col = colors[loc], lwd = 2)
}
lines(avg_mat2$MinuteOfDay, avg_mat2$Overall, col = "black", lwd = 2)
lines(avg_mat2$MinuteOfDay, trend2, col = "brown", lwd = 2, lty = 2)

legend("topright", legend = c(names(colors)[1:5], "Overall Average", "Overall Trend"),
       col = colors, lty = c(rep(1, 5), 1, 2), lwd = 2, title = "Electric Demand by Region")

dev.off()
sink()
setwd("/home/lorenz/Documents/master-thesis/data")
data <- data.frame(read.csv("data_awareness2024_2024-12-08_13-47.csv",
                     header=TRUE, sep=";", dec=",", fileEncoding="UTF-16LE"))

## DATA PREPARATION
## CLEANUP
# Delete unused columns
data[,c('CASE', 'SERIAL', 'REF', 'QUESTNNR', 'MODE', 'MAILSENT', 'Q_VIEWER', 'LASTDATA')] <- list(NULL)
# Delete users that quit before first bias awareness question
data <- data[!is.na(data$BA01_01),]
# Delete single user with gave unclear job description
data <- data[data$BE01_04 != "Hallo",]
# Replace -9 with NA
data[] <- lapply(data, function(x) replace(x, x == -9, NA))
# Discard users that didn't answer at least to page 6 with BA questions about SuS
data <- data[data$LASTPAGE > 5,]

## REFACTORING
# Combine columns for origin country
data$SD19 <- ifelse(data$SD19 == 1, "Deutschland", data$SD19_02)
data$SD19_02 <- NULL
# Combine columns for paternal origin country
data$SD20 <- ifelse(data$SD20 == 1, "Deutschland", data$SD20_02)
data$SD20_02 <- NULL
# Combine columns for maternal origin country
data$SD21 <- ifelse(data$SD21 == 1, "Deutschland", data$SD21_02)
data$SD21_02 <- NULL
# Combine columns for teaching experience
data$BE07_01 <- ifelse(data$BE07 == 2, 0, ifelse(data$BE07 == -9, NA, data$BE07_01))
data$BE07 <- data$BE07_01
data$BE07_01 <- NULL

# Correct job descriptions
# Set correct job for students who gave unclear job description
data$BE01[grepl("stud", data$BE01_04, ignore.case=TRUE)] <- 1
# Now the other users which gave an additional job description are teachers
data$BE01[data$BE01_04 != "" & !grepl("stud", data$BE01_04, ignore.case=TRUE)] <- 3
# Set job for users in Referendariat to students
data$BE01[data$BE01 == 2] <- 1
# Additional job descriptions can be deleted now
data[,c("BE01_04")] <- list(NULL)

percent_equal <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  hw <- max(table(x))
  hw / length(x) * 100
}

# Users that gave the same answer more than 75% of the time can be discarded
data$ProzentGleich <- apply(data[,c(2:45)], 1, percent_equal)
data <- data[data$ProzentGleich < 75,]

# Combine 4-pairs into a single value representing the construct
data$BA01 <- rowMeans(data[,c("BA01_01", "BA01_02", "BA01_03", "BA01_04")], na.rm=TRUE)
data$BA02 <- rowMeans(data[,c("BA02_01", "BA02_02", "BA02_03", "BA02_04")], na.rm=TRUE)
data$BA03 <- rowMeans(data[,c("BA03_01", "BA03_02", "BA03_03", "BA03_04")], na.rm=TRUE)
data$BA04 <- rowMeans(data[,c("BA04_01", "BA04_02", "BA04_03", "BA04_04")], na.rm=TRUE)
data$BA05 <- rowMeans(data[,c("BA05_01", "BA05_02", "BA05_03", "BA05_04")], na.rm=TRUE)
data$BA06 <- rowMeans(data[,c("BA06_01", "BA06_02", "BA06_03", "BA06_04")], na.rm=TRUE)
data$BA07 <- rowMeans(data[,c("BA07_01", "BA07_02", "BA07_03", "BA07_04")], na.rm=TRUE)
data$BA08 <- rowMeans(data[,c("BA08_01", "BA08_02", "BA08_03", "BA08_04")], na.rm=TRUE)
data$BA09 <- rowMeans(data[,c("BA09_01", "BA09_02", "BA09_03", "BA09_04")], na.rm=TRUE)
data$BA10 <- rowMeans(data[,c("BA10_01", "BA10_02", "BA10_03", "BA10_04")], na.rm=TRUE)
data$BA11 <- rowMeans(data[,c("BA11_01", "BA11_02", "BA11_03", "BA11_04")], na.rm=TRUE)

## DESCRIPTIVE STATISTICS
# Split data into student and teacher groups
data_STU <- data[data$BE01 == 1,]
data_LK <- data[data$BE01 == 3,]

# total amount of study participants
total_n <- nrow(data)
# Number of students
stu_n <- nrow(data_STU)
# Number of teachers
lk_n <- nrow(data_LK)

## Analyse all study participants
# Calculate amount of female/male/diverse participants
total_female <- nrow(data[data$SD01 == 1,])
total_male <- nrow(data[data$SD01 == 2,])
total_diverse <- nrow(data[data$SD01 == 3,])
# Calculate average age
total_avg_age <- psych::describe(data$SD02_01, na.rm=TRUE)
# Calculate amount of participants with migration background
total_migr <- nrow(data[data$SD19 != "Deutschland"
                       | data$SD20 != "Deutschland"
                       | data$SD21 != "Deutschland",])
# Calculate amount of participants with teaching experience
total_exp <- nrow(data[data$BE07 > 0,])
# Calculate average teaching experience (in years)
total_avg_exp <- psych::describe(data$BE07, na.rm=TRUE)
# Calculate amount of participants with Hauptschulabschluss
total_fb_hs <- nrow(data[data$BE02_01 == 2,])
# Calculate amount of participants with Realschulabschluss
total_fb_rs <- nrow(data[data$BE02_02 == 2,])
# Calculate amount of participants with Fachhochschulreife
total_fb_fh <- nrow(data[data$BE02_03 == 2,])
# Calculate amount of participants with Abitur
total_fb_abi <- nrow(data[data$BE02_04 == 2,])
# Calculate amount of participants with bachlor's degree
total_fb_ba <- nrow(data[data$BE02_05 == 2,])
# Calculate amount of participants with master's degree
total_fb_ma <- nrow(data[data$BE02_06 == 2,])
# Calculate amount of participants with Diplom
total_fb_dipl <- nrow(data[data$BE02_07 == 2,])
# Calculate amount of participants with Staatsexamen
total_fb_stex <- nrow(data[data$BE02_08 == 2,])
# Calculate amount of participants with doctorate's degree
total_fb_doc <- nrow(data[data$BE02_09 == 2,])

## Analyse students
# Calculate amount of female/male/diverse students
stu_female <- nrow(data_STU[data_STU$SD01 == 1,])
stu_male <- nrow(data_STU[data_STU$SD01 == 2,])
stu_diverse <- nrow(data_STU[data_STU$SD01 == 3,])
# Calculate average age
stu_avg_age <- psych::describe(data_STU$SD02_01, na.rm=TRUE)
# Calculate amount of students with migration background
stu_migr <- nrow(data_STU[data_STU$SD19 != "Deutschland"
                           | data_STU$SD20 != "Deutschland"
                           | data_STU$SD21 != "Deutschland",])
# Calculate amount of students with teaching experience
stu_exp <- nrow(data_STU[data_STU$BE07 > 0,])
# Calculate average teaching experience (in years)
stu_avg_exp <- psych::describe(data_STU$BE07, na.rm=TRUE)
# Calculate average semester 
stu_avg_sem <- psych::describe(data_STU$BE03_01, na.rm=TRUE)
# Calculate amount that passed OSP or SPS
stu_osp_sps <- nrow(data_STU[data_STU$BE05 == 1
                             | data_STU$BE06 == 1,])
# Calculate amount of students with Hauptschulabschluss
stu_fb_hs <- nrow(data_STU[data_STU$BE02_01 == 2,])
# Calculate amount of students with Realschulabschluss
stu_fb_rs <- nrow(data_STU[data_STU$BE02_02 == 2,])
# Calculate amount of students with Fachhochschulreife
stu_fb_fh <- nrow(data_STU[data_STU$BE02_03 == 2,])
# Calculate amount of students with Abitur
stu_fb_abi <- nrow(data_STU[data_STU$BE02_04 == 2,])
# Calculate amount of students with bachlor's degree
stu_fb_ba <- nrow(data_STU[data_STU$BE02_05 == 2,])
# Calculate amount of students with master's degree
stu_fb_ma <- nrow(data_STU[data_STU$BE02_06 == 2,])
# Calculate amount of students with Diplom
stu_fb_dipl <- nrow(data_STU[data_STU$BE02_07 == 2,])
# Calculate amount of students with Staatsexamen
stu_fb_stex <- nrow(data_STU[data_STU$BE02_08 == 2,])
# Calculate amount of students with doctorate's degree
stu_fb_doc <- nrow(data_STU[data_STU$BE02_09 == 2,])
# Calculate amount of students with goal Grundschule
stu_sa_gru <- nrow(data_STU[data_STU$BE08 == 1 & !is.na(data_STU$BE08),])
# Calculate amount of students with goal Sekundarstufe 1
stu_sa_ <- nrow(data_STU[data_STU$BE08 == 2 & !is.na(data_STU$BE08),])
# Calculate amount of students with goal Gymnasium
stu_sa_ <- nrow(data_STU[data_STU$BE08 == 3 & !is.na(data_STU$BE08),])
# Calculate amount of students with goal Berufliche Schule
stu_sa_ <- nrow(data_STU[data_STU$BE08 == 4 & !is.na(data_STU$BE08),])
# Calculate amount of students with goal Sonderpädagogik
stu_sa_ <- nrow(data_STU[data_STU$BE08 == 5 & !is.na(data_STU$BE08),])

## Analyse teachers
# Calculate amount of female/male/diverse teachers
lk_female <- nrow(data_LK[data_LK$SD01 == 1,])
lk_male <- nrow(data_LK[data_LK$SD01 == 2,])
lk_diverse <- nrow(data_LK[data_LK$SD01 == 3,])
# Calculate average age
lk_avg_age <- psych::describe(data_LK$SD02_01, na.rm=TRUE)
# Calculate amount of teachers with migration background
lk_migr <- nrow(data_LK[data_LK$SD19 != "Deutschland"
                          | data_LK$SD20 != "Deutschland"
                          | data_LK$SD21 != "Deutschland",])
# Calculate average teaching experience (in years)
lk_avg_exp <- psych::describe(data_LK$BE07, na.rm=TRUE)
# Calculate amount of teachers with Hauptschulabschluss
lk_fb_hs <- nrow(data_LK[data_LK$BE02_01 == 2,])
# Calculate amount of teachers with Realschulabschluss
lk_fb_rs <- nrow(data_LK[data_LK$BE02_02 == 2,])
# Calculate amount of teachers with Fachhochschulreife
lk_fb_fh <- nrow(data_LK[data_LK$BE02_03 == 2,])
# Calculate amount of teachers with Abitur
lk_fb_abi <- nrow(data_LK[data_LK$BE02_04 == 2,])
# Calculate amount of teachers with bachlor's degree
lk_fb_ba <- nrow(data_LK[data_LK$BE02_05 == 2,])
# Calculate amount of teachers with master's degree
lk_fb_ma <- nrow(data_LK[data_LK$BE02_06 == 2,])
# Calculate amount of teachers with Diplom
lk_fb_dipl <- nrow(data_LK[data_LK$BE02_07 == 2,])
# Calculate amount of teachers with Staatsexamen
lk_fb_stex <- nrow(data_LK[data_LK$BE02_08 == 2,])
# Calculate amount of teachers with doctorate's degree
lk_fb_doc <- nrow(data_LK[data_LK$BE02_09 == 2,])
# Calculate amount of teachers at Grundschule
lk_sa_gru <- nrow(data_LK[data_LK$BE04 == 1 & !is.na(data_LK$BE04),])
# Calculate amount of teachers at Sekundarstufe 1
lk_sa_sek1 <- nrow(data_LK[data_LK$BE04 == 2 & !is.na(data_LK$BE04),])
# Calculate amount of teachers at Gymnasium
lk_sa_gym <- nrow(data_LK[data_LK$BE04 == 3 & !is.na(data_LK$BE04),])
# Calculate amount of teachers at Berufliche Schule
lk_sa_bs <- nrow(data_LK[data_LK$BE04 == 4 & !is.na(data_LK$BE04),])
# Calculate amount of teachers at Sonderpädagogik teacher
lk_sa_snd <- nrow(data_LK[data_LK$BE04 == 5 & !is.na(data_LK$BE04),])
# The rest
lk_sa_oth <- nrow(data_LK[data_LK$BE04 == 6 & !is.na(data_LK$BE04),])

# Run normalcy tests for combined variables, all participants
shapiro.test(data$BA01)
shapiro.test(data$BA02)
shapiro.test(data$BA03)
shapiro.test(data$BA04)
shapiro.test(data$BA05)
shapiro.test(data$BA06)
shapiro.test(data$BA07)
shapiro.test(data$BA08)
shapiro.test(data$BA09)
shapiro.test(data$BA10)
shapiro.test(data$BA11)

# Calculate cronbach alphas for all study participants
psych::alpha(data[,c("BA01_01", "BA01_02", "BA01_03", "BA01_04")])
psych::alpha(data[,c("BA02_01", "BA02_02", "BA02_03", "BA02_04")])
psych::alpha(data[,c("BA03_01", "BA03_02", "BA03_03", "BA03_04")])
psych::alpha(data[,c("BA04_01", "BA04_02", "BA04_03", "BA04_04")])
psych::alpha(data[,c("BA05_01", "BA05_02", "BA05_03", "BA05_04")])
psych::alpha(data[,c("BA06_01", "BA06_02", "BA06_03", "BA06_04")])
psych::alpha(data[,c("BA07_01", "BA07_02", "BA07_03", "BA07_04")])
psych::alpha(data[,c("BA08_01", "BA08_02", "BA08_03", "BA08_04")])
psych::alpha(data[,c("BA09_01", "BA09_02", "BA09_03", "BA09_04")])
psych::alpha(data[,c("BA10_01", "BA10_02", "BA10_03", "BA10_04")])
psych::alpha(data[,c("BA11_01", "BA11_02", "BA11_03", "BA11_04")])

# Run normalcy tests for combined variables, students only
shapiro.test(data_STU$BA01)
shapiro.test(data_STU$BA02)
shapiro.test(data_STU$BA03)
shapiro.test(data_STU$BA04)
shapiro.test(data_STU$BA05)
shapiro.test(data_STU$BA06)
shapiro.test(data_STU$BA07)
shapiro.test(data_STU$BA08)
shapiro.test(data_STU$BA09)
shapiro.test(data_STU$BA10)
shapiro.test(data_STU$BA11)

# Calculate cronbach alphas for students only
psych::alpha(data_STU[,c("BA01_01", "BA01_02", "BA01_03", "BA01_04")])
psych::alpha(data_STU[,c("BA02_01", "BA02_02", "BA02_03", "BA02_04")])
psych::alpha(data_STU[,c("BA03_01", "BA03_02", "BA03_03", "BA03_04")])
psych::alpha(data_STU[,c("BA04_01", "BA04_02", "BA04_03", "BA04_04")])
psych::alpha(data_STU[,c("BA05_01", "BA05_02", "BA05_03", "BA05_04")])
psych::alpha(data_STU[,c("BA06_01", "BA06_02", "BA06_03", "BA06_04")])
psych::alpha(data_STU[,c("BA07_01", "BA07_02", "BA07_03", "BA07_04")])
psych::alpha(data_STU[,c("BA08_01", "BA08_02", "BA08_03", "BA08_04")])
psych::alpha(data_STU[,c("BA09_01", "BA09_02", "BA09_03", "BA09_04")])
psych::alpha(data_STU[,c("BA10_01", "BA10_02", "BA10_03", "BA10_04")])
psych::alpha(data_STU[,c("BA11_01", "BA11_02", "BA11_03", "BA11_04")])

# Run normalcy tests for combined variables, teachers only
shapiro.test(data_LK$BA01)
shapiro.test(data_LK$BA02)
shapiro.test(data_LK$BA03)
shapiro.test(data_LK$BA04)
shapiro.test(data_LK$BA05)
shapiro.test(data_LK$BA06)
shapiro.test(data_LK$BA07)
shapiro.test(data_LK$BA08)
shapiro.test(data_LK$BA09)
shapiro.test(data_LK$BA10)
shapiro.test(data_LK$BA11)

# Calculate cronbach alphas for teachers only
psych::alpha(data_LK[,c("BA01_01", "BA01_02", "BA01_03", "BA01_04")])
psych::alpha(data_LK[,c("BA02_01", "BA02_02", "BA02_03", "BA02_04")])
psych::alpha(data_LK[,c("BA03_01", "BA03_02", "BA03_03", "BA03_04")])
psych::alpha(data_LK[,c("BA04_01", "BA04_02", "BA04_03", "BA04_04")])
psych::alpha(data_LK[,c("BA05_01", "BA05_02", "BA05_03", "BA05_04")])
psych::alpha(data_LK[,c("BA06_01", "BA06_02", "BA06_03", "BA06_04")])
psych::alpha(data_LK[,c("BA07_01", "BA07_02", "BA07_03", "BA07_04")])
psych::alpha(data_LK[,c("BA08_01", "BA08_02", "BA08_03", "BA08_04")])
psych::alpha(data_LK[,c("BA09_01", "BA09_02", "BA09_03", "BA09_04")])
psych::alpha(data_LK[,c("BA10_01", "BA10_02", "BA10_03", "BA10_04")])
psych::alpha(data_LK[,c("BA11_01", "BA11_02", "BA11_03", "BA11_04")])

## INFERENCE STATISTICS
# TESTS WITH ALL DATA
# Test BA with allg items
# BA allg/allg <--> BA SuS/allg
t.test(data$BA01, data$BA06, paired=TRUE)
sd(data$BA01 - data$BA06, na.rm=TRUE)
print(lsr::cohensD(data$BA01, data$BA06, method="paired"))
# BA allg/allg <--> BA eigene SuS
t.test(data$BA01, data$BA07, paired=TRUE)
sd(data$BA01 - data$BA07, na.rm=TRUE)
print(lsr::cohensD(data$BA01, data$BA07, method="paired"))
# BA SuS/allg <--> BA eigene SuS
t.test(data$BA06, data$BA07, paired=TRUE)
sd(data$BA06 - data$BA07, na.rm=TRUE)
print(lsr::cohensD(data$BA06, data$BA07, method="paired"))

# Test BA with domain-specific items
# BA allg/migr <--> BA SuS/migr
t.test(data$BA02, data$BA08, paired=TRUE)
sd(data$BA02 - data$BA08, na.rm=TRUE)
print(lsr::cohensD(data$BA02, data$BA08, method="paired"))
# BA allg/sozök <--> BA SuS/sozök
t.test(data$BA03, data$BA09, paired=TRUE)
sd(data$BA03 - data$BA09, na.rm=TRUE)
print(lsr::cohensD(data$BA03, data$BA09, method="paired"))
# BA allg/frauen <--> BA SuS/frauen
t.test(data$BA04, data$BA10, paired=TRUE)
sd(data$BA04 - data$BA10, na.rm=TRUE)
print(lsr::cohensD(data$BA04, data$BA10, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data$BA04, data$BA10, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data$BA04, data$BA10, paired=TRUE)
# BA allg/männer <--> BA SuS/männer
t.test(data$BA05, data$BA11, paired=TRUE)
sd(data$BA05 - data$BA11, na.rm=TRUE)
print(lsr::cohensD(data$BA05, data$BA11, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data$BA05, data$BA11, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data$BA05, data$BA11, paired=TRUE)

# TESTS WITH STUDENTS ONLY
# Test BA with allg items
# BA allg/allg <--> BA SuS/allg
t.test(data_STU$BA01, data_STU$BA06, paired=TRUE)
sd(data_STU$BA01 - data_STU$BA06, na.rm=TRUE)
print(lsr::cohensD(data_STU$BA01, data_STU$BA06, method="paired"))
# BA allg/allg <--> BA eigene SuS
t.test(data_STU$BA01, data_STU$BA07, paired=TRUE)
sd(data_STU$BA01 - data_STU$BA07, na.rm=TRUE)
print(lsr::cohensD(data_STU$BA01, data_STU$BA07, method="paired"))
# BA SuS/allg <--> BA eigene SuS
t.test(data_STU$BA06, data_STU$BA07, paired=TRUE)
sd(data_STU$BA06 - data_STU$BA07, na.rm=TRUE)
print(lsr::cohensD(data_STU$BA06, data_STU$BA07, method="paired"))

# Test BA with domain-specific items
# BA allg/migr <--> BA SuS/migr
t.test(data_STU$BA02, data_STU$BA08, paired=TRUE)
sd(data_STU$BA02 - data_STU$BA08, na.rm=TRUE)
print(lsr::cohensD(data_STU$BA02, data_STU$BA08, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data_STU$BA02, data_STU$BA08, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data_STU$BA02, data_STU$BA08, paired=TRUE)
# BA allg/sozök <--> BA SuS/sozök
t.test(data_STU$BA03, data_STU$BA09, paired=TRUE)
sd(data_STU$BA03 - data_STU$BA09, na.rm=TRUE)
print(lsr::cohensD(data_STU$BA03, data_STU$BA09, method="paired"))
# BA allg/frauen <--> BA SuS/frauen
t.test(data_STU$BA04, data_STU$BA10, paired=TRUE)
sd(data_STU$BA04 - data_STU$BA10, na.rm=TRUE)
print(lsr::cohensD(data_STU$BA04, data_STU$BA10, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data_STU$BA04, data_STU$BA10, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data_STU$BA04, data_STU$BA10, paired=TRUE)
# BA allg/männer <--> BA SuS/männer
t.test(data_STU$BA05, data_STU$BA11, paired=TRUE)
sd(data_STU$BA05 - data_STU$BA11, na.rm=TRUE)
print(lsr::cohensD(data_STU$BA05, data_STU$BA11, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data_STU$BA05, data_STU$BA11, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data_STU$BA05, data_STU$BA11, paired=TRUE)

# TESTS WITH TEACHERS ONLY
# Test BA with allg items
# BA allg/allg <--> BA SuS/allg
t.test(data_LK$BA01, data_LK$BA06, paired=TRUE)
sd(data_LK$BA01 - data_LK$BA06, na.rm=TRUE)
print(lsr::cohensD(data_LK$BA01, data_LK$BA06, method="paired"))
# BA allg/allg <--> BA eigene SuS
t.test(data_LK$BA01, data_LK$BA07, paired=TRUE)
sd(data_LK$BA01 - data_LK$BA07, na.rm=TRUE)
print(lsr::cohensD(data_LK$BA01, data_LK$BA07, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data_LK$BA01, data_LK$BA07, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data_LK$BA01, data_LK$BA07, paired=TRUE)
# BA SuS/allg <--> BA eigene SuS
t.test(data_LK$BA06, data_LK$BA07, paired=TRUE)
sd(data_LK$BA06 - data_LK$BA07, na.rm=TRUE)
print(lsr::cohensD(data_LK$BA06, data_LK$BA07, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data_LK$BA06, data_LK$BA07, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data_LK$BA06, data_LK$BA07, paired=TRUE)

# Test BA with domain-specific items
# BA allg/migr <--> BA SuS/migr
t.test(data_LK$BA02, data_LK$BA08, paired=TRUE)
sd(data_LK$BA02 - data_LK$BA08, na.rm=TRUE)
print(lsr::cohensD(data_LK$BA02, data_LK$BA08, method="paired"))
# BA allg/sozök <--> BA SuS/sozök
t.test(data_LK$BA03, data_LK$BA09, paired=TRUE)
sd(data_LK$BA03 - data_LK$BA08, na.rm=TRUE)
print(lsr::cohensD(data_LK$BA03, data_LK$BA09, method="paired"))
# BA allg/frauen <--> BA SuS/frauen
t.test(data_LK$BA04, data_LK$BA10, paired=TRUE)
sd(data_LK$BA04 - data_LK$BA10, na.rm=TRUE)
print(lsr::cohensD(data_LK$BA04, data_LK$BA10, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data_LK$BA04, data_LK$BA10, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data_LK$BA04, data_LK$BA10, paired=TRUE)
# BA allg/männer <--> BA SuS/männer
t.test(data_LK$BA05, data_LK$BA11, paired=TRUE)
sd(data_LK$BA05 - data_LK$BA11, na.rm=TRUE)
print(lsr::cohensD(data_LK$BA05, data_LK$BA11, method="paired"))
# Additional wilcoxon test because shapiro-wilk failed
wilcox.test(data_LK$BA05, data_LK$BA11, exact=FALSE, paired=TRUE)
effectsize::rank_biserial(data_LK$BA05, data_LK$BA11, paired=TRUE)

# COMPARISON OF STUDENTS AND TEACHERS
# BA allg/allg
t.test(data_STU$BA01, data_LK$BA01, paired=FALSE)
mean(data_STU$BA01, na.rm=TRUE) - mean(data_LK$BA01, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA01, data_LK$BA01)
print(lsr::cohensD(data_STU$BA01, data_LK$BA01))
# BA allg/migr
t.test(data_STU$BA02, data_LK$BA02, paired=FALSE)
mean(data_STU$BA02, na.rm=TRUE) - mean(data_LK$BA02, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA02, data_LK$BA02)
print(lsr::cohensD(data_STU$BA02, data_LK$BA02))
# BA allg/sozök
t.test(data_STU$BA03, data_LK$BA03, paired=FALSE)
mean(data_STU$BA03, na.rm=TRUE) - mean(data_LK$BA03, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA03, data_LK$BA03)
print(lsr::cohensD(data_STU$BA03, data_LK$BA03))
# BA allg/frauen
t.test(data_STU$BA04, data_LK$BA04, paired=FALSE)
mean(data_STU$BA04, na.rm=TRUE) - mean(data_LK$BA04, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA04, data_LK$BA04)
print(lsr::cohensD(data_STU$BA04, data_LK$BA04))
# BA allg/männer
t.test(data_STU$BA05, data_LK$BA05, paired=FALSE)
mean(data_STU$BA05, na.rm=TRUE) - mean(data_LK$BA05, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA05, data_LK$BA05)
print(lsr::cohensD(data_STU$BA05, data_LK$BA05))
# BA SuS/allg
t.test(data_STU$BA06, data_LK$BA06, paired=FALSE)
mean(data_STU$BA06, na.rm=TRUE) - mean(data_LK$BA06, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA06, data_LK$BA06)
print(lsr::cohensD(data_STU$BA06, data_LK$BA06))
# BA own SuS/allg
t.test(data_STU$BA07, data_LK$BA07, paired=FALSE)
mean(data_STU$BA07, na.rm=TRUE) - mean(data_LK$BA07, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA07, data_LK$BA07)
print(lsr::cohensD(data_STU$BA07, data_LK$BA07))
# BA SuS/migr
t.test(data_STU$BA08, data_LK$BA08, paired=FALSE)
mean(data_STU$BA08, na.rm=TRUE) - mean(data_LK$BA08, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA08, data_LK$BA08)
print(lsr::cohensD(data_STU$BA08, data_LK$BA08))
# BA SuS/sozök
t.test(data_STU$BA09, data_LK$BA09, paired=FALSE)
mean(data_STU$BA09, na.rm=TRUE) - mean(data_LK$BA09, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA09, data_LK$BA09)
print(lsr::cohensD(data_STU$BA09, data_LK$BA09))
# BA SuS/frauen
t.test(data_STU$BA10, data_LK$BA10, paired=FALSE)
mean(data_STU$BA10, na.rm=TRUE) - mean(data_LK$BA10, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA10, data_LK$BA10)
print(lsr::cohensD(data_STU$BA10, data_LK$BA10))
# BA SuS/männer
t.test(data_STU$BA11, data_LK$BA11, paired=FALSE)
mean(data_STU$BA11, na.rm=TRUE) - mean(data_LK$BA11, na.rm=TRUE)
effectsize::sd_pooled(data_STU$BA11, data_LK$BA11)
print(lsr::cohensD(data_STU$BA11, data_LK$BA11))

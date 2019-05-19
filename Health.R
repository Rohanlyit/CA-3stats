# Reading the csv file

dat <- read.csv("C:/Users/dell/ca3_R/Health.csv", header = TRUE, stringsAsFactors = FALSE)
str(dat)

# Making a subset of the original dataset
df1 <- subset(dat, select = c(TotalDischargesMale, TotalDischargesFemale, Year))
head(df1)
View(df1)
str(df1)

# Changing the type from factor to numeric

df1$TotalDischargesMale <- as.numeric(gsub(",","",df1$TotalDischargesMale))
df1$TotalDischargesFemale <- as.numeric(gsub(",","",df1$TotalDischargesFemale))
df1$Year <- as.numeric(df1$Year)
str(df1)

# Omitting all the null values

df1[df1 == "0"] <- NA
df1 <- na.omit(df1)
View(df1)
str(df1)



xx <- data.frame(Gender = rep(c("Male", "Female"), each = 234), 
                 Discharge = c(df1$TotalDischargesMale, df1$TotalDischargesFemale),
                 Year = c(df1$Year)
)
str(xx)


# Plotting
#install.packages("ggpubr")
library(ggpubr)
ggboxplot(xx, x = "Gender", y = "Discharge", 
          color = "Gender", palette = c("#00AFBB", "#E7B800"),
          ylab = "Discharge", xlab = "Gender")

# Performing Shapiro-test

with(xx, shapiro.test(Discharge[Gender == "Male"]))
with(xx, shapiro.test(Discharge[Gender == "Female"]))

# H0 : Total discharge of male and female during the period 2015-2017 is equal
# HA : Total discharge of male and female during the period 2015-2017 is not equal

# t-test
test1 <- var.test(Discharge~ Gender, data = xx)
test1

xx1 <- xx[ which(xx$Gender == "Male"),]
View(xx1)
xx2 <- xx[ which(xx$Gender == "Female"),]
View(xx2)

mean(xx1$Discharge) # 78275.33
mean(xx2$Discharge) # 84548.05

sd(xx$Discharge) # 282538.1

# Power-test and analysis:
# Calculating standard deviation= 282538.1
# mean(xx1) - mean(xx2) = 6272.72 
# delta = 6272.72/282,201 = 0.02

library(pwr)
power.t.test(delta =   0.02, n = NULL, sig.level = 0.05, power = 0.90,type= "two.sample", alternative = "two.sided")
power_information <- pwr.t.test(d = 0.02, 
                                sig.level = 0.05, 
                                power = 0.99, 
                                type = "two.sample", 
                                alternative = "two.sided")
plot(power_information)

# Performing cohen-test
cohen.ES(test = c("t"), size = c("small"))
# Performing Spearman's test
res2 <- cor.test(df1$TotalDischargesMale, df1$TotalDischargesFemale, 
                 method = "spearman", exact = FALSE)
res2
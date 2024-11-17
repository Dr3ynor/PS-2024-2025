install.packages("readxl")
install.packages("moments")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("forcats")
install.packages("svglite")

library(readxl)
library(moments)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(forcats)
library(svglite)

getwd()
# setwd("/home/jakub/school/7th_semester/PS/PROJEKT")
setwd("C:/Users/kubik/Desktop/projects/PS-2024-2025/PROJEKT")
data = read_excel("ukol_181.xlsx", sheet = "vysledky_mereni")
# data = na.omit(data)

colnames(data) = c("id","2080Ti_release","2080Ti_patched","3070Ti_release","3070Ti_patched","6800XT_release","6800XT_patched","7700XT_release","7700XT_patched") 
colnames(data)

rtx3070ti_release = data[["3070Ti_release"]]
rtx3070ti_patched = data[["3070Ti_patched"]]
rx7700xt_release = data[["7700XT_release"]]
rx7700xt_patched = data[["7700XT_patched"]]

rtx3070Ti_difference = rtx3070ti_patched - rtx3070ti_release
rx7700xt_difference = rx7700xt_patched - rx7700xt_release



# Save the plots to a PNG file
png("boxplot_outliers.png", width=1200, height=600)
par(mfrow=c(1, 1))
boxplot(rtx3070Ti_difference, rx7700xt_difference, 
        names=c("RTX 3070 Ti", "RX 7700 XT"), 
        main="Krabicový graf s odlehlými pozorováními", 
        xlab="Grafické karty", 
        ylab="FPS", 
        col="gray", 
        border="black")
dev.off()


# Remove outliers using IQR method
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  cleaned_data <- x[x >= lower_bound & x <= upper_bound]
  return(cleaned_data)
}


rtx3070Ti_cleaned <- remove_outliers(rtx3070Ti_difference)
rx7700xt_cleaned <- remove_outliers(rx7700xt_difference)



summerized_rtx3070ti = data %>% 
  summarise(rozsah = length(na.omit(rtx3070Ti_difference)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(rtx3070Ti_difference)),
            minimum = min(rtx3070Ti_difference, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(rtx3070Ti_difference, 0.25, na.rm=T),
            prumer = mean(rtx3070Ti_difference, na.rm=T),
            median = median(rtx3070Ti_difference, na.rm=T),
            Q3 = quantile(rtx3070Ti_difference, 0.75, na.rm=T),
            maximum = max(rtx3070Ti_difference, na.rm=T),
            rozptyl = var(rtx3070Ti_difference, na.rm=T),
            smerodatna_odchylka = sd(rtx3070Ti_difference,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(rtx3070Ti_difference, na.rm=T)),              # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(rtx3070Ti_difference, na.rm=T)-3))

t(summerized_rtx3070ti)



summerized_rx7700xt = data %>% 
  summarise(rozsah = length(na.omit(rx7700xt_difference)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(rx7700xt_difference)),
            minimum = min(rx7700xt_difference, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(rx7700xt_difference, 0.25, na.rm=T),
            prumer = mean(rx7700xt_difference, na.rm=T),
            median = median(rx7700xt_difference, na.rm=T),
            Q3 = quantile(rx7700xt_difference, 0.75, na.rm=T),
            maximum = max(rx7700xt_difference, na.rm=T),
            rozptyl = var(rx7700xt_difference, na.rm=T),
            smerodatna_odchylka = sd(rx7700xt_difference,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(rx7700xt_difference, na.rm=T)),              # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(rx7700xt_difference, na.rm=T)-3))

t(summerized_rx7700xt)






cleaned_summerized_rtx3070ti = data %>% 
  summarise(rozsah = length(na.omit(rtx3070Ti_cleaned)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(rtx3070Ti_cleaned)),
            minimum = min(rtx3070Ti_cleaned, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(rtx3070Ti_cleaned, 0.25, na.rm=T),
            prumer = mean(rtx3070Ti_cleaned, na.rm=T),
            median = median(rtx3070Ti_cleaned, na.rm=T),
            Q3 = quantile(rtx3070Ti_cleaned, 0.75, na.rm=T),
            maximum = max(rtx3070Ti_cleaned, na.rm=T),
            rozptyl = var(rtx3070Ti_cleaned, na.rm=T),
            smerodatna_odchylka = sd(rtx3070Ti_cleaned,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(rtx3070Ti_cleaned, na.rm=T)),              # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(rtx3070Ti_cleaned, na.rm=T)-3))

t(cleaned_summerized_rtx3070ti)


cleaned_summerized_rx7700xt = data %>% 
  summarise(rozsah = length(na.omit(rx7700xt_cleaned)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(rx7700xt_cleaned)),
            minimum = min(rx7700xt_cleaned, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(rx7700xt_cleaned, 0.25, na.rm=T),
            prumer = mean(rx7700xt_cleaned, na.rm=T),
            median = median(rx7700xt_cleaned, na.rm=T),
            Q3 = quantile(rx7700xt_cleaned, 0.75, na.rm=T),
            maximum = max(rx7700xt_cleaned, na.rm=T),
            rozptyl = var(rx7700xt_cleaned, na.rm=T),
            smerodatna_odchylka = sd(rx7700xt_cleaned,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(rx7700xt_cleaned, na.rm=T)),              # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(rx7700xt_cleaned, na.rm=T)-3))

t(cleaned_summerized_rx7700xt)



# Save the plots to a PNG file
png("boxplot_without_outliers.png", width=1200, height=600)
par(mfrow=c(1, 1))
boxplot(rtx3070Ti_cleaned, rx7700xt_cleaned, 
        names=c("RTX 3070 Ti", "RX 7700 XT"), 
        main="Krabicový graf bez odlehlých pozorování", 
        xlab="Grafické karty", 
        ylab="FPS", 
        col="gray", 
        border="black")
dev.off()


# Save the combined QQ plot and histogram for RX 7700 XT to a PNG file
png("combined_qq_histogram_rx7700xt.png", width=1200, height=600)
par(mfrow=c(1, 2))
# QQ plot for RX 7700 XT
qqnorm(rx7700xt_cleaned, main="RX 7700 XT", xlab="Norm. teoretické kvantily", ylab="Výběrové kvantily")
qqline(rx7700xt_cleaned, col="black")
# Histogram for RX 7700 XT
hist(rx7700xt_cleaned, main="RX 7700 XT", xlab="Nárůst FPS", ylab="Četnost", col="gray", border="black")
par(mfrow=c(1, 1))
dev.off()


# Save the combined QQ plot and histogram for RTX 3070 Ti to a PNG file
png("combined_qq_histogram_rtx3070ti.png", width=1200, height=600)
par(mfrow=c(1, 2))
# QQ plot for RTX 3070 Ti
qqnorm(rtx3070Ti_cleaned, main="RTX 3070 Ti", xlab="Norm. teoretické kvantily", ylab="Výběrové kvantily")
qqline(rtx3070Ti_cleaned, col="black")
# Histogram for RTX 3070 Ti
hist(rtx3070Ti_cleaned, main="RTX 3070 Ti", xlab="Hodnota", ylab="Četnost", col="gray", border="black")
par(mfrow=c(1, 1))
dev.off()

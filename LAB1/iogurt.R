library(readr)
library(ggplot2)
library(dplyr)
library(doBy)
Iogurt <- read_delim("Iogurt.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)
head(Iogurt)
summary(Iogurt)

Iogurt$Ferm <- as.factor(Iogurt$Ferm)
# Lets check the data itself
boxplot(Iogurt$pH ~ Iogurt$Ferm)
# We can see how there are clear outliers in our data, in this case we choose to remove them.
Iogurt <- Iogurt[Iogurt$pH > 100,]

boxplot(Iogurt$strep ~ Iogurt$Ferm)
# This also appears to have clear outliers in the data
Iogurt <- Iogurt[Iogurt$strep > 100,]

boxplot(Iogurt$lactob ~ Iogurt$Ferm)
# The same for lactob
Iogurt <- Iogurt[Iogurt$lactob > 100,]
#### Plot the data  ####
plot(x = Iogurt$dia,
     y = Iogurt$pH,
     col = Iogurt$Ferm,
     pch = 16,
     xlab = "Day",
     ylab = "pH")
legend('topright',
       legend = levels(Iogurt$Ferm),
       col = 1:2,
       cex = 1,
       pch = 16)
r_line <- lm(pH ~ dia, Iogurt[Iogurt$Ferm == "T42",])
abline(r_line, col=1)
r_line <- lm(pH ~ dia, Iogurt[Iogurt$Ferm == "T43.5",])
abline(r_line, col=2)



plot(x = Iogurt$dia,
     y = Iogurt$strep,
     col = Iogurt$Ferm,
     pch = 16,
     xlab = "Day",
     ylab = "strep")
legend('topright',
       legend = levels(Iogurt$Ferm),
       col = 1:2,
       cex = 1,
       pch = 16)
r_line <- lm(strep ~ dia, Iogurt[Iogurt$Ferm == "T42",])
abline(r_line, col=1)
r_line <- lm(strep ~ dia, Iogurt[Iogurt$Ferm == "T43.5",])
abline(r_line, col=2)

plot(x = Iogurt$dia,
     y = Iogurt$lactob,
     col = Iogurt$Ferm,
     pch = 16,
     xlab = "Day",
     ylab = "lactob")
legend('topright',
       legend = levels(Iogurt$Ferm),
       col = 1:2,
       cex = 1,
       pch = 16)
r_line <- lm(lactob ~ dia, Iogurt[Iogurt$Ferm == "T42",])
abline(r_line, col=1)
r_line <- lm(lactob ~ dia, Iogurt[Iogurt$Ferm == "T43.5",])
abline(r_line, col=2)

#### Table summary ####
Idf <- as.data.frame(Iogurt)

(te<-summaryBy(cbind(pH, lactob, strep)~dia * Ferm,data=Idf,FUN= c(mean,sd)))



#### B ####

g1 <- Iogurt[Iogurt$Ferm == "T42",]
g1_7 <- g1[g1$dia == 7,]
g1_14 <- g1[g1$dia == 14,]
g1_28 <- g1[g1$dia == 28,]

g2 <- Iogurt[Iogurt$Ferm == "T43.5",]
g2_7 <- g2[g2$dia == 7,]
g2_14 <- g2[g2$dia == 14,]
g2_28 <- g2[g2$dia == 28,]

# Use T student p-value > 0.005 means similar
t.test(g1_7$pH, g2_7$pH)
var.test(g1_7$pH, g2_7$pH) # Smaller than 0.005 same variance

t.test(g1_7$strep, g2_7$strep)
var.test(g1_7$strep, g2_7$strep)

t.test(g1_7$lactob, g2_7$lactob)
var.test(g1_7$lactob, g2_7$lactob)

t.test(g1_14$pH, g2_14$pH)
var.test(g1_14$pH, g2_14$pH)

t.test(g1_14$strep, g2_14$strep)
var.test(g1_14$strep, g2_14$strep)

t.test(g1_14$lactob, g2_14$lactob)
var.test(g1_14$lactob, g2_14$lactob)

t.test(g1_28$pH, g2_28$pH)
var.test(g1_28$pH, g2_28$pH)

t.test(g1_28$strep, g2_28$strep)
var.test(g1_28$strep, g2_28$strep)

t.test(g1_28$lactob, g2_28$lactob)
var.test(g1_28$lactob, g2_28$lactob)

#### C ####

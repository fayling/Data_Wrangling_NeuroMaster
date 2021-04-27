data <- read.table("data/data.csv", sep=",", header=T)
data
library(reshape2)
datalong <- melt(data, id.vars=c("id", "intervention"))

#figuring out a vector for the session column
test <- c(rep(1,66), rep(2,66))
test
test2 <-c(rep(test, 14))
test2
test3 <- data.frame(test2)
session <-c(rep(c(rep(1,66), rep(2,66)), 14)) #use this one



#figuring out a vector for the variable column
test <- c(rep(1,132), rep(2, 132), rep(3,132), rep(4, 132), rep(5,132), rep(6, 132), rep(7,132), rep(8, 132), rep(9,132), rep(10, 132),
          rep(11,132), rep(12, 132), rep(13,132), rep(14, 132))
variable <- c(rep(1,132), rep(2, 132), rep(3,132), rep(4, 132), rep(5,132), rep(6, 132), rep(7,132), rep(8, 132), rep(9,132), rep(10, 132),
                         rep(11,132), rep(12, 132), rep(13,132), rep(14, 132)) #use this one

#concatenating
datatest <- datalong
datatest$session = session
datatest$var = variable

str(datatest)

#trying with tidyr
library(tidyr)
tidier <- data %>%
    gather(key, value, -id, -intervention)
tidier %>% head(8)

tidy <- tidier %>%
    separate(key, into = c("variable", "session"), sep = "_")
tidy %>% head(8)

#replacing with pre and post
tidy$session[tidy$session == "ses1"] <- "pre"
tidy$session[tidy$session == "ses2"] <- "post"

#back to wide
datawide <- dcast(tidy, id + intervention + session ~ variable)

#standardize
datawide$var1_scaled<-scale(datawide$var1) #one by one

#all at once
datatest <- dcast(tidy, id + intervention + session ~ variable)
library(dplyr)
test <- datatest %>% mutate_at(c('var1', 'var2', 'var3', 'var4', 'var5', 'var6', 'var7', 'var8', 'var9', 'var10', 'var11', 'var12', 'var13', 'var14'),
                                 ~(scale(.) %>% as.vector))
summary(test)
test1 <- na.omit(test)



#plotting
library(GGally)
ggpairs(forged[, -c(1, 2, 3)], aes(colour=forged$intervention, alpha=0.4))
ggpairs(forged[, -c(1, 2, 3)], aes(colour=forged$session, alpha=0.4))
ggpairs(forged[, -c(1, 2, 3)])


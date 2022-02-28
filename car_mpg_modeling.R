library(tidyverse)
library(lattice)
library(reshape2)
install.packages("car")
install.packages("ggpubr")
library(car)
library(ggpubr)
car_df <- mtcars
colnames(mtcars)
?mtcars
summary(mtcars)
str(mtcars)
head(mtcars)


car_df2 <- melt(car_df, "mpg") #changes df from car level to mpg*var level
png(filename = "mpg_var_relation.png")
p1 <- ggplot(data = car_df2, aes(value, mpg)) + geom_point(alpha = 0.3) + facet_wrap(.~variable, scales = "free")
p1
dev.off()

car_df3 <- melt(car_df, "am")
png(filename = "am_var_relation.png")
p2 <- ggplot(data = car_df3, aes(value, am)) + geom_count(alpha = 0.3) + facet_wrap(.~variable, scales = "free")
p2
dev.off()

p5 <- ggplot(data = car_df, aes(factor(am, levels = c(0,1), labels = c('automatic', 'manual') ), mpg)) +
    geom_boxplot() + 
    labs(x = "Transmission")
p5

#BEST PLOT
car_df4 <- melt(car_df, c("mpg", "am"))
png(filename = "mpg_am_var_relation.png")
p3 <- ggplot(data = car_df4, aes(value, mpg)) + 
    geom_point(aes(color = factor(am, levels = c(0,1), labels = c('automatic', 'manual'))), alpha = 0.3) + 
    facet_wrap(.~variable, scales = "free") +
    labs(color = "Transmission")
p3
dev.off()

factor_names <- c("cyl", "vs", "am", "gear", "carb")
car_df[,factor_names] <- lapply(car_df[,factor_names], factor)

am_model <- lm(mpg~am, data=car_df)
summary(am_model)
plot(predict(am_model), resid(am_model))
par(mfrow = c(2,2), mar = c(1,1,1,1))
plot(am_model)

full_model <- lm(mpg~., data = car_df)
summary(full_model)

am_wt_model <- lm(mpg~am+wt, data=car_df)
summary(am_wt_model)

am_hp_model <- lm(mpg~am+hp, data=car_df)
summary(am_hp_model)

am_disp_model <- lm(mpg~am+disp, data = car_df)
summary(am_disp_model)

anova(am_model, am_wt_model)

#BEST MODEL
am_wt_cyl_model <- lm(mpg~am+wt+cyl, data = car_df)
summary(am_wt_cyl_model)
confint(am_wt_cyl_model)

am_wt_cyl_carb <- lm(mpg~am+wt+cyl+carb, data = car_df)
summary(am_wt_cyl_carb)

am_wt_cyl_hp_model <- lm(mpg~am+wt+cyl+hp, data = car_df)
summary(am_wt_cyl_hp_model)
confint(am_wt_cyl_hp_model)

anova(am_model,am_wt_model, am_wt_cyl_model, am_wt_cyl_hp_model, full_model)
sqrt(vif(am_wt_cyl_model))
sqrt(vif(am_wt_cyl_hp_model))
sqrt(vif(full_model))

#Diagnostics on selected model
summary(am_wt_cyl_model)
confint(am_wt_cyl_model)

png(filename = "model_diagnostics.png")
par(mfrow = c(2,2), mar = c(1,1,1,1))
p4 <- plot(am_wt_cyl_model)
p4
dev.off()
dfbetas(am_wt_cyl_model)
hatvalues(am_wt_cyl_model)



###SCRATCH WORK
factor_names <- c("cyl", "vs", "am", "gear", "carb")
car_df[,factor_names] <- lapply(car_df[,factor_names], factor)

car_df3 <- subset(car_df2, variable %in% c("cyl", "am"))
test <- mtcars %>%
    gather(-mpg, -hp, -cyl, key = "var", value = "value")

test2 <- melt(car_df, c("mpg", "hp", "cyl"))
test3 <- pivot_longer(data=car_df, cols = !mpg, names_to = "var")
with(car_df, plot(disp,mpg))
with(car_df, plot(cyl,mpg))

plot(mpg~., data=car_df)

with(car_df, xyplot(mpg ~ as.factor(am) | cyl))

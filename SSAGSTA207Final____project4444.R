## ----global_options, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(fig.pos = 'H')
knitr::opts_chunk$set(warn = FALSE)
knitr::opts_chunk$set(echo = FALSE)
library(ggplot2)
library(knitr)
library(gplots)
library(stats)
library(KScorrect)
library(nortest)
library(maps)
library(ggplot2)
library(foreign)
suppressWarnings(library(dplyr))


## ---- echo = FALSE, warning=FALSE, message=FALSE----------------------------------------------------------

STAR = read.table("STAR_Students.sav", header = TRUE, sep = ",", quote = "\"")
STAR = read.spss("STAR_Students.sav", to.data.frame = TRUE)
#str(STAR)
#attr(STAR, "variable.labels")


## ---------------------------------------------------------------------------------------------------------

# Calculate proportions
prop_table <- as.data.frame(prop.table(table(STAR$yearsstar)))
names(prop_table) = c("Years", "Students")
#ggplot(stack(prop.table(table(STAR$yearsstar))), aes(x = "", y = values, fill = ind)) + geom_bar(stat = "identity", position = "fill", width = 0.5) + coord_flip() + labs(title = "Proportions of yearsstar", x = NULL, y = "Percentage") + scale_fill_brewer(palette = "Set3") + theme_minimal() + theme(legend.position = "top", axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(margin = margin(r = 20)), panel.grid.major = element_line(size = 0.2)) + geom_text(aes(label = paste0(round(values * 100), "%")), position = position_fill(vjust = 0.5))

df = as.data.frame(rbind(table(STAR$yearsstar), prop.table(table(STAR$yearsstar))))
df[2,] = df[2,]*100
df = round(df, 0)
rownames(df) =c("Number of students", "Percentage of students")
kable(df, caption = "Number and Percentage of Students Participating in STAR Project for Different Number of Years")
#table(STAR$cmpstype)
#table(STAR$cmpsdura)


## ---- include=FALSE---------------------------------------------------------------------------------------
STAR1 = STAR[, c("stdntid","gender", "race", "birthmonth", "birthday", "birthyear", "FLAGSG1" ,"flagg1", "g1classtype", "g1schid", "g1surban", "g1tchid", "g1tgen", "g1trace", "g1thighdegree", "g1tcareer", "g1tyears", "g1classsize", "g1present", "g1absent",  "g1tmathss", "g1mathbsraw", "g1freelunch")]
STAR1 = STAR1[which(STAR$FLAGSG1=="YES"),]
STAR1 = STAR1[which(is.na(STAR1$g1tmathss)==FALSE),]
STAR1 = STAR1[which(is.na(STAR1$g1classtype)==FALSE),]
STAR1$g1schid = as.factor(STAR1$g1schid)
STAR1$g1tchid = as.factor(STAR1$g1tchid)
STAR1$stdntid = as.factor(STAR1$stdntid)
#str(STAR1)



## ---- echo = FALSE, warning=FALSE, include=FALSE----------------------------------------------------------

cat("Table: Small Class sizes")
table(with(STAR1, g1classsize[g1classtype=="SMALL CLASS"]))

cat("\nTable: Regular Class sizes")
table(with(STAR1, g1classsize[g1classtype=="REGULAR CLASS"]))

cat("\nTable: Regular+aide Class sizes")
table(with(STAR1, g1classsize[g1classtype=="REGULAR + AIDE CLASS"]))



## ---- echo = FALSE----------------------------------------------------------------------------------------
suppressWarnings({group_1g = STAR1 %>%
  group_by(g1tchid) %>%
  summarise(mean_math1 = mean(g1tmathss), median_math1=median(g1tmathss), students = n(), perfreeluch=sum(g1freelunch=="FREE LUNCH")/students)
})
#str(group_1g)
STAR1 = STAR1 %>% merge(group_1g)
#STAR1$g1tfreelunch = as.factor(ifelse(STAR1$perfreeluch>=2/3, "Large%FreeLunch", ifelse(STAR1$perfreeluch>=1/3, "Medium%FreeLunch", "Small%FreeLunch") ) )
STAR1$g1tfreelunch = as.factor(ifelse(STAR1$perfreeluch>=1/2, "High%FreeLunch", "Low%FreeLunch" ) )
#str(STAR1)
#STAR1[which((abs(STAR1$g1classsize-STAR1$students)>7)),]


## ---- fig.cap="Distribution of variables of interest"-----------------------------------------------------
par(mfrow=c(2,2))
class_table <- table(STAR1$g1classtype)
class_df <- data.frame(Class_Type = names(class_table), Frequency = as.numeric(class_table))
barplot(class_df$Frequency, names.arg = class_df$Class_Type, col = "skyblue", main = "Class Types", xlab = "Class Type", ylab = "Frequency")

class_table <- table(STAR1$g1surban)
class_df <- data.frame(Class_Type = names(class_table), Frequency = as.numeric(class_table))
barplot(class_df$Frequency, names.arg = class_df$Class_Type, col = "skyblue", main = "Urbanicity", xlab = "Class Type", ylab = "Frequency")

class_table <- table(STAR1$g1tfreelunch)
class_df <- data.frame(Class_Type = names(class_table), Frequency = as.numeric(class_table))
barplot(class_df$Frequency, names.arg = class_df$Class_Type, col = "skyblue", main = "% of Free Lunch by Class", xlab = "Class Type", ylab = "Frequency")

hist(STAR1$median_math1,
     main = "Median Math Scores in 1st grade per Teacher",
     xlab = "Math Score",
     ylab = "Frequency",
     col = "skyblue",
     border = "white",
     breaks = 20,
     xlim = c(450, 620),
     ylim = c(0, 1000),
     las = 1,
     cex.axis = 0.8
    )



## ---- echo = FALSE----------------------------------------------------------------------------------------
# [ref:Bacon, Patrik, colleague that provided help in this part]
# summary(na.omit(STAR1))
# summary(na.omit(STAR1[match(unique(STAR1$g1tchid), STAR1$g1tchid),]))
# STAR1[match(unique(STAR1$g1schid), STAR1$g1schid),]


## ---------------------------------------------------------------------------------------------------------
library(ggplot2)

ggplot(STAR1, aes(x = g1classtype, y = median_math1)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Class Type", y = "Median Math Score grouped by teacher", title = "Median Math Score Distribution by Class Type") +
  theme_minimal()


## ---- results='hide',fig.keep='all'-----------------------------------------------------------------------

ggplot(STAR1[which(!is.na(STAR1$g1tfreelunch)),], aes(x = g1tfreelunch, y = median_math1, fill = g1classtype)) +
  geom_boxplot(color = "black") +
  labs(x = "Level of Absences",
       y = "Median Math Score",
       title = "Median Math Score Distribution by Free Luch and Class Type") +
  theme_minimal()


## ---- echo = FALSE, results='hide',fig.keep='all'---------------------------------------------------------
#options(repr.plot.width=12, repr.plot.height=12)
par(mfrow=c(2,2))
# Main effect plot for class type
suppressWarnings(plotmeans(median_math1 ~ g1classtype, data = STAR1, xlab = "ClassType", ylab = "Median Math Score",
          main="Main  effect, class type",cex.lab=1.5, col = "#FF7F0E") )
# Main effect plot for urbanicity
suppressWarnings(plotmeans(median_math1 ~ g1surban, data = STAR1, xlab = "Urbanicity", ylab = "Median Math Score",
          main="Main  effect, location",cex.lab=1.5, col = "#2CA02C") )
# Main effect plot for school ID
suppressWarnings(plotmeans(median_math1 ~ g1schid, data = STAR1, xlab = "SchoolID", ylab = "Median Math Score",
          main="Main  effect, school ID",cex.lab=1.5, col = "#1F77B4") )
# Main effect plot for free lunch
suppressWarnings(plotmeans(median_math1 ~ g1tfreelunch, data = STAR1, xlab = "Free Lunch", ylab = "Median Math Score",
          main="Main  effect, school ID",cex.lab=1.5, col = "#1F77B4") )



## ---- include=FALSE---------------------------------------------------------------------------------------
suppressMessages(attach(STAR1))


## ---- echo = FALSE, results='hide',fig.keep='all'---------------------------------------------------------

par(mfrow=c(2,2))
#Interaction plot


suppressWarnings(interaction.plot(g1classtype, g1surban, median_math1
                ,cex.lab=1.5,ylab="Median Math Score",xlab='Class Type', 
                main="Interaction effects between class type and location", col = c("#1F77B4", "#FF7F0E", "#2CA02C", "#E82ABC") )) 

suppressWarnings(interaction.plot(g1schid,g1classtype, median_math1
                ,cex.lab=1.5,ylab="Median Math Score",xlab='School ID', 
                main="Interaction effects between class type and School ID", col = c("#1F77B4", "#2CA02C", "#E82ABC") )) 

suppressWarnings(
  interaction.plot(
    g1schid, 
    g1surban, 
    median_math1, 
    cex.lab = 1.5, 
    ylab = "Median Math Score", 
    xlab = "School ID", lwd = 2 ,
    main = "Effects of School ID plot", 
    col = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")  # Colors for each level
  )
)

suppressWarnings(interaction.plot(STAR1$g1tfreelunch,g1classtype, median_math1
                ,cex.lab=1.5,ylab="Median Math Score",xlab='School ID', 
                main="Interaction effects between class type and free luch", col = c("#1F77B4", "#2CA02C", "#E82ABC") )) 

# # Create a data frame with your variables
# data <- data.frame(g1classtype, g1surban, g1schid, median_math1)
# 
# # Plot interaction effects using ggplot2
# ggplot(data, aes(x = g1surban, y = median_math1, color = factor(g1surban), group = factor(g1surban))) +
#   geom_line() +
#   facet_wrap(~g1classtype) +
#   labs(x = "Class Type", y = "Median Math Score", 
#        title = "Interaction Effects between Class Type, Location, and School ID")




## ---- echo = FALSE, include=FALSE-------------------------------------------------------------------------
ggplot(STAR1, aes(x = g1tcareer, y = median_math1)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Class Type", y = "Median Math Score grouped by teacher career", title = "Median Math Score Distribution by Teacher Career") +
  theme_minimal()
STAR1$exp_level <- cut(STAR1$g1tyears, breaks = c(0, 4, 10, Inf), labels = c("Low", "Medium", "High"), include.lowest = TRUE)

# Plot the boxplots
ggplot(STAR1, aes(x = exp_level, y = median_math1)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Teacher Experience Level", y = "Median Math Score", title = "Median Math Score Distribution by Teacher Experience Level") +
  theme_minimal()
ggplot(STAR1, aes(x = g1thighdegree, y = median_math1)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Class Type", y = "Median Math Score grouped by teacher", title = "Median Math Score Distribution by Class Type") +
  theme_minimal()
STAR1$abs_level <- cut(STAR1$g1absent,
                       breaks = c(0, 3, 6, 11, Inf),
                       labels = c("None", "Low", "Some", "Frequent"),
                       include.lowest = TRUE)

ggplot(STAR1, aes(x = abs_level, y = median_math1, fill = g1classtype)) +
  geom_boxplot(color = "black") +
  labs(x = "Level of Absences",
       y = "Median Math Score",
       title = "Median Math Score Distribution by Absences Level") +
  theme_minimal()
STAR1$abs_level <- cut(STAR1$g1absent,
                       breaks = c(0, 3, 6, 11, Inf),
                       labels = c("None", "Low", "Some", "Frequent"),
                       include.lowest = TRUE)


ggplot(STAR1, aes(x = g1surban, y = median_math1, fill = g1freelunch)) +
  geom_boxplot(color = "black") +
  labs(x = "Level of Absences",
       y = "Median Math Score",
       title = "Median Math Score Distribution by Free Luch and Location") +
  theme_minimal()
ggplot(STAR1, aes(x = g1tfreelunch, y = median_math1, fill = g1classtype)) +
  geom_boxplot(color = "black") +
  labs(x = "Free Lunch level",
       y = "Median Math Score",
       title = "Median Math Score Distribution by Free Luch and Class type") +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------------
fit2 = aov(median_math1 ~ g1classtype + g1schid + g1tfreelunch, data = STAR1)
summary(fit2)
#anova(fit2)
#plot(fit2)
#plot(fit2$residuals)
#hist(fit2$residuals)
#lillie.test(fit2$residuals)$p.value




## ---- echo = FALSE----------------------------------------------------------------------------------------
# fit0 =aov(mean_math1 ~ g1classtype * g1schid, data = STAR1)
# summary(fit0)


## ---- echo = FALSE----------------------------------------------------------------------------------------
# fit1 = aov(mean_math1 ~ g1classtype + g1schid, data = STAR1)
# summary(fit1)
#fit2$coefficients


## ---- echo=FALSE------------------------------------------------------------------------------------------

kwPval <- kruskal.test(STAR1$median_math1, STAR1$g1classtype)$p.value
kwPval


## ---------------------------------------------------------------------------------------------------------
# Main effect plot for class type
suppressWarnings(plotmeans(median_math1 ~ g1classtype, data = STAR1, xlab = "ClassType", ylab = "Median Math Score",
          main="Main  effect, class type",cex.lab=1.5, col = "#FF7F0E") )


## ---------------------------------------------------------------------------------------------------------
#kruskal.test(median_math1 ~ g1classtype + g1schid + g1tfreelunch, data = STAR1)


## ---- echo = FALSE----------------------------------------------------------------------------------------
# summary(fit2)


## ---- include=FALSE---------------------------------------------------------------------------------------
library(FSA)
library(knitr)
dtest = dunnTest(STAR1$median_math1, STAR1$g1classtype, "bonferroni")


## ---------------------------------------------------------------------------------------------------------
kable(dtest$res)


## ---- echo = FALSE----------------------------------------------------------------------------------------
#TukeyHSD(fit2, "g1classtype", conf.level = 0.01)


## ---- echo = FALSE----------------------------------------------------------------------------------------
par(mfrow=c(2,2))
  plot(fit2)


## ---- echo = FALSE----------------------------------------------------------------------------------------
hist(fit2$residuals, breaks = 20, col = "skyblue", main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequency")


## ---- echo = FALSE----------------------------------------------------------------------------------------
library(KScorrect)
library(nortest)
suppressMessages(lillie.test(fit2$residuals)$p.value)

#ad.test(fit1$residuals)$p.value


## ---- echo = FALSE----------------------------------------------------------------------------------------
# library(KScorrect)
# library(nortest)
# lillie.test(fit2$residuals)$p.value
# ad.test(fit2$residuals)$p.value


## ---- echo = FALSE, include=FALSE-------------------------------------------------------------------------
library(car)


## ---------------------------------------------------------------------------------------------------------
as.data.frame(leveneTest(median_math1 ~ g1classtype*g1tfreelunch*(g1schid), data = STAR1))
#oneway.test(median_math1 ~ g1classtype*g1tfreelunch*g1schid, data = STAR1)


## ---------------------------------------------------------------------------------------------------------
fit2$coefficients


## ---------------------------------------------------------------------------------------------------------
sessionInfo()


## ----ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE-------------------------------------------------
## NA


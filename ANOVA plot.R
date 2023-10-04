library(ggpubr)
library(dplyr)
by_cyl <- mtcars %>% group_by(cyl)
ggline(ASD, x = "Season", y = "Salinity",
            add = c("mean_se", "jitter"),
       order = c("Monsoon","Winter","Summer"),
             ylab = "Salinity", xlab = "season")
ggline(CD1, x = "Rings_number", y = "Control",
       add = c("mean_se", "jitter"),
       order = c("2R","3R","4R","5R","6R"),
       ylab = "Concentration combinedspot",b = "Number of benzene ring")

plotmeans(weight ~ group, data = data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI")
phytoplankton <- read_excel("Diu data/phytoplankton.xlsx")
View(phytoplankton)
attach(phytoplankton)
phyto<-data.frame(phytoplankton)
phyto1 <- phyto %>% group_by(Season)
ggline(phyto1, x = "Season", y = "Cosinodiscus",
add = c("mean_se", "jitter"),
order = c("Summer","Monsoon","Winter"),
ylab = "Cosinodiscus", xlab = "season")
Season
phyto1
phyto<group_by(Season)
phyto2<-phyto %>% group_by(Season)
phyto2
ggline(phyto1, x = "Season", y = "Biddulphia",
add = c("mean_se", "jitter"),
order = c("Summer","Monsoon","Winter"),
ylab = "Biddulphia", xlab = "Season")
ggline(phyto1, x = "Season", y = "Chaetoceros",
       add = c("mean_se", "jitter"),
       order = c("Summer","Monsoon","Winter"),
       ylab = "Chaetoceros", xlab = "Season")
RANOVA<-aov(S1~Rings)
TukeyHSD(RANOVA,"Rings", ordered = TRUE)
plot(TukeyHSD(RANOVA, "Rings"))


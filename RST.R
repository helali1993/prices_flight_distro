library("dplyr")
library("ggpubr")
library("readr")
library("reshape")

data_all <- read_csv("data.csv")

head(data_all)

data_all$price <- as.numeric(data_all$price)

data_all$Day_Week

data_all$Day_Week <- as.factor(data_all$Day_Week)

levels(data_all$Day_Week)

data_all$Day_Week <- ordered(data_all$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

group_by(data_all, Day_Week) %>%
   summarise(
              count = n(),
              mean = mean(price, na.rm = TRUE),
              sd = sd(price, na.rm = TRUE),
              median = median(price, na.rm = TRUE),
              IQR = IQR(price, na.rm = TRUE)
    )


ggboxplot(data_all, x = "Day_Week", y ="price", color = "Day_Week",
           order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          ylab = "Prices", xlab = "Day of The Week" )


kruskal.test(price ~ Day_Week, data = data_all)

pairwise.wilcox.test(data_all$price, data_all$Day_Week, p.adjust.method =  "BH")

# FOR CAIRO PRICES VS. DAYS OF WEEK

data_cai <- read_csv("data_cai.csv")

data_cai$price <- as.numeric(data_cai$price)

data_cai$Day_Week <- as.factor(data_cai$Day_Week)

levels(data_cai$Day_Week)

data_cai$Day_Week <- ordered(data_cai$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

group_by(data_cai, Day_Week) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    IQR = IQR(price, na.rm = TRUE)
  )


ggboxplot(data_cai, x = "Day_Week", y ="price", color = "Day_Week",
          order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          ylab = "Prices", xlab = "Day of The Week" )


kruskal.test(price ~ Day_Week, data = data_cai)

pairwise.wilcox.test(data_cai$price, data_cai$Day_Week, p.adjust.method =  "BH")

# FOR NYC PRICES VS. DAYS OF WEEK

data_nyc <- read_csv("data_nyc.csv")

data_nyc$price <- as.numeric(data_nyc$price)

data_nyc$Day_Week <- as.factor(data_nyc$Day_Week)

levels(data_nyc$Day_Week)

data_nyc$Day_Week <- ordered(data_nyc$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

group_by(data_nyc, Day_Week) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    IQR = IQR(price, na.rm = TRUE)
  )


ggboxplot(data_nyc, x = "Day_Week", y ="price", color = "Day_Week",
          order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          ylab = "Prices", xlab = "Day of The Week" )


kruskal.test(price ~ Day_Week, data = data_nyc)

pairwise.wilcox.test(data_nyc$price, data_nyc$Day_Week, p.adjust.method =  "BH")


# FOR SYD PRICES VS. DAYS OF WEEK

data_syd <- read_csv("data_syd.csv")

data_syd$price <- as.numeric(data_syd$price)

data_syd$Day_Week <- as.factor(data_syd$Day_Week)

levels(data_syd$Day_Week)

data_syd$Day_Week <- ordered(data_syd$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

group_by(data_syd, Day_Week) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    IQR = IQR(price, na.rm = TRUE)
  )


ggboxplot(data_syd, x = "Day_Week", y ="price", color = "Day_Week",
          order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          ylab = "Prices", xlab = "Day of The Week" )


kruskal.test(price ~ Day_Week, data = data_syd)

pairwise.wilcox.test(data_syd$price, data_syd$Day_Week, p.adjust.method =  "BH")

# FOR ROME PRICES VS. DAYS OF WEEK

data_rome <- read_csv("data_rome.csv")

data_rome$price <- as.numeric(data_rome$price)

data_rome$Day_Week <- as.factor(data_rome$Day_Week)

levels(data_rome$Day_Week)

data_rome$Day_Week <- ordered(data_rome$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

group_by(data_rome, Day_Week) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    IQR = IQR(price, na.rm = TRUE)
  )


ggboxplot(data_rome, x = "Day_Week", y ="price", color = "Day_Week",
          order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          ylab = "Prices", xlab = "Day of The Week" )


kruskal.test(price ~ Day_Week, data = data_rome)

pairwise.wilcox.test(data_rome$price, data_rome$Day_Week, p.adjust.method =  "BH")

# FOR RIO PRICES VS. DAYS OF WEEK

data_rio <- read_csv("data_rio.csv")

data_rio$price <- as.numeric(data_rio$price)

data_rio$Day_Week <- as.factor(data_rio$Day_Week)

levels(data_rio$Day_Week)

data_rio$Day_Week <- ordered(data_rio$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

group_by(data_rio, Day_Week) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    IQR = IQR(price, na.rm = TRUE)
  )


ggboxplot(data_rio, x = "Day_Week", y ="price", color = "Day_Week",
          order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          ylab = "Prices", xlab = "Day of The Week" )


kruskal.test(price ~ Day_Week, data = data_rio)

pairwise.wilcox.test(data_rio$price, data_rio$Day_Week, p.adjust.method =  "BH")

# FOR TYO PRICES VS. DAYS OF WEEK

data_tyo <- read_csv("data_tyo.csv")

data_tyo$price <- as.numeric(data_tyo$price)

data_tyo$Day_Week <- as.factor(data_tyo$Day_Week)

levels(data_tyo$Day_Week)

data_tyo$Day_Week <- ordered(data_tyo$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

group_by(data_tyo, Day_Week) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    IQR = IQR(price, na.rm = TRUE)
  )


ggboxplot(data_tyo, x = "Day_Week", y ="price", color = "Day_Week",
          order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          ylab = "Prices", xlab = "Day of The Week" )


kruskal.test(price ~ Day_Week, data = data_tyo)

pairwise.wilcox.test(data_tyo$price, data_tyo$Day_Week, p.adjust.method =  "BH")


# Me trying to make the data look nice
ggplot(data = data_tyo, 
       mapping = aes(x = Day_Week, y = price, color)) + geom_point() 

ggdensity(data = data_tyo, x = "price", add = "mean", fill = "lightgray", rug= TRUE)

ggplot(data = data_tyo, mapping = aes(x = Day_Week, y = price, color = Day_Week)) + geom_boxplot()

# Trying to do stuff after adding extra row

data_2 <- read_csv("data_2.csv")

data_2$price <- as.numeric(data_2$price)

data_2$Destination <- as.factor(data_2$Destination)

levels(data_2$Destination)

unique(data_2$Destination)

data_2$Day_Week <- as.factor(data_2$Day_Week)

levels(data_2$Day_Week)

data_2$Day_Week <- ordered(data_2$Day_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(data = data_2, aes(x = Day_Week, y = price, color = Day_Week)) + geom_boxplot() + facet_grid(rows = vars(Destination))

#which(is.na(data_2), arr.ind = TRUE)

data_2_CN <- subset(data_2, Destination == "Cairo" | Destination =="New York")

ggplot(data = data_2_CN, aes(x = Day_Week, y = price, color = Day_Week)) + geom_boxplot() + facet_grid(rows = vars(Destination))

data_2_SR <- subset(data_2,Destination =="Sydney"| Destination =="Rio")

ggplot(data = data_2_STR, aes(x = Day_Week, y = price, color = Day_Week)) + geom_boxplot() + facet_grid(rows = vars(Destination), scales =  "free_y")  + labs(color = "Day", x = "Day", y ="Prices") + theme_bw()

ggplot(data = data_2_CNR, aes(x = Day_Week, y = price, color = Day_Week)) + geom_boxplot() + facet_grid(rows = vars(Destination), scales= "free_y") + labs(color = "Day", x = "Day", y ="Prices") + theme_bw()

ggplot(data = data_2, aes(x = Day_Week, y = price, color = Day_Week)) + geom_boxplot() + facet_grid(facets = vars(Destination), scales =  "free_y")  + labs(color = "Day", x = "Day", y ="Prices") + theme_bw()


### So is it the airline or is it the day?

data_cai$Airline <- as.factor(data_cai$Airline)

levels(data_cai$Airline)

kruskal.test(price ~ Airline, data = data_cai)

ggplot(data = data_cai, aes(x = Airline, y = price, color = Airline)) + geom_boxplot()

pairwise.wilcox.test(data_cai$price, data_cai$Airline, p.adjust.method =  "BH")

warnings( pairwise.wilcox.test(data_cai$price, data_cai$Airline, p.adjust.method =  "BH"))

shapiro.test(data_cai$price)

cor.test(data_cai$price, data_cai$Day_Week, method = "spearman")

kruskal.test(Airline ~ Day_Week, data =  data_all)

g_box_airline = ggplot(data = data_2, aes(x = Airline, y = price, color = Airline)) + geom_boxplot() + facet_grid(rows = vars(Destination), scales = "free_y") + theme(axis.text.x = element_text(colour = "grey20", size = 5, angle = 90, hjust = 0.5, vjust = 0.5), axis.ticks.x =  element_blank(), legend.position =  "none")
g_box_airline



shapiro.test(data_nyc$price)

x <-dnorm(100, 0, 1)

data_cai_luf = subset(data_cai, Airline == "Lufthansa")

ggplot(data = data_all, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_wrap(facets = vars(Airline)) + theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 5, angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")

pairwise.wilcox.test(data_cai$price, data_cai$Airline, p.adjust.method =  "BH")

data_not_multi <- subset(data_2, Airline != "Multiple airlines")

#ggplot(data = data_2_CNR, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_grid(cols = vars(Airline), rows = vars(Destination), drop = TRUE) + theme_bw() +
 # theme(axis.text.x = element_text(colour = "grey20", size = 5, angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")

#Drawing a g plot then will add grids

#CAI

data_cai_no_multi <- subset(data_2, Destination == "Cairo" & Airline != "Multiple airlines")

g1_cai <- ggplot(data = data_cai_no_multi, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_grid(cols = vars(Airline), rows = vars(Destination), drop = TRUE) + theme_bw() +
  theme(axis.ticks.x =  element_blank() ,axis.text.x = element_blank(), axis.title = element_blank(), legend.position = "none")
g1_cai + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))

g_cai_histo_airlines <- ggplot(data = data_cai_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Airline), scales = "free")

g_cai_histo_airlines

g_cai_histo_prices <- ggplot(data = data_cai_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Day_Week), scales = "free")

g_cai_histo_prices

#NYC

data_nyc_no_multi <- subset(data_2, Destination == "New York" & Airline != "Multiple airlines")

g1_nyc <- ggplot(data = data_nyc_no_multi, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_grid(cols = vars(Airline), rows = vars(Destination), drop = TRUE) + theme_bw() +
  theme(axis.ticks.x =  element_blank() ,axis.text.x = element_blank(), axis.title = element_blank(), legend.position = "none")
g1_nyc + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))

g_nyc_histo_airlines <- ggplot(data = data_nyc_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Airline), scales = "free")

g_nyc_histo_airlines

g_nyc_histo_prices <- ggplot(data = data_nyc_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Day_Week), scales = "free")

g_nyc_histo_prices

#SYD

data_syd_no_multi <- subset(data_2, Destination == "Sydney" & Airline != "Multiple airlines")

g1_syd <- ggplot(data = data_syd_no_multi, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_grid(cols = vars(Airline), rows = vars(Destination), drop = TRUE) + theme_bw() +
  theme(axis.ticks.x =  element_blank() ,axis.text.x = element_blank(), axis.title = element_blank(), legend.position = "none")
g1_syd + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))

g_syd_histo_airlines <- ggplot(data = data_syd_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Airline), scales = "free")

g_syd_histo_airlines

g_syd_histo_prices <- ggplot(data = data_syd_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Day_Week), scales = "free")

g_syd_histo_prices

#Rome

data_rome_no_multi <- subset(data_2, Destination == "Rome" & Airline != "Multiple airlines")

g1_rome <- ggplot(data = data_rome_no_multi, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_grid(cols = vars(Airline), rows = vars(Destination), drop = TRUE) + theme_bw() +
  theme(axis.ticks.x =  element_blank() ,axis.text.x = element_blank(), axis.title = element_blank(), legend.position = "none")
g1_rome + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))

g_rome_histo_airlines <- ggplot(data = data_rome_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Airline), scales = "free")

g_rome_histo_airlines

g_rome_histo_prices <- ggplot(data = data_rome_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Day_Week), scales = "free")

g_rome_histo_prices


#RIO

data_rio_no_multi <- subset(data_2, Destination == "Rio" & Airline != "Multiple airlines")

g1_rio <- ggplot(data = data_rio_no_multi, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_grid(cols = vars(Airline), rows = vars(Destination), drop = TRUE) + theme_bw() +
  theme(axis.ticks.x =  element_blank() ,axis.text.x = element_blank(), axis.title = element_blank(), legend.position = "none")
g1_rio + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))

g_rio_histo_airlines <- ggplot(data = data_rio_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Airline), scales = "free")

g_rio_histo_airlines

g_rio_histo_prices <- ggplot(data = data_rio_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Day_Week), scales = "free")

g_rio_histo_prices

#TYO

data_tyo_no_multi <- subset(data_2, Destination == "Tokyo" & Airline != "Multiple airlines")

g1_tyo <- ggplot(data = data_tyo_no_multi, aes(x = Day_Week, colour = Day_Week, fill = Day_Week)) + geom_bar() + facet_grid(cols = vars(Airline), rows = vars(Destination), drop = TRUE) + theme_bw() +
  theme(axis.ticks.x =  element_blank() ,axis.text.x = element_blank(), axis.title = element_blank(), legend.position = "none")
g1_tyo + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))

g_tyo_histo_airlines <- ggplot(data = data_tyo_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Airline), scales = "free")

g_tyo_histo_airlines

g_tyo_histo_prices <- ggplot(data = data_tyo_no_multi, aes(x = price)) + geom_histogram() +facet_wrap(facets = vars(Day_Week), scales = "free_y")

g_tyo_histo_prices


#Layovers
kruskal.test(price ~ Stops, data = data_all)

data_all_factor_Stops <- data_2

data_all_factor_Stops$Stops <- as.factor(data_all_factor_Stops$Stops)

levels(data_all_factor_Stops$Stops)

ggplot(data = data_all_factor_Stops, aes(x = Stops, y = price, color = Stops)) + geom_boxplot()


ggplot(data = data_all_factor_Stops, aes(x = Stops, y = price, color = Stops)) + geom_boxplot() + facet_grid(facets = vars(Destination), scales = "free_y")  + labs(color = "No of Layovers", y ="Prices") + theme_bw()



ggplot(data = data_all_factor_Stops, aes(x = price)) + geom_histogram() + facet_grid(cols = vars(Stops), rows = vars(Destination), scales = "free")

wilcox.test(price ~ Day_Week, data = data_2 )

kruskal.test(price ~ Day_Week, data = data_2)


kruskal.test(price ~ Day_Week, data = data_cai)

kruskal.test(price ~ Day_Week, data = data_nyc)

shapiro.test(data_2$price[1:5000])
shapiro.test(data_nyc$price)


data_2_g <- subset(data_2, Destination == "New York")


ggplot(data = data_2, aes(x=price, fill = Day_Week, color = Day_Week)) + geom_histogram(alpha = 0.5, position = "stack") + facet_wrap(facets = vars(Day_Week)) + theme(axis.title.y = element_blank(), legend.position = "top")

dep <- ggplot(data = data_2, aes(x = price)) + geom_density() + theme( legend.position =  "none", axis.title.y = element_blank())
dep


dedur <- ggplot(data = data_2, aes(x = price)) + geom_density()
dedur <- ggplot(data = data_2, aes(x = price)) + geom_density()



data_all_selected <- select(data_2, Airline, price, Day_Week, Stops)

data_all_selected$refseq <- c("Airline","Departure","Arrival","price","Duration","Stops","Departure_Time", "Arrival_Time", "Day_Week", "Day_Month")

long = melt(data_all_selected, id.vars= "refseq")


kruskal.test(price ~ Day_Week, data = data_2)

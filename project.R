setwd("/Users/coltonrobinson/Desktop/e256 project")
library("tidyverse")
library("directlabels")
library("leaflet")
library("sf")
library("plotly")



## Opening DATA ##
acs2019 <- read_csv("ACS2019.csv")
mortality<- read_csv("Mortality.csv")


## Cleaning DATA 1 ## 
gundeath1<- mortality %>%
  filter(YEAR=="2019") %>%
  select(YEAR,STATE,RATE,DEATHS) %>%
  rename("year"=YEAR, "state"=STATE, "rate"=RATE, "deaths"=DEATHS) 
# switching state abbreviations to full names
state <- read_csv("state.csv") %>%
  rename("state"=Code)
gundeath2 <- left_join(gundeath1,state,by="state")  
gundeath3 <- gundeath2 %>% 
  rename("code"=state, "state"=State) %>% 
  select(state, year, rate, deaths)


## Clearning DATA2 ## 
ACS2019 <- acs2019 %>%
  mutate(prop_white=SE_A03001_002/SE_A03001_001, prop_collgrad=(SE_A12001_005+SE_A12001_006+SE_A12001_007+SE_A12001_008)/SE_A12001_001, unemp=(SE_A17002_006/SE_A17002_004)*100, pvty=(SE_A13003A_002+SE_A13003B_002+SE_A13003C_002)/(SE_A13003A_001+SE_A13003B_001+SE_A13003C_001)) %>%
  rename("FIPS"=Geo_FIPS, "state"=Geo_NAME, "popsqmile"=SE_A00002_002, "popover25"=SE_A12001_001, "lessHS"=SE_A12001_002, "HS"=SE_A12001_003, "somecoll"=SE_A12001_004, "Bachelors"=SE_A12001_005, "Masters"=SE_A12001_006, "profdeg"=SE_A12001_007, "PhD"=SE_A12001_008) %>% 
  select(FIPS, state, popsqmile, popover25, lessHS, HS, somecoll, Bachelors, Masters, profdeg, PhD, prop_white, prop_collgrad, unemp, pvty)


## Merging Data1 & 2 ## 
mydata <- left_join(gundeath3, ACS2019, by="state")
save(mydata, file="mydata.RData")
 

## Cleaning Data 3 ## 
gunlaw <- read_csv("Strictness.csv")
gunlaw1 <- rename(gunlaw, "state"=State)

## Merging Data 1+2+3 ## 
mydata0 <- left_join(gunlaw1, mydata, by="state")
state1<- rename(state, "state"=State, "abbv"=state)
mydata1 <- left_join(mydata0, state1, by="state")
ownership <- rename(ownership, "state"=State)
mydata1 <- left_join(mydata1, ownership, by="state")


## Regression ## 
regression1 <- lm(data=mydata1, gunDeathRate ~ lawsRank + popsqmile + gunOwnership + prop_collgrad + unemp + pvty)
regression2 <- lm(data=mydata1, gunDeathRate ~ lawsRank)
summary(regression1)
summary(regression2)

## Scatterplot of the Regression2 ## 
ggplot(data=mydata1, mapping = aes(x=lawsRank,y=gunDeathRate))+
  geom_point(shape=19, size=2, color="blue")+
  geom_text(aes(label=abbv),hjust=0, vjust=2, size=3)+
  geom_smooth(method = "lm", se = FALSE, col="red")+ 
  labs(caption = "Data source: Giffords Law Center to Prevent Gun Violence for the Gun Laws. CDC for Firarm Mortality Rate")+
  ggtitle("Figure3: Gun Laws and Deaths by State, 2019")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Rank of Strictness of Gun Laws")+
  ylab("Firearm Mortality Rate")

cor(mydata1$lawsRank,mydata1$gunDeathRate)

## Stargazer ##
library(stargazer)
t<-stargazer(regression2, regression1, type="html", title = "Regressions",
          dep.var.labels = c("Gun-Related Death Rate"), 
          covariate.labels = c("Rank of Stricness of Gun Laws","Population Density (Per Sq. Mile)",
          "Proportion of Adults Living in a Household With a Firearm ","Bachelor’s Degree or Higher","Unemployment Rate","Poverty Rate"),
          keep.stat = c("n","adj.rsq","rsq"), 
          keep=c("gunDeathRate", "lawsRank", "popsqmile", "gunOwnership","prop_collgrad", "unemp", "pvty"),out="table.doc")


## Intro - over time ## 
# US Overtime
overtime <- read_csv("overtime.csv")
pp<-ggplot(data=overtime, aes(x=Year,y=Deaths))+ # first try
  geom_line(color="red")+
  geom_point()+
  theme_classic()+
  scale_x_continuous(breaks=overtime$Year)+
  theme(axis.text.x=element_text(angle =90, hjust = 0, size=10), plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  labs(title="Figure1: U.S. Gun-related Deaths, 1999-2019",
       caption = "Data source: Centers for Disease Control and Prevention, National Center for Health Statistics.")
print(pp)
ggplotly(pp)



#Global 2021
global <- read_csv("global.csv")
global1 <- global %>%
  rename("death"= total,"totaldeath"=totalNumber)%>%
  mutate(country=fct_reorder(country,death))#decsending order

global2<-  global1[!(is.na(global1$death) | global1$death==""), ] # removed emty row of total death

# Making an interactive barplot using plotly
p <- ggplot(global2,aes(fill=,x=country, y=death,fill=ifelse(country=="United States","Highlighted","Normal")))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "country", values = c("red","grey50"))+
  ylab("Death Rate")+
  labs(title = "Figure2: Gun-related Death Rate by Country, 2021", subtitle = "(per 100,000 population)", caption = "Data Source: Global Mortality From Firearms, 1990-2016 by the Institute for Health Metrics and Evaluations")+
  coord_flip()+
  theme(legend.position = "none",axis.title.y = element_blank())
print(p)
ggplotly(p)

# Barplot of only OECD member Countries
oecd <- read_csv("oecd.csv")
combo <- left_join(oecd,global2,by="country")
combo <-combo [!(is.na(combo$death) | combo$death==""), ]  
combo <- mutate(combo, country=fct_reorder(country,death))


b <- ggplot(combo,aes(fill=,x=country, y=death,fill=ifelse(country=="United States","Highlighted","Normal")))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "country", values = c("red","grey50"))+
  ylab("Death Rate")+
  labs(title = "Figure2: Gun-related Death Rate by OECD Country, 2016", subtitle = "(per 100,000 population)", caption = "Data Source: Global Mortality From Firearms, 1990-2016 by the Institute for Health Metrics and Evaluations")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none",axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(b)
ggplotly(b)



  
  



















### Arnolde' Code ###
# Load base packages manually
library("tidyverse")
library("sf")

# Import CSV files
mortality <- read_csv("Mortality.csv")
ownership <- read_csv("Ownership.csv")
strictness <- read_csv("Strictness.csv")

# Import SHP file
usa <- read_sf(dsn="states",layer="states")

# Filter SHP data
usa2 <- filter(usa,STATE_NAME!="District of Columbia")

# Join data sets
usa_mortality <- left_join(usa2,mortality,by=c("STATE_ABBR"="STATE"))
usa_ownership <- left_join(usa2,ownership,by=c("STATE_NAME"="State"))
usa_strictness <- left_join(usa2,strictness,by=c("STATE_NAME"="State"))

# Plot the data sets
ggplot(usa_mortality,aes(fill=RATE))+ 
  geom_sf()+
  theme_void()+
  #  theme(panel.background = element_rect(fill = "#009ac9"))+
  labs(
    title = "Firearm Mortality by State per 100,000 Total Population in 2019",
    caption = "Data source: CDC/National Center for Health Statistics."
  )

ggplot(usa_ownership,aes(fill=gunOwnership))+ 
  geom_sf()+
  theme_void()+
  #  theme(panel.background = element_rect(fill = "#009ac9"))+
  labs(
    title = "Firearm Ownership by State in 2019",
    caption = "Data source: RAND Corporation."
  )

ggplot(usa_strictness,aes(fill=grade2019))+ 
  geom_sf()+
  theme_void()+
  #  theme(panel.background = element_rect(fill = "#009ac9"))+
  scale_color_brewer(palette = "Dark2")+
  labs(
    title = "Strictest Firearm Laws by State in 2019",
    caption = "Data source: Giffords Law Center to Prevent Gun Violence."
  )









## Updated ##
# Import SHP file
usa <- read_sf(dsn="states",layer="states")

# Filter SHP data
usa2 <- filter(usa,STATE_NAME!="District of Columbia")

# Join data sets
usa_mortality <- left_join(usa2,mortality,by=c("STATE_ABBR"="STATE"))
usa_ownership <- left_join(usa2,ownership,by=c("STATE_NAME"="State"))
usa_strictness <- left_join(usa2,strictness,by=c("STATE_NAME"="State"))

# Plot data sets
leaflet(usa_mortality)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  addPolygons(fillColor=~colorNumeric("YlOrRd",RATE)(RATE),fillOpacity=1,label=usa_mortality$RATE)%>%
  addControl(html="Firearm Mortality by State per 100,000 Total Population in 2019",position="topright")%>%
  addControl(html="Data source: CDC/National Center for Health Statistics.",position="bottomleft")

leaflet(usa_ownership)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  addPolygons(fillColor=~colorNumeric("YlOrRd",gunOwnership)(gunOwnership),fillOpacity=1,label=usa_ownership$gunOwnership)%>%
  addControl(html="Firearm Ownership by State in 2019",position="topright")%>%
  addControl(html="Data source: RAND Corporation.",position="bottomleft")

leaflet(usa_strictness)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  addPolygons(fillColor=~colorNumeric("YlOrRd", lawsRank)(lawsRank),fillOpacity=1,label=usa_strictness$grade2019)%>%
  addControl(html="Strictest Firearm Laws by State in 2019",position="topright")%>%
  addControl(html="Data source: Giffords Law Center to Prevent Gun Violence.",position="bottomleft")



## Dasol's Scatter plot ##
##Scatterplots: Three Variables ## 
onwership1 <- rename(ownership, "state"=State) # ownership data from Arnolde's code
mydata2 <- left_join(mydata1,onwership1, by="state")

ggplot(data=mydata2, mapping = aes(x=gunOwnership,y=gunDeathRate))+
  geom_point(aes(color=grade2019))+
  geom_text(aes(label=abbv),hjust=0.5, vjust=1.5, size=3)+
  labs(caption = "Source: Giffords Law Center to Prevent Gun Violence for the Grade of Gun Laws.
       CDC/National Center for Health Statistics for Gun Mortality Rate. RAND Corporation for the Gun Ownership", fill="Grade of Gun Law")+
  ggtitle("Figure4: Gun Ownership and Mortality by State, 2019")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion of Gun Ownership")+
  ylab("Gun Mortality Rate")+
  scale_color_ordinal(name="Grade of Gun Laws",breaks = c("A", "A-","B+","B","B-","C+","C","C-","D","D-","F"))


# Clear environment
rm(list = ls())

# Clear packages
detach()

# Clear console
cat("\014")






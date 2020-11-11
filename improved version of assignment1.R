
#Introduction:- From the phylum of aquatic invertebrate animals, the following project explored phylum Bryozoa. The project is designed to investigate biodiversity. The idea was inspired from the article (A. J. CONSTABLE et al.2014) and It compares the biodiversity between Australia and United States and the idea is to hypothesize that Southern Ocean has less diverse species of Benthos because of climate change and ocean acidification and bryozoan species are largely effected by ice scour in Southern Ocean including region of Australia. Not only this but benthic predators that invade the Antarctic Peninsula effected benthnic species(B. Aronson. 2007) and evidences have been found that climate change has more adverse effect on benthnic species than predators(Gutt et al. 2014). In this project it is also compared which region among US and Australia are well sampled.

#Detailed R script:
#### This project will explore some questions related to biodiversity by using the data obtained from BOLD from the phylum Bryozoa.

#### The initial step is to load library.
#install.package("tidyverse")
library("tidyverse")
#install.package('vegan')
library('vegan')
#install.packages("ggplot2")
library("ggplot2")
#install.packages("plyr")
library("plyr")
#install.packages("rworldmap")
library(rworldmap)

#To download data directly into R from BOLD following command is used:
dfbryozoa <- read_tsv(file ="http://www.boldsystems.org/index.php/API_Public/combined?taxon=bryozoa&format=tsv")

#The data is now assigned to object 'dfbryozoa' which can be visualized using view command and it contains 2285 obs of 80 variables.

View(dfbryozoa)

#Data of all types must have some characteristics. For analysis we must explore some of the characteristics like which type of data is this and for this we will look into class of data.

class(dfbryozoa)

#This tells us that the class of this data is data frame.
#For understanding the details of this data. Following are some commands that will be used to explore like summary, length of data etc.

length(dfbryozoa)
#It tells that there are 80 variables.

summary(dfbryozoa)
#summary enabled us to explore names and mean, median, mode of each variable.

names(dfbryozoa)
#It shows names of all the variables.

dim(dfbryozoa)
#dim function shows the dimensions. It shows 2285 observations are there for 80 variables.

#To see that which country has the most barcoded data. We will use following command using piping.
dfbryozoa %>%
  dplyr::count(country, sort = TRUE)

#It shows list of countries along with detail that which country have the most barcoded data.
#Now I have created a new object 'dfbryozoa.sub' to look into data of dfbryozoa and isolate the following details from the whole data.
dfbryozoa.sub <- dfbryozoa[, c("processid", "bin_uri", "species_name", "country", "class_name",'voucher_status')]

#Now I have created a table to look into data that how many samples are collected from each country.
table(dfbryozoa.sub$country)

#I want to look into just top 4 countries. For this purpose I will use 'sort' command.
sort(table(dfbryozoa.sub$country), decreasing = TRUE)[1:4]

##To get great insight into data in terms of no of species we will calculate total no of species in this data frame

#Create a function that returns the total number of unique species for a specified dataset. 
UniqueTotal <- function(df) {
  #The function will take an inputted data frame, filter out NA values, take unique values (or names) and return the length of unique variables. 
  #df = data frame. Use data frame with an extracted / specified column. 
  result <- length(unique(na.omit(df)))
  #Return the result to the console.
  return(result)
}

#Use function to find the total unique species names in the data set df.bryozoa.sub. This function reduced 6 lines of code to 1 line of code)
UniqueTotal(dfbryozoa.sub$species_name)

#It tells that total number of species in this whole data frame are 240.

#I have chosen to focus on two regions to compare biodiversity. These are United States and Australia. So, I would create an object 'dfbryozoa.countries' and filter data of only these two countries.
dfbryozoa.countries <- filter(dfbryozoa.sub, country %in% c("Australia", "United States"))

#To view dfbryozoa.countries, I will run the following line.
View(dfbryozoa.countries)
class(dfbryozoa.countries)

#Its important to always check the class of data with which we are working, and the class function shows that we are working with data frame.

#For further analysis I want to get details from each country, so following steps are filtering out that. I have created an object 'dfbryozoa.australia' and this contains data of only Australia.
dfbryozoa.australia <- filter(dfbryozoa.sub, country=="Australia")

length(dfbryozoa.australia)
#It shows that it contains detail of 6 variables of Australia.
class(dfbryozoa.australia)

#Now we have created an object 'dfbryozoa.US' and this contains data of only United States.
dfbryozoa.US <- filter(dfbryozoa.sub, country=="United States")

length(dfbryozoa.US)
#It also shows that it contain detail of 6 variables of United States. Those variables are processid,bin_uri, species_name, country, class_name, voucher_status.
class(dfbryozoa.US)

#Firstly we will determine that how many unique species are there in the record of US and Australia.
dfbryozoa.sp.rich <- table(unique(dfbryozoa.countries$species_name))
length(dfbryozoa.sp.rich)
class(dfbryozoa.sp.rich)
#Above analysis shows that total no of unique species of Australia and US are 71.

#Now I will filter this detail for each country and for this purpose I will create an object 'dfbryozoa.f.aus'to analyse that how much unique no of species are there in the data collected from Australia.
dfbryozoa.f.aus <- filter(dfbryozoa.sub, country %in% c('Australia'))

#Now I will create an object 'bryozoa.aus.u.s' and will use unique function which will separate out unique species only.
bryozoa.aus.u.s <- unique(dfbryozoa.f.aus$species_name)
length(bryozoa.aus.u.s)
bryozoa.aus.na <- unique(na.omit(bryozoa.aus.u.s))
bryozoa.aus.na
length(bryozoa.aus.na)
class(bryozoa.aus.na)
#Above analysis shows that Initially there were 15 unique no of species but after removing NA it becomes 14.

#Now we will do further analysis. I have created an object 'bryozoa.div.aus' which will contain only species analysed from Australia So, I have created an object 'bryozoa.div.aus1' in which NA's are removed and then diversity is calculated.
bryozoa.div.aus <- (dfbryozoa.australia$species_name)
bryozoa.div.aus1 <- (na.omit(bryozoa.div.aus))
bryozoa.div.ausT <- table(bryozoa.div.aus1)
diversity(bryozoa.div.ausT, index = 'shannon')

#It gives the abundance of species from what was sampled from Australia. Total number of species are 14 and the shannon diversity index of the species sampled in Australia is 1.367.In addition to this and to make this result more logical in terms of comparison of biodiversity, I am converting this value of shannon index into effective number of species(ENS) and for that I am using function exp()
exp(1.3677)
#It shows value 3, which means that a community with shannon index of 1.3677 is as diverse as a community with 3 equally common species. 

#Now we will plot number of species of Australia against their frequency.
hist(bryozoa.div.ausT, xlab = 'Frequency', ylab = 'No. of species' , main = 'Histogram of species of Australia', border = 'red', col = 'light blue', breaks = 10)
#The histogram shows that 10-12 no of species are found in the range of 0-10 but (only two species 'Mucropetraliella ellerii' found 51 times and only 'Bugula neritina' found 94 times).

#Now I will create object to analyze that how much unique no of species are there in the data collected from United States.
bryozoa.US <- filter(dfbryozoa.sub, country %in% c('United States'))
bryozoa.US.1 <- unique(bryozoa.US$species_name)
length(bryozoa.US.1)
bryozoa.US.2 <- unique(na.omit(bryozoa.US.1))
length(bryozoa.US.2)
class(bryozoa.US.2)
#This tells us that if we look at the data of US then we came to know that there are 67 total unique species among the collected samples and after removing NA it becomes 66.

#Now we will do further analysis. I have created an object 'bryozoa.div.us' which will contain only species analysed from US So, I have created an object 'bryozoa.div.us1' in which NA's are removed and then diversity is calculated.
bryozoa.div.us <- (bryozoa.US$species_name)
bryozoa.div.us1 <- (na.omit(bryozoa.div.us))
bryozoa.div.usT <- table(bryozoa.div.us1)
sum(bryozoa.div.usT)

diversity(bryozoa.div.usT,index = 'shannon')

#It gives the abundance of species from what was sampled from United States. Total number of species are 66 and the shannon diversity index of the species sampled in US is 2.679 which shows that United States have rich biodiversity as compared to australia. Moreover, to make this result more logical in terms of comparison of biodiversity, I am converting this value of shannon index into effective number of species(ENS) and for that I am using function exp()
exp(2.679)
#It shows value 14, which means that a community with Shannon index of 2.679 is as diverse as a community with 14 equally common species.

##The Shannon diversity index value of Australia is 1.36 and for US it is 2.67 which shows that these results are supporting the hypothesis that Australia have less species in terms of numbers and diversity as compared to United States and that is because of effects of climate changes in Southern Ocean and it is severely effecting the habitat of benthos. Benthic animals especially bryozoans are also vulnerable to change in temperature and salinity and this is adversely affecting their survival.

##Now we will plot number of species of United States against their frequency.

hist(bryozoa.div.usT, xlab = 'Frequency', ylab = 'No. of species' , main = 'Histogram of species of United
States', border = 'red', col = 'light blue', breaks = 8)
#The above histogram shows near about 60 species are found in the range of 1-50, but only two species'Watersipora subtorquata' found 85 times and only 'Bugula neritina' found almost 226 times.

###Now I will construct rarefaction curves for species of both Australia and US and will do comparison between them that which region is well sampled.

#Firstly, to analyze that how well sampled is the region of Australia from the data frame. I will construct rarefaction curve which will represent sampled species from Australia. For that I am creating an object 'dfcount.species.aus' and grouping it by species name.

dfCount.species.aus <- dfbryozoa.australia %>%
  group_by(species_name) %>%
  dplyr::count(species_name)

#Now 'dfcount.species.aus' has all the data which I need for further analysis. So, I am just re-structuring this data by using the spread() function, so that the species names will become column headers and to this new object I am assigning name 'df.species.sp.aus'
df.species.sp.aus <- spread(data = dfCount.species.aus, key = species_name, value = n)
view(df.species.sp.aus)
class(df.species.sp.aus)

rarefac.curve.aus <- rarecurve(df.species.sp.aus, xlab = "Individuals", ylab = "No. of species", main ='Rarefaction curve for species of Australia', col.main = 'red', col.lab = 'blue')

#Rarefaction curve is an average of all options of species accumulation curve. When we collect samples from communities, we hardly ever record all individuals belonging to different species. Rarefaction curve indicates that how extensively we have sampled the species in the community. By looking at shape of this curve, we can say that this curve at the top does not bend but remains linear. There is no plateau so here lot of sampling is required to get the understanding of how rich is the biodiversity hereand with that lot of new species will be discovered. Therefore, we concluded that region of Australia is not well sampled. More sampling is required.

##now, to analyse that how well sampled is the region of United States from the data frame. I will construct rarefaction curve that will represent sampled species from US. For that, I am creating an object 'dfcount.species.US' and grouping it by species name. 
dfCount.species.US <- dfbryozoa.US %>%
  group_by(species_name) %>%
  dplyr::count(species_name)

#Now 'dfcount.species.US' has all the data which I need for further analysis. So, I am just re-structuring this data by using the spread() function, so that the species names will become column headers and to this new object I am assigning name 'df.species.sp.US'

df.species.sp.US <- spread(data = dfCount.species.US, key = species_name, value = n)
view(df.species.sp.US)
class(df.species.sp.US)

#Now rarefaction curve will be generated for species of US.

rarefac.curv.US <- rarecurve(df.species.sp.US, xlab = "Individuals", ylab = "No. of species", main ='Rarefaction curve for species of United States', col.main = 'blue', col.lab = 'red')

#By looking at the shape of this curve we can say that after 300 individuals on the x-axis it becomes considerably linear and might be nearing plateau, which means that new discoveries can be made but asignificant amount and new samples do have the potential to reveal a bit more but not to a great extent.Therefore, we can say that region of United States is well sampled as compared to Australia.

#Conclusion: The above analysis does not directly support the hypothesis that Australia is has less diversity of Bryozoa species due to changes Southern Ocean environment in terms of like physical disturbance, seasonal flux etc, although Ocean acidification is a concern for calcifying organisms like bryozoa but not only this there can be lot of factors that contribute towards low biodiversity of benthnic (bryozoa) species in Australia. Furthermore, it is also calculated that which region is well sampled and it shows that United States ismore well sampled as compared to Australia and lot of more sampling is required in Australia for biodiversity analysis. 
#clear: useful to run first
rm(list=ls())

#set the folder
setwd("C:\\Users\\Bianca cecato\\Dropbox\\PhD UBC\\Labour\\Assigment2")

#mostra o que tem na pasta
dir()
#mostra qual pasta eh
getwd()

#esses dois comandos sao pra checar se ta no diretorio certo

#install packages (just need to install once): this one installs to read data in dta
#install.packages("readstata13") oara instalar stata

#para carregar pacotes: sempre que abrir um Rscript tem que carregar os pacotes que vai usar
#library(readstata13) para ler stata
#install.packages("ipumsr")
library(ipumsr)

#usar <- ao inves de = (no geral funciona de qq jeito msm)

#load the data
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)

#se esquecer o que a funcao faz, clica duas vezes na funcao e aperta F1 (pro help)

#save data (sem aspas pq ja eh uma variavel do ambiente)
#save(us_data,file ="us_data.Rdata")

load("us_data.Rdata")

#funcao util: str
#tipo de arquivo: dataframe - matriz de dados (cada coluna eh uma variavel)

str(data)

# Deleting a variable: 
data$INCTOT <- NULL
data$HHWT <- NULL
data$SERIAL <- NULL

#read table: le arquivos txt e similares
#nomear o dataframe
cpi <- read.table("CPI.txt",header = TRUE)


# Creating a deflating factor (dfl): choose 2007 as base year. A / divide o valor de 2007 e divide pelos outros anos
#como acessar coluna no dataframe: usa $
cpi$dfl = cpi[6,2]/cpi$Annual

#transformar factor variable em string variable, e depois em numerico
data$YEAR = as.numeric(as.character(data$YEAR))

# Merging the cpi
data$YEAR = as.numeric(as.character(data$YEAR))
data$dfl = NA
data[data$YEAR==1980,c("dfl")] = cpi[cpi$Year==1980,3]
data[data$YEAR==1990,c("dfl")] = cpi[cpi$Year==1990,3]
data[data$YEAR==2000,c("dfl")] = cpi[cpi$Year==2000,3]
data[data$YEAR==2005,c("dfl")] = cpi[cpi$Year==2005,3]
data[data$YEAR==2006,c("dfl")] = cpi[cpi$Year==2006,3]
data[data$YEAR==2007,c("dfl")] = cpi[cpi$Year==2007,3]


#replacing missing data (999999), coloca o NA que eh tipo o "." do stata (sginifica missing)
#summary(data$INCWAGE)
data2 <- subset(data, (INCWAGE>0 & INCWAGE!=999999))
#summary(data2$INCWAGE)

#summary(data2$OCC1990)
data2 <- subset(data2, OCC1990!=999 )
#summary(data2$OCC1990)

#summary(data2$PERWT)
data2 <- subset(data2, PERWT!=0 )
#summary(data2$PERWT)

#summary(data2$UHRSWORK)
data2 <- subset(data2, UHRSWORK!=0 & UHRSWORK!=99)
#summary(data2$UHRSWORK)

#dropping aggriculture
#summary(data2$OCC1990)
data2 <- subset(data2, (data2$OCC1990!=999 & (data2$OCC1990<473 | data2$OCC1990>498) ) )
#summary(data2$OCC1990)

#testing if it worked. quero pegar os elementos do vetor incwage que sao maiores que 999000
#table(us_data$incwage[us_data$incwage>999000])

#criar variavel que eh o log dos wages deflacionados
data2$log_earnings = log(data2$INCWAGE*data2$dfl)

#collapse the data into 3-digit level occupation and year
#install.packages("tidyverse")

library(tidyverse)

# data2$PERWT[data2$INCWAGE==0] = NA

data2$l_earnings_wt = data2$log_earnings*data2$PERWT*data2$UHRSWORK


#%>% - esse eh o pipe (pega o da esquerda e joga no que ta na direita)
#sum: na.rm means remove (rm) data points that are not available (na)
#No tidy, o input é sempre um data.frame (tbl), e o output é sempre um data.frame (tbl).
#No primeiro argumento colocamos o data.frame, e nos outros argumentos colocamos o que queremos fazer.
#a utilizacao eh facilitada pelo emprego do operador %>%

weight <- data2 %>%
  group_by(YEAR) %>% 
  summarise(tot_year = sum(PERWT*UHRSWORK,na.rm = TRUE))

us_data_agg <- data2 %>%
  group_by(OCC1990, YEAR) %>% 
  summarise(tot_pr = sum(PERWT*UHRSWORK,na.rm = TRUE),
            mean_l_earnings_wt = sum(l_earnings_wt,na.rm = TRUE)/tot_pr)


#merging weight and us_data_agg
us_data_agg$tot_year = NA
us_data_agg[us_data_agg$YEAR==1980,c("tot_year")] = weight[weight$YEAR==1980,2]
us_data_agg[us_data_agg$YEAR==1990,c("tot_year")] = weight[weight$YEAR==1990,2]
us_data_agg[us_data_agg$YEAR==2000,c("tot_year")] = weight[weight$YEAR==2000,2]
us_data_agg[us_data_agg$YEAR==2005,c("tot_year")] = weight[weight$YEAR==2005,2]
us_data_agg[us_data_agg$YEAR==2006,c("tot_year")] = weight[weight$YEAR==2006,2]
us_data_agg[us_data_agg$YEAR==2007,c("tot_year")] = weight[weight$YEAR==2007,2]

rm(cpi,data)
save(us_data_agg, file = "us_data_agg.RData")


#annual growth of earnings by 3-digit occupation and employment shares
#mutate: it executes the transformations iteratively so that later transformations 
#can use the columns created by earlier transformation

us_data_agg <- us_data_agg %>%
  mutate(sh_occ_pc = 100*tot_pr/tot_year)

# Computing the growth
#function ifelse: ifelse(tes,yes,no)
us_data_agg2 <- us_data_agg %>%
  group_by(OCC1990) %>% 
  mutate(growth_against_80 = 
           100*ifelse(YEAR==1980, NA, 
                      mean_l_earnings_wt - mean_l_earnings_wt[YEAR==1980] ) ) %>% 
  mutate(growth_against_80_anualized = 
           100*ifelse(YEAR==1980, NA, 
                      (growth_against_80/100+1)^(1/(YEAR-1980))-1) )


us_data_agg2$tot_year <- NULL

#creating a variable that is the share of each occupation in 1980 only
us_data_agg2 <- us_data_agg2 %>%
  group_by(OCC1990) %>% 
  mutate(sh_80 = ifelse(YEAR==1980, sh_occ_pc, sh_occ_pc[YEAR==1980]) )


us_data_agg2 <- us_data_agg2 %>%
  group_by(OCC1990) %>% 
  mutate(l_earnings_80 = ifelse(YEAR==1980, mean_l_earnings_wt, mean_l_earnings_wt[YEAR==1980]) )

# Converting everything to numbers
#apply(X, MARGIN, FUN, ...). Margin: for matrix, 1 indicates rows and 2 indicates columns. c(1,2) indicates both
#X: an array, including a matrix
#returns a vector obtained by applying a fn to margins of a matrix
us_data_agg3 = as.data.frame(apply(us_data_agg2, 2, function(x) as.numeric(x)))
str(us_data_agg3)


#plotting the data and fitting a 4th order polynomial (weighted by the shares)
#install.packages("ggplot2")
library(ggplot2)

# Ploting the data
formula = y ~ polym(x,degree=4)
ggplot(data=us_data_agg3, 
       aes(x=l_earnings_80, y=growth_against_80_anualized)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, formula = formula, 
              mapping = aes(weight = sh_80), colour = "red") +
  xlab("Log of earnings in 1980") +
  ylab ("Earnings growth (against 1980)")




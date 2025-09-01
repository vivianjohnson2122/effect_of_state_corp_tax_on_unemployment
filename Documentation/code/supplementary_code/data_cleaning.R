

library(tidyverse)
library(rvest)
library(tidyr)
library(lubridate)



# Cleaning Unemployment Data 
# loading in the data 
AK <- read_csv("Documentation/data/original_data/state_unemployment_data/Alaska.csv")
AL <- read_csv("Documentation/data/original_data/state_unemployment_data/Alabama.csv")
AZ <- read_csv("Documentation/data/original_data/state_unemployment_data/Arizona.csv")
ARK<- read_csv("Documentation/data/original_data/state_unemployment_data/Arkansas.csv")
CA <- read_csv("Documentation/data/original_data/state_unemployment_data/California.csv")
COL <- read_csv("Documentation/data/original_data/state_unemployment_data/Colorado.csv")
CONN <- read_csv("Documentation/data/original_data/state_unemployment_data/Connecticut.csv")
DC <- read_csv("Documentation/data/original_data/state_unemployment_data/DC.csv")
DEL <- read_csv("Documentation/data/original_data/state_unemployment_data/Delaware.csv")
FL <- read_csv("Documentation/data/original_data/state_unemployment_data/Florida.csv")
GA <- read_csv("Documentation/data/original_data/state_unemployment_data/Georgia.csv")
HA <- read_csv("Documentation/data/original_data/state_unemployment_data/Hawaii.csv")
ID <- read_csv("Documentation/data/original_data/state_unemployment_data/Idaho.csv")
IL <- read_csv("Documentation/data/original_data/state_unemployment_data/Illinois.csv")
IN <- read_csv("Documentation/data/original_data/state_unemployment_data/Indiana.csv")
IOWA <- read_csv("Documentation/data/original_data/state_unemployment_data/Iowa.csv")
KS <- read_csv("Documentation/data/original_data/state_unemployment_data/Kansas.csv")
KEN <- read_csv("Documentation/data/original_data/state_unemployment_data/Kentucky.csv")
LA <- read_csv("Documentation/data/original_data/state_unemployment_data/Louisiana.csv")
MAINE <- read_csv("Documentation/data/original_data/state_unemployment_data/Maine.csv")
MAR <- read_csv("Documentation/data/original_data/state_unemployment_data/Maryland.csv")
MASS <- read_csv("Documentation/data/original_data/state_unemployment_data/Mass.csv")
MI <- read_csv("Documentation/data/original_data/state_unemployment_data/Michigan.csv")
MINN <- read_csv("Documentation/data/original_data/state_unemployment_data/Minn.csv")
MISS <- read_csv("Documentation/data/original_data/state_unemployment_data/Mississippi.csv")
MIS <- read_csv("Documentation/data/original_data/state_unemployment_data/Missouri.csv")
MON <- read_csv("Documentation/data/original_data/state_unemployment_data/Montana.csv")
NEB <- read_csv("Documentation/data/original_data/state_unemployment_data/Nebraska.csv")
NEV <- read_csv("Documentation/data/original_data/state_unemployment_data/Nevada.csv")
NH <- read_csv("Documentation/data/original_data/state_unemployment_data/NewHampshire.csv")
NJ <- read_csv("Documentation/data/original_data/state_unemployment_data/NewJersey.csv")
NM <- read_csv("Documentation/data/original_data/state_unemployment_data/NewMexico.csv")
NY <- read_csv("Documentation/data/original_data/state_unemployment_data/NewYork.csv")
NC <- read_csv("Documentation/data/original_data/state_unemployment_data/NorthCarolina.csv")
ND <- read_csv("Documentation/data/original_data/state_unemployment_data/NorthDakota.csv")
OHIO <- read_csv("Documentation/data/original_data/state_unemployment_data/Ohio.csv")
OK <- read_csv("Documentation/data/original_data/state_unemployment_data/Oklahoma.csv")
OR <- read_csv("Documentation/data/original_data/state_unemployment_data/Oregon.csv")
PENN <- read_csv("Documentation/data/original_data/state_unemployment_data/Penn.csv")
RI <- read_csv("Documentation/data/original_data/state_unemployment_data/RhodeIsland.csv")
SC <- read_csv("Documentation/data/original_data/state_unemployment_data/SouthCarolina.csv")
SD <- read_csv("Documentation/data/original_data/state_unemployment_data/SouthDakota.csv")
TENN <- read_csv("Documentation/data/original_data/state_unemployment_data/Tennessee.csv")
TX <- read_csv("Documentation/data/original_data/state_unemployment_data/Texas.csv")
UT <- read_csv("Documentation/data/original_data/state_unemployment_data/Utah.csv")
VM <- read_csv("Documentation/data/original_data/state_unemployment_data/Vermont.csv")
VIR <- read_csv("Documentation/data/original_data/state_unemployment_data/Virginia.csv")
WASH <- read_csv("Documentation/data/original_data/state_unemployment_data/Washington.csv")
WV <- read_csv("Documentation/data/original_data/state_unemployment_data/WestVirginia.csv")
WIS <- read_csv("Documentation/data/original_data/state_unemployment_data/Wisconsin.csv")
WY <- read_csv("Documentation/data/original_data/state_unemployment_data/Wyoming.csv")

# binding together 
state_unemployment <- bind_rows(AK,AL,ARK,AZ,
                                CA,COL,CONN,
                                DC,DEL,
                                FL,
                                GA,
                                HA,
                                ID,IL,IN,IOWA,
                                KEN,KS,
                                LA,
                                MAINE,MAR,MASS,MI,MINN,MIS,MISS,MON,
                                NEB,NEV,NH,NJ,NM,NY,NC,ND,
                                OHIO,OK,OR,
                                PENN,
                                RI,
                                SC,SD,
                                TENN,TX,
                                UT,
                                VIR,VM,
                                WASH,WIS,WV,WY)
# getting rid of the na's 
state_unemployment <- na.omit(state_unemployment)
# renaming columns to dates 
state_unemployment <- state_unemployment %>% rename(JAN = ...2,
                                                    FEB = ...3,
                                                    MAR = ...4,
                                                    APR = ...5,
                                                    MAY = ...6,
                                                    JUN = ...7,
                                                    JUL = ...8,
                                                    AUG = ...9,
                                                    SEP = ...10,
                                                    OCT = ...11,
                                                    NOV = ...12,
                                                    DEC = ...13,
                                                    state = ...14,
                                                    year = ...1) 

state_unemployment <- state_unemployment %>%
  filter(year != "Year") 

# turning into numeric columns 
for(i in 2:13){
  state_unemployment[[i]] <- as.numeric(state_unemployment[[i]])
}

# creating average yearly state unemployment 
state_unemployment <- state_unemployment %>%
  mutate(
    avg_unemployment = round((JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC) / 12, 4)
  )

# reordering cols 
state_unemployment <- state_unemployment[, c("year",
                                             "state",
                                             "avg_unemployment")]


# changing cols to numbers 
state_unemployment$year <- as.numeric(state_unemployment$year)



# writing the csv file 
write_csv(state_unemployment,"Documentation/data/analysis_data/state_unemployment.csv")




# Cleaning State Corporate Tax Rates
# loading in the data
state_corporate_tax <- read_csv("Documentation/data/original_data/state_tax_rt/state_corp_tax.csv")

# reordering cols and renaming 
state_corporate_tax <- state_corporate_tax[, c("YEAR",
                                               "STATE",
                                               "AVERAGE CORPORATE TAX RT",
                                               "PROGRESSIVE")] %>%
  rename(
    year = YEAR,
    state = STATE,
    avg_corp_income_tax = `AVERAGE CORPORATE TAX RT`,
    progressive_tax = PROGRESSIVE
  ) 

# fixing dc 
state_corporate_tax <- state_corporate_tax %>%
  mutate(
    state = str_replace_all(state, "Dist. of Columbia", "District of Columbia")
  ) %>%
  arrange(state,
          year) %>%
  filter(year != "1992")


# writing the csv file 
write_csv(state_corporate_tax,"Documentation/data/analysis_data/state_corporate_tax.csv")



# Cleaning State Minimum Wage Data

# Alabama 
al <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGAL.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Alabama")

# Alaska, had to add dates from a different file 
ak <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGAK.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Alaska",
         DATE = al$DATE)

# Arkansas 
ar <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGAR.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Arkansas")

# Arizona 
# need to add the federal minimum wage before 2007 (bind w alabama)
az <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGAZ.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Arizona") %>%
  arrange(DATE)

to_join <- anti_join(al, az,
                     by = "DATE") %>%
  mutate(state="Arizona")
# adding that data to az, changing 
az <- bind_rows(to_join,az)

# California 
ca <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGCA.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "California")

# Colorado 
co <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGCO.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  
  mutate(state = "Colorado")

# Connecticut 
ct <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGCT.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Connecticut")

# DC 
dc <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGDC.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "District of Columbia")

# Deleware 
de <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGDE.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Delaware")

# Florida 
fl <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGFL.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Florida")

to_join <- anti_join(al, fl,
                     by = "DATE") %>%
  mutate(state="Florida")
# adding that data to az, changing 
fl <- bind_rows(to_join,fl)

# Georgia - need to group by year
ga <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGGAM.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Georgia",
         year = year(DATE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(-year)

# Hawaii
hi <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGHI.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Hawaii")

# Idaho 
id <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGID.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Idaho")

# Illinois
il <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGIL.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Illinois")

# Indiana 
ind <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGIN.csv",
                col_types = cols(DATE = col_date()),
                col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Indiana")

# Iowa
ia <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGIA.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Iowa")

# Kansas 
ks <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGKS.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Kansas")

# Kentucky 
ky <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGKY.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Kentucky")

# Louisiana
la <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGLA.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Louisiana")

# Maine
me <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGME.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Maine")

# Maryland
md <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGMD.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Maryland")

# Massachusetts 
ma <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGMA.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Massachusetts")

# Michigan
mi <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGMI.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Michigan")

# Minnesota
mn <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGMN.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Minnesota")

# Mississippi
ms <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGMS.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Mississippi")

# Missouri
mo <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGMO.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Missouri")

# Montana
mt <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGMT.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Montana")

# Nebraska
ne <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGNE.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Nebraska")

# Nevada 
nv <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGNV.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Nevada")

# New Hampshire
nh <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGNH.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "New Hampshire")

# New Jersey 
nj <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGNJ.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "New Jersey")

# New Mexico
nm <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGNM.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "New Mexico")

# New York 
ny <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGNY.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "New York")

# North Carolina 
nc <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGNC.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "North Carolina")

# North Dakota
nd <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGND.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "North Dakota")

# Ohio
oh <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGOH.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Ohio")

# Oklahoma
ok <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGOK.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Oklahoma")

# Oregon
or <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGOR.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Oregon")

# Pennsylvania
pa <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGPA.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Pennsylvania")

# Rhode Island
ri <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGRI.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Rhode Island")

# South Carolina
sc <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGSC.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "South Carolina")

# South Dakota
sd <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGSD.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "South Dakota")

# Tennessee
tn <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGTN.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Tennessee")

# Texas 
tx <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGTX.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Texas")

# Utah
ut <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGUT.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Utah")

# Virginia
va <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGVA.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Virginia")

# Vermont
vt <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGVT.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Vermont")

# Washington
wa <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGWA.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Washington")

# West Virginia
wv <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGWV.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "West Virginia")

# Wisconsin
wi <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGWI.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Wisconsin")

# Wyoming 
wy <- read_csv("Documentation/data/original_data/minimum_wage_data/STTMINWGWY.csv",
               col_types = cols(DATE = col_date()),
               col_names = c("DATE", "minimum_wage", "state")) %>%
  slice(-1) %>%
  mutate(state = "Wyoming")

# binding the separate state datasets together 
state_minimum_wage <- bind_rows(ak,al,ar,az,
                                ca,co,ct,
                                dc,de,
                                fl,
                                ga,
                                hi,
                                ia,id,il,ind,
                                ks,ky,
                                la,
                                ma,md,me,mi,mn,mo,ms,mt,
                                nc,nd,ne,nh,nj,nm,nv,ny,
                                oh,ok,or,
                                pa,
                                ri,
                                sc,sd,
                                tn,tx,
                                ut,
                                va,vt,
                                wa,wi,wv,wy)

# only want the year in the date column
# changing date to year 
# filtering so only have years 1997-2023 
state_minimum_wage <- state_minimum_wage %>%
  mutate(
    DATE = year(state_minimum_wage$DATE)
  ) %>%
  rename(year = DATE) %>%
  filter(year >= 1997 & year <= 2023) %>%
  distinct(state, year, .keep_all = TRUE)

# writing the csv file 
write_csv(state_minimum_wage,"Documentation/data/analysis_data/state_minimum_wage.csv")




# State Population Data

Ak <- read_csv("Documentation/data/original_data/state_population_data/AKPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Alaska") %>%
  slice(-1)

Al <- read_csv("Documentation/data/original_data/state_population_data/ALPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Alabama") %>%
  slice(-1)

Ar <- read_csv("Documentation/data/original_data/state_population_data/ARPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Arkansas") %>%
  slice(-1)

Az <- read_csv("Documentation/data/original_data/state_population_data/AZPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Arizona") %>%
  slice(-1)

Ca <- read_csv("Documentation/data/original_data/state_population_data/CAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "California") %>%
  slice(-1)

Co <- read_csv("Documentation/data/original_data/state_population_data/COPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Colorado") %>%
  slice(-1)

Ct <- read_csv("Documentation/data/original_data/state_population_data/CTPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Connecticut") %>%
  slice(-1)

Dc <- read_csv("Documentation/data/original_data/state_population_data/DCPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "District of Columbia") %>%
  slice(-1)

De <- read_csv("Documentation/data/original_data/state_population_data/DEPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Delaware") %>%
  slice(-1)

Fl <- read_csv("Documentation/data/original_data/state_population_data/FLPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Florida") %>%
  slice(-1)

Ga <- read_csv("Documentation/data/original_data/state_population_data/GAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Georgia") %>%
  slice(-1)

Hi <- read_csv("Documentation/data/original_data/state_population_data/HIPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Hawaii") %>%
  slice(-1)

Ia <- read_csv("Documentation/data/original_data/state_population_data/IAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Iowa") %>%
  slice(-1)

Id <- read_csv("Documentation/data/original_data/state_population_data/IDPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Idaho") %>%
  slice(-1)

Il <- read_csv("Documentation/data/original_data/state_population_data/ILPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Illinois")

Ind <- read_csv("Documentation/data/original_data/state_population_data/INPOP.csv",
                col_types = cols(observation_date = col_date()),
                col_names = c("year", "population", "state")) %>%
  mutate(state = "Indiana") %>%
  slice(-1)

Ks <- read_csv("Documentation/data/original_data/state_population_data/KSPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Kansas") %>%
  slice(-1)

Ky <- read_csv("Documentation/data/original_data/state_population_data/KYPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Kentucky") %>%
  slice(-1)

La <- read_csv("Documentation/data/original_data/state_population_data/LAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Louisiana") %>%
  slice(-1)

Ma <- read_csv("Documentation/data/original_data/state_population_data/MAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Massachusetts") %>%
  slice(-1)

Md <- read_csv("Documentation/data/original_data/state_population_data/MDPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Maryland") %>%
  slice(-1)

Me <- read_csv("Documentation/data/original_data/state_population_data/MEPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Maine") %>%
  slice(-1)

Mi <- read_csv("Documentation/data/original_data/state_population_data/MIPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Michigan") %>%
  slice(-1)

Mn <- read_csv("Documentation/data/original_data/state_population_data/MNPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Minnesota") %>%
  slice(-1)

Mo <- read_csv("Documentation/data/original_data/state_population_data/MOPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Missouri") %>%
  slice(-1)

Ms <- read_csv("Documentation/data/original_data/state_population_data/MSPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Mississippi") %>%
  slice(-1)

Mt <- read_csv("Documentation/data/original_data/state_population_data/MTPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Montana") %>%
  slice(-1)

Nc <- read_csv("Documentation/data/original_data/state_population_data/NCPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "North Carolina") %>%
  slice(-1)

Nd <- read_csv("Documentation/data/original_data/state_population_data/NDPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "North Dakota") %>%
  slice(-1)

Ne <- read_csv("Documentation/data/original_data/state_population_data/NEPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Nebraska") %>%
  slice(-1)

Nh <- read_csv("Documentation/data/original_data/state_population_data/NHPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "New Hampshire") %>%
  slice(-1)

Nj <- read_csv("Documentation/data/original_data/state_population_data/NJPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "New Jersey") %>%
  slice(-1)

Nm <- read_csv("Documentation/data/original_data/state_population_data/NMPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "New Mexico") %>%
  slice(-1)

Nv <- read_csv("Documentation/data/original_data/state_population_data/NVPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Nevada") %>%
  slice(-1)

Ny <- read_csv("Documentation/data/original_data/state_population_data/NYPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "New York") %>%
  slice(-1)

Oh <- read_csv("Documentation/data/original_data/state_population_data/OHPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Ohio") %>%
  slice(-1)

Ok <- read_csv("Documentation/data/original_data/state_population_data/OKPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Oklahoma") %>%
  slice(-1)

Or <- read_csv("Documentation/data/original_data/state_population_data/ORPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Oregon") %>%
  slice(-1)

Pa <- read_csv("Documentation/data/original_data/state_population_data/PAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Pennsylvania") %>%
  slice(-1)

Ri <- read_csv("Documentation/data/original_data/state_population_data/RIPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Rhode Island") %>%
  slice(-1)

Sc <- read_csv("Documentation/data/original_data/state_population_data/SCPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "South Carolina") %>%
  slice(-1)

Sd <- read_csv("Documentation/data/original_data/state_population_data/SDPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "South Dakota") %>%
  slice(-1)

Tn <- read_csv("Documentation/data/original_data/state_population_data/TNPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Tennessee") %>%
  slice(-1)

Tx <- read_csv("Documentation/data/original_data/state_population_data/TXPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Texas") %>%
  slice(-1)

Ut <- read_csv("Documentation/data/original_data/state_population_data/UTPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Utah") %>%
  slice(-1)

Va <- read_csv("Documentation/data/original_data/state_population_data/VAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Virginia") %>%
  slice(-1)

Vt <- read_csv("Documentation/data/original_data/state_population_data/VTPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Vermont") %>%
  slice(-1)

Wa <- read_csv("Documentation/data/original_data/state_population_data/WAPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Washington") %>%
  slice(-1)

Wi <- read_csv("Documentation/data/original_data/state_population_data/WIPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Wisconsin") %>%
  slice(-1)

Wv <- read_csv("Documentation/data/original_data/state_population_data/WVPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "West Virginia") %>%
  slice(-1)

Wy <- read_csv("Documentation/data/original_data/state_population_data/WYPOP.csv",
               col_types = cols(observation_date = col_date()),
               col_names = c("year", "population", "state")) %>%
  mutate(state = "Wyoming") %>%
  slice(-1)

# binding the separate state datasets together 
state_population <- bind_rows(Ak,Al,Ar,Az,
                              Ca,Co,Ct,
                              Dc,De,
                              Fl,
                              Ga,
                              Hi,
                              Ia,Id,Il,Ind,
                              Ks,Ky,
                              La,
                              Ma,Md,Me,Mi,Mn,Mo,Ms,Mt,
                              Nc,Nd,Ne,Nh,Nj,Nm,Nv,Ny,
                              Oh,Ok,Or,
                              Pa,
                              Ri,
                              Sc,Sd,
                              Tn,Tx,
                              Ut,
                              Va,Vt,
                              Wa,Wi,Wv,Wy)

# only want the year in the date column
# changing date to year 
# filtering so only have years 1997-2023 
state_population$year <- as_date(state_population$year)

state_population <- state_population %>%
  mutate(
    year = year(state_population$year)
  ) %>%
  filter(year >= 1997 & year <= 2023)


# writing the csv file 
write_csv(state_population,"Documentation/data/analysis_data/state_population.csv")




# State Annual GDP Data
ak <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_AK_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))
  

al <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_AL_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ar <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_AR_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

az <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_AZ_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ca <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_CA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

co <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_CO_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ct <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_CT_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

dc <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_DC_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

de <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_DE_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

fl <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_FL_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ga <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_GA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

hi <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_HI_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ia <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_IA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

id <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_ID_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

il <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_IL_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))


ind <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_IN_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ks <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_KS_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ky <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_KY_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

la <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_LA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ma <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_MA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

md <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_MD_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

me <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_ME_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

mi <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_MI_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

mn <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_MN_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

mo <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_MO_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ms <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_MS_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

mt <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_MT_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

nc <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_NC_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

nd <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_ND_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ne <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_NE_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

nh <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_NH_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

nj <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_NJ_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

nm <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_NM_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

nv <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_NV_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ny <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_NY_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

oh <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_OH_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ok <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_OK_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

or <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_OR_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

pa <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_PA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ri <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_RI_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

sc <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_SC_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

sd <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_SD_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

tn <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_TN_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

tx <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_TX_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

ut <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_UT_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

va <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_VA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

vt <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_VT_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

wa <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_WA_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

wi <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_WI_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

wv <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_WV_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

wy <- read_csv("Documentation/data/original_data/state_gdp_data/SAGDP2N_WY_1997_2023.csv") %>%
  filter(Description == "All industry total") %>%
  select(GeoName,
         9:ncol(.)) %>%
  mutate(across(2:ncol(.), as.numeric))

# binding all state gdp together 
state_gdp <- bind_rows(ak,al,ar,az,
                                ca,co,ct,
                                dc,de,
                                fl,
                                ga,
                                hi,
                                ia,id,il,ind,
                                ks,ky,
                                la,
                                ma,md,me,mi,mn,mo,ms,mt,
                                nc,nd,ne,nh,nj,nm,nv,ny,
                                oh,ok,or,
                                pa,
                                ri,
                                sc,sd,
                                tn,tx,
                                ut,
                                va,vt,
                                wa,wi,wv,wy) %>%
  pivot_longer(.,
               # making into panel data 
               cols = `1997`:`2023`,
               names_to = "year",
               values_to = "gdp") %>%
  mutate(year = as.numeric(year)) %>%
  rename(state = GeoName) # renaming to 'state' column 


# saving the dataset as a csv file 
write_csv(state_gdp,"Documentation/data/analysis_data/state_gdp.csv")


# Joining all datasets together 
full_project_data <- left_join(
  x = state_minimum_wage,
  y = state_corporate_tax,
  by = c("state", "year"))

full_project_data <- full_project_data %>%
  left_join(x = .,
            y = state_population,
            by = c("state", "year"))

full_project_data <- full_project_data %>%
  left_join(x = .,
            y = state_unemployment,
            by = c("state", "year"))

full_project_data <- full_project_data %>%
  left_join(x = .,
            y = state_gdp,
            by = c("state", "year"))

# writing the csv file 
write_csv(full_project_data,"Documentation/data/analysis_data/full_project_data.csv")





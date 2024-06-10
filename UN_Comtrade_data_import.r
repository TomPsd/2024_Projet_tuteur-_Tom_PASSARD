##### PARTIE CHARGEMENT DES PACKAGES NECESSAIRES A L'IMPORTATION#####
# UN Comtrade data import

# Install pacman package if not installed
if (!("pacman" %in% rownames(installed.packages()))) {
  install.packages("pacman")
}

# Install and load used packages
pacman::p_load(reticulate, rstudioapi)

#####LIAISON DU PACKAGE R AVEC ANACONDA#####
# Indicate anaconda environment
use_condaenv( "C:/Users/Stagiaire/anaconda3/python.exe" )
# More info on how to find anaconda environment path here:
# https://docs.anaconda.com/free/working-with-conda/configurations/python-path/

#####CONFIGURATION ESPACE DE TRAVAIL#####
# Set up working directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

##### TELECHARGEMENT DES DONNEES DU PRODUIT 440131#####
#Remarque : les données pour ce produit ne sont répertoriées que depuis 2012.
# Run python script for data download
for (file in list.files(path = dir, pattern = ".py")){
  source_python(file)
}

# Get API key
apikey <- readLines(list.files(path = dir, pattern = ".txt"))
# Set up parameters
years <- 2000 : 2020
cmd_code <- paste(sort(c( "440131")),
                  sep = "",
                  collapse = ",")
flow_code <- paste(sort(c("M","X")),
                   sep = "",
                   collapse = ",")
# Store data into a data.frame
annual_trade_list <- list()
for (i in seq_along(years)) {
  annual_trade_list[[i]] <-get_UN_Comtrade_data(apikey,
                                                 year = as.character(years[i]),
                                                 cmd = cmd_code,
                                                 flow = flow_code)
}
trade_data <- do.call(rbind, annual_trade_list)
###VERIFICATION DU JEU DE DONNEES TELECHARGE###
# Check if data have been correctly downloaded
print(paste0("Years covered: ",
             paste(unique(trade_data$period), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(trade_data$cmdCode), sep = "", collapse = ", ")))
print(paste0("Flows considered: ",
             paste(unique(trade_data$flowDesc), sep = "", collapse = ", ")))
###EXPORTATION DES JEUX DE DONNEES ET VERIFICATIONS###
# Export data as a csv file
write.csv(trade_data,
          "./raw_data/data_440131.csv",
          row.names = FALSE)

all(dim(data) == dim(trade_data)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

#####CITATION DES PACKAGES UTILISES#####
citation("comtradeapicall")
citation("reticulate")
citation("rstudioapi")
citation("ggplot2")
citation("dplyr")
citation("tidyr")
#####IMPORTATION DU JEU DE DONNEES 4418#####

# Indicate anaconda environment
use_condaenv( "C:/Users/Stagiaire/anaconda3/python.exe" )
# More info on how to find anaconda environment path here:
# https://docs.anaconda.com/free/working-with-conda/configurations/python-path/

# Set up working directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# Run python script for data download
for (file in list.files(path = dir, pattern = ".py")){
  source_python(file)
}

# Get API key
apikey <- readLines(list.files(path = dir, pattern = ".txt"))
# Set up parameters
years <- 2000 : 2020
cmd_code <- paste(sort(c( "4418")),
                  sep = "",
                  collapse = ",")
flow_code <- paste(sort(c("M","X")),
                   sep = "",
                   collapse = ",")
# Store data into a data.frame
annual_trade_list <- list()
for (i in seq_along(years)) {
  annual_trade_list[[i]] <-get_UN_Comtrade_data(apikey,
                                                year = as.character(years[i]),
                                                cmd = cmd_code,
                                                flow = flow_code)
}
trade_data_2 <- do.call(rbind, annual_trade_list)
###VERIFICATION DU JEU DE DONNEES TELECHARGE###
# Check if data have been correctly downloaded
print(paste0("Years covered: ",
             paste(unique(trade_data_2$period), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(trade_data_2$cmdCode), sep = "", collapse = ", ")))
print(paste0("Flows considered: ",
             paste(unique(trade_data_2$flowDesc), sep = "", collapse = ", ")))
###EXPORTATION DES JEUX DE DONNEES ET VERIFICATIONS###
# Export data as a csv file
write.csv(trade_data_2,
          "./raw_data/data_4418.csv",
          row.names = FALSE)

all(dim(data2) == dim(trade_data_2)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

#####IMPORTATION DU JEU DE DONNNEES 4412#####

# Indicate anaconda environment
use_condaenv( "C:/Users/Stagiaire/anaconda3/python.exe" )
# More info on how to find anaconda environment path here:
# https://docs.anaconda.com/free/working-with-conda/configurations/python-path/

# Set up working directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# Run python script for data download
for (file in list.files(path = dir, pattern = ".py")){
  source_python(file)
}

# Get API key
apikey <- readLines(list.files(path = dir, pattern = ".txt"))
# Set up parameters
years <- 2000 : 2020
cmd_code <- paste(sort(c( "4412")),
                  sep = "",
                  collapse = ",")
flow_code <- paste(sort(c("M","X")),
                   sep = "",
                   collapse = ",")
# Store data into a data.frame
annual_trade_list <- list()
for (i in seq_along(years)) {
  annual_trade_list[[i]] <-get_UN_Comtrade_data(apikey,
                                                year = as.character(years[i]),
                                                cmd = cmd_code,
                                                flow = flow_code)
}
trade_data_3 <- do.call(rbind, annual_trade_list)
###VERIFICATION DU JEU DE DONNEES TELECHARGE###
# Check if data have been correctly downloaded
print(paste0("Years covered: ",
             paste(unique(trade_data_3$period), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(trade_data_3$cmdCode), sep = "", collapse = ", ")))
print(paste0("Flows considered: ",
             paste(unique(trade_data_3$flowDesc), sep = "", collapse = ", ")))
###EXPORTATION DES JEUX DE DONNEES ET VERIFICATIONS###
# Export data as a csv file
write.csv(trade_data_3,
          "./raw_data/data_4412.csv",
          row.names = FALSE)

all(dim(data3) == dim(trade_data_3)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

#####IMPORTATION DU JEU DE DONNEES 4410#####

# Indicate anaconda environment
use_condaenv( "C:/Users/Stagiaire/anaconda3/python.exe" )
# More info on how to find anaconda environment path here:
# https://docs.anaconda.com/free/working-with-conda/configurations/python-path/

# Set up working directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# Run python script for data download
for (file in list.files(path = dir, pattern = ".py")){
  source_python(file)
}

# Get API key
apikey <- readLines(list.files(path = dir, pattern = ".txt"))
# Set up parameters
years <- 2000 : 2020
cmd_code <- paste(sort(c( "4410")),
                  sep = "",
                  collapse = ",")
flow_code <- paste(sort(c("M","X")),
                   sep = "",
                   collapse = ",")
# Store data into a data.frame
annual_trade_list <- list()
for (i in seq_along(years)) {
  annual_trade_list[[i]] <-get_UN_Comtrade_data(apikey,
                                                year = as.character(years[i]),
                                                cmd = cmd_code,
                                                flow = flow_code)
}
trade_data_4 <- do.call(rbind, annual_trade_list)
###VERIFICATION DU JEU DE DONNEES TELECHARGE###
# Check if data have been correctly downloaded
print(paste0("Years covered: ",
             paste(unique(trade_data_4$period), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(trade_data_4$cmdCode), sep = "", collapse = ", ")))
print(paste0("Flows considered: ",
             paste(unique(trade_data_4$flowDesc), sep = "", collapse = ", ")))
###EXPORTATION DES JEUX DE DONNEES ET VERIFICATIONS###
# Export data as a csv file
write.csv(trade_data_4,
          "./raw_data/data_4410.csv",
          row.names = FALSE)

all(dim(data4) == dim(trade_data_4)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

#####IMPORTATION DU JEU DE DONNEES 4407#####

# Indicate anaconda environment
use_condaenv( "C:/Users/Stagiaire/anaconda3/python.exe" )
# More info on how to find anaconda environment path here:
# https://docs.anaconda.com/free/working-with-conda/configurations/python-path/

# Set up working directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# Run python script for data download
for (file in list.files(path = dir, pattern = ".py")){
  source_python(file)
}

# Get API key
apikey <- readLines(list.files(path = dir, pattern = ".txt"))
# Set up parameters
years <- 2000 : 2020
cmd_code <- paste(sort(c( "4407")),
                  sep = "",
                  collapse = ",")
flow_code <- paste(sort(c("M","X")),
                   sep = "",
                   collapse = ",")
# Store data into a data.frame
annual_trade_list <- list()
for (i in seq_along(years)) {
  annual_trade_list[[i]] <-get_UN_Comtrade_data(apikey,
                                                year = as.character(years[i]),
                                                cmd = cmd_code,
                                                flow = flow_code)
}
trade_data_5 <- do.call(rbind, annual_trade_list)
###VERIFICATION DU JEU DE DONNEES TELECHARGE###
# Check if data have been correctly downloaded
print(paste0("Years covered: ",
             paste(unique(trade_data_5$period), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(trade_data_5$cmdCode), sep = "", collapse = ", ")))
print(paste0("Flows considered: ",
             paste(unique(trade_data_5$flowDesc), sep = "", collapse = ", ")))
###EXPORTATION DES JEUX DE DONNEES ET VERIFICATIONS###
# Export data as a csv file
write.csv(trade_data_5,
          "./raw_data/data_4407.csv",
          row.names = FALSE)

all(dim(data5) == dim(trade_data_5)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

#####RAPPEL DES JEUX DE DONNEES#####
data = read.csv2("./raw_data/data_440131.csv",sep = ",")
data2 = read.csv2("./raw_data/data_4418.csv",sep = ",")
data3 = read.csv2("./raw_data/data_4412.csv",sep = ",")
data4 = read.csv2("./raw_data/data_4410.csv",sep = ",")
data5 = read.csv2("./raw_data/data_4407.csv",sep = ",")

#####NETTOYAGE DES DONNEES#####
toMatch = c('\\d','XX','_X')

data = data[!(grepl(paste(toMatch,collapse="|"), data$reporterISO)) &
       !(grepl(paste(toMatch,collapse="|"), data$partnerISO)),]

data2 = data2[!(grepl(paste(toMatch,collapse="|"), data2$reporterISO)) &
       !(grepl(paste(toMatch,collapse="|"), data2$partnerISO)),]

data3 = data3[!(grepl(paste(toMatch,collapse="|"), data3$reporterISO)) &
       !(grepl(paste(toMatch,collapse="|"), data3$partnerISO)),]

data4 = data4[!(grepl(paste(toMatch,collapse="|"), data4$reporterISO)) &
       !(grepl(paste(toMatch,collapse="|"), data4$partnerISO)),]

data5 = data5[!(grepl(paste(toMatch,collapse="|"), data5$reporterISO)) &
       !(grepl(paste(toMatch,collapse="|"), data5$partnerISO)),]


#####RAPPEL DES TABLEAUX IMPORT/EXPORT#####
export_440131 = data[(data$flowCode == "X"),]
import_440131 = data[(data$flowCode == "M"),]

export_4418 = data2[(data2$flowCode == "X"),]
import_4418 = data2[(data2$flowCode == "M"),]

export_4412 = data3[(data3$flowCode == "X"),]
import_4412 = data3[(data3$flowCode == "M"),]

export_4410 = data4[(data4$flowCode == "X"),]
import_4410 = data4[(data4$flowCode == "M"),]

export_4407 = data5[(data5$flowCode == "X"),]
import_4407 = data5[(data5$flowCode == "M"),]

#####RAPPEL DES DONNEES MIROIR#####
mirror_flow_440131 = merge(x= export_440131[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                           y= import_440131[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                           by.x=c("cmdCode", "period", "reporterDesc", "partnerDesc"),
                           by.y=c("cmdCode", "period", "partnerDesc", "reporterDesc"),
                           all.x = TRUE, all.y = TRUE)
sum(is.na(mirror_flow_440131))

mirror_flow_4418 = merge(x= export_4418[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         y= import_4418[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         by.x=c("cmdCode", "period", "reporterDesc", "partnerDesc"),
                         by.y=c("cmdCode", "period", "partnerDesc", "reporterDesc"),
                         all.x = TRUE, all.y = TRUE)
sum(is.na(mirror_flow_4418))

mirror_flow_4412 = merge(x= export_4412[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         y= import_4412[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         by.x=c("cmdCode", "period", "reporterDesc", "partnerDesc"),
                         by.y=c("cmdCode", "period", "partnerDesc", "reporterDesc"),
                         all.x = TRUE, all.y = TRUE)
sum(is.na(mirror_flow_4412))

mirror_flow_4410 = merge(x= export_4410[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         y= import_4410[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         by.x=c("cmdCode", "period", "reporterDesc", "partnerDesc"),
                         by.y=c("cmdCode", "period", "partnerDesc", "reporterDesc"),
                         all.x = TRUE, all.y = TRUE)
sum(is.na(mirror_flow_4410))

mirror_flow_4407 = merge(x= export_4407[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         y= import_4407[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         by.x=c("cmdCode", "period", "reporterDesc", "partnerDesc"),
                         by.y=c("cmdCode", "period", "partnerDesc", "reporterDesc"),
                         all.x = TRUE, all.y = TRUE)
sum(is.na(mirror_flow_4407))

#####CREATION D'UN VECTEUR UE27 REGROUPANT LES 27 PAYS MEMBRES DE L'UE#####
UE_27 = c('Austria','Belgium','Bulgaria','Croatia','Cyprus',
          'Czech Republic','Denmark','Estonia','Finland','France',
          'Germany','Greece','Hungary','Ireland','Italy','Latvia',
          'Lithuania','Luxembourg','Malta','Netherlands','Poland',
          'Portugal','Romania','Slovakia','Slovenia','Spain','Sweden')
pattern <- paste(UE_27, collapse="|")

#####CREATION DES DIFFERENTS SOUS-ENSEMBLES POUR ANALYSE#####

##PRODUIT 440131##

#EXPORTATION UE27 DE 440131#
EXPORT_UE27_440131 <- mirror_flow_440131[grepl(pattern, mirror_flow_440131$reporterDesc)&
                                           !(grepl(pattern, mirror_flow_440131$partnerDesc)),]
EXPORT_UE27_440131$primaryValue.x<-as.numeric(EXPORT_UE27_440131$primaryValue.x)
EXPORT_UE27_440131$primaryValue.y<-as.numeric(EXPORT_UE27_440131$primaryValue.y)
EXPORT_UE27_440131$mediane = (EXPORT_UE27_440131$primaryValue.x+EXPORT_UE27_440131$primaryValue.y)/2
str(EXPORT_UE27_440131)
#IMPORTATION UE27 DE 440131#
IMPORT_UE27_440131 <- mirror_flow_440131[!(grepl(pattern, mirror_flow_440131$reporterDesc))&
                                           grepl(pattern, mirror_flow_440131$partnerDesc),]
IMPORT_UE27_440131$primaryValue.x<-as.numeric(IMPORT_UE27_440131$primaryValue.x)
IMPORT_UE27_440131$primaryValue.y<-as.numeric(IMPORT_UE27_440131$primaryValue.y)
IMPORT_UE27_440131$mediane = (IMPORT_UE27_440131$primaryValue.x+IMPORT_UE27_440131$primaryValue.y)/2
str(IMPORT_UE27_440131)
##PRODUIT 4418##

#EXPORTATION UE27 DE 4418#
EXPORT_UE27_4418 <- mirror_flow_4418[grepl(pattern, mirror_flow_4418$reporterDesc)&
                                           !(grepl(pattern, mirror_flow_4418$partnerDesc)),]
EXPORT_UE27_4418$primaryValue.x<-as.numeric(EXPORT_UE27_4418$primaryValue.x)
EXPORT_UE27_4418$primaryValue.y<-as.numeric(EXPORT_UE27_4418$primaryValue.y)
str(EXPORT_UE27_4418)
#IMPORTATION UE27 DE 4418#
IMPORT_UE27_4418 <- mirror_flow_4418[!(grepl(pattern, mirror_flow_4418$reporterDesc))&
                                           grepl(pattern, mirror_flow_4418$partnerDesc),]
IMPORT_UE27_4418$primaryValue.x=as.numeric(IMPORT_UE27_4418$primaryValue.x)
IMPORT_UE27_4418$primaryValue.y=as.numeric((IMPORT_UE27_4418$primaryValue.y))
str(IMPORT_UE27_4418)

##PRODUIT 4412##

#EXPORTATION UE27 DE 4412#
EXPORT_UE27_4412 <- mirror_flow_4412[grepl(pattern, mirror_flow_4412$reporterDesc)&
                                       !(grepl(pattern, mirror_flow_4412$partnerDesc)),]
EXPORT_UE27_4412$primaryValue.x=as.numeric(EXPORT_UE27_4412$primaryValue.x)
EXPORT_UE27_4412$primaryValue.y=as.numeric(EXPORT_UE27_4412$primaryValue.y)
str(EXPORT_UE27_4412)
#IMPORTATION UE27 DE 4412#
IMPORT_UE27_4412 <- mirror_flow_4412[!(grepl(pattern, mirror_flow_4412$reporterDesc))&
                                       grepl(pattern, mirror_flow_4412$partnerDesc),]
IMPORT_UE27_4412$primaryValue.x=as.numeric(IMPORT_UE27_4412$primaryValue.x)
IMPORT_UE27_4412$primaryValue.y=as.numeric(IMPORT_UE27_4412$primaryValue.y)
str(IMPORT_UE27_4412)
##PRODUIT 4410##

#EXPORTATION UE27 DE 4410#
EXPORT_UE27_4410 <- mirror_flow_4410[grepl(pattern, mirror_flow_4410$reporterDesc)&
                                       !(grepl(pattern, mirror_flow_4410$partnerDesc)),]
EXPORT_UE27_4410$primaryValue.x=as.numeric(EXPORT_UE27_4410$primaryValue.x)
EXPORT_UE27_4410$primaryValue.y=as.numeric(EXPORT_UE27_4410$primaryValue.y)
str(EXPORT_UE27_4410)
#IMPORTATION UE27 DE 4410#
IMPORT_UE27_4410 <- mirror_flow_4410[!(grepl(pattern, mirror_flow_4410$reporterDesc))&
                                       grepl(pattern, mirror_flow_4410$partnerDesc),]
IMPORT_UE27_4410$primaryValue.x=as.numeric(IMPORT_UE27_4410$primaryValue.x)
IMPORT_UE27_4410$primaryValue.y=as.numeric(IMPORT_UE27_4410$primaryValue.y)
str(IMPORT_UE27_4410)
#PRODUIT 4407#
#EXPORTATION UE27 DE 4407#
EXPORT_UE27_4407 <- mirror_flow_4407[grepl(pattern, mirror_flow_4407$reporterDesc)&
                                       !(grepl(pattern, mirror_flow_4407$partnerDesc)),]
EXPORT_UE27_4407$primaryValue.x=as.numeric(EXPORT_UE27_4407$primaryValue.x)
EXPORT_UE27_4407$primaryValue.y=as.numeric(EXPORT_UE27_4407$primaryValue.y)
str(EXPORT_UE27_4407)
#IMPORTATION UE27 DE 4407#
IMPORT_UE27_4407<- mirror_flow_4407[!(grepl(pattern, mirror_flow_4407$reporterDesc))&
                                       grepl(pattern, mirror_flow_4407$partnerDesc),]
IMPORT_UE27_4407$primaryValue.x=as.numeric(IMPORT_UE27_4407$primaryValue.x)
IMPORT_UE27_4407$primaryValue.y=as.numeric(IMPORT_UE27_4407$primaryValue.y)
str(IMPORT_UE27_4407)


######ANALYSE DES GROUPES DE DONNEES#####

###Installation du package dlyr pour proceder aux analyses de donnees standard
###(sumamrise, arrange et autres manipulations qui permettent de montrer les 
###principaux importateurs et exportateurs)###

install.packages('dplyr')
library('dplyr')

##PRODUIT 440131##

#EXPORT#
yr_pays_EXPORT_UE27_440131 <- EXPORT_UE27_440131 %>%
  group_by(period, partnerDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result1 <- yr_pays_EXPORT_UE27_440131 %>%
  group_by(period) %>%
  mutate(proportion = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_EXPORT_UE27_440131$proportion_y <- result1$proportion

result2 <- yr_pays_EXPORT_UE27_440131 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_EXPORT_UE27_440131$proportion_x <- result2$proportion_x

TOTAL_EXPORT_UE27_440131<- yr_pays_EXPORT_UE27_440131%>%
  group_by(period)%>%
  summarise(IMPORT_HORS_UE = sum(totalValue_y),
            EXPORT_UE = sum(totalValue_x))%>%
  arrange(period)

top_EXPORT_UE27_440131 <- yr_pays_EXPORT_UE27_440131 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_EXPORT_UE27_440131_2 <- yr_pays_EXPORT_UE27_440131 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#IMPORT#

yr_pays_IMPORT_UE27_440131 <- IMPORT_UE27_440131 %>%
  group_by(period, reporterDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result3 <- yr_pays_IMPORT_UE27_440131 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_IMPORT_UE27_440131$proportion_y <- result3$proportion_y

result4 <- yr_pays_IMPORT_UE27_440131 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_IMPORT_UE27_440131$proportion_x <- result4$proportion_x

TOTAL_IMPORT_UE27_440131<- yr_pays_IMPORT_UE27_440131%>%
  group_by(period)%>%
  summarise(IMPORT_UE = sum(totalValue_y),
            EXPORT_HORS_UE = sum(totalValue_x))%>%
  arrange(period)

top_IMPORT_UE27_440131 <- yr_pays_IMPORT_UE27_440131 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_IMPORT_UE27_440131_2 <- yr_pays_IMPORT_UE27_440131 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#VERIFICATION DU SOLDE DE BALANCE COMMERCIALE#
solde_IMPORT <- TOTAL_EXPORT_UE27_440131$IMPORT_HORS_UE - TOTAL_IMPORT_UE27_440131$IMPORT_UE
solde_EXPORT <- TOTAL_EXPORT_UE27_440131$EXPORT_UE - TOTAL_IMPORT_UE27_440131$EXPORT_HORS_UE
balance_commerciale_440131 <- data.frame(TOTAL_EXPORT_UE27_440131$period,solde_IMPORT,solde_EXPORT)

##PRODUIT 4418##
#EXPORT#
yr_pays_EXPORT_UE27_4418 <- EXPORT_UE27_4418 %>%
  group_by(period, partnerDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result5 <- yr_pays_EXPORT_UE27_4418 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4418$proportion_y <- result5$proportion_y

result6 <- yr_pays_EXPORT_UE27_4418 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4418$proportion_x <- result6$proportion_x

TOTAL_EXPORT_UE27_4418<- yr_pays_EXPORT_UE27_4418%>%
  group_by(period)%>%
  summarise(IMPORT_HORS_UE = sum(totalValue_y),
            EXPORT_UE = sum(totalValue_x))%>%
  arrange(period)

top_EXPORT_UE27_4418 <- yr_pays_EXPORT_UE27_4418 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_EXPORT_UE27_4418_2 <- yr_pays_EXPORT_UE27_4418 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#IMPORT#
yr_pays_IMPORT_UE27_4418 <- IMPORT_UE27_4418 %>%
  group_by(period, reporterDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result7 <- yr_pays_IMPORT_UE27_4418 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4418$proportion_y <- result7$proportion_y

result8 <- yr_pays_IMPORT_UE27_4418 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4418$proportion_x <- result8$proportion_x

TOTAL_IMPORT_UE27_4418<- yr_pays_IMPORT_UE27_4418%>%
  group_by(period)%>%
  summarise(IMPORT_UE = sum(totalValue_y),
            EXPORT_HORS_UE = sum(totalValue_x))%>%
  arrange(period)

top_IMPORT_UE27_4418 <- yr_pays_IMPORT_UE27_4418 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_IMPORT_UE27_4418_2 <- yr_pays_IMPORT_UE27_4418 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#VERIFICATION DU SOLDE DE BALANCE COMMERCIALE#
solde_decla_IMPORT_4418 <- TOTAL_EXPORT_UE27_4418$IMPORT_HORS_UE - TOTAL_IMPORT_UE27_4418$IMPORT_UE
solde_decla_EXPORT_4418 <- TOTAL_EXPORT_UE27_4418$EXPORT_UE - TOTAL_IMPORT_UE27_4418$EXPORT_HORS_UE
balance_commerciale_4418 <- data.frame(TOTAL_EXPORT_UE27_4418$period,solde_decla_IMPORT_4418,solde_decla_EXPORT_4418)

##PRODUIT 4412##
#EXPORT#
yr_pays_EXPORT_UE27_4412 <- EXPORT_UE27_4412 %>%
  group_by(period, partnerDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result9 <- yr_pays_EXPORT_UE27_4412 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4412$proportion_y <- result9$proportion_y

result10 <- yr_pays_EXPORT_UE27_4412 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4412$proportion_x <- result10$proportion_x

TOTAL_EXPORT_UE27_4412<- yr_pays_EXPORT_UE27_4412%>%
  group_by(period)%>%
  summarise(IMPORT_HORS_UE = sum(totalValue_y),
            EXPORT_UE = sum(totalValue_x))%>%
  arrange(period)

top_EXPORT_UE27_4412 <- yr_pays_EXPORT_UE27_4412 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_EXPORT_UE27_4412_2 <- yr_pays_EXPORT_UE27_4412 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#IMPORT#
yr_pays_IMPORT_UE27_4412 <- IMPORT_UE27_4412 %>%
  group_by(period, reporterDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result11 <- yr_pays_IMPORT_UE27_4412 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4412$proportion_y <- result11$proportion_y

result12 <- yr_pays_IMPORT_UE27_4412 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4412$proportion_x <- result12$proportion_x

TOTAL_IMPORT_UE27_4412<- yr_pays_IMPORT_UE27_4412%>%
  group_by(period)%>%
  summarise(IMPORT_UE = sum(totalValue_y),
            EXPORT_HORS_UE = sum(totalValue_x))%>%
  arrange(period)

top_IMPORT_UE27_4412 <- yr_pays_IMPORT_UE27_4412 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_IMPORT_UE27_4412_2 <- yr_pays_IMPORT_UE27_4412 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#VERIFICATION DU SOLDE DE BALANCE COMMERCIALE#
solde_IMPORT_4412 <- TOTAL_EXPORT_UE27_4412$IMPORT_HORS_UE - TOTAL_IMPORT_UE27_4412$IMPORT_UE
solde_EXPORT_4412 <- TOTAL_EXPORT_UE27_4412$EXPORT_UE - TOTAL_IMPORT_UE27_4412$EXPORT_HORS_UE
balance_commerciale_4412 <- data.frame(TOTAL_EXPORT_UE27_4412$period,solde_IMPORT_4412,solde_EXPORT_4412)

##PRODUIT 4410##
#EXPORT#
yr_pays_EXPORT_UE27_4410 <- EXPORT_UE27_4410 %>%
  group_by(period, partnerDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result13 <- yr_pays_EXPORT_UE27_4410 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4410$proportion_y <- result13$proportion_y

result14 <- yr_pays_EXPORT_UE27_4410 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4410$proportion_x <- result14$proportion_x

TOTAL_EXPORT_UE27_4410<- yr_pays_EXPORT_UE27_4410%>%
  group_by(period)%>%
  summarise(IMPORT_HORS_UE = sum(totalValue_y),
            EXPORT_UE = sum(totalValue_x))%>%
  arrange(period)

top_EXPORT_UE27_4410 <- yr_pays_EXPORT_UE27_4410 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_EXPORT_UE27_4410_2 <- yr_pays_EXPORT_UE27_4410 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#IMPORT#
yr_pays_IMPORT_UE27_4410 <- IMPORT_UE27_4410 %>%
  group_by(period, reporterDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result15 <- yr_pays_IMPORT_UE27_4410 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4410$proportion_y <- result15$proportion_y

result16 <- yr_pays_IMPORT_UE27_4410 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4410$proportion_x <- result16$proportion_x

TOTAL_IMPORT_UE27_4410<- yr_pays_IMPORT_UE27_4410%>%
  group_by(period)%>%
  summarise(IMPORT_UE = sum(totalValue_y),
            EXPORT_HORS_UE = sum(totalValue_x))%>%
  arrange(period)

top_IMPORT_UE27_4410 <- yr_pays_IMPORT_UE27_4410 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_IMPORT_UE27_4410_2 <- yr_pays_IMPORT_UE27_4410 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#VERIFICATION DU SOLDE DE BALANCE COMMERCIALE#
solde_IMPORT_4410 <- TOTAL_EXPORT_UE27_4410$IMPORT_HORS_UE - TOTAL_IMPORT_UE27_4410$IMPORT_UE
solde_EXPORT_4410 <- TOTAL_EXPORT_UE27_4410$EXPORT_UE - TOTAL_IMPORT_UE27_4410$EXPORT_HORS_UE
balance_commerciale_4410 <- data.frame(TOTAL_EXPORT_UE27_4410$period,solde_IMPORT_4410,solde_EXPORT_4410)

##PRODUIT 4407##
#EXPORT#
yr_pays_EXPORT_UE27_4407 <- EXPORT_UE27_4407 %>%
  group_by(period, partnerDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result17 <- yr_pays_EXPORT_UE27_4407 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4407$proportion_y <- result17$proportion_y

result18 <- yr_pays_EXPORT_UE27_4407 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_EXPORT_UE27_4407$proportion_x <- result18$proportion_x

TOTAL_EXPORT_UE27_4407<- yr_pays_EXPORT_UE27_4407%>%
  group_by(period)%>%
  summarise(IMPORT_HORS_UE = sum(totalValue_y),
            EXPORT_UE = sum(totalValue_x))%>%
  arrange(period)

top_EXPORT_UE27_4407 <- yr_pays_EXPORT_UE27_4407 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_EXPORT_UE27_4407_2 <- yr_pays_EXPORT_UE27_4407 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#IMPORT#
yr_pays_IMPORT_UE27_4407 <- IMPORT_UE27_4407 %>%
  group_by(period, reporterDesc) %>%
  summarise(totalValue_x = sum(primaryValue.x, na.rm = TRUE),
            totalValue_y = sum(primaryValue.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(period,desc(totalValue_y))

result19 <- yr_pays_IMPORT_UE27_4407 %>%
  group_by(period) %>%
  mutate(proportion_y = totalValue_y / sum(totalValue_y)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4407$proportion_y <- result19$proportion_y

result20 <- yr_pays_IMPORT_UE27_4407 %>%
  group_by(period) %>%
  mutate(proportion_x = totalValue_x / sum(totalValue_x)) %>%
  ungroup()
yr_pays_IMPORT_UE27_4407$proportion_x <- result20$proportion_x

TOTAL_IMPORT_UE27_4407<- yr_pays_IMPORT_UE27_4407%>%
  group_by(period)%>%
  summarise(IMPORT_UE = sum(totalValue_y),
            EXPORT_HORS_UE = sum(totalValue_x))%>%
  arrange(period)

top_IMPORT_UE27_4407 <- yr_pays_IMPORT_UE27_4407 %>%
  group_by(period) %>%
  slice_max(proportion_y, n = 5) %>%
  ungroup()

top_IMPORT_UE27_4407_2 <- yr_pays_IMPORT_UE27_4407 %>%
  group_by(period) %>%
  slice_max(proportion_x, n = 5) %>%
  ungroup()

#VERIFICATION DU SOLDE DE BALANCE COMMERCIALE#
solde_IMPORT_4407 <- TOTAL_EXPORT_UE27_4407$IMPORT_HORS_UE - TOTAL_IMPORT_UE27_4407$IMPORT_UE
solde_EXPORT_4407 <- TOTAL_EXPORT_UE27_4407$EXPORT_UE - TOTAL_IMPORT_UE27_4407$EXPORT_HORS_UE
balance_commerciale_4407 <- data.frame(TOTAL_EXPORT_UE27_4407$period,solde_IMPORT_4407,solde_EXPORT_4407)

balance_commerciale_UE27<- EXPORT_UE27_440131%>%
  group_by(period)%>%
  summarise(declaration_export = sum(primaryValue.x, na.rm = TRUE),
            declaration_import= sum(primaryValue.y, na.rm = TRUE), .groups = 'drop')
#####AFFICHAGE GRAPHIQUE DES DONNEES UN COMTRADE#####
install.packages("ggplot2")
library("ggplot2")
install.packages("tidyr")
library(tidyr)
#####SUIVI DE LA VALEUR DES MARCHANDISES AU COURS DU TEMPS#####
###permet de visualiser toutes les évolutions des produits en même temps mais pas conserver
import_export_440131_4407_4410_4412_4418<-TOTAL_EXPORT_UE27_440131 %>%
  full_join(TOTAL_IMPORT_UE27_440131, by = "period") %>%
  full_join(TOTAL_EXPORT_UE27_4407, by = "period") %>%
  full_join(TOTAL_IMPORT_UE27_4407, by = "period") %>%
  full_join(TOTAL_EXPORT_UE27_4410, by = "period") %>%
  full_join(TOTAL_IMPORT_UE27_4410, by = "period") %>%
  full_join(TOTAL_EXPORT_UE27_4412, by = "period") %>%
  full_join(TOTAL_IMPORT_UE27_4412, by = "period") %>%
  full_join(TOTAL_EXPORT_UE27_4418, by = "period") %>%
  full_join(TOTAL_IMPORT_UE27_4418, by = "period")
import_export_440131_4407_4410_4412_4418<- data.frame("period" = import_export_440131_4407_4410_4412_4418$period,
                                                      "Import hors UE 440131" = import_export_440131_4407_4410_4412_4418$IMPORT_HORS_UE.x,
                                                      "Export UE 440131" = import_export_440131_4407_4410_4412_4418$EXPORT_UE.x,
                                                      "Import UE 440131" = import_export_440131_4407_4410_4412_4418$IMPORT_UE.x,
                                                      "Export hors UE 440131" = import_export_440131_4407_4410_4412_4418$EXPORT_HORS_UE.x,
                                                      "Import hors UE 4407" = import_export_440131_4407_4410_4412_4418$IMPORT_HORS_UE.y,
                                                      "Export UE 4407" = import_export_440131_4407_4410_4412_4418$EXPORT_UE.y,
                                                      "Import UE 4407" = import_export_440131_4407_4410_4412_4418$IMPORT_UE.y,
                                                      "Export hors UE 4407" = import_export_440131_4407_4410_4412_4418$EXPORT_HORS_UE.y,
                                                      "Import hors UE 4410" = import_export_440131_4407_4410_4412_4418$IMPORT_HORS_UE.x.x,
                                                      "Export UE 4410" = import_export_440131_4407_4410_4412_4418$EXPORT_UE.x.x,
                                                      "Import UE 4410" = import_export_440131_4407_4410_4412_4418$IMPORT_UE.x.x,
                                                      "Export hors UE 4410" = import_export_440131_4407_4410_4412_4418$EXPORT_HORS_UE.x.x,
                                                      "Import hors UE 4412" = import_export_440131_4407_4410_4412_4418$IMPORT_HORS_UE.y.y,
                                                      "Export UE 4412" = import_export_440131_4407_4410_4412_4418$EXPORT_UE.y.y,
                                                      "Import UE 4412" = import_export_440131_4407_4410_4412_4418$IMPORT_UE.y.y,
                                                      "Export hors UE 4412" = import_export_440131_4407_4410_4412_4418$EXPORT_HORS_UE.y.y,
                                                      "Import hors UE 4418" = import_export_440131_4407_4410_4412_4418$IMPORT_HORS_UE,
                                                      "Export UE 4418" = import_export_440131_4407_4410_4412_4418$EXPORT_UE,
                                                      "Import UE 4418" = import_export_440131_4407_4410_4412_4418$IMPORT_UE,
                                                      "Export hors UE 4418" = import_export_440131_4407_4410_4412_4418$EXPORT_HORS_UE)
import_export_440131_4407_4410_4412_4418<- pivot_longer(import_export_440131_4407_4410_4412_4418, cols = c("Import.hors.UE.440131","Export.UE.440131","Import.UE.440131","Export.hors.UE.440131","Import.hors.UE.4407","Export.UE.4407","Import.UE.4407","Export.hors.UE.4407","Import.hors.UE.4410","Export.UE.4410","Import.UE.4410","Export.hors.UE.4410","Import.hors.UE.4412","Export.UE.4412","Import.UE.4412","Export.hors.UE.4412","Import.hors.UE.4418","Export.UE.4418","Import.UE.4418","Export.hors.UE.4418"),
                                                        names_to = "Type", 
                                                        values_to = "Value")

ggplot(import_export_440131_4407_4410_4412_4418, aes(x = period , y =Value, color = Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Import.hors.UE.440131" = "red","Export.UE.440131"="red","Import.UE.440131"="orange","Export.hors.UE.440131"="orange","Import.hors.UE.4407"="blue","Export.UE.4407"="blue","Import.UE.4407"="skyblue","Export.hors.UE.4407"="skyblue","Import.hors.UE.4410"="green","Export.UE.4410"="green","Import.UE.4410"="lightgreen","Export.hors.UE.4410"="lightgreen","Import.hors.UE.4412"="black","Export.UE.4412"="black","Import.UE.4412"="grey","Export.hors.UE.4412"="grey","Import.hors.UE.4418"="purple","Export.UE.4418"="purple","Import.UE.4418"="violet","Export.hors.UE.4418"="violet")) +
  labs(title = "Import/Export de 440131/4407/4410/4412/4418 au cours du temps",
       x = "Année",
       y = "Valeur économique",
       color = "Type de commerce") +
  theme_minimal()+
theme(
  plot.title = element_text(size = 9),
  legend.text = element_text(size = 6),            # Taille de la police de la légende
  legend.title = element_text(size = 9),           # Taille de la police du titre de la légende
  legend.key.size = unit(0.3, "cm"),                # Taille des signes (symboles) dans la légende
  legend.spacing.y = unit(0.2, "cm")   
)


##EVOLUTION DE LA VALEUR DE 440131##
IMPORT_EXPORT_440131 = full_join(TOTAL_EXPORT_UE27_440131,TOTAL_IMPORT_UE27_440131, by = "period")
IMPORT_EXPORT_440131$moyenne_export = (IMPORT_EXPORT_440131$IMPORT_HORS_UE+IMPORT_EXPORT_440131$EXPORT_UE)/2
IMPORT_EXPORT_440131$moyenne_import = (IMPORT_EXPORT_440131$IMPORT_UE+IMPORT_EXPORT_440131$EXPORT_HORS_UE)/2

ggplot(IMPORT_EXPORT_440131, aes(x = period)) +
  geom_line(aes(y = moyenne_import, color = "moyenne_import"), size = 1) +
  geom_line(aes(y = moyenne_export, color = "moyenne_export"), size = 1) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_440131$EXPORT_HORS_UE, ymax = IMPORT_EXPORT_440131$IMPORT_UE), alpha = 0.2) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_440131$EXPORT_UE, ymax = IMPORT_EXPORT_440131$IMPORT_HORS_UE), alpha = 0.2) +
  geom_point(aes(y = moyenne_import), size = 1) +
  geom_point(aes(y = moyenne_export), size = 1) +
  labs(title = "Évolution de la valeur économique import/export 440131 UE",
       x = "Année",
       y = "valeur économique",
       color = "type de flux") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)  
    )



##EVOLUTION DE LA VALEUR DE 4407##
IMPORT_EXPORT_4407 = full_join(TOTAL_EXPORT_UE27_4407,TOTAL_IMPORT_UE27_4407, by = "period")
IMPORT_EXPORT_4407$moyenne_export = (IMPORT_EXPORT_4407$IMPORT_HORS_UE+IMPORT_EXPORT_4407$EXPORT_UE)/2
IMPORT_EXPORT_4407$moyenne_import = (IMPORT_EXPORT_4407$IMPORT_UE+IMPORT_EXPORT_4407$EXPORT_HORS_UE)/2

ggplot(IMPORT_EXPORT_4407, aes(x = period)) +
  geom_line(aes(y = moyenne_import, color = "moyenne_import"), size = 1) +
  geom_line(aes(y = moyenne_export, color = "moyenne_export"), size = 1) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4407$EXPORT_HORS_UE, ymax = IMPORT_EXPORT_4407$IMPORT_UE), alpha = 0.2) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4407$EXPORT_UE, ymax = IMPORT_EXPORT_4407$IMPORT_HORS_UE), alpha = 0.2) +
  geom_point(aes(y = moyenne_import), size = 1) +
  geom_point(aes(y = moyenne_export), size = 1) +
  labs(title = "Évolution de la valeur économique import/export 4407 UE",
       x = "Année",
       y = "valeur économique",
       color = "type de flux")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)  
  )

##EVOLUTION DE LA VALEUR DE 4410##
IMPORT_EXPORT_4410 = full_join(TOTAL_EXPORT_UE27_4410,TOTAL_IMPORT_UE27_4410, by = "period")
IMPORT_EXPORT_4410$moyenne_export = (IMPORT_EXPORT_4410$IMPORT_HORS_UE+IMPORT_EXPORT_4410$EXPORT_UE)/2
IMPORT_EXPORT_4410$moyenne_import = (IMPORT_EXPORT_4410$IMPORT_UE+IMPORT_EXPORT_4410$EXPORT_HORS_UE)/2

ggplot(IMPORT_EXPORT_4410, aes(x = period)) +
  geom_line(aes(y = moyenne_import, color = "moyenne_import"), size = 1) +
  geom_line(aes(y = moyenne_export, color = "moyenne_export"), size = 1) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4410$EXPORT_HORS_UE, ymax = IMPORT_EXPORT_4410$IMPORT_UE), alpha = 0.2) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4410$EXPORT_UE, ymax = IMPORT_EXPORT_4410$IMPORT_HORS_UE), alpha = 0.2) +
  geom_point(aes(y = moyenne_import), size = 1) +
  geom_point(aes(y = moyenne_export), size = 1) +
  labs(title = "Évolution de la valeur économique import/export 4410 UE",
       x = "Année",
       y = "valeur économique",
       color = "type de flux")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)  
  )

##EVOLUTION DE LA VALEUR DE 4412##
IMPORT_EXPORT_4412 = full_join(TOTAL_EXPORT_UE27_4412,TOTAL_IMPORT_UE27_4412, by = "period")
IMPORT_EXPORT_4412$moyenne_export = (IMPORT_EXPORT_4412$IMPORT_HORS_UE+IMPORT_EXPORT_4412$EXPORT_UE)/2
IMPORT_EXPORT_4412$moyenne_import = (IMPORT_EXPORT_4412$IMPORT_UE+IMPORT_EXPORT_4412$EXPORT_HORS_UE)/2

ggplot(IMPORT_EXPORT_4412, aes(x = period)) +
  geom_line(aes(y = moyenne_import, color = "moyenne_import"), size = 1) +
  geom_line(aes(y = moyenne_export, color = "moyenne_export"), size = 1) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4412$EXPORT_HORS_UE, ymax = IMPORT_EXPORT_4412$IMPORT_UE), alpha = 0.2) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4412$EXPORT_UE, ymax = IMPORT_EXPORT_4412$IMPORT_HORS_UE), alpha = 0.2) +
  geom_point(aes(y = moyenne_import), size = 1) +
  geom_point(aes(y = moyenne_export), size = 1) +
  labs(title = "Évolution de la valeur économique import/export 4412 UE",
       x = "Année",
       y = "valeur économique",
       color = "type de flux")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)  
  )

##EVOLUTION DE LA VALEUR DE 4418##
IMPORT_EXPORT_4418 = full_join(TOTAL_EXPORT_UE27_4418,TOTAL_IMPORT_UE27_4418, by = "period")
IMPORT_EXPORT_4418$moyenne_export = (IMPORT_EXPORT_4418$IMPORT_HORS_UE+IMPORT_EXPORT_4418$EXPORT_UE)/2
IMPORT_EXPORT_4418$moyenne_import = (IMPORT_EXPORT_4418$IMPORT_UE+IMPORT_EXPORT_4418$EXPORT_HORS_UE)/2

ggplot(IMPORT_EXPORT_4418, aes(x = period)) +
  geom_line(aes(y = moyenne_import, color = "moyenne_import"), size = 1) +
  geom_line(aes(y = moyenne_export, color = "moyenne_export"), size = 1) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4418$EXPORT_HORS_UE, ymax = IMPORT_EXPORT_4418$IMPORT_UE), alpha = 0.2) +
  geom_ribbon(aes(ymin = IMPORT_EXPORT_4418$EXPORT_UE, ymax = IMPORT_EXPORT_4418$IMPORT_HORS_UE), alpha = 0.2) +
  geom_point(aes(y = moyenne_import), size = 1) +
  geom_point(aes(y = moyenne_export), size = 1) +
  labs(title = "Évolution de la valeur économique import/export 4418 UE",
       x = "Année",
       y = "valeur économique",
       color = "type de flux")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)  
  )

#####EVOLUTION DES 5 PRINCIPAUX PAYS DE DESTINATION D'EXPORTS/IMPORTS EUROPEEN#####

##POUR LE PRODUIT 440131##
#EXPORT#
custom_colors = c("cyan","orange","blue","black","pink","darkgreen","purple","brown","green","violet","red","aquamarine","grey")

ggplot(top_EXPORT_UE27_440131, aes(x = factor(period), y = proportion_y, fill = reorder(partnerDesc, proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 440131 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


ggplot(top_EXPORT_UE27_440131_2, aes(x = factor(period), y = proportion_x, fill = reorder(partnerDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 440131 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


somme_proportions_440131_E_y <- aggregate(proportion_y ~ period, top_EXPORT_UE27_440131, sum)
somme_proportions_440131_E_x <- aggregate(proportion_x ~ period, top_EXPORT_UE27_440131, sum)
mean(somme_proportions_440131_E_y$proportion_y)
mean(somme_proportions_440131_E_x$proportion_x)
#IMPORT#
ggplot(top_IMPORT_UE27_440131, aes(x = factor(period), y = proportion_y, fill = reorder(reporterDesc,proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 440131 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))

ggplot(top_IMPORT_UE27_440131_2, aes(x = factor(period), y = proportion_x, fill = reorder(reporterDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 440131 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


somme_proportions_440131_I_y <- aggregate(proportion_y ~ period, top_IMPORT_UE27_440131, sum)
somme_proportions_440131_I_x <- aggregate(proportion_x ~ period, top_IMPORT_UE27_440131, sum)
mean(somme_proportions_440131_I_y$proportion_y)
mean(somme_proportions_440131_I_x$proportion_x)
##POUR LE PRODUIT 4418##
#EXPORT#
ggplot(top_EXPORT_UE27_4418, aes(x = factor(period), y = proportion_y, fill = reorder(partnerDesc, proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4418 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))

ggplot(top_EXPORT_UE27_4418_2, aes(x = factor(period), y = proportion_x, fill = reorder(partnerDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill",) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4418 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))

somme_proportions_4418_E_y <- aggregate(proportion_y ~ period, top_EXPORT_UE27_4418, sum)
somme_proportions_4418_E_x <- aggregate(proportion_x ~ period, top_EXPORT_UE27_4418, sum)
mean(somme_proportions_4418_E_y$proportion_y)
mean(somme_proportions_4418_E_x$proportion_x)
#IMPORT#
ggplot(top_IMPORT_UE27_4418, aes(x = factor(period), y = proportion_y, fill = reorder(reporterDesc,proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4418 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


ggplot(top_IMPORT_UE27_4418_2, aes(x = factor(period), y = proportion_x, fill = reorder(reporterDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4418 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


somme_proportions_4418_I_y <- aggregate(proportion_y ~ period, top_IMPORT_UE27_4418, sum)
somme_proportions_4418_I_x <- aggregate(proportion_x ~ period, top_IMPORT_UE27_4418, sum)
mean(somme_proportions_4418_I_y$proportion_y)
mean(somme_proportions_4418_I_x$proportion_x)
##POUR LE PRODUIT 4412##
#EXPORT#
ggplot(top_EXPORT_UE27_4412, aes(x = factor(period), y = proportion_y, fill = reorder(partnerDesc, proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4412 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


ggplot(top_EXPORT_UE27_4412_2, aes(x = factor(period), y = proportion_x, fill = reorder(partnerDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4412 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


somme_proportions_4412_E_y <- aggregate(proportion_y ~ period, top_EXPORT_UE27_4412, sum)
somme_proportions_4412_E_x <- aggregate(proportion_x ~ period, top_EXPORT_UE27_4412, sum)
mean(somme_proportions_4412_E_y$proportion_y)
mean(somme_proportions_4412_E_x$proportion_x)
#IMPORT#
ggplot(top_IMPORT_UE27_4412, aes(x = factor(period), y = proportion_y, fill = reorder(reporterDesc,proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4412 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


ggplot(top_IMPORT_UE27_4412_2, aes(x = factor(period), y = proportion_x, fill = reorder(reporterDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4412 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))

somme_proportions_4412_I_y <- aggregate(proportion_y ~ period, top_IMPORT_UE27_4412, sum)
somme_proportions_4412_I_x <- aggregate(proportion_x ~ period, top_IMPORT_UE27_4412, sum)
mean(somme_proportions_4412_I_y$proportion_y)
mean(somme_proportions_4412_I_x$proportion_x)
##POUR LE PRODUIT 4410##
#EXPORT#
ggplot(top_EXPORT_UE27_4410, aes(x = factor(period), y = proportion_y, fill = reorder(partnerDesc, proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4410 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),
    axis.text.x = element_text(size =10, angle = 90),
    legend.title = element_text(size = 9))

   


ggplot(top_EXPORT_UE27_4410_2, aes(x = factor(period), y = proportion_x, fill = reorder(partnerDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4410 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


somme_proportions_4410_E_y <- aggregate(proportion_y ~ period, top_EXPORT_UE27_4410, sum)
somme_proportions_4410_E_x <- aggregate(proportion_x ~ period, top_EXPORT_UE27_4410, sum)
mean(somme_proportions_4410_E_y$proportion_y)
mean(somme_proportions_4410_E_x$proportion_x)
#IMPORT#
ggplot(top_IMPORT_UE27_4410, aes(x = factor(period), y = proportion_y, fill = reorder(reporterDesc,proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4410 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


ggplot(top_IMPORT_UE27_4410_2, aes(x = factor(period), y = proportion_x, fill = reorder(reporterDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4410 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),
    axis.text.x = element_text(size =10, angle = 90),
    legend.title = element_text(size = 9))


somme_proportions_4410_I_y <- aggregate(proportion_y ~ period, top_IMPORT_UE27_4410, sum)
somme_proportions_4410_I_x <- aggregate(proportion_x ~ period, top_IMPORT_UE27_4410, sum)
mean(somme_proportions_4410_I_y$proportion_y)
mean(somme_proportions_4410_I_x$proportion_x)
##POUR LE PRODUIT 4407##
#EXPORT#
ggplot(top_EXPORT_UE27_4407, aes(x = factor(period), y = proportion_y, fill = reorder(partnerDesc, proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4407 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


ggplot(top_EXPORT_UE27_4407_2, aes(x = factor(period), y = proportion_x, fill = reorder(partnerDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE export 4407 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des exports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


somme_proportions_4407_E_y <- aggregate(proportion_y ~ period, top_EXPORT_UE27_4407, sum)
somme_proportions_4407_E_x <- aggregate(proportion_x ~ period, top_EXPORT_UE27_4407, sum)
mean(somme_proportions_4407_E_y$proportion_y)
mean(somme_proportions_4407_E_x$proportion_x)
#IMPORT#
ggplot(top_IMPORT_UE27_4407, aes(x = factor(period), y = proportion_y, fill = reorder(reporterDesc,proportion_y))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4407 (basé sur déclaration import)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


ggplot(top_IMPORT_UE27_4407_2, aes(x = factor(period), y = proportion_x, fill = reorder(reporterDesc, proportion_x))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "partenaires principaux UE import 4407 (basé sur déclaration export)",
       x = "Année",
       y = "Proportion des imports",
       fill = "Pays partenaires") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.text.x = element_text(size =10, angle = 90))


somme_proportions_4407_I_y <- aggregate(proportion_y ~ period, top_IMPORT_UE27_4407, sum)
somme_proportions_4407_I_x <- aggregate(proportion_x ~ period, top_IMPORT_UE27_4407, sum)
mean(somme_proportions_4407_I_y$proportion_y)
mean(somme_proportions_4407_I_x$proportion_x)

######REGROUPEMENT DES MOYENNES DES PRINCIPAUX IMPORTATEURS EXPORTATEURS#####
mean(somme_proportions_440131_E_y$proportion_y)
mean(somme_proportions_440131_E_x$proportion_x)

mean(somme_proportions_440131_I_y$proportion_y)
mean(somme_proportions_440131_I_x$proportion_x)

mean(somme_proportions_4418_E_y$proportion_y)
mean(somme_proportions_4418_E_x$proportion_x)

mean(somme_proportions_4418_I_y$proportion_y)
mean(somme_proportions_4418_I_x$proportion_x)

mean(somme_proportions_4412_E_y$proportion_y)
mean(somme_proportions_4412_E_x$proportion_x)

mean(somme_proportions_4412_I_y$proportion_y)
mean(somme_proportions_4412_I_x$proportion_x)

mean(somme_proportions_4410_E_y$proportion_y)
mean(somme_proportions_4410_E_x$proportion_x)

mean(somme_proportions_4410_I_y$proportion_y)
mean(somme_proportions_4410_I_x$proportion_x)

mean(somme_proportions_4407_E_y$proportion_y)
mean(somme_proportions_4407_E_x$proportion_x)

mean(somme_proportions_4407_I_y$proportion_y)
mean(somme_proportions_4407_I_x$proportion_x)
######PARTIE DONNEES FAOSTAT######
uno<- read.csv2("./raw_data/TAB_FAOSTAT.csv",sep = ",")
print(paste0("Years covered: ",
             paste(unique(uno$Year.Code), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(uno$Element), sep = "", collapse = ", ")))
print(paste0("items: ",
             paste(unique(uno$Item), sep = "", collapse = ", ")))
##WOOD PELLETS##
wood_pellets<- uno[uno$Item=="Wood pellets",]
autoconso_pellets = wood_pellets%>%
  group_by(Year.Code,Element)%>%
  summarise(somme = sum(Value, na.rm = TRUE),.groups = 'drop') %>%
  arrange(Year.Code)
autoconso_pellets = autoconso_pellets%>%
  group_by(Year.Code)%>%
  mutate(proportion = somme/ sum(somme)) %>%
  ungroup()

export_pellets = autoconso_pellets[autoconso_pellets$Element=="Export Quantity",]
import_pellets = autoconso_pellets[autoconso_pellets$Element=="Import Quantity",]
production_pellets = autoconso_pellets[autoconso_pellets$Element== "Production",]

calcul_autoconso_pellets = production_pellets$somme+(import_pellets$somme - export_pellets$somme)
autoconsommation_pellets = data.frame("period"= c(2012:2020),import_pellets$somme,export_pellets$somme,production_pellets$somme,calcul_autoconso_pellets)
autoconsommation_pellets$proportion_production = autoconsommation_pellets$production_pellets.somme/autoconsommation_pellets$calcul_autoconso_pellets
autoconsommation_pellets$proportion_importation = autoconsommation_pellets$import_pellets.somme/autoconsommation_pellets$calcul_autoconso_pellets
autoconsommation_pellets$total = (autoconsommation_pellets$proportion_production+autoconsommation_pellets$proportion_importation)

##SAWNWOOD##
Sawnwood = uno[uno$Item=="Sawnwood",]
autoconso_sawnwood = Sawnwood%>%
  group_by(Year.Code,Element)%>%
  summarise(somme = sum(Value, na.rm = TRUE),.groups = 'drop') %>%
  arrange(Year.Code)
autoconso_sawnwood = autoconso_sawnwood%>%
  group_by(Year.Code)%>%
  mutate(proportion = somme/ sum(somme)) %>%
  ungroup()

export_sawnwood = autoconso_sawnwood[autoconso_sawnwood$Element=="Export Quantity",]
import_sawnwood = autoconso_sawnwood[autoconso_sawnwood$Element=="Import Quantity",]
production_sawnwood = autoconso_sawnwood[autoconso_sawnwood$Element== "Production",]

calcul_autoconso_sawnwood = production_sawnwood$somme+(import_sawnwood$somme - export_sawnwood$somme)
autoconsommation_sawnwood = data.frame("period"=c(2000:2020),import_sawnwood$somme,export_sawnwood$somme,production_sawnwood$somme,calcul_autoconso_sawnwood)
autoconsommation_sawnwood$proportion_production = autoconsommation_sawnwood$production_sawnwood.somme/autoconsommation_sawnwood$calcul_autoconso_sawnwood
autoconsommation_sawnwood$proportion_importation = autoconsommation_sawnwood$import_sawnwood.somme/autoconsommation_sawnwood$calcul_autoconso_sawnwood
autoconsommation_sawnwood$total = (autoconsommation_sawnwood$proportion_production+autoconsommation_sawnwood$proportion_importation)

##WOOD-BASED PANELS
WBP = uno[uno$Item=="Wood-based panels",]
autoconso_WBP = WBP %>%
  group_by(Year.Code,Element)%>%
  summarise(somme = sum(Value, na.rm = TRUE),.groups = 'drop') %>%
  arrange(Year.Code)
autoconso_WBP = autoconso_WBP%>%
  group_by(Year.Code)%>%
  mutate(proportion = somme/ sum(somme)) %>%
  ungroup()

export_WBP = autoconso_WBP[autoconso_WBP$Element=="Export Quantity",]
import_WBP = autoconso_WBP[autoconso_WBP$Element=="Import Quantity",]
production_WBP = autoconso_WBP[autoconso_WBP$Element== "Production",]

calcul_autoconso_WBP = production_WBP$somme+(import_WBP$somme - export_WBP$somme)
autoconsommation_WBP = data.frame("period"=c(2000:2020),import_WBP$somme,export_WBP$somme,production_WBP$somme,calcul_autoconso_WBP)
autoconsommation_WBP$proportion_production = autoconsommation_WBP$production_WBP.somme/autoconsommation_WBP$calcul_autoconso_WBP
autoconsommation_WBP$proportion_importation = autoconsommation_WBP$import_WBP.somme/autoconsommation_WBP$calcul_autoconso_WBP
autoconsommation_WBP$total = (autoconsommation_WBP$proportion_production+autoconsommation_WBP$proportion_importation)

##PARTICLE BOARD##
PB=uno[uno$Item=="Particle board",]
autoconso_PB = PB %>%
  group_by(Year.Code,Element)%>%
  summarise(somme = sum(Value, na.rm = TRUE),.groups = 'drop') %>%
  arrange(Year.Code)
autoconso_PB = autoconso_PB%>%
  group_by(Year.Code)%>%
  mutate(proportion = somme/ sum(somme)) %>%
  ungroup()

export_PB = autoconso_PB[autoconso_PB$Element=="Export Quantity",]
import_PB = autoconso_PB[autoconso_PB$Element=="Import Quantity",]
production_PB = autoconso_PB[autoconso_PB$Element== "Production",]

calcul_autoconso_PB = production_PB$somme+(import_PB$somme - export_PB$somme)
autoconsommation_PB = data.frame("period"=c(2000:2020),import_PB$somme,export_PB$somme,production_PB$somme,calcul_autoconso_PB)
autoconsommation_PB$proportion_production = autoconsommation_PB$production_PB.somme/autoconsommation_PB$calcul_autoconso_PB
autoconsommation_PB$proportion_importation = autoconsommation_PB$import_PB.somme/autoconsommation_PB$calcul_autoconso_PB
autoconsommation_PB$total = (autoconsommation_PB$proportion_production+autoconsommation_PB$proportion_importation)

##OSB##
OSB=uno[uno$Item=="OSB",]
autoconso_OSB = OSB %>%
  group_by(Year.Code,Element)%>%
  summarise(somme = sum(Value, na.rm = TRUE),.groups = 'drop') %>%
  arrange(Year.Code)
autoconso_OSB = autoconso_OSB%>%
  group_by(Year.Code)%>%
  mutate(proportion = somme/ sum(somme)) %>%
  ungroup()

export_OSB = autoconso_OSB[autoconso_OSB$Element=="Export Quantity",]
import_OSB = autoconso_OSB[autoconso_OSB$Element=="Import Quantity",]
production_OSB = autoconso_OSB[autoconso_OSB$Element== "Production",]

calcul_autoconso_OSB = production_OSB$somme+(import_OSB$somme - export_OSB$somme)
autoconsommation_OSB = data.frame("period"=c(2000:2020),import_OSB$somme,export_OSB$somme,production_OSB$somme,calcul_autoconso_OSB)
autoconsommation_OSB$proportion_production = autoconsommation_OSB$production_OSB.somme/autoconsommation_OSB$calcul_autoconso_OSB
autoconsommation_OSB$proportion_importation = autoconsommation_OSB$import_OSB.somme/autoconsommation_OSB$calcul_autoconso_OSB
autoconsommation_OSB$total = (autoconsommation_OSB$proportion_production+autoconsommation_OSB$proportion_importation)


#####RENDEMENT GRAPHIQUE DES DONNEES FAOSTAT#####
ggplot(autoconsommation_pellets, aes(x = period , y = calcul_autoconso_pellets)) +
  geom_line() +
  geom_point() +
  labs(title = "évolution autoconsommation de granulés de bois en UE",
       x = "Année",
       y = "quantité en tonne")+
  theme_minimal()+
theme(
  plot.title = element_text(size = 11)
)
auto_conso_WBP_SW = full_join(autoconsommation_WBP,autoconsommation_sawnwood, by = "period")
auto_conso_WBP_SW <- pivot_longer(auto_conso_WBP_SW, cols = c("calcul_autoconso_WBP", "calcul_autoconso_sawnwood"), names_to = "Type", values_to = "Value")


ggplot(auto_conso_WBP_SW, aes(x = period , y =Value, color = Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("calcul_autoconso_WBP" = "red","calcul_autoconso_sawnwood"="black")) +
  labs(title = "Evolution autoconsommation panneaux de bois et bois sciés en UE",
       x = "Année",
       y = "quantité en m3",
       color = "Type de marchandise") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)
  )
granules_de_bois = data.frame("année"= autoconsommation_pellets$period, "part de production" = autoconsommation_pellets$proportion_production, "part d'importations" =autoconsommation_pellets$proportion_importation, "total à disposition" = autoconsommation_pellets$total)
granules_de_bois <- pivot_longer(granules_de_bois, cols = c("part.de.production", "part.d.importations","total.à.disposition"), names_to = "Type", values_to = "Value")

ggplot(granules_de_bois, aes(x = année , y =Value, color = Type)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 1, slope = 0, color = "red")+
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.2), labels = scales::comma)+
  labs(title = "évolution part import/production dans autoconsommation granulés de bois",
       x = "Année",
       y = "part dans l'autoconsommation",
       color = "répartition") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)
    
  )

panneaux_de_bois = data.frame("année"= autoconsommation_WBP$period, "part de production" = autoconsommation_WBP$proportion_production, "part d'importations" =autoconsommation_WBP$proportion_importation, "total à disposition" = autoconsommation_WBP$total)
panneaux_de_bois <- pivot_longer(panneaux_de_bois, cols = c("part.de.production", "part.d.importations","total.à.disposition"), names_to = "Type", values_to = "Value")

ggplot(panneaux_de_bois, aes(x = année , y =Value, color = Type)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 1, slope = 0, color = "red")+
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.2), labels = scales::comma)+
  labs(title = "évolution part import/production dans l'autoconsommation panneaux de bois",
       x = "Année",
       y = "part dans l'autoconsommation",
       color = "répartition") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)
  )

bois_scies = data.frame("année"= autoconsommation_sawnwood$period, "part de production" = autoconsommation_sawnwood$proportion_production, "part d'importations" =autoconsommation_sawnwood$proportion_importation, "total à disposition" = autoconsommation_sawnwood$total)
bois_scies <- pivot_longer(bois_scies, cols = c("part.de.production", "part.d.importations","total.à.disposition"), names_to = "Type", values_to = "Value")

ggplot(bois_scies, aes(x = année , y =Value, color = Type)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 1, slope = 0, color = "red")+
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.2), labels = scales::comma)+
  labs(title = "évolution part import/production dans l'autoconsommation bois sciés",
       x = "Année",
       y = "part dans l'autoconsommation",
       color = "répartition") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10)
  )


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
str(EXPORT_UE27_440131)
#IMPORTATION UE27 DE 440131#
IMPORT_UE27_440131 <- mirror_flow_440131[!(grepl(pattern, mirror_flow_440131$reporterDesc))&
                                           grepl(pattern, mirror_flow_440131$partnerDesc),]
IMPORT_UE27_440131$primaryValue.x<-as.numeric(IMPORT_UE27_440131$primaryValue.x)
IMPORT_UE27_440131$primaryValue.y<-as.numeric(IMPORT_UE27_440131$primaryValue.y)
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
solde_IMPORT <- TOTAL_EXPORT_UE27_440131$IMPORT - TOTAL_IMPORT_UE27_440131$IMPORT
solde_EXPORT <- TOTAL_EXPORT_UE27_440131$EXPORT - TOTAL_IMPORT_UE27_440131$EXPORT
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
solde_IMPORT_4418 <- TOTAL_EXPORT_UE27_4418$IMPORT - TOTAL_IMPORT_UE27_4418$IMPORT
solde_EXPORT_4418 <- TOTAL_EXPORT_UE27_4418$EXPORT - TOTAL_IMPORT_UE27_4418$EXPORT
balance_commerciale_4418 <- data.frame(TOTAL_EXPORT_UE27_4418$period,solde_IMPORT_4418,solde_EXPORT_4418)

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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
solde_IMPORT_4412 <- TOTAL_EXPORT_UE27_4412$IMPORT - TOTAL_IMPORT_UE27_4412$IMPORT
solde_EXPORT_4412 <- TOTAL_EXPORT_UE27_4412$EXPORT - TOTAL_IMPORT_UE27_4412$EXPORT
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
solde_IMPORT_4410 <- TOTAL_EXPORT_UE27_4410$IMPORT - TOTAL_IMPORT_UE27_4410$IMPORT
solde_EXPORT_4410 <- TOTAL_EXPORT_UE27_4410$EXPORT - TOTAL_IMPORT_UE27_4410$EXPORT
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
  summarise(IMPORT = sum(totalValue_y),
            EXPORT = sum(totalValue_x))%>%
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
solde_IMPORT_4407 <- TOTAL_EXPORT_UE27_4407$IMPORT - TOTAL_IMPORT_UE27_4407$IMPORT
solde_EXPORT_4407 <- TOTAL_EXPORT_UE27_4407$EXPORT - TOTAL_IMPORT_UE27_4407$EXPORT
balance_commerciale_4407 <- data.frame(TOTAL_EXPORT_UE27_4407$period,solde_IMPORT_4407,solde_EXPORT_4407)

balance_commerciale_UE27<- EXPORT_UE27_440131%>%
  group_by(period)%>%
  summarise(declaration_export = sum(primaryValue.x, na.rm = TRUE),
            declaration_import= sum(primaryValue.y, na.rm = TRUE), .groups = 'drop')
#####AFFICHAGE GRAPHIQUE DES DONNEES UN COMTRADE#####
install.packages("ggplot2")
library("ggplot2")
##EVOLUTION DE LA VALEUR DU COMMERCE DE PELLETS AU COURS DU TEMPS##

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
autoconsommation_pellets = data.frame(c(2012:2020),import_pellets$somme,export_pellets$somme,production_pellets$somme,calcul_autoconso_pellets)
autoconsommation_pellets$proportion_production = autoconsommation_pellets$production_pellets.somme/autoconsommation_pellets$calcul_autoconso_pellets
autoconsommation_pellets$proportion_importation = autoconsommation_pellets$import_pellets.somme/autoconsommation_pellets$calcul_autoconso_pellets
autoconsommation_pellets$excedents = (autoconsommation_pellets$proportion_production+autoconsommation_pellets$proportion_importation)-1

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
autoconsommation_sawnwood = data.frame(c(2000:2020),import_sawnwood$somme,export_sawnwood$somme,production_sawnwood$somme,calcul_autoconso_sawnwood)
autoconsommation_sawnwood$proportion_production = autoconsommation_sawnwood$production_sawnwood.somme/autoconsommation_sawnwood$calcul_autoconso_sawnwood
autoconsommation_sawnwood$proportion_importation = autoconsommation_sawnwood$import_sawnwood.somme/autoconsommation_sawnwood$calcul_autoconso_sawnwood
autoconsommation_sawnwood$excedents = (autoconsommation_sawnwood$proportion_production+autoconsommation_sawnwood$proportion_importation)-1

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
autoconsommation_WBP = data.frame(c(2000:2020),import_WBP$somme,export_WBP$somme,production_WBP$somme,calcul_autoconso_WBP)
autoconsommation_WBP$proportion_production = autoconsommation_WBP$production_WBP.somme/autoconsommation_WBP$calcul_autoconso_WBP
autoconsommation_WBP$proportion_importation = autoconsommation_WBP$import_WBP.somme/autoconsommation_WBP$calcul_autoconso_WBP
autoconsommation_WBP$excedents = (autoconsommation_WBP$proportion_production+autoconsommation_WBP$proportion_importation)-1

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
autoconsommation_PB = data.frame(c(2000:2020),import_PB$somme,export_PB$somme,production_PB$somme,calcul_autoconso_PB)
autoconsommation_PB$proportion_production = autoconsommation_PB$production_PB.somme/autoconsommation_PB$calcul_autoconso_PB
autoconsommation_PB$proportion_importation = autoconsommation_PB$import_PB.somme/autoconsommation_PB$calcul_autoconso_PB
autoconsommation_PB$excedents = (autoconsommation_PB$proportion_production+autoconsommation_PB$proportion_importation)-1

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
autoconsommation_OSB = data.frame(c(2000:2020),import_OSB$somme,export_OSB$somme,production_OSB$somme,calcul_autoconso_OSB)
autoconsommation_OSB$proportion_production = autoconsommation_OSB$production_OSB.somme/autoconsommation_OSB$calcul_autoconso_OSB
autoconsommation_OSB$proportion_importation = autoconsommation_OSB$import_OSB.somme/autoconsommation_OSB$calcul_autoconso_OSB
autoconsommation_OSB$excedents = (autoconsommation_OSB$proportion_production+autoconsommation_OSB$proportion_importation)-1


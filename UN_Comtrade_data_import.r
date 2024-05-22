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

#####IMPOTATION DU JEU DE DONNEES 4418#####

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

#####IMPORTATION DU JEU DE DONNEES 4406#####

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
cmd_code <- paste(sort(c( "4406")),
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
          "./raw_data/data_4406.csv",
          row.names = FALSE)

all(dim(data5) == dim(trade_data_5)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

#########################################################################
#########################################################################

#####MISE EN FORME DES JEUX DE DONNEES ET EXPLOITATION DES INFORMATIONS#####

###RAPPEL DES JEUX DE DONNEES###
data = read.csv2("./raw_data/data_440131.csv",sep = ",")
data2 = read.csv2("./raw_data/data_4418.csv",sep = ",")
data3 = read.csv2("./raw_data/data_4412.csv",sep = ",")
data4 = read.csv2("./raw_data/data_4410.csv",sep = ",")
data5 = read.csv2("./raw_data/data_4406.csv",sep = ",")

###NETTOYAGE DES DONNEES###
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


###RAPPEL DES TABLEAUX IMPORT/EXPORT###
export_440131 = data[(data$flowCode == "X"),]
import_440131 = data[(data$flowCode == "M"),]

export_4418 = data2[(data2$flowCode == "X"),]
import_4418 = data2[(data2$flowCode == "M"),]

export_4412 = data3[(data3$flowCode == "X"),]
import_4412 = data3[(data3$flowCode == "M"),]

export_4410 = data4[(data4$flowCode == "X"),]
import_4410 = data4[(data4$flowCode == "M"),]

export_4406 = data5[(data5$flowCode == "X"),]
import_4406 = data5[(data5$flowCode == "M"),]

###RAPPEL DES DONNEES MIROIR###
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

mirror_flow_4406 = merge(x= export_4406[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         y= import_4406[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                         by.x=c("cmdCode", "period", "reporterDesc", "partnerDesc"),
                         by.y=c("cmdCode", "period", "partnerDesc", "reporterDesc"),
                         all.x = TRUE, all.y = TRUE)
sum(is.na(mirror_flow_4406))

###CREATION D'UN VECTEUR UE27 REGROUPANT LES 27 PAYS MEMBRES DE L'UE###
UE_27 = c('Austria','Belgium','Bulgaria','Croatia','Cyprus',
          'Czech Republic','Denmark','Estonia','Finland','France',
          'Germany','Greece','Hungary','Ireland','Italy','Latvia',
          'Lithuania','Luxembourg','Malta','Netherlands','Poland',
          'Portugal','Romania','Slovakia','Slovenia','Spain','Sweden')
###CREATION DES DIFFERENTS SOUS-ENSEMBLES POUR ANALYSE###

##PRODUIT 440131##
pattern <- paste(UE_27, collapse="|")
UE_27_440131_interne <- data[grepl(pattern, data$reporterDesc) & 
                       grepl(pattern,data$partnerDesc), ]
###vérifier que la compréhension des données est bonne, est ce qu'on peut 
## effectuer ce tri à partir du tableau des données miroir ou faut-il se baser
## sur les tableaux data nettoyés? 
ue_27_commerce <- data[grepl(pattern,data$reporterDesc)&
                         !(grepl(pattern,data$partnerDesc)),]
monde_europe_commerce <- data[!(grepl(pattern,data$reporterDesc))&
                                !(grepl(pattern,data$partnerDesc)),]


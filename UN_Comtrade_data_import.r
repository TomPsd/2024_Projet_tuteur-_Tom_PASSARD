# UN Comtrade data import

# Install pacman package if not installed
if (!("pacman" %in% rownames(installed.packages()))) {
  install.packages("pacman")
}

# Install and load used packages
pacman::p_load(reticulate, rstudioapi)

# Indicate anaconda environment
use_condaenv(as.character(conda_list())[2])
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
years <- 2000:2020
cmd_code <- paste(sort(c("4403", "4407")),
                  sep = "",
                  collapse = ",")
flow_code <- paste(sort(c("M", "X")),
                   sep = "",
                   collapse = ",")

# Store data into a data.frame
annual_trade_list <- list()
for (i in seq_along(years)) {
  annual_trade_list[[i]] <- get_UN_Comtrade_data(apikey,
                                                 year = as.character(years[i]),
                                                 cmd = cmd_code,
                                                 flow = flow_code)
}
trade_data <- do.call(rbind, annual_trade_list)

# Check if data have been correctly downloaded
print(paste0("Years covered: ",
             paste(unique(trade_data$period), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(trade_data$cmdCode), sep = "", collapse = ", ")))
print(paste0("Flows considered: ",
             paste(unique(trade_data$flowDesc), sep = "", collapse = ", ")))

# Export data as a csv file
write.csv(trade_data,
          "./raw_data/uncomtrade_data.csv",
          row.names = FALSE)

# To load back the csv file
data <- read.csv(list.files(path = "./raw_data",
                            pattern = ".csv",
                            full.names = TRUE))
all(dim(data) == dim(trade_data)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

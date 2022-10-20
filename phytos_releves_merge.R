## Merging abundance relevés in a single phytosociological (or presence/absence) table

# set your working directory
# setwd(...)


# importing a file containing different sheets with one (or more) relevé(s) each
# in each sheet, the first column contains the taxa, the others the abundance (or presence/absence) values in each relevé

library(readxl) # if you have an excel spreadsheet

tab <- list()
nrel <- 5 ## BE CAREFUL: insert the total number of your releves 

for(a in 1:nrel){
  tab[[a]] <- read_xlsx("Cartel1.xlsx", # if your file is not ".xlsx", use another function to import the file
                        col_names = F,  # if your spreadsheets have column names, put col_names = T
                        sheet = a) 
  colnames(tab[[a]]) <- c("Species", 1:(dim(tab[[a]])[2]-1))
  for(b in 1:(dim(tab[[a]])[2]-1)){
    tab[[a]][which(tab[[a]][,b+1]=="-"),b+1] <- 0 # provare con .... <- ""
    tab[[a]][which(tab[[a]][,b+1]=="+"),b+1] <- 0.7
  }
}



names <- tab[[1]][,1]
for(a in 2:length(tab)){
  names <- rbind(names, tab[[a]][,1])
}
spp <- as.factor(names$Species)

taxa <- levels(spp) # list of different taxa



# creating a vector which indicates the nth column for the first relevé of the tab list
# this vector is essential to create the merging table
steps <- vector(mode = "integer", length = (length(tab)+1))
steps[1] <- 1
steps[length(steps)] <- sum(as.integer(summary(tab)[,1])) - length(tab) + 1
for(a in 2:(length(steps)-1)){
  steps[a] <- steps[a-1] + as.integer(summary(tab)[a-1, 1]) - 1
}


# creating the merging table (only abundance or presence/absence data)
data <- matrix(nrow = nlevels(spp), ncol = (sum(as.integer(summary(tab)[,1])) - length(tab)))

# filling the table
for(a in 1:nlevels(spp)){
  for(b in 1:length(tab)){
    if(any(tab[[b]][,1]==levels(spp)[a])==T){
      data[a, steps[b]:(steps[b+1]-1)] <- as.character(tab[[b]][which(tab[[b]][,1]==levels(spp)[a]),2:(steps[b+1]-steps[b]+1)])
    }
  }
}



####
# this is optional, it depends on what value you want to have to indicate absence, i.e. NA, 0 or an empty space
for(a in 1:dim(data)[1]){
  data[a, which(is.na(data[a,]))] <- ""  # to convert NA values to empty spaces
}
####



# dataframe with taxa (first column) and abundance or presence/absence
final.tab <- data.frame(taxa, data)


# exporting the merged table
write.csv(final.tab, file = "Merged_tab.csv", quote = F)

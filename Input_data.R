############
#
###########
#
#

# Load the required libraries
library(worms)
library(dplyr)
library(stringr)
library(tidyr)

#Read in#Read in#Read in the data
df = read.csv("RTMS_ob_RTLE.csv")
df = data.frame(df$Red.Tide.Sighting.No.,df$Date.of.Report,df$Group, df$Species, df$Location, df$Latitude, df$Longitude)
colnames(df) = c("Sighting", "Date", "Group", "Species", "Location", "lat", "lon")

#Extract years from the submission date
df$Year <- substr(df$Date, start = 1, stop = 4)

#Alternative spellings and typos (only need to run this once to figure out what names to replace)
#spec_list = unique(df$Species) #To automatically remove these, you would need an AI. However, we can do it ourselves for short lists
#spec_list = sort(spec_list)#sorting alows for easy detection of typos.

#zz <- file("SortedSpecies.txt", "wb") #Open file
#writeBin( paste(sort(spec_list), collapse="\n"), zz ) #write to the file
#close(zz) #close file - this file is just easier to inspect.

#Put int he right format, Genus/species (one space)
#spec_list <- gsub("^(\\S+ \\S+) ", "\\1_", spec_list)

#w <- wormsbynames(spec_list) 

#replace the names
df$Species = gsub("Cochlodinium cf. geminatum","Polykrikos geminatus", df$Species) #cf. and also outdated name
df$Species = gsub("Gymndinium simplex","Protodinium simplex", df$Species) #typo and also outdated name
df$Species = gsub("Gymnodinium sp.X","Gymnodinium sp.", df$Species) #remove extra X
df$Species = gsub("Polykrikos geminatum","Polykrikos geminatus", df$Species) #misspelled name
df$Species = gsub("Protopolykrikos distortus","Polykrikos geminatus", df$Species) #incorrect name

#again (previous part checked for types and unrecognized names, now we look for unaccepted names)
df$Species <- gsub("sp\\..*|spp\\..*", "", df$Species)
Undetermined = subset(df, Species == "Undetermined ")
df <- df %>% filter(Species != "Undetermined ")

w = wormsbynames(df$Species)
u = wormsbyid(w$AphiaID)

df = data.frame(df$Sighting, df$Date, df$Year, df$Group, df$Location, df$lat, df$lon, df$Species, u$valid_name, u$kingdom, u$phylum, u$order, u$family, u$genus)

#this is okay, but some mistakes are introduces, such as not recognizing a species, replacing an unrecognized species with NA, or confusing a species with an associated virus.
#Go through it manually to correct
rows_with_na <- df[!complete.cases(df), ]
write.csv(rows_with_na, file = "wrongworms.csv")

#every example of Heterosigma akashiwo has been replaced with a virus
df <- df %>%
  mutate(u.valid_name = if_else(u.valid_name == "Heterosigma akashiwo virus 01", "Heterosigma akashiwo", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Heterosigma akashiwo", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Heterosigma akashiwo", "Ochrophyta", u.phylum),
         u.order = if_else(u.valid_name == "Heterosigma akashiwo", "Chattonellales", u.order),
         u.family = if_else(u.valid_name == "Heterosigma akashiwo", "Chattonellaceae", u.family),
         u.genus = if_else(u.valid_name == "Heterosigma akashiwo", "Heterosigma", u.genus))

#every example of Heterocapsa circularisquama has been replaced by a virus
df <- df %>%
  mutate(u.valid_name = if_else(u.valid_name == "Heterocapsa circularisquama RNA virus 01", "Heterocapsa circularisquama", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Heterocapsa circularisquama", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Heterocapsa circularisquama", "Myzozoa", u.phylum),
         u.order = if_else(u.valid_name == "Heterocapsa circularisquama", "Peridiniales", u.order),
         u.family = if_else(u.valid_name == "Heterocapsa circularisquama", "Heterocapsaceae", u.family),
         u.genus = if_else(u.valid_name == "Heterocapsa circularisquama", "Heterocapsa", u.genus))

#skipped over Nitzschia lorenziana var. incerta valid name for some reason
df <- df %>%
  mutate(u.valid_name = if_else(u.valid_name == "Nitzschia lorenziana var. incerta", "Nitzschia lorenziana var. incerta", u.valid_name))

#skipped over Scrippsiella trochidea, which should be Scrippsiella acuminata
df <- df %>%
  mutate(u.valid_name = if_else(df.Species == "Scrippsiella trochoidea", "Scrippsiella acuminata", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Scrippsiella acuminata", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Scrippsiella acuminata", "Myzozoa", u.phylum),
         u.order = if_else(u.valid_name == "Scrippsiella acuminata", "Peridiniales", u.order),
         u.family = if_else(u.valid_name == "Scrippsiella acuminata", "Peridiniaceae", u.family),
         u.genus = if_else(u.valid_name == "Scrippsiella acuminata", "Scrippsiella", u.genus))

#skipped over Dictyocha octonaria, which should be Octactis octonaria 
df <- df %>%
  mutate(u.valid_name = if_else(df.Species == "Dictyocha octonaria", "Octactis octonaria", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Octactis octonaria", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Octactis octonaria", "Ochrophyta", u.phylum),
         u.order = if_else(u.valid_name == "Octactis octonaria", "Dictyochales", u.order),
         u.family = if_else(u.valid_name == "Octactis octonaria", "Dictyochaceae", u.family),
         u.genus = if_else(u.valid_name == "Octactis octonaria", "Octactis", u.genus))

#Pedinomonadaceae is only identified to family level

#Protodinium simplex does not have order or family

#Levanderina fissa does not have a family

#Hermesinum adriaticum does not have a phylum

#every example of Chaetoceros socialis is a virus
df <- df %>%
  mutate(u.valid_name = if_else(u.valid_name == "Chaetoceros socialis f. radians RNA virus 01", "Chaetoceros socialis", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Chaetoceros socialis", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Chaetoceros socialis", "Bacillariophyta", u.phylum),
         u.order = if_else(u.valid_name == "Chaetoceros socialis", "Chaetocerotanae incertae sedis", u.order),
         u.family = if_else(u.valid_name == "Chaetoceros socialis", "Chaetocerotaceae", u.family),
         u.genus = if_else(u.valid_name == "Chaetoceros socialis", "Chaetoceros", u.genus))

#every example of Chaetoceros tenuissimus is a virus
df <- df %>%
  mutate(u.valid_name = if_else(u.valid_name == "Chaetoceros tenuissimus RNA virus 01", "Chaetoceros tenuissimus", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Chaetoceros tenuissimus", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Chaetoceros tenuissimus", "Bacillariophyta", u.phylum),
         u.order = if_else(u.valid_name == "Chaetoceros tenuissimus", "Chaetocerotanae incertae sedis", u.order),
         u.family = if_else(u.valid_name == "Chaetoceros tenuissimus", "Chaetocerotaceae", u.family),
         u.genus = if_else(u.valid_name == "Chaetoceros tenuissimus", "Chaetoceros", u.genus))

#Chattonella did not correctly add the family or genus (also includes a space at the end because we removed spp.)
df <- df %>%
  mutate(u.family = if_else(u.valid_name == "Chattonella ", "Vacuolariaceae", u.family),
         u.genus = if_else(u.valid_name == "Chattonella ", "Chattonella", u.genus))

#skipped over Neoceratium furca which should be Tripos furca
df <- df %>%
  mutate(u.valid_name = if_else(df.Species == "Neoceratium furca", "Tripos furca", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Tripos furca", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Tripos furca", "Myzozoa", u.phylum),
         u.order = if_else(u.valid_name == "Tripos furca", "Gonyaulacales", u.order),
         u.family = if_else(u.valid_name == "Tripos furca", "Ceratiaceae", u.family),
         u.genus = if_else(u.valid_name == "Tripos furca", "Tripos", u.genus))

#every example of Chaetoceros salsugineum is a virus, it should also be Chaetoceros salsugineus
df <- df %>%
  mutate(u.valid_name = if_else(u.valid_name == "Chaetoceros salsugineum DNA virus 01", "Chaetoceros salsugineus", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Chaetoceros salsugineus", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Chaetoceros salsugineus", "Bacillariophyta", u.phylum),
         u.order = if_else(u.valid_name == "Chaetoceros salsugineus", "Chaetocerotanae incertae sedis", u.order),
         u.family = if_else(u.valid_name == "Chaetoceros salsugineus", "Chaetocerotaceae", u.family),
         u.genus = if_else(u.valid_name == "Chaetoceros salsugineus", "Chaetoceros", u.genus))

#it skipped over Nitzschia incerta
df <- df %>%
  mutate(u.valid_name = if_else(df.Species == "Nitzschia incerta", "Nitzschia lorenziana var. incerta", u.valid_name),
         u.kingdom = if_else(u.valid_name == "Nitzschia lorenziana var. incerta", "Chromista", u.kingdom),
         u.phylum = if_else(u.valid_name == "Nitzschia lorenziana var. incerta", "Bacillariophyta", u.phylum),
         u.order = if_else(u.valid_name == "Nitzschia lorenziana var. incerta", "Bacillariales", u.order),
         u.family = if_else(u.valid_name == "Nitzschia lorenziana var. incerta", "Bacillariaceae", u.family),
         u.genus = if_else(u.valid_name == "Nitzschia lorenziana var. incerta", "Nitzschia", u.genus))

#add back the Undetermined species
Undetermined = data.frame(Undetermined$Sighting, Undetermined$Date, Undetermined$Year, Undetermined$Group, Undetermined$Location, Undetermined$lat, Undetermined$lon, Undetermined$Species, "u.valid_name" = NA, "u.kingdom" = NA, "u.phylum" = NA, "u.order" = NA, "u.family" = NA, "u.genus" = NA)
colnames(Undetermined) = colnames(df)

df = rbind(df, Undetermined)

#rename columns
colnames(df) = c("Sighting", "Date" ,"Year","Group","Location","lat","lon","EPD_speciesname", "Species", "Kingdom", "Phylum", "Order", "Phamily", "Genus")

#add back sp.
df$EPD_speciesname <- ifelse(substr(df$EPD_speciesname, nchar(df$EPD_speciesname), nchar(df$EPD_speciesname)) == " ", paste0(df$EPD_speciesname, "sp."), df$EPD_speciesname)

#The WoRMS script also tried to attach a species to genus names, so we can fix that
df$Species <- ifelse(substr(df$EPD_speciesname, nchar(df$EPD_speciesname), nchar(df$EPD_speciesname)) == ".",
                     sub("^(\\S+).*", "\\1 sp.", df$Species),
                     df$Species)

#save it as a csv
write.csv(df, file = "WoRMSCorrected_redtides.csv")
########################################################################

#subset(df, df$Phylum == "Ocrophyta")
#df$Phylum = gsub("Ocrophyta", "Ochrophyta", df$Phylum)
#df[1:(length(df)-1)]

library(magrittr)
library(xlsx)
library(tidyverse)
library(data.table)

# Upload MTurk hits

in_path <- "/Users/pedrorodriguez/Drobox/GitHub/Semantica/Semantic-Search/data/RShiny/Context/Raw/"
out_path <- "/Volumes/Potosi/Research/EmbeddingsProject/CR/Human-Validation/"

# Define pre-processing options (in addition to lower-casing and removing punctuation)

PLURALS <- FALSE
COLLOCATIONS <- FALSE
STEM <- FALSE

# --------------------------
# LOAD LISTS
# --------------------------

mturk_hits <- as.list(list.files(in_path))

surveys <- mturk_hits[grepl("survey", mturk_hits)] # Survey files
fluency <- mturk_hits[grepl("fluency", mturk_hits)] # Fluency files

# Function to clean HITs

cleanHIT <- function(hit_file){
  hit <- read.table(paste0(in_path, hit_file), sep = ",", header = TRUE) # Load hit
  
  cues <- as.character(unique(hit$cue))
  
  if(length(cues) == 11){
  workerid <- as.character(unique(hit$workerid))
  
  # Fluency data
  
  fluency <- t(hit[hit$variable == "Fluency", -c(1,3)]) # Keep fluency rows and cue
  
  fluency[trimws(fluency) == ""] <- NA # NA empty entries
  
  fluency <- data.table(fluency) %>% set_colnames(cues)
  
  fluency <- gather(fluency) %>% set_colnames(c("cue", "fluency"))
  
  hit <- data.table(workerid, fluency)
  
  hit <- hit[!is.na(hit$fluency),]
  
  return(hit)}else{return(data.table(workerid = NA, cue = NA, fluency = NA))}
}

# Function to clean survey files

cleanSURVEY <- function(hit_file){
  hit <- read.table(paste0(in_path, hit_file), sep = ",", header = TRUE) # Load hit
  
  return(hit)
}

# Apply function and bind results

hitsFluency <- lapply(fluency, function(x) cleanHIT(x)) %>% .[!is.na(.)] %>% do.call(rbind, .)

hitsSurvey <- lapply(surveys, function(x) cleanSURVEY(x)) %>% do.call(rbind,.)

# Save results to excel to apply spell check

# write.xlsx(hitsFluency, paste0(out_path,"hitsFluency.xlsx"), col.names = TRUE)

# --------------------------
# PRE-PROCESSING
# --------------------------

#hitsFluency <- data.table(read.xlsx(paste0(out_path, "hitsFLUENCY_spellchecked.xlsx"), sheetName = "Sheet1"), stringsAsFactors = FALSE)

names(hitsFluency)[names(hitsFluency) == 'workerid'] <- 'pid'
names(hitsFluency)[names(hitsFluency) == 'workerid'] <- 'pid'

hitsFluency$fluency <- tolower(hitsFluency$fluency)
hitsFluency$fluency <- gsub("'", "", hitsFluency$fluency) # Remove apostrophes
hitsFluency$fluency <- gsub("\\.", "", hitsFluency$fluency) # Remove full stops
hitsFluency$fluency <- gsub("[^[:alnum:]]", " ", hitsFluency$fluency) # Remove all non-alpha characters
hitsFluency$fluency <- str_replace_all(hitsFluency$fluency, "^ +| +$|( ) +", "\\1") # Remove excess white space
hitsFluency$fluency <- gsub("^us$", "united states", hitsFluency$fluency) # Combine multiple word entries
hitsFluency$fluency <- gsub(" ", "_", hitsFluency$fluency) # Combine multiple word entries

hitsFluency <- hitsFluency %>% filter(fluency!="")

if(PLURALS){
  vocab_plurals <- hitsFluency$fluency
  vocab_plurals <- vocab_plurals[!grepl("_", vocab_plurals)]
  vocab_plurals <- vocab_plurals[grepl("s$", vocab_plurals)]
  vocab_plurals <- tibble(pattern = unique(vocab_plurals), replacement = gsub("s$", "", unique(vocab_plurals)))
  vocab_plurals <- vocab_plurals %>% filter(replacement %in% hitsFluency$fluency)
  for(i in 1:nrow(vocab_plurals)){hitsFluency$fluency <- gsub(paste0("\\<", vocab_plurals$pattern[i],"\\>"), vocab_plurals$replacement[i], hitsFluency$fluency)} # slower but safer
}

# Collocations

if(COLLOCATIONS){
  vocab_collocs <- hitsFluency$fluency
  vocab_collocs <- vocab_collocs[grepl("_", vocab_collocs)]
  vocab_collocs <- tibble(pattern = unique(vocab_collocs), replacement = gsub("_", "", unique(vocab_collocs)))
  vocab_collocs <- vocab_collocs %>% filter(replacement %in% hitsFluency$fluency)
  
  for(i in 1:nrow(vocab_collocs)){hitsFluency$fluency <- gsub(paste0("\\<", vocab_collocs$pattern[i],"\\>"), vocab_collocs$replacement[i], hitsFluency$fluency)} # slower but safer
}

if(STEM){
  hitsFluency$fluency <- wordStem(hitsFluency$fluency, language = "en", warnTested = FALSE)
}

# Drop repetitions in fluency lists (technically not allowed in SFT)

fluency_list <- hitsFluency %>% group_by(pid, cue) %>% distinct(fluency, .keep_all = TRUE) %>% ungroup() %>% select(pid, cue, fluency) # Model will not work w/o deleting duplicates

# --------------------------
# CLEAN SURVEY
# --------------------------

names(hitsSurvey)[names(hitsSurvey) == 'workerid'] <- 'pid'

# Re-code party

hitsSurvey$party2 <- hitsSurvey$party
hitsSurvey$party2[hitsSurvey$party2 %in% c(8,9)] <- NA
hitsSurvey$party2[hitsSurvey$party2 %in% c(1,2,3)] <- "democrat"
hitsSurvey$party2[hitsSurvey$party2 == 4] <- "independent"
hitsSurvey$party2[hitsSurvey$party2 %in% c(5,6,7)] <- "republican"

# Re-code ideology

hitsSurvey$ideology2 <- hitsSurvey$ideology
hitsSurvey$ideology2[hitsSurvey$ideology ==8] <- NA
hitsSurvey$ideology2[hitsSurvey$ideology %in% c(1,2,3)] <- "liberal"
hitsSurvey$ideology2[hitsSurvey$ideology == 4] <- "independent"
hitsSurvey$ideology2[hitsSurvey$ideology %in% c(5,6,7)] <- "conservative"

# Re-code gender

hitsSurvey$gender <- hitsSurvey$sex
hitsSurvey$gender[hitsSurvey$gender == 3] <- NA
hitsSurvey$gender[hitsSurvey$gender ==1] <- "male"
hitsSurvey$gender[hitsSurvey$gender == 2] <- "female"

# --------------------------
# CONVERT TO SS CORPUS
# --------------------------

# Keep only relevant survey data

survey <- hitsSurvey %>% select(pid, ideology, ideology2, party2, gender)

survey <- unique(survey)

# Add random grouping

set.seed(1984L)

random_pid <- sample(survey$pid, length(survey$pid)/2, replace = FALSE)

survey$gang <- ifelse(survey$pid %in% random_pid, "jets", "sharks")

# Tags

tags <- survey

tags$tags <- apply(tags[ , c("ideology2", "party2", "gender", "gang")] , 1 , paste , collapse = " ")

tags <- tags[,c("pid", "tags")]

# SS corpus

sscorpus <- list("fluency" = fluency_list,
                "tags" = tags,
                "survey" = survey)

# Save data

saveRDS(sscorpus, paste0(out_path, "sscorpus.rds"))
# set global variables
global.eye.data <- read.delim("data/Event Statistics - Single.txt", stringsAsFactors = FALSE) # get eye data
global.stimuli <- read.csv("data/stimuli.csv", stringsAsFactors = FALSE) # get stimuli / images
global.colors <- read.csv("data/colors.csv", stringsAsFactors = FALSE) # get colors

global.participants <- merge(data.frame("id" = seq_along(unique(global.eye.data$Participant)), 
                                        "Participant" = unique(global.eye.data$Participant)), 
                             global.colors) # get participants, merge by ID?

# Give participant eye data R-friendly colors and integer ids
global.eye.data <- merge(global.eye.data, global.participants) 

# create empty data frame for AOIs
global.aois <- data.frame()

# read AOIS if the file exists
if (file.exists("data/aois.csv")) {
  global.aois <- read.csv("data/aois.csv", stringsAsFactors = FALSE)
}

enableBookmarking(store = "url")
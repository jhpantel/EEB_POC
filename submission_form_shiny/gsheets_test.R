# Script to figure out how to use Google Sheets

# install.packages("googlesheets4")
library(googlesheets4)
library(tidyr)
library(rorcid)


# HOW TO USE THE TOKEN HERE???
# https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html

# gs4_auth(email = "eebpocdatabase@gmail.com", use_oob = TRUE)
# Can ask people to enter a code maybe?
# After opening another tab to authorize?
#   Enter authorization code: 4/1wFESWiCV5P12zxEsif_bhOr8E7opO3HCvf43E15VTbqvJFA7nRsx78

# read_sheet
ss <- "https://docs.google.com/spreadsheets/d/1iZEkqhK-wr9L5OIKdWPQzlCXpjph2ePmQk_4FlwFshI/edit#gid=1168690498"
sheet <- read_sheet(ss)

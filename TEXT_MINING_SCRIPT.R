library(dplyr)    # used for efficiently manipulating datasets in R
library(stringr)  # provide a cohesive set of functions designed to make working with strings as easy as possible
library(pdftools) #for extracting text, fonts, attachments and metadata from a PDF file
library(magrittr) # needs to be run every time you start R and want to use %>%
library(tesseract)#extracts text from images and documents without a text layer and outputs the document into a new searchable text file, PDF, or most other popular formats
library(writexl)  # extracting to excel file
library(readxl)   # reading excel file
library(tidyr)    # used for data cleaning
library(ggplot2)  # used for visualization

filename <- file.choose()        # choosing the file from the local drive
GRIexcel <-  readRDS(filename)   # reading the RDS file
as.data.frame(GRIexcel)
data_CSR <- GRIexcel
View(data_CSR)                   # viewing the RDS file
R_data <- (dplyr::filter(GRIexcel, 
                         Sector %in% "Retailers")) # filetering the RDS file for retailers sector

# creating the new column by concatinating 1Name and Publication Year
R_data$filenames <- str_c(R_data$Name,'_',R_data$`Publication Year`) 
R_data

# removes the space in between names
R_data$filenames <- gsub(" ", "", R_data$filenames) 
R_data

#Seperate dataframe for the Name, Publication Year, filenames
DATA <- R_data[, c('Name', 'Publication Year', 'filenames')]
DATA
#Seperate dataframe for the Name, Publication Year, Size, Country, Region, Type, filenames
DF1 <- R_data[, c('Name', 'Publication Year','Size','Country','Region','Type', 'filenames')]
#Exporting the dataframe to excel to analyse the data
write_xlsx(DF1,"G:/dfonedrives/DF1.xlsx")

# TEXT MINING
# choosing the directory we will use to retrieve the files and creating a vector that contains all files
directory <-
  "G:/dfonedrive/"
files <- list.files(directory, pattern = ".pdf$" , full.names=T)

# creating a vector for keywords that need for analysis
# The below 10 keyword in 5 different language is used because Retailers folder
#had files in 5 different language
# 4 key word is changed to 10 different word to extract different words with same menaing 
keywords <-
  c("greenhouse gas emission",
    "diversity",
    "employee health & safety",
    "employee health and safety",
    "employee health",
    "employee safety",
    "safety",
    "customer welfare",
    "consumer welfare",
    "welfare",
    
    #Chinese  #it will be chinese charachets but once once it is saved it will be ? symbol
    "??????????????????",
    "?????????",
    "?????????????????????",
    "?????????????????????",
    "????????????",
    "????????????",
    "??????",
    "????????????",
    "???????????????",
    "??????",
    
    #German
    "Treibhausgasemission",
    "Diversität",
    "Mitarbeitergesundheit & Sicherheit",
    "Gesundheit und Sicherheit der Mitarbeiter",
    "Mitarbeitergesundheit",
    "Mitarbeitersicherheit",
    "Sicherheit",
    "Kundenwohl",
    "Verbraucherwohlfahrt",
    "Wohlfahrt",
    
    #French
    "émission de gaz à effet de serre",
    "la diversité",
    "santé et sécurité des employés",
    "santé et sécurité des employés",
    "santé des salariés",
    "la sécurité des employés",
    "sécurité",
    "bien-être client",
    "bien-être des consommateurs",
    "bien-être",
    
    #Spanish
    "emisión de gases de efecto invernadero",
    "diversidad",
    "salud y seguridad de los empleados",
    "salud y seguridad de los empleados",
    "salud de los empleados",
    "seguridad de los empleados",
    "la seguridad",
    "bienestar del cliente",
    "bienestar del consumidor",
    "bienestar"
    
  )

filelength <- length(files)
wordlength <- length(keywords)

# matrix creation for data
word_c <- seq(1, filelength * wordlength)
dim(word_c) <- c(filelength, wordlength)

# Below code extracts the texts from pdf then clean and process the texts for the analysis
# By using below method it is fast when compared to tidyr and wont remove unwanted languages, symbols which is needed.

word_list <- c()
for (j in 1:length(files)) {
  P1 <- pdftools::pdf_text(pdf = files[j]) %>%
    str_to_lower() %>%
    str_replace_all("\\t", "") %>%
    str_replace_all("\n", " ") %>%
    str_replace_all("      ", " ") %>%
    str_replace_all("    ", " ") %>%
    str_replace_all("   ", " ") %>%
    str_replace_all("  ", " ") %>%
    str_replace_all("[:digit:]", "") %>%
    str_replace_all("[:punct:]", "") %>%      
    str_trim()
  
  vector_words <- str_split(P1," ")
  for (i in vector_words){
    word_list <- c(word_list,i)
  }
  
  for (i in 1:length(keywords)) {
    word_c[j, i] <- P1 %>% str_count(keywords[i]) %>% sum()
  }
  
}

df0 <- data.frame(sort(table(word_list),decreasing = T))

#Exporting the dataframe to excel contains frequencies of all words
write_xlsx(df0,"G:/dfonedrives/df0.xlsx")

# converting the matrix into a dataframe
word_c <- as.data.frame(word_c)

# Naming of rows and columns 
rownames(word_c) <- files
colnames(word_c) <- keywords

# checking the matrix 
View(word_c[1])
#word_c <- as.data.frame(word_c)
word_c <- cbind(Names = rownames(word_c), word_c)
View(word_c)

#Exporting the dataframe to excel which contains frequencies for all keywords
write_xlsx(word_c,"G:/dfonedrives/keywords.xlsx")
#import the cleaned excel file in R as dataframe 
exceldata = read_excel("G:/dfonedrives/keywords2_added.xlsx")                                                                            
word_c_sums = data.frame(exceldata)
sep_pdf <- separate(word_c_sums,filenames,
                    into= "filenames", sep=".pdf")
DF2 <-  sep_pdf

#merging of two excel DF1 and DF2 by filnames
Final_df <- inner_join(DF1,DF2, by= c("filenames"))

#Exporting the dataframe to excel to analyse the data further
write_xlsx(Final_df,"G:/dfonedrives/Final_df.xlsx")


# Reference 
#1.stackoverflow
#2.Youtube

# Comments:
# Here all the text from the pdf is extracted and its frequencies are counted
# Sentiment analysis is not done because as the primary aim of this project is to give
# the meaningful insights form the text analysis. As its sentiment is not asked so it is not used


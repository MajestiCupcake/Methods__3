#set working directory
setwd('.')
#load packages
pacman::p_load(tidyverse,
               janitor,
               dplyr)

#load datasets
first <- read.csv('blah.csv')
second <- read.csv('blah.csv')
third <- read.csv('blah.csv')
fourth <- read.csv('blah.csv')


#function
#clean for lower caps
cleaning.data <- function(df_original){
  df_work <- read.csv(df_original)                                                #read into dataframe
  df_work <- mutate_if(df_work,is.character,str_to_upper)                         #clean characters
  colnames(df_work)[1] <- 'CHILDID'                                      
  colnames(df_work)[2] <- 'VISIT'
  df_work$CHILDID <- gsub("[?.;!¡¿·']", "", df_work$CHILDID)
  df_work$VISIT <- str_extract(df_work$VISIT, "\\d")
  df_work <- mutate(df_work$Gender=recode(df_work$Gender,"1"="MALE","2"="FEMALE"))
  df_work <- mutate(df_work$Diagnosis=recode(df_work$Diagnosis,"A"="ASD","B"="TD"))
  df1 <- subset(df, VISIT==1, select = c('SUBJ',
                                         'ADOS',
                                         'MullenRaw',
                                         'ExpressiveLangRaw',
                                         'Socialization'
  )
}
cleaning.data('demo_train.csv')

token_df_new <- token_df %>% mutate_if(is.character, str_to_upper)
lu_df_new <- lu_df %>% mutate_if(is.character,str_to_upper)
demo_df_new <-demo_df_clean %>% mutate_if(is.character,str_to_upper)

# clean column names
demo_df_clean <- demo_df %>% 
  rename( 'SUBJ'= "Child.ID",
          "VISIT" ="Visit")

#clean subject id for weird characters
lu_df_new$SUBJ <- gsub("[?.;!¡¿·']", "", lu_df_new$SUBJ)
token_df_new$SUBJ <- gsub("[?.;!¡¿·']", "", token_df_new$SUBJ)
demo_df_new$SUBJ <- gsub("[?.;!¡¿·']", "", demo_df_new$SUBJ)

#clean visits use for loop to run through the columns
lu_df_new$VISIT <- str_extract(lu_df_new$VISIT, "\\d")

#making new whole dataframe
#the first dataframes
total <- merge(demo_df_new,lu_df_new,by=c("SUBJ","VISIT"), all=TRUE) #all=TRUE for keeping the subjects what do not have any varaibles in the other dataframes, gets assigned NA
#the last dataframe
df_all <- merge(total,token_df_new,by=c("SUBJ","VISIT"),all=TRUE)

#creating subset
df <-  subset(df_all, select = c("SUBJ","VISIT","Diagnosis","Ethnicity","Gender","Age","ADOS","MullenRaw","ExpressiveLangRaw","Socialization","MOT_MLU","CHI_MLU","types_MOT","types_CHI" ,"tokens_MOT","tokens_CHI") )

#making subset for visit 1 variables
df1 <- subset(df, VISIT==1, select = c('SUBJ',
                                       'ADOS',
                                       'MullenRaw',
                                       'ExpressiveLangRaw',
                                       'Socialization'
                                      )
              )
df1 <- df1 %>% 
  rename('ADOS1'='ADOS') %>% 
  rename('MullenRaw1'='MullenRaw') %>% 
  rename('ExpLangRaw1'='ExpressiveLangRaw') %>% 
  rename('Soc1'='Socialization') 

df_all_1 <- merge(df,df1,by=c("SUBJ"),all=TRUE)

#Anonymise
df_all_1$SUBJ <- as.factor(df_all_1$SUBJ)#levels 
df_all_1$SUBJ <- as.numeric(df_all_1$SUBJ)#assigning the level by overwriting the id.
df_all_1$SUBJ <- as.factor(df_all_1$SUBJ)
head(df_all_1)

#Factor gender to meaning
df_all_1 <- df_all_1 %>% 
  mutate(Gender=recode(Gender,"1"="MALE","2"="FEMALE")) %>% 
  mutate(Diagnosis=recode(Diagnosis,"A"="ASD","B"="TD"))

#save to csv
write.csv(df_all_1,"cleaned_df_complete.csv",row.names = FALSE)






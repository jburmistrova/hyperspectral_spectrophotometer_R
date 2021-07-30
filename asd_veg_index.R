#Author - Julia Burmistrova 
#Purpose - Calculate vegetation indexes from hyperspectral spectrophotometers



#### Packages 

library(tidyverse)
#library(asdreader)


##### FILE LOCATION
asd_textfiles <- "C:/Users/jburmistrova/Downloads"
output_path_for_csv <- "C:/Users/jburmistrova/Downloads"
output_filename_for_csv <- "asd_index"

##### INDEXES WAVELENGTH INPUT

      #wavelength choices for hyperspectral - https://www.frontiersin.org/articles/10.3389/fpls.2017.00820/full

myNIR <- 815
myRED <- 675
myGREEN <- 525
myBLUE <- 460
my2000nm <- 2000
my2100nm <- 2100
my2200nm <- 2200

#for mNDVI
#myMIR <- 
#myC1 <- 
#myC2 <- 
#myH1 <- 
#myH2 <- 

##########################################################################

##### INDEX FUNCTIONS 
NDVI <- function(NIR, RED){
  (NIR - RED) / (NIR + RED)
}

# I am not sure what to input into C1, C2, H1, H2
#mNDVI <- function (NIR, MIR) {
#  (NDVI_val / (1 + (C1 * H1)) * (1 + (C2 * H2))

EVI <- function(NIR, RED, BLUE){
  2.5*(NIR - RED) / (NIR + 6*RED - 7.5*BLUE + 1)
}

#not sure if this is the CAI you want, there is also the chlorophyll
CAI <- function(nm2000, nm2100, nm2200){
  0.5 * (nm2000 + nm2200) - nm2100
}

SAVI <- function(NIR, RED){
  ((NIR - RED) / (NIR + RED + 0.5)) * (1 + 0.5)
} 

OSAVI <- function (NIR, RED){
  ((NIR - RED) / (NIR - RED + 0.16)) * (1 + 0.16)
} 

MSAVI <- function (NIR, RED){
  0.5 * ((2  * NIR) + 1 - (sqrt((2 * NIR + 1) ^ 2) - 8 * (NIR - RED)))
} 


##### READ IN TEXT FILES, FORMAT AS TABLES AND CALC MEAN & SD

asd_textfiles_list <- as_tibble(list.files(asd_textfiles, full.name = T, pattern="asd.txt$"))

asd_values <- read_delim(asd_textfiles_list$value[[1]], delim="\t", trim_ws = T)

for (i in 2:length(asd_textfiles_list$value)){
  asd_values_new <- read_delim(asd_textfiles_list$value[[i]], delim="\t", trim_ws = T)
  
  asd_values <- bind_cols(asd_values, asd_values_new[,2])
}

asd_values

asd_values_tidy <- pivot_longer(asd_values, cols = ends_with(".asd"), names_to = "spectrum_name")%>%
  mutate(sample_name = str_split(spectrum_name, pattern = "-", simplify=T)[,4])%>% #note the string split sample number/name came out as 4 but this could change
  group_by(Wavelength, sample_name)
asd_values_tidy

asd_mean_sd <- asd_values_tidy %>%
  summarise(mean = mean(value), 
            sd = sd(value))
asd_mean_sd
         
         
##### CALC INDEXES

#note - you will need to copy/paste and add calcINDEXNAME because I hard coded
#       this is not the best coding, but it worked!

  indexes <- tibble(
    spectrum_name = asd_values_tidy$spectrum_name[asd_values_tidy$Wavelength==myNIR]
  )
  
  calcNDVI <- tibble(
    NDVI = NDVI(asd_values_tidy$value[asd_values_tidy$Wavelength==myNIR], 
                asd_values_tidy$value[asd_values_tidy$Wavelength==myRED]))
  
  indexes <- bind_cols(indexes, calcNDVI)
  
  calcEVI <- tibble(
    EVI = EVI(asd_values_tidy$value[asd_values_tidy$Wavelength==myNIR], 
              asd_values_tidy$value[asd_values_tidy$Wavelength==myRED], 
              asd_values_tidy$value[asd_values_tidy$Wavelength==myBLUE]))
  
  indexes <- bind_cols(indexes, calcEVI)
  
  calcCAI <- tibble(
    CAI = CAI(asd_values_tidy$value[asd_values_tidy$Wavelength==my2000nm], 
              asd_values_tidy$value[asd_values_tidy$Wavelength==my2100nm], 
              asd_values_tidy$value[asd_values_tidy$Wavelength==my2200nm]))
  
  indexes <- bind_cols(indexes, calcCAI)
  
  calcSAVI<- tibble(
    SAVI = SAVI(asd_values_tidy$value[asd_values_tidy$Wavelength==myNIR], 
                asd_values_tidy$value[asd_values_tidy$Wavelength==myRED]))
  
  indexes <- bind_cols(indexes, calcSAVI)
  
  calcOSAVI<- tibble(
    OSAVI = OSAVI(asd_values_tidy$value[asd_values_tidy$Wavelength==myNIR], 
                asd_values_tidy$value[asd_values_tidy$Wavelength==myRED]))
  
  indexes <- bind_cols(indexes, calcOSAVI)
  
  calcMSAVI<- tibble(
    MSAVI = MSAVI(asd_values_tidy$value[asd_values_tidy$Wavelength==myNIR], 
                asd_values_tidy$value[asd_values_tidy$Wavelength==myRED]))
  
  indexes <- bind_cols(indexes, calcMSAVI)
  indexes
  
#if you want to save, you can use this as an example
#write_csv(indexes, paste0(output_path_for_csv, "/", output_filename_for_csv, ".csv"))

indexes_tidy <- indexes %>%
  pivot_longer(cols = -spectrum_name, names_to = "index_name")%>%
  mutate(sample_name = str_split(spectrum_name, pattern = "-", simplify=T)[,4]) #note the string split sample number/name came out as 4 but this could change
indexes_tidy

indexes_mean_sd <- indexes_tidy%>%
  group_by(sample_name, index_name)%>%
  summarise(mean = mean(value), 
            sd = sd(value))

#### plot options

asd_all <- ggplot(data=asd_values_tidy, aes(x=Wavelength, y=value, color=spectrum_name)) +
  geom_line()+
  theme_bw()

asd_all

asd_mean <- ggplot(data=asd_mean_sd, aes(x=Wavelength, y=mean, color=sample_name)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), colour="gray", width=.1) +
  geom_line()+
  theme_bw()

asd_mean

index_bar <- ggplot(data=indexes_mean_sd, aes(x=index_name, y=mean, fill=sample_name))+
  geom_col(position=position_dodge(0.8), width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), colour="black", width=.1, position=position_dodge(0.8)) +
  theme_bw()

index_bar


##### preparation####
install.packages("car")
install.packages("BAS")
install.packages("Bolstad")
install.packages("BayesFactor")
install.packages("psych")

library(Bolstad)
library(BayesFactor)
library(tidyr)
library(car)
library(dplyr)
library(psych)
library(BAS)
###

##### preparing data ####
rm(list = ls())
data <- read.csv("W2_dataset.csv")  # read csv file 
data <- data %>% drop_na("politics")
data$politics <- as.numeric(data$politics)
  # making a new variable for general moral
  g_moral <- data.frame(data$harm1,data$harm2,data$harm3,data$fairness1,data$fairness2,data$fairness3,data$Ingroup1,data$Ingroup2,data$Ingroup3,data$Authority1,data$Authority2,data$Authority3,data$Purity1,data$Purity2,data$Purity3)
  data$g_moral <- rowMeans((g_moral),na.rm = TRUE)
###
#### question 1 ####
  #simple regression
  lm.a <- lm((politics ~ g_moral) , data)
  print(lm.a)
  summary(lm.a)
  ###
  
#### question 2 ####
  # preparing data
    # recode the inverse scale(R = reverse, FR= fixed reverse)
    data$O_FR <- 8-data$O_R
    data$C_FR <- 8-data$C_R
    #making new variables for openness & con
    g_op <- data.frame(data$O,data$O_FR)
    data$g_op <- rowMeans(g_op,na.rm = TRUE)
    data$g_op_ce <- (data$g_op - mean(data$g_op,na.rm = TRUE)) #centered
    
    g_co <- data.frame(data$C,data$C_FR)
    data$g_co <- rowMeans(g_co,na.rm = TRUE)
    data$g_co_ce <- (data$g_co - mean(data$g_co,na.rm = TRUE)) #centered
   

    # statistical analysis
    #option dor
    lm.po_op <- lm(politics ~ g_op_ce, data) #simple regression op
    summary(lm.po_op)
    
    lm.po_co <- lm(politics ~ g_co_ce, data) #simple regression co
    summary(lm.po_co)
    
    ### interaction with weird
    data$mul_we_op <- data$g_op_ce*data$Weird
    lm.po_op.wem <- lm(politics ~ mul_we_op,data)
    summary(lm.po_op.wem)
    
    data$mul_we_co <- data$g_co_ce*data$Weird
    lm.po_co.wem <- lm(politics ~ mul_we_co,data)
    summary(lm.po_co.wem)

#### question 3 ####
    #tolerence
    lm.t <- lm(g_moral ~ g_op + g_co,data)
    
    tolerance <- 1-(summary(lm.t)$r.squared)
    tolerance
    
    #bayesian check
    g_auth <- data.frame(data$Authority1,data$Authority2,data$Authority3)
    data$g_auth <- rowMeans(g_auth,na.rm = TRUE)
    
    data1 <- na.omit(data, cols=c("g_op", "g_co", "Weird", "g_auth"))
    null_model <- generalTestBF(politics ~ g_op+g_co+Weird +  g_auth,data1,whichModels = "top")
    null_model
    
#### question 4 ####
    
    subjwell_ce <- (data$subjwell-mean(data$subjwell,na.rm = TRUE)) #centered subjectwell varaible
    data$crt <- rowSums(data.frame(data$cogref.1,data$cogref.2,data$cogref.3)) #create combined crt variable
    data$crt_ce <- (data$crt - mean(data$crt,na.rm = TRUE)) #centered
    #multiple regression 
    
    lm.4 <- lm(politics~subjwell_ce + crt_ce + subjwell_ce*crt_ce, data=data)
    summary(lm.4)
    
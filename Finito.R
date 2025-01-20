
#Final analysis 21 cases 3 conditions

rm(list = ls())
library(QCA)
library(SetMethods)
install.packages('moments')
library(moments)

setwd("~/Documents/RData")

Rudi <- read.csv("Last_M_Data.csv", row.names = 1)

#########------Calibration Full Coherence----------#######
# Cohe: raw data to FULCO: calibrated data
Rudi$FULCO <- calibrate(Rudi$Cohe, 
                        type = "fuzzy",
                        method = "direct", 
                        c(0, 50, 100) 
)

#########------Calibrate High Communicability-------######
# Comm: raw data  to HICOM: calibrated data

Rudi$HICOM <- calibrate(Rudi$Comm, 
                        type = "fuzzy",
                        method = "direct", 
                        c(0, 6.5, 13) 
)

#########------Calibrating Active Participation------######
# APART. Already calibrated with direct assignment
#########------Calibrating Satisfactory Experience------######
# SATEX. Already calibrated with direct assignment

########--------------Adjustments-----------#########
# Round to two digits
rRudi <- round(Rudi, digits = 2)


# Eliminate raw data colunms
RudiCAL = subset(rRudi, select = -c(Cohe,Comm) )

# Check skewness
skew.check(RudiCAL, hist = TRUE)

###################################
#Descriptive statistics

skewness(RudiCAL$SATEX)
#-0.2889553
skewness(RudiCAL$APART)
#P1 -0.3464498

skewness(RudiCAL$FULCO)
#P1 -0.4599594 -0.7999131

skewness(RudiCAL$HICOM)
# -0.5699463

#(-0.5, 0.5) — low or approximately symmetric.
#(-1, -0.5) U (0.5, 1) — moderately skewed.
#Beyond -1 and 1 — Highly skewed.

kurtosis(RudiCAL$SATEX)
# 2.325434
kurtosis(RudiCAL$APART)
#2.030864
kurtosis(RudiCAL$FULCO)
#1.695813  2.166472
kurtosis(RudiCAL$HICOM)
# 1.868816

################


#-----------------------------------------------------------------------#
# ----------------------- Analysis of necessity ------------------------#
#-----------------------------------------------------------------------#

#########------Single necessary conditions-------####

QCAfit(x= RudiCAL[,c("APART","HICOM","FULCO")], 
       y= RudiCAL$SATEX
)
# No single condition is necessary for the outcome ConS<0.9


#########------SUIN combinations---------####
#Necessary combinations of conditions
DIS<-superSubset(data= RudiCAL,
                 outcome = "SATEX",
                 conditions = c("APART","HICOM","FULCO"),
                 incl.cut = 0.9,
)
DIS
#                    inclN   RoN   covN  
#------------------------------------- 
# 1   ~APART + HICOM  0.908  0.523  0.689 
# 2  APART + HICOM   0.901  0.580  0.713 
# 3  APART + FULCO   0.924  0.372  0.637   0.351  0.629 
# 4  HICOM + FULCO   0.950  0.443  0.677  0.953  0.372  0.651 
#------------------------------------- 

# No combination is necessary, RoN is too low

#Specifying cuts
DIS2<-superSubset(data= RudiCAL,
                  outcome = "SATEX",
                  conditions = c("APART","HICOM","FULCO"),
                  incl.cut = 0.9,
                  ron.cut = 0.6,
                  cov.cut = 0.6
)

# No combinations found

#-----------------------------------------------------------------------#
# ----------------------- Analysis of sufficiency ----------------------#
#-----------------------------------------------------------------------#

####-----Truth table for sufficient conditions--------####
iTT <- truthTable(RudiCAL, "SATEX", 
                  complete = TRUE, 
                  show.cases = TRUE,
                  n.cut = 1,
                  incl.cut = 0.8, 
                  pri.cut = 0.6,
                  sort.by = "incl, n"
)
iTT

stargazerTT(iTT,
            show.cases = TRUE,
            type = "latex", 
            digits = 3
)

#####-----Conservative solution--------####
C_sol <- minimize(iTT, 
                  include = "1", 
                  details = TRUE
)

C_sol

stargazerSol(results = C_sol, 
             outcome = "SATEX",
             type = "latex",
             show.cases = TRUE
)
pimplot(data = RudiCAL,
        outcome = "SATEX",
        results = C_sol,
        all_labels = TRUE,
        jitter = TRUE)

#####-----Parsimonious solution--------####
P_sol <- minimize(iTT, 
                  include = "?", 
                  details = TRUE
)
P_sol 
P_sol$SA


#########################################################################
#-----------------------------------------------------------------------#
# ----------------------- Analysis negated outcome ---------------------#
#-----------------------------------------------------------------------#

#######------Single necessary conditions ~outcome------####

QCAfit(x= RudiCAL[,c("APART","HICOM","FULCO")], 
       y= RudiCAL$SATEX, neg.out= TRUE
)

########------SUIN combinations ~outcome----####

nDIS<-superSubset(data= RudiCAL,
                  outcome = "~SATEX",
                  conditions = c("APART","HICOM","FULCO"),
                  incl.cut = 0.9,
                
)

nDIS

#                           inclN   RoN   covN  
#---------------------------------------------- 
# 1   APART + ~HICOM         0.908  0.422  0.529   
# 2  ~HICOM + FULCO          0.945  0.484  0.573  0.938  0.403  0.534 
# 3  ~APART + HICOM + FULCO  0.914  0.267  0.472  0.960  0.420  0.551
#---------------------------------------------- 

# No combination is necessary for ~outcome

########----------Truth table ~outcome----------####
naTT <- truthTable(RudiCAL, "~SATEX", 
                    complete = TRUE, 
                    show.cases = TRUE,
                    n.cut = 1,
                    incl.cut = 0.79,
                    pri.cut = 0.6,
                    sort.by = "incl, n"
)
naTT

stargazerTT(naTT,
            show.cases = TRUE,
            type = "latex", 
            digits = 3
)

#####-----Conservative solution ~outcome--------####

nC_sol <- minimize(naTT, 
                   include = "1", 
                   details = TRUE
)
nC_sol

pimplot(data = RudiCAL,
        outcome = "SATEX",
        results = nC_sol,
        all_labels = TRUE,
        jitter = TRUE)

stargazerSol(results = nC_sol, 
             outcome = "~SATEX",
             type = "latex",
             show.cases = TRUE
)
nC_sol$pims

stargazerSol(results = nP_sol, 
             outcome = "SATEX",
             type = "latex",
             show.cases = TRUE
)
##################################

#####---------Adding results to dataframe-------######
Susi <- RudiCAL
Susi$P1<- C_sol$pims$`APART*HICOM`
Susi$P2<- C_sol$pims$`FULCO*HICOM`
Susi$CSOL<- fuzzyor(Susi$P1,Susi$P2)
Susi$NP1<- (1-RudiCAL$APART)
Susi$NP2<- (1-RudiCAL$HICOM)
Susi$NCSOL<- fuzzyand(Susi$NP1,Susi$NP2)
Susi$NSATEX <- (1-RudiCAL$SATEX)

write.csv(Susi,"Susi.csv")


#########################################################################
#-----------------------------------------------------------------------#
#---------------------------Robustness tests----------------------------#
#-----------------------------------------------------------------------#

TO <- c("APART","FULCO","HICOM")

######################        IS         #################################
####-------For calibration of FULCO-------#

rob.calibrange(raw.data = Rudi,
               calib.data = RudiCAL,
               test.cond.raw = "Cohe",
               test.cond.calib = "FULCO",
               test.thresholds = c(0,50,100),
               step = 0.25,
               max.runs = 200,
               outcome = "SATEX",
               conditions = TO,
               incl.cut = 0.8,
               n.cut = 1,
               include = "?"
)
#Exclusion:  Lower bound  NA Threshold  0 Upper bound  31 
#Crossover:  Lower bound  6 Threshold  50 Upper bound  70.75 
#Inclusion:  Lower bound  NA Threshold  100 Upper bound  NA 
#
#
####-------For calibration of HICOM-------#
rob.calibrange(raw.data = Rudi,
               calib.data = RudiCAL,
               test.cond.raw = "HICOM",
               test.cond.calib = "Comm",
               test.thresholds = c(0,6.5,13),
               step = 0.1,
               max.runs = 200,
               outcome = "SATEX",
               conditions = TO,
               incl.cut = 0.8,
               n.cut = 1,
               include = "?"
)
#Exclusion:  Lower bound  NA Threshold  0 Upper bound  NA 
#Crossover:  Lower bound  0.200000000000006 Threshold  6.5 Upper bound  6.5
#Inclusion:  Lower bound  NA Threshold  13 Upper bound  NA 

####-------Sensitivity range consistency------#
rob.inclrange(data = Rudi,
              step = 0.01,
              max.runs = 200,
              outcome = "SATEX",
              conditions = TO,
              incl.cut = 0.8,
              n.cut = 1,
              include = "?"
)
#Raw Consistency T.:  Lower bound  0.79 Threshold  0.8 Upper bound  0.82 
####-------Sensitivity range  frequency------#
rob.ncutrange(data = Rudi,
              step = 1,
              max.runs = 200,
              outcome = "SATEX",
              conditions = TO,
              incl.cut = 0.8,
              n.cut = 1,
              include = "?"
)
#N.Cut:  Lower bound  1 Threshold  1 Upper bound  2 

######################        InS         #################################
####-------For calibration of FULCO-------#
rob.calibrange(raw.data = Rudi,
               calib.data = RudiCAL,
               test.cond.raw = "Cohe",
               test.cond.calib = "FULCO",
               test.thresholds = c(0,50,100),
               step = 0.25,
               max.runs = 200,
               outcome = "~SATEX",
               conditions = TO,
               incl.cut = 0.79,
               n.cut = 1,
               include = "?"
)             
#Exclusion:  Lower bound  NA Threshold  0 Upper bound  15 
#Crossover:  Lower bound  16.5 Threshold  50 Upper bound  58.5 
#Inclusion:  Lower bound  69.75 Threshold  100 Upper bound  NA 
####-------For calibration of HICOM-------#
rob.calibrange(raw.data = Rudi,
               calib.data = RudiCAL,
               test.cond.raw = "HICOM",
               test.cond.calib = "Comm",
               test.thresholds = c(0,6.5,13),
               step = 0.1,
               max.runs = 200,
               outcome = "~SATEX",
               conditions = TO,
               incl.cut = 0.79,
               n.cut = 1,
               include = "?"
)
#Exclusion:  Lower bound  NA Threshold  0 Upper bound  NA 
#Crossover:  Lower bound  0.200000000000006 Threshold  6.4 Upper bound  6.4 
#Inclusion:  Lower bound  NA Threshold  13 Upper bound  NA 

####-------Sensitivity range consistency------#
rob.inclrange(data = Rudi,
              step = 0.01,
              max.runs = 200,
              outcome = "~SATEX",
              conditions = TO,
              incl.cut = 0.79,
              n.cut = 1,
              include = "?"
)
#Raw Consistency T.:  Lower bound  0.77 Threshold  0.79 Upper bound  0.79 

####-------Sensitivity range  frequency------#
rob.ncutrange(data = Rudi,
              step = 1,
              max.runs = 200,
              outcome = "~SATEX",
              conditions = TO,
              incl.cut = 0.79,
              n.cut = 1,
              include = "?"
)               

#N.Cut:  Lower bound  1 Threshold  1 Upper bound  1 
########################################################              
###########------- Analysis excluding cases-------######

# Excluding Salado ATCC Montes
TRudi<-RudiCAL[-c(1,2,12),]

####------- New truth table-------#
tiTT<- truthTable(TRudi, "SATEX", 
                  complete = TRUE, 
                  show.cases = TRUE,
                  n.cut = 1,
                  incl.cut = 0.8, 
                  pri.cut = 0.6,
                  sort.by = "incl, n"
)
tiTT
# n=1 excludes Argelia
stargazerTT(tiTT,
            show.cases = TRUE,
            type = "latex", 
            digits = 3
)

#####------- Conservative solution-------###
tC_sol <- minimize(tiTT, 
                   include = "1", 
                   details = TRUE
)
tC_sol

################Negated outcome same exclusions############
tnTT<- truthTable(TRudi, "SATEX", 
                  neg.out = TRUE,
                  complete = TRUE, 
                  show.cases = TRUE,
                  n.cut = 1,
                  incl.cut = 0.79, 
                  pri.cut = 0.6,
                  sort.by = "incl, n")
tnTT

stargazerTT(tnTT,
            show.cases = TRUE,
            type = "latex", 
            digits = 3
)

#####------- Conservative solution-------###
tnC_sol <- minimize(tnTT, 
                    include = "1", 
                    details = TRUE
)
tnC_sol            

########################### F I N I T O ############################


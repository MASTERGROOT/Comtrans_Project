# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_SP",
  modelDescr      = "Simple MNL model on mode choice SP data",
  indivID         = "ID",
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

path <- "./R_code//RunApollo.csv"

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database = read.csv(path, header=TRUE, sep = ",")
### for data dictionary, use ?apollo_modeChoiceData

### Use only SP data
database = subset(database,database$SP==1)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(ASC_BUS  = 0,
              ASC_WALK      = 0,
              ASC_MOTOBIKE      = 0,
              b_tt_BUS     = 0,
              b_tt_WALK     = 0,
              b_tt_MOTOBIKE     = 0,
              b_cost     = 0,
              b_air     = 0,
              b_shade     = 0,
              b_helmet     = 0,
              b_pavement     = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("ASC_WALK","ASC_MOTOBIKE")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["BUS"]]  = ASC_BUS  + (b_tt_BUS * time_bus)  + (b_cost * cost_bus) + (b_air * Attbus_air) 
  V[["WALK"]]  = ASC_WALK  + (b_tt_WALK  * time_walk) + (b_shade * Attwalk_shade) + (b_pavement * Attwalk_pavement) 
  V[["MOTOBIKE"]]  = ASC_MOTOBIKE  + (b_tt_MOTOBIKE  * time_mc)   + (b_cost * cost_mc) + (b_helmet * Attmc_helmet) 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(WALK=1, BUS=2, MOTOBIKE=3), 
    avail         = list(BUS=av_bus, WALK=av_walk, MOTOBIKE=av_mc), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model)


apollo_saveOutput(model)
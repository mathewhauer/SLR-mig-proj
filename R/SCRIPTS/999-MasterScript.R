## Loading the workspace and setting up the computing environment
source("./R/SCRIPTS/000-Libraries.R")

## Loading in a list of FIPS codes.
source('./R/SCRIPTS/001-fipscodes.R')

## Outlier Detection
source('./R/SCRIPTS/101-AnomalyDetection.R')

## Displacement Model and Evaluation
source('./R/SCRIPTS/102-DispModelMakeEval.R')

## Calculating the at-risk Population and Generating the Reductions in CCR
source('./R/SCRIPTS/201-AtRiskCalculation26.R')
source('./R/SCRIPTS/201-AtRiskCalculation45.R')
source('./R/SCRIPTS/201-AtRiskCalculation85.R')

##### Setting Up the Projections
## Building and projecting the Origin-Destination Matrices
source('./R/SCRIPTS/301-MigrationMatricies.R')

## Projecting the Fertility Rates
source('./R/SCRIPTS/302-state_level_fert_proj.R')

## Projecting the CCRs
source('./R/SCRIPTS/303-ProjectingCCRs.R')

## Making the S and MS matrices
source('./R/SCRIPTS/304-MakingtheSandMSmatrices26.R')
source('./R/SCRIPTS/304-MakingtheSandMSmatrices45.R')
source('./R/SCRIPTS/304-MakingtheSandMSmatrices85.R')

######### Population Projections
source('./R/SCRIPTS/401-PROJECTIONS26.R')
source('./R/SCRIPTS/401-PROJECTIONS45.R')
source('./R/SCRIPTS/401-PROJECTIONS85.R')

## Controlled to the SSPs
source('./R/SCRIPTS/402-ProjectionsControlled_eae.R')

##### Figures
# Multipanel figure
source('./R/501-Figure1ProjLines.R')
# Stuck Populations
source('./R/502-FigStuck.R')
# Map of Pop Change
source('./R/503-MapPopChange.R')
# Text Statistics
source('./R/504-TextStatistics.R')
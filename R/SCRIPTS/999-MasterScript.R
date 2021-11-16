## Loading the workspace and setting up the computing environment
source("./R/SCRIPTS/000-Libraries.R")

## Loading in a list of FIPS codes.
source('./R/SCRIPTS/001-fipscodes.R')

## Outlier Detection
source('./R/SCRIPTS/101-AnomalyDetection.R')

## Displacement Model and Evaluation
source('./R/SCRIPTS/102-DispModelMakeEval.R')

## Calculating the at-risk Population and Generating the Reductions in CCR
source('./R/SCRIPTS/201-AtRiskCalculation.R')

##### Setting Up the Projections
## Building and projecting the Origin-Destination Matrices
source('./R/SCRIPTS/301-MigrationMatricies.R')

## Projecting the Fertility Rates
source('./R/SCRIPTS/302-state_level_fert_proj.R')

## Projecting the CCRs
source('./R/SCRIPTS/302-ProjectingCCRs.R')

## Making the S and MS matrices
source('./R/SCRIPTS/304-MakingtheSandMSmatrices.R')
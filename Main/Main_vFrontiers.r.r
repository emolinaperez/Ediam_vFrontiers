#cloud
 root<-"C:\\~TechChange-RDM\\"
 Number.Cores<-32

## =================================================================================================================================================
## This section creates the Experimental Design Based on Input Tables
## =================================================================================================================================================
  dir.exp.inputs<-paste(root,"RDM Inputs\\",sep="")
  Limits.File<-"Limits.csv"
  Policies.File<-"Policies.csv"
  Climate.File<-"Climate.csv"
  sample.size<-300
  Policy.Switch<-TRUE
  Climate.Switch<-TRUE
  source(paste(dir.exp.inputs,"create_experiment_function.r",sep=""))
  Exp.design<-exp.design.table(dir.exp.inputs,Limits.File,sample.size,Policies.File,Policy.Switch,Climate.File,Climate.Switch)
  write.csv(Exp.design, paste(dir.exp.inputs, "Exp.design.csv", sep=""), row.names=FALSE)

## ==================================================================================================================================================
## This section runs the model across the experimental design and prints an output file for each run
## ==================================================================================================================================================
#Source Model
  dir.model<-paste(root,"TechChange Model\\",sep="")
  model.version<-"InternationalGreenTechChangeModel_10_22_2015.r"
  source(paste(dir.model,model.version,sep=""))
#Source Experimental Design
  dir.exp<-paste(root,"RDM Inputs\\",sep="")
  experiment.version<-"Exp.design.csv"
  Exp.design<-read.csv(paste(dir.exp,experiment.version,sep=""))
#Define directory to print output files
  dir.harness<-paste(root,"RDM Harness\\",sep="")
#Clean output folder
  do.call(file.remove,list(paste(dir.harness,list.files(dir.harness, pattern="*.csv", full.names=FALSE),sep="")))
#Set up parallel environment
#Run Model in Parallel
  library(snow,lib=paste(root,"Rlibraries\\",sep=""))
  library(deSolve,lib=paste(root,"Rlibraries\\",sep=""))
  library(optimx,lib=paste(root,"Rlibraries\\",sep=""))
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("Exp.design","TechChangeMod","dir.harness","dede","lagderiv","lagvalue","optimx") # dede, lagderiv are functions od deSolve
  clusterExport(cl,global.elements,envir=environment())
 #Execute code
  parApply(cl,Exp.design,1,function(x) {
                                     params<-c(
                                     S.0 = as.numeric(x['S.0']),
                                     TimeStep = as.numeric(x['TimeStep']),
                                     EndTime = as.numeric(x['EndTime']),
                                     alfa = as.numeric(x['alfa']),
                                     epsilon = as.numeric(x['epsilon']),
                                     Gamma.re = as.numeric(x['Gamma.re']),
                                     k.re = as.numeric(x['k.re']),
                                     Gamma.ce = as.numeric(x['Gamma.ce']),
                                     k.ce = as.numeric(x['k.ce']),
                                     Eta.re = as.numeric(x['Eta.re']),
                                     Eta.ce = as.numeric(x['Eta.ce']),
                                     Nu.re = as.numeric(x['Nu.re']),
                                     Nu.ce = as.numeric(x['Nu.ce']),
                                     qsi = as.numeric(x['qsi']),
                                     Delta.S = as.numeric(x['Delta.S']),
						             Delta.Temp.Disaster = as.numeric(x['Delta.Temp.Disaster']),
						             Beta.Delta.Temp = as.numeric(x['Beta.Delta.Temp']),
						             CO2.base = as.numeric(x['CO2.base']),
						             CO2.Disaster = as.numeric(x['CO2.Disaster']),
                                     labor.growth_N = as.numeric(x['labor.growth_N']),
						             labor.growth_S = as.numeric(x['labor.growth_S']),
                                     lambda.S = as.numeric(x['lambda.S']),
						             sigma.utility = as.numeric(x['sigma.utility']),
						             rho = as.numeric(x['rho']),
                                     Yre.0_N = as.numeric(x['Yre.0_N']),
                                     Yce.0_N = as.numeric(x['Yce.0_N']),
                                     Yre.0_S = as.numeric(x['Yre.0_S']),
                                     Yce.0_S = as.numeric(x['Yce.0_S']),
						             size.factor = as.numeric(x['size.factor']),
						             Run.ID = as.numeric(x['Run.ID']),
									 policy.name = as.character(x['policy.name']),
						             dir.harness = dir.harness);

#Policy dimensions
#tax north
 ceN.0<-ifelse(x['Climate.Model']%in%c("GFDL-ESM2G","GFDL-ESM2M")==TRUE,0.30,
        ifelse(as.numeric(x['epsilon'])<8,0.25,
 		ifelse(as.numeric(x['epsilon'])<9,0.20,
 		ifelse(as.numeric(x['epsilon'])<10,0.15,0.10))));#initial tax north
 ceN.m<-0.05 ;#min tax north
 ceN.M<-0.5 ;#max tax north
 #ceN.M<-1.0 ;#max tax north
#tax south
 ceS.0<-ifelse(x['Climate.Model']%in%c("GFDL-ESM2G","GFDL-ESM2M")==TRUE,0.30,
        ifelse(as.numeric(x['epsilon'])<8,0.25,
		ifelse(as.numeric(x['epsilon'])<9,0.20,
 		ifelse(as.numeric(x['epsilon'])<10,0.15,0.10)))) ;#initial tax south
 ceS.m<-0.05 ;#min tax south
 ceS.M<-0.5 ;#max tax south
 #ceS.M<-1.0 ;#max tax south
#Tech Subsidy North
 tN.0<-0.10 ;#initial Tech Subsidy North
 tN.m<-0.01 ;#min tax Tech Subsidy North
 tN.M<-0.15 ;#max tax Tech Subsidy North
#RD Subsidy North
 sN.0<-2.0 ;#initial RD Subsidy North
 sN.m<-0.5 ;#min tax RD Subsidy North
 sN.M<-3.0 ;#max tax RD Subsidy North
#Tech Subsidy South
 tS.0<-0.05 ;#initial Tech Subsidy South
 tS.m<-0.01 ;#min tax Tech Subsidy South
 tS.M<-0.15 ;#max tax Tech Subsidy South
#RD Subsidy South
 sS.0<-0.50 ;#initial RD Subsidy South
 sS.m<-0.01 ;#min tax RD Subsidy South
 sS.M<-3.0 ;#max tax RD Subsidy South

if (x['policy.name']=="FWA")
 {
 TechChangeMod(c(0.0,0.0,0.0,0.0),params)

 } else{
       if (x['policy.name']=="Nordhaus")
        {
          optimx(c(ceN.0,ceS.0), TechChangeMod, lower=c(ceN.m,ceS.m), upper=c(ceN.M,ceS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01),parscale=c(10,10),maxit=200000),params=params)
         } else {
		 if (x['policy.name']=="Nordhauds+TechnologyPolicy")
		 {
		  optimx(c(ceN.0,ceS.0,tN.0,sN.0), TechChangeMod, lower=c(ceN.m,ceS.m,tN.m,sN.m), upper=c(ceN.M,ceS.M,tN.M,sN.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(10,10,3,3),maxit=200000),params=params)
		 } else {
		   if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund")
		  {
		   	optimx(c(ceN.0,tN.0,sN.0,tS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m), upper=c(ceN.M,tN.M,sN.M,tS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(10,3,3,3),maxit=200000),params=params)
		  } else {
		    if (x['policy.name']=="Nordhaus+R&DGreenClimateFund")
		     {
			  optimx(c(ceN.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01),parscale=c(10,3,3,3,3),maxit=200000),params=params)
			 } else {
			  if (x['policy.name']=="Nordhaus+TechnologyPolicy.Both")
		      {
			   optimx(c(ceN.0,ceS.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,ceS.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,ceS.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(10,10,3,3,3,3),maxit=200000),params=params)
			  } else {
			     if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund+R&DS")
		        {
			      optimx(c(ceN.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01),parscale=c(10,3,3,3,3),maxit=200000),params=params)
				} else {
			      if (x['policy.name']=="Nordhaus+CoR&DGreenClimateFund")
		           {
			         optimx(c(ceN.0,tN.0,sN.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,sS.m), upper=c(ceN.M,tN.M,sN.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(10,3,3,3),maxit=200000),params=params)
				   } else {
				     if (x['policy.name']=="Nordhaus+CoR&DGreenClimateFund+TecS")
		              {
			            optimx(c(ceN.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01),parscale=c(10,3,3,3,3),maxit=200000),params=params)
				      } else { "NA" }}}}}}}}}
	})
   #Stop cluster
   stopCluster(cl)



## =====================================================================================================
## This section reads the output of simulations and reshapes into a format ready for scenario discovery
## =====================================================================================================
  Number.Cores<-4
 #Define directory parameters
  dir.inputs<-paste(root,"RDM Inputs\\",sep="")
  dir.harness<-paste(root,"RDM Harness\\",sep="")
  dir.output<-paste(root,"RDM Outputs\\",sep="")
 #load needed libraries
  library(reshape2,lib= paste(root,"Rlibraries\\",sep=""))
  library(data.table,lib=paste(root,"Rlibraries\\",sep=""))
  library(snow,lib=paste(root,"Rlibraries\\",sep=""))
#create vector with file names
  filenames <- list.files(dir.harness, pattern="*.csv", full.names=FALSE)
#source function to process harnessed output data
  source(paste(dir.inputs,"harness_processing.r",sep=""))
#run post-processing in parallel
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("dir.harness","process.prim.data","data.table")
  clusterExport(cl,global.elements,envir=environment())
  prim.data <- parLapply(cl,filenames, function(x){data.table(process.prim.data(x,dir.harness))} )
  stopCluster(cl)
  prim.data<-rbindlist(prim.data)
#merge data with experimental design
  experiment.version<-"Exp.design.csv"
  prim.data<-merge.exp.design(dir.inputs,experiment.version,prim.data)
#create future without action consumption
  prim.data.fwa<-subset(prim.data,prim.data$policy.name=="FWA")
  prim.data.fwa<-prim.data.fwa[,c("Future.ID","Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S","Climate.Coef","CO2.GrowthRate","Delta.Temp.GrowthRate","Consumption.Total_N.300","Consumption.Total_S.300"),with=FALSE]
  setnames( prim.data.fwa,c("Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S","Climate.Coef","CO2.GrowthRate","Delta.Temp.GrowthRate","Consumption.Total_N.300","Consumption.Total_S.300"),c("Y.Total_N.fwa","Y.Total_S.fwa","Consumption.Total_N.fwa","Consumption.Total_S.fwa","Climate.Coef.fwa","CO2.GrowthRate.fwa","Delta.Temp.GrowthRate.fwa","Consumption.Total_N.300.fwa","Consumption.Total_S.300.fwa"))
  prim.data<-merge(prim.data,prim.data.fwa,by="Future.ID")
#standarize relative values
  prim.data$Z.Relative.Gamma<-prim.data$Gamma.re/prim.data$Gamma.ce
  prim.data$Z.Relative.Gamma<-scale(prim.data$Z.Relative.Gamma, center=TRUE, scale=TRUE)
  prim.data$Z.Relative.Eta<-prim.data$Eta.re/prim.data$Eta.ce
  prim.data$Z.Relative.Eta<-scale(prim.data$Z.Relative.Eta, center=TRUE, scale=TRUE)
  prim.data$Z.Relative.Nu<-prim.data$Nu.re/prim.data$Nu.ce
  prim.data$Z.Relative.Nu<-scale(prim.data$Z.Relative.Nu, center=TRUE, scale=TRUE)
  prim.data$Z.epsilon<-scale(prim.data$epsilon, center=TRUE, scale=TRUE)

  #write.csv(prim.data, paste(dir.output, "prim.data_7_06_2015.csv", sep=""), row.names=FALSE)
  write.csv(prim.data, paste(dir.output, "prim.data_extras_seminar.csv", sep=""), row.names=FALSE)

## =====================================================================================================
## This section reads the output of simulations and reshapes it into time series split by region,
## =====================================================================================================
Number.Cores<-18
#Define directory parameters
 dir.inputs<-paste(root,"RDM Inputs\\",sep="")
 dir.harness<-paste(root,"RDM Harness\\",sep="")
 dir.output<-paste(root,"RDM Outputs\\",sep="")

#crate vector with file names
 experiment.version<-"Exp.design.csv"
 #experiment.version<-"Exp.design_with_control_runs.csv"
 #experiment.version<-"Exp.design_defense_seminar_extras.csv"
 filenames <- list.files(dir.harness, pattern="*.csv", full.names=FALSE)
#source function to process harnessed output data
 source(paste(dir.inputs,"harness_processing.r",sep=""))
#run post-processing in parallel
  library(data.table,lib=paste(root,"Rlibraries\\",sep=""))
  library(snow,lib=paste(root,"Rlibraries\\",sep=""))
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("dir.inputs","experiment.version","dir.harness","process.harness.data")
  clusterExport(cl,global.elements,envir=environment())
  modelruns <- parLapply(cl,filenames, function(x){process.harness.data(x,dir.inputs,experiment.version,dir.harness)} )
  stopCluster(cl)
  modelruns<-rbindlist(modelruns)
#print time series for model
  write.csv(modelruns, paste(dir.output, "model.runs_7_09_2015.csv", sep=""), row.names=FALSE)

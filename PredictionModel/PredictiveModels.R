
#Load libraries 
source("DataProcessing.R")
source("Functions_NCOV.R")
#head(Local_information)

#Visualize it 
library(ggplot2)
library(reshape)
#install.packages("translateR")
library(translateR)
#install.packages("showtext")
library("showtext")



#Load data 
DateSource=read.csv("../results/OrganizedData.csv")

AllProvinces=DateSource[DateSource$Location!="WholeChina",]


AllProvinces=as.data.frame(AllProvinces)
head(AllProvinces)
AllProvinces$Location=as.character(AllProvinces$Location)
AllProvinces$Date=as.Date(as.character(AllProvinces$Date),origin = "1970-01-01")
max(AllProvinces$Date) 


library(dplyr)

AllProvinces=as.data.frame(AllProvinces)
head(AllProvinces)
AllProvinces$Date=as.Date(as.character(AllProvinces$Date),origin="1970-01-01")
nrow(AllProvinces)




#Set model details 
Lags=3
daysahead=1
UseR0_as_predictor=F
B_global_loops=20
Normalization=T
First_index_date=as.Date("2020-02-01",origin = "1970-01-01")
#First_index_date=NULL
FeatureSelection=F
GroundTruthName="New_confirmed"
FirstDate_use_data=as.Date("2020-01-23",origin = "1970-01-01")
#FirstDate_use_data=as.Date("2020-01-23",origin = "1970-01-01")
Aggregation=2
Binary=F
Clustering=F
Augmentation=T
ExperimentName="Autoregression_without_clustering_with_augmentation "
#AllProvinces=AllProvinces[AllProvinces$Date>=FirstDate_use_data,]

  #ARGO 
  #Information_to_include=c("New_confirmed")
  Information_to_include=c("New_confirmed","Cumulative_confirmed","New_suspect","Cumulative_suspect")
  #"Search_index_coronavirus_symptom","MediaCloudCount"
  #"Search_index_coronavirus_symptom","MediaCloudCount","New_suspect","Cumulative_suspect",
  #"Cumulative_confirmed","Search_index_how_many_degree_is_fever","Search_index_symptom_of_fever","Mechanistic_pred"
 # "New_death","New_confirmed","MediaCloudCount","New_suspect","AllMobilityInbound","AllMobilityFromWuhan"m"Mechanistic_pred"
  #"New_suspect","Cumulative_confirmed","Search_index","Cumulative_suspect","MediaCloudCount", 
  if (Lags>0){
  for (Lag in 1:Lags){
    Information_to_include=c(Information_to_include,
                             paste0(GroundTruthName,'_Lag_',Lag)
                             )
    
  }
  }
  
  
  

  

  #Train the model 

   
  DataForRegression=AllProvinces
  
  All_X=NULL
  All_Y=NULL
  All_Results=NULL
  All_Outputs=NULL
  All_VariableImportance=NULL
  
  AllPrediction_in_sample=NULL
  for (Loop in 1:B_global_loops){
  print(paste("the last day of data",max(DataForRegression$Date)))  
  Output=Run_models(DataForRegression,Lags=Lags,daysahead=daysahead,Information_to_include,
                    GroundTruthName=GroundTruthName,UseR0_as_predictor=UseR0_as_predictor,
                    First_index_date=First_index_date,Normalization=Normalization,
                    FeatureSelection =FeatureSelection ,Aggregation=Aggregation,Binary=F,
                    Clustering=Clustering,Augmentation=Augmentation)
  #
    
    
    Results=Output$Results
    #range(Results$Y_test,na.rm = T)
    
    X=Output$X
    Y=Output$Y
    
    #in sample prediction for visualization purpose 
    library(glmnet)
    LastModel=Output$LastModel
    method="lambda.1se"
    X_vec=X[,names(X)%in%Information_to_include]
    Prediction_in_sample=predict(LastModel[[1]],type = "class",newx=as.matrix(X_vec),s=c(method))
    Prediction_in_sample=data.frame(Prediction_in_sample=Prediction_in_sample,
                                    date_T=X$Date,
                                    location=X$Location,
                                    Round=Loop)
    AllPrediction_in_sample=rbind(AllPrediction_in_sample,Prediction_in_sample)
    #importance of variable 
    
    VariableImportance=Output$VaraibleImportance

    
    
    Vec=Reduce(lapply(VariableImportance,function(x){Reduce(x,f = rbind)}),f = rbind)
    All_VariableImportance[[length(All_VariableImportance)+1]]=Vec
    
    
    
    Results$Run=Loop 
    X$Run=Loop 
    Y$Run=Loop 
    #VariableImportance$Run=Loop 
    
    All_Results=rbind(All_Results ,Results)
    All_Y=rbind(All_Y,Y)
    All_X=rbind(All_X,X)
    #All_VariableImportance=rbind(All_VariableImportance,VariableImportance)
    #AllImportance[[length(AllImportance)+1]]=VariableImportance
    
  
      AllOutput=NULL 
      for (Location in unique(Results$Location)){
        print(Location)
        Vec_results=Results[Results$Location==Location,]
      OutputVec=Get_Performance_each_province(Vec_results,X = X[X$Location==Location,],Aggregation = Aggregation,daysahead=daysahead,GroundTruthName,Print = T)
      AllOutput=rbind(AllOutput,OutputVec$Performance)
      }
      sum(AllOutput$RMSE-AllOutput$Baseline_RMSE<0)
      sum(AllOutput$Cor-AllOutput$Baseline_Cor>0,na.rm = T)
      sum(AllOutput$Mape-AllOutput$Baseline_Mape>0,na.rm = T)
      AllOutput$Locations[AllOutput$RMSE-AllOutput$Baseline_RMSE>0]
      
      #MeanImportance=Reduce(AllImportance,f = "+")/length(AllImportance)
      #apply(MeanImportance,MARGIN = 1,mean)
      
      AllOutput$Run=Loop 
      
      All_Outputs=rbind(All_Outputs,AllOutput)

  }
  
  
  
  
  #save results 
  # write.csv(All_Results,paste0("../figures/ResultsForFigure",".csv"))
   write.csv(All_X,paste0("../figures/XForFigure",".csv"))
   All_Y$Location=All_X$Location
   write.csv(All_Y,paste0("../figures/YForFigure",".csv"))
  # write.csv(All_Outputs,file = "../figures/ModelPerformance_For_Figure.csv")
   write.csv(AllPrediction_in_sample,paste0("../figures/AllPrediction_in_sample_",ExperimentName,".csv"))
  
  
  
  
  #Look at performance of mean prediction across 20 runs 
  Vec=All_Results
  Vec_pred=aggregate(Vec$Predictions,list(Vec$Dates,Vec$Location),mean)
  Vec_sd=aggregate(Vec$Predictions,list(Vec$Dates,Vec$Location),sd)
  Vec_test=aggregate(Vec$Y_test,list(Vec$Dates,Vec$Location),mean)
  Vec=Vec_pred
  names(Vec)= c("Dates","Location","Predictions")
  Vec$Y_test=Vec_test$x
  Vec$Aggregation=Aggregation
  Vec$DaysAhead =daysahead
  Vec$prediction_sd =Vec_sd$x
  
  Results=Vec
  Results$Location=as.character(Results$Location)
  
  AllOutput=NULL
  AllPrediction=NULL
  for (Location in unique(Results$Location)){
    print(Location)
    Vec_results=Results[Results$Location==Location,]
    OutputVec=Get_Performance_each_province(Vec_results,X = X[X$Location==Location,],Aggregation = Aggregation,daysahead=daysahead,GroundTruthName,Print = T)
    AllOutput=rbind(AllOutput,OutputVec$Performance)
    AllPrediction=rbind(AllPrediction,OutputVec$Prediction)
  }
  sum(AllOutput$RMSE-AllOutput$Baseline_RMSE<0)
  sum(AllOutput$Cor-AllOutput$Baseline_Cor>0,na.rm = T)
  sum(AllOutput$Mape-AllOutput$Baseline_Mape>0,na.rm = T)
  AllOutput$Locations[AllOutput$RMSE-AllOutput$Baseline_RMSE<0]
  
  sum(AllOutput$Mechanistic_RMSE-AllOutput$Baseline_RMSE<0)
  sum(AllOutput$Mechanistic_Cor-AllOutput$Baseline_Cor>0,na.rm = T)
  sum(AllOutput$Mechanistic_Mape-AllOutput$Baseline_Mape>0,na.rm = T)
  AllOutput$Locations[AllOutput$Mechanistic_Mape-AllOutput$Baseline_RMSE>0]
  
  sum(AllOutput$Mechanistic_RMSE-AllOutput$RMSE<0)
  sum(AllOutput$Mechanistic_Cor-AllOutput$Cor>0,na.rm = T)
  sum(AllOutput$Mechanistic_Mape-AllOutput$Mape>0,na.rm = T)
  AllOutput$Locations[AllOutput$Mechanistic_Mape-AllOutput$RMSE>0]
  
  
  

  write.csv(AllPrediction,paste0("../figures/",ExperimentName,"_Predictions_averaged_acrossRounds",".csv"))
  write.csv(AllOutput,paste0("../figures/",ExperimentName,"_Performance_averaged_acrossRounds",".csv"))
  
  
  
  #organize importance of variables 
  Importance_ToSave=All_VariableImportance[[1]]
  Vec=Reduce(lapply(All_VariableImportance,function(x){x$Weight}),f = cbind)
  
  Importance_ToSave$Weight=apply(Vec,MARGIN = 1,mean)
  Importance_ToSave$Weight_sd=apply(Vec,MARGIN = 1,sd)
  names(Importance_ToSave)[1]="Weight_mean"
  names(Importance_ToSave)[3]="IndexDate(T)"
  
  Importance_ToSave$feature
  Importance_ToSave$T_plus_deltaT=as.Date(Importance_ToSave$IndexDate,origin="1970=01-01")+Aggregation*daysahead
  VecSelect=(!grepl(Importance_ToSave$feature,pattern = "lag",ignore.case = T))&(Importance_ToSave$feature!="(Intercept)")
  Importance_ToSave$feature[VecSelect]=paste(Importance_ToSave$feature[VecSelect]," at time T")
  Importance_ToSave$feature=gsub(Importance_ToSave$feature,pattern = "Lag_",replacement = "at T-delta(t)*")
  unique(Importance_ToSave$feature)
  
  write.csv(Importance_ToSave,file = paste0("../figures/",ExperimentName,"_All_VariableImportance_For_Figure.csv"))
  
  
  
  
  

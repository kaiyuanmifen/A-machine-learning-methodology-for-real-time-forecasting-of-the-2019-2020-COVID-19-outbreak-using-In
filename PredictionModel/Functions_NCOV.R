# Hierachical clusting choosing the best number of K 

HierachiClustering=function(AllProvinces,GroundTruthName){
  #clustering 
  AllDates=AllProvinces$Date
  AllDates=unique(AllDates[AllDates>FirstDate_use_data & AllDates <=First_index_date])
  EachProvince=NULL
  AllNames=NULL
  for (Location in unique(AllProvinces$Location)){
    Vec=AllProvinces[AllProvinces$Location==Location,c(GroundTruthName,"Date" )]
    if(nrow(Vec)>0){
      Vec=Vec[match(AllDates,Vec$Date),GroundTruthName]
      EachProvince=cbind(EachProvince,Vec)
      AllNames=c(AllNames,Location)
    }
    
  }
  colnames(EachProvince)=AllNames
  EachProvince=as.data.frame(EachProvince)
  EachProvince=EachProvince+rnorm(n =length(EachProvince) ,sd = 0.01,mean = 0)
  CorMat=cor(EachProvince,method = "spearman")
  dissimilarity=as.dist(1-CorMat) 
  #clusters
  clusters=hclust(dissimilarity,method = "average")
  #plot(clusters)
  
  #calculated CH indexs 

  GetCH=function(K,EachProvince){
  clusterCut <- cutree(clusters, K)
  
  Vec_each_cluster=list()
  W=0
  for (Cluster in unique(clusterCut)){
    Vec=EachProvince[,colnames(EachProvince)%in%names(clusterCut)[clusterCut==Cluster],drop=F]
    Vec_each_cluster[[length(Vec_each_cluster)+1]]=Vec
    
    #caculate W
    
    W=W+sum((Vec-apply(Vec,MARGIN = 1,mean))^2)
  }
  X_ave_all=apply(EachProvince,MARGIN = 1,mean)
  X_k=lapply(Vec_each_cluster,function(x){apply(x,MARGIN = 1,mean)})
  N_k=unlist(lapply(Vec_each_cluster,function(x){ncol(x)}))
  
  B=sum(N_k*unlist(lapply(X_k,function(x){sum((x-X_ave_all)^2)})))
  N= ncol(EachProvince) 
  CH=(B/(K-1))/(W/(N-K))
  return(CH)
  }
  
  
  AllCH=NULL
  AllKs=2:10
  for (K in AllKs){
    AllCH=c(AllCH,GetCH(K,EachProvince))
  }
  
  K_max=AllKs[which.max(AllCH)]
  
  clusterCut <- cutree(clusters, K_max)
  
  return(clusterCut)
}





#data aggregation

AggregateByDate=function(X,Aggregation,
                         TargetColumns=c("New_confirmed","New_suspect","New_death" ,"MediaCloudCount",
                                         "Search_index_coronavirus_symptom",
                                         "Search_index_how_many_degree_is_fever","Search_index_symptom_of_fever","MobilityWuhan")){
  print("Doing aggregation")
  #Aggregation= number of dayes to Aggregation
  #Only aggregate specific columns 
  X$Date=as.Date(as.character(X$Date),origin="1970-01-01")
  DatesLeft=seq(from = min(X$Date),to = max(X$Date),by = Aggregation)
  DatesLeft=DatesLeft[2:length(DatesLeft)]#skip the first aggregation date 
  XVec=X[X$Date%in%DatesLeft,]
  
  for (Date in DatesLeft){
    XVec[XVec$Date==Date,TargetColumns]=apply(X[X$Date%in%c(Date-c(0:(Aggregation-1))),TargetColumns],2,sum)
  }
  return(XVec)
}




#normalize data.

DataNormalization=function(X,Y){
  
  X_original=X
  Y_original=Y
  #X=X+rnorm(n=length(X),mean = 0,sd = 0.01)
  #Y=Y+rnorm(n=length(Y),mean = 0,sd = 0.01)
  
 print("doning normalizaiton")
  
  #Normalize X
  for (i in 1:ncol(X)){
    if (class(X[,i])== "numeric"|class(X[,i])== "integer"){
      for (location in unique((X$Location)) ){
        Vec=X[X$Location==location,i]
        Vec=Vec[!is.na(Vec)]
        Mu=mean(Vec,rm.na=T) 
        Sigma=sd(Vec)
        if(Sigma!=0){
        X[X$Location==location,i]=(X[X$Location==location,i]-Mu)/Sigma
        }
        if(Sigma==0){#sum times Sigma==0
          X[X$Location==location,i]=0
        }
        
      } 
    }
  }
  
  

#Normalize Y

    Y$Location=X$Location
    Y$Mu=NA
    Y$Sigma=NA
    
    for (location in unique((Y$Location)) ){#Y has the same location as X
      Vec=Y$Y[Y$Location==location]
      Vec=Vec[!is.na(Vec)]
      Mu=mean(Vec) 
      Sigma=sd(Vec)
      
      if(Sigma!=0){
        Y$Y[Y$Location==location]=(Y$Y[Y$Location==location]-Mu)/Sigma
      }
      if(Sigma==0){#sum times Sigma==0
        Y$Y[Y$Location==location]=0
      }
      
      
      Y$Mu[Y$Location==location]=Mu
      Y$Sigma[Y$Location==location]=Sigma
      
    } 



  
  
  return(list(X=X,Y=Y,
              X_original=X_original,
              Y_original=Y_original))
}






#coordinate decent algorithm for optimization 

#function to get input, output predictions 
Make_prediction=function(X_train,Y_train,X_test,alpha=1,FeatureSelection=F,GroundTruthName,Augmentation=T){
  
  
  
  
 
  #bootstraping 
  if (Augmentation==T){
  print("with augementation")
  Indexes=sample(1:length(Y_train),replace = T,100*length(Y_train))
  
  X_train_vec= X_train[Indexes,]
  Y_train_vec=Y_train[Indexes]
  #sometimes there are several continous days without data change 
  X_train_vec= X_train_vec+rnorm(mean = 0,sd = 0.01,n=length( X_train_vec))
  Y_train_vec= Y_train_vec+rnorm(mean = 0,sd = 0.01,n=length(Y_train_vec))
  }
  if (Augmentation==F){
    print("without augementation")
    X_train_vec= X_train
    Y_train_vec=Y_train
  }
  
  #feature selection 
  
  if (FeatureSelection==T){
    print("doning feature selection")
    Vec=abs(cor(X_train,Y_train))[,1]
    Vec=sort(Vec,decreasing = T)
    Features=names(Vec)
    Features=Features[Features%in%names(Vec)[1:floor(length(Vec)/3)]|grepl(pattern =GroundTruthName,Features)]
    X_train=X_train[,colnames(X_train)%in%Features,drop=F]
    X_test=X_test[,colnames( X_test)%in%Features,drop=F]
    print(paste("Top features:",Features[1:5]))
    
    }
  
  
  
  #simple LM methods 

   #  Data=as.data.frame(X_train)
   #  Data$Y_train=Y_train
   #  fit = lm(Y_train~.,data=Data,na.action = "na.exclude")
   # 
   # print(summary(fit))
   # 
   # Prediction1=predict(fit,X_test)
  
  
  
  #fix lambda 
  # 
  # fit = glmnet( as.matrix(X_train),  Y_train, alpha = alpha,lambda = 0.1)
  # Prediction1=predict(fit, newx = as.matrix(X_test), type = "response")
  # 
  
  #manualy validation and select lambda without validation
  #  print("doing manual lambda selections")
  # 
  #  fit = glmnet( as.matrix(X_train),  Y_train, alpha = alpha, nlambda = 20)
  #  AllLambda=fit$lambda
  #  Lambda= AllLambda[which(fit$dev.ratio>=0.8*max(fit$dev.ratio))[1]]
  # 
  # Prediction1=predict(fit, newx = as.matrix(X_test), type = "response", s =Lambda)
  # 
  # 
    
  #manualy validation and select lambda using validation 
# 
#   print("doing manual lambda selections")
#   X_val=X_train[floor(0.75*nrow(X_train)):nrow(X_train),]
#   X_train_vec=X_train[1:floor(0.75*nrow(X_train)),]
#   Y_val=Y_train[floor(0.75*nrow(X_train)):nrow(X_train)]
#   Y_train_vec=Y_train[1:floor(0.75*nrow(X_train))]
# 
#   fit = glmnet( as.matrix(X_train_vec),  Y_train_vec, alpha = alpha, nlambda = 20)
#   AllLambda=fit$lambda
# 
#    RMSE=NULL
#    for (Vec in AllLambda){
#      RMSE=c(RMSE,sqrt(mean((Y_val-predict(fit, newx = as.matrix(X_val), type = "response", s =Vec ))^2)))
#   }
#   Lambda=AllLambda[which.min(RMSE)]
# 
# 
#   Prediction1=predict(fit, newx = as.matrix(X_test), type = "response", s =Lambda)

  # 

#   #cross validation
   library(glmnet)
  #linear regression cross validation version
  method="lambda.1se"
  fit = cv.glmnet(x=as.matrix(X_train_vec),
                  y= c(Y_train_vec),
                  alpha=alpha)

#lambda=c(10^(seq(from = -6,to = 1,by = 0.1)
  print(coef(fit, s = method))
  fit$lambda.min
  fit$lambda.1se
  Prediction1=predict(fit,newx=as.matrix(X_test),s=c(method))

  VaraibleImportance=as.data.frame(as.matrix(coef(fit, s = method)))
  
  return(list(Predictions=c(Prediction1),
              Models=list(fit),
              VaraibleImportance=VaraibleImportance))}





Make_prediction_binary=function(X_train,Y_train,X_test,alpha=1,FeatureSelection=F,GroundTruthName){
  Y_train=as.numeric(Y_train)
  
 
  #as there are very few data points for cross valdation , balanced 
  
 
  Indexes_pos=which(Y_train==1)
  Indexes_pos=sample(x = c(Indexes_pos,Indexes_pos),3000,replace = T)
  
  Indexes_neg=which(Y_train==0)
  Indexes_neg=sample(Indexes_neg,3000,replace = T)
  
  Indexes=c(Indexes_pos,Indexes_neg)
  Indexes=Indexes[sample(1:length(Indexes))]
  
  X_train_vec= X_train[Indexes,]
  Y_train_vec=Y_train[Indexes]
  
  
  X_train=X_train+rnorm(mean = 0,sd = 0.05,n=length(X_train))
  Y_train_vec=as.numeric(Y_train_vec)
  
  #   
   library(glmnet)
  #linear regression cross validation version
  method="lambda.1se"
  fit = cv.glmnet(x=as.matrix(X_train_vec),
                  y= c( Y_train_vec),
                  family = "binomial")

  #lambda=c(10^(seq(from = -6,to = 1,by = 0.1)
  print(coef(fit, s = method))
  fit$lambda.min
  fit$lambda.1se
  Prediction1=predict(fit,type = "class",newx=as.matrix(X_test),s=c(method))
  #}
  
  # 
  # #SVM simple version 
  # library(e1071)
  # Data=X_train_vec
  # Data$Y_train_vec=Y_train_vec
  # fit = svm(Y_train_vec~ ., data = Data, kernel = "linear",scale = FALSE)
  # Prediction1=predict(fit ,X_test)
  # Prediction1[ Prediction1>0]=1
  # Prediction1[ Prediction1<0]=0
  # 
  # 
  # #SVM advanced version
  # library(e1071)
  # Data=X_train_vec
  # Data$Y_train_vec=Y_train_vec
  # 
  # tuned = tune.svm(Y_train_vec~ ., data = Data, gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=10))
  # fit = tuned$best.model
  # 
  # Prediction1=predict(fit ,X_test)
  # Prediction1[ Prediction1>0]=1
  # Prediction1[ Prediction1<0]=0
  # 
  return(list(Predictions=c(Prediction1),
              Models=list(fit)))
}





#function to run models 
Run_models=function(DataForRegression,Lags=1,daysahead=1,Information_to_include,
                    GroundTruthName="New_confirmed",First_index_date=NULL,
                    UseR0_as_predictor=T,Normalization=TRUE,FeatureSelection=F,Aggregation=1,Binary=F,
                    Clustering=T,Augmentation=T){
  
  DataForRegression$Date=as.Date(as.character(DataForRegression$Date),origin="1970-01-01")
  
  AllX=NULL
  AllY=NULL
  All_Y_original=NULL
  All_X_original=NULL
  #process data
  for (Location in unique(DataForRegression$Location)){
  
  X=DataForRegression[DataForRegression$Location==Location,]
  
  if(Aggregation!=1){#aggregate by date to enhance signal
    Aggregated=AggregateByDate(X,Aggregation)
    X=Aggregated
  }
  
  if(Lags>0){
  X_vec=X
  
  Names=names(X)
  for (Lag in 1:Lags){
    #lag in by data point 
    Vec=X
    
    Vec=Vec[match(X$Date-Aggregation*Lag,Vec$Date),]
    
    X_vec=cbind(X_vec,Vec)
    Names=c(Names,paste0(names(X),'_Lag_',Lag))
  }
  names(X_vec)=Names
  X=X_vec
  }
  
  #days_ahead
  AllTarges=NULL
  Target=X[,GroundTruthName]
  
  Vec=Target[match(X$Date+daysahead*Aggregation,X$Date)]
    
  Y=data.frame(Y=Vec,Date=X$Date,daysahead=daysahead*Aggregation)
  Y$Date=as.Date(Y$Date,origin="1970-01-01")
  
  
  if (Normalization==TRUE){
    Output=DataNormalization(X,Y)
    X=Output$X
    Y=Output$Y
    Y_original=Output$Y_original
    X_original=Output$X_original
  }
  
  

   AllX=rbind(AllX,X)
   AllY=rbind(AllY,Y)
   All_Y_original=rbind(All_Y_original,Y_original)
   All_X_original=rbind(All_X_original,X_original)
     }
  
  
  X=AllX
  #X=unique(X)
  Y=AllY
 # Y=unique(Y)
  Y_original=All_Y_original
  X_original=All_X_original
  #remove all records in X with NA, NA are allowed in Y as making prediction ahead of time 
  if(sum(is.na(X))>0){
  Y=Y[-unique(which(is.na(X),arr.ind = T)[,1]),]
  Y_original=Y_original[-unique(which(is.na(X),arr.ind = T)[,1]),]
  X_original=X_original[-unique(which(is.na(X),arr.ind = T)[,1]),]
  X=X[-unique(which(is.na(X),arr.ind = T)[,1]),]
  }
  Y$Date==X$Date
  #Y_original$Date==X$Date
  
  if(is.null(First_index_date)){
    First_index_date=min(X$Date)
    
  }
  
    
      Lags=Lags*Aggregation#to make days conssistent
      daysahead=daysahead*Aggregation
      
      X$Date=as.Date(X$Date,origin="1970-01-01")
      IndexDates=X$Date
      IndexDates=IndexDates[IndexDates>(min(X$Date)+2*Lags)]
      IndexDates=IndexDates[IndexDates>=First_index_date]#some dates do not exist in report 
      IndexDates=unique(IndexDates)
      #vector tos save 
      AllDates=NULL
      AllPredictions=NULL
      AllModels=list()
      Y_train_max_all=list()
      Vec_Predictions=NULL
      ALlDaysForward=NULL
      All_Y_test=NULL
      AllLocations=NULL
      AllVaraibleImportance=list()
      #bianry (if the number goes up or down)
      if(Binary==T){
      
        BinaryVec=(Y$Y-c(NA,Y$Y[1:(nrow(Y)-1)]))>0
        Y$Binary=BinaryVec
      }
      
      
        
      for (IndexDate in IndexDates){
        IndexDate=as.Date(IndexDate,origin="1970-01-01")
        print(IndexDate)
        ImportantVecForIndexDate=list()
        #now do the clustering 
        if (Clustering){
        clusterCut=HierachiClustering(DataForRegression[DataForRegression$Date<=IndexDate,],GroundTruthName = GroundTruthName)
        X$Cluster=clusterCut[match(X$Location,names(clusterCut))]
        Y$Cluster=clusterCut[match(Y$Location,names(clusterCut))]
        print(paste("number of clusters:",length(unique(clusterCut))))
        }
        if (!Clustering){
          clusterCut=as.integer(unique(as.factor(X$Location)))
          names(clusterCut)=as.character(unique(as.factor(X$Location)))
          X$Cluster=as.integer(as.factor(X$Location))
          Y$Cluster=as.integer(as.factor(Y$Location))
          print(paste("number of clusters:",length(unique(Y$Cluster))))
        }
        
        VecVI=NULL
        for (CLuster in unique(clusterCut)){
      
        if(Binary==FALSE){
      X_train=X[(X$Date<=IndexDate-daysahead)&(X$Cluster==CLuster),]
      X_train=X_train[,colnames(X)[colnames(X)%in%Information_to_include],drop=FALSE]
      class(X_train)
      names(X_train)
      
      Locations_train=X$Location[(X$Date<=IndexDate-daysahead)]
      
      Y_train=data.frame(Y=Y[(Y$Date<=(IndexDate-daysahead))&(Y$Cluster==CLuster),"Y"])#not including dates
      
      X_train=X_train[!is.na(Y_train$Y),,drop=FALSE]#some data points does not have the next day 
      Y_train=Y_train[!is.na(Y_train$Y),]#some data points does not have the next day 
      
      X_test=X[(X$Date==IndexDate)&(X$Cluster==CLuster),which(colnames(X)%in%Information_to_include),drop=FALSE]
      Y_test=data.frame(Y=Y[(Y$Date==IndexDate)&(Y$Cluster==CLuster),"Y"])
      Locations=X$Location[(X$Date==IndexDate)&(X$Cluster==CLuster)]
      
      #Y_test_original=Y_original[Y$Date==IndexDate,"Y"]
      #Locations=Locations[!is.na(Y_test$Y)]
      #X_test=X_test[!is.na(Y_test$Y),]
      #Y_test=Y_test[!is.na(Y_test$Y),]
      
      if (UseR0_as_predictor==TRUE){
        R0_median=median(Get_R0(X)$AllR0)
        R0_mean=mean(Get_R0(X)$AllR0)
        if(!is.na(R0_median)){
        X_train$R0=rep(R0_median,nrow(X_train))
        X_test$R0=R0_median
        } else {print("not enough data to calculate R0")}
      }
      print(paste(" Number of input features:",ncol(X_train)))
      print(paste(" input features:",names(X_train),collapse = " "))
      #print(summary(X_train))
      #X_train=as.matrix(X_train)
 
      
      Y_train=as.data.frame(Y_train)
      Vec_predict=Make_prediction(X_train,Y_train[,1],X_test,alpha=1,
                                  FeatureSelection=FeatureSelection,GroundTruthName=GroundTruthName,
                                  Augmentation=Augmentation)
      
      Predictions=Vec_predict$Predictions
      Y_test=Y_test
      VaraibleImportance=Vec_predict$VaraibleImportance
      names(VaraibleImportance)="Weight"
      VaraibleImportance$cluster=CLuster
      VaraibleImportance$IndexDate=IndexDate
      for (x in Locations){
        VaraibleImportance$feature=rownames(VaraibleImportance)
        VaraibleImportance$Location=x
        ImportantVecForIndexDate[[length(ImportantVecForIndexDate)+1]]=VaraibleImportance
        
        
      }        
      
        
        
      if (Normalization==TRUE){#scale back 
        Mu=Y$Mu[(Y$Date==IndexDate)&(X$Cluster==CLuster)]
        Sigma=Y$Sigma[(Y$Date==IndexDate)&(X$Cluster==CLuster)]
        Predictions=Vec_predict$Predictions*Sigma+Mu#return the value back to origina range 
        Y_test=Y_test*Sigma+Mu
       
      }
     
      }
      
        
        # #Do binary classficiiation 
        # if(Binary==TRUE){
        #   X_train=X[(X$Date<=IndexDate-daysahead),]
        #   X_train=X_train[,colnames(X)[colnames(X)%in%Information_to_include],drop=FALSE]
        #   class(X_train)
        #   names(X_train)
        #   
        #   Locations_train=X$Location[(X$Date<=IndexDate-daysahead)]
        #   
        #   Y_train=data.frame(Y=Y[(Y$Date<=(IndexDate-daysahead)),"Binary"])#not including dates
        #   
        #   X_train=X_train[!is.na(Y_train$Y),,drop=FALSE]#some data points does not have the next day 
        #   Y_train=Y_train[!is.na(Y_train$Y),,drop=FALSE]#some data points does not have the next day 
        #   
        #   X_test=X[X$Date==IndexDate,which(colnames(X)%in%Information_to_include),drop=FALSE]
        #   Y_test=data.frame(Y=Y[Y$Date==IndexDate,"Binary",drop=FALSE])
        #   
        #   Y_test_original=Y_original[Y$Date==IndexDate,"Y"]
        #   Locations=X$Location[X$Date==IndexDate]
        #   #Locations=Locations[!is.na(Y_test$Y)]
        #   #X_test=X_test[!is.na(Y_test$Y),]
        #   #Y_test=Y_test[!is.na(Y_test$Y),]
        #   
        #   if (UseR0_as_predictor==TRUE){
        #     R0_median=median(Get_R0(X)$AllR0)
        #     R0_mean=mean(Get_R0(X)$AllR0)
        #     if(!is.na(R0_median)){
        #       X_train$R0=rep(R0_median,nrow(X_train))
        #       X_test$R0=R0_median
        #     } else {print("not enough data to calculate R0")}
        #   }
        #   print(paste(" Number of input features:",ncol(X_train)))
        #   print(paste(" input features:",names(X_train),collapse = " "))
        #   #print(summary(X_train))
        #   #X_train=as.matrix(X_train)
        #   
        #   if(length(unique(Y_train$Y))>1){
        #   Y_train=as.data.frame(Y_train)
        #   Vec_predict=Make_prediction_binary(X_train,Y_train[,1],X_test,alpha=1,
        #                               FeatureSelection=FeatureSelection,GroundTruthName=GroundTruthName)
        #   
        #   Predictions=Vec_predict$Predictions
        #   } else {Predictions=Y_train$Y[length(Y_train$Y)]}
        #   
        #   
        #   Y_test=Y_test
        # }
       
        
        
        
          
        #save
        
          AllPredictions=c(AllPredictions,Predictions)
          ALlDaysForward=c(ALlDaysForward,rep(daysahead,length(Predictions)))
          AllModels[[length(AllModels)+1]]=Vec_predict$Models
          #Y_train_max_all[[length(Y_train_max_all)+1]]=Y_train_max
          AllDates=c(AllDates,rep(as.character(as.Date(IndexDate,origin = "1970-01-01")),length(Predictions)))
          All_Y_test=c(All_Y_test,Y_test)
          AllLocations=c(AllLocations,Locations)
          VecVI=cbind(VecVI,as.matrix(VaraibleImportance))
        }
        AllVaraibleImportance[[length(AllVaraibleImportance)+1]]=ImportantVecForIndexDate
      }
      names(AllVaraibleImportance)=IndexDates
        
    
  Results=data.frame(Dates=as.Date(AllDates),
                     Predictions=AllPredictions,
                     DaysAhead=ALlDaysForward,
                     Aggregation=Aggregation,
                     Y_test=unlist(All_Y_test),
                     Location=AllLocations)
  #names(AllModels)=paste0(Results$Dates,"_",Results$DaysAhead)
  
 
  return(list(Results=Results,X=X_original,Y=Y_original,
              X_train=X_train,VaraibleImportance=AllVaraibleImportance,
              LastModel=Vec_predict$Models))
}




#Generated plots 

Get_plots=function(Results,X,GroundTruthName="New_suspected"){
  
  
  library(ggplot2)
  #Look at performance
  #pdf(paste0('figures/',i,"_plot.pdf"))
  AllPlots=list()
  for (IndexDate in unique(Results$Dates)){
    
    
    VecPlot=data.frame(New_count=X[,GroundTruthName][X$Date<=IndexDate],
                       Date=X$Date[X$Date<=IndexDate])
    VecPlot$Type="Observed"
    VecPlot$Date=as.Date(as.character(VecPlot$Date),origin="1970-01-01")
    
    
    VecPlot_0=data.frame(New_count=X[,GroundTruthName],
                         Date=X$Date)
    VecPlot_0$Type="Ground_truth(unseen)"
    VecPlot_0$Date=as.Date(as.character(VecPlot_0$Date),origin="1970-01-01")
    
    
    
    
    VecPlot2= data.frame(New_count=Results$Predictions[Results$Dates==IndexDate],
                         Date=c(Results$Dates[Results$Dates==IndexDate]+Results$DaysAhead[Results$Dates==IndexDate]))   
    VecPlot2$Type="Model_Predicts"
    VecPlot2$Date=as.Date(as.character(VecPlot2$Date),origin="1970-01-01")
    
    
    
    
    
    
    #calculate R0 based prediction 
    CumuSum_Count=sum(VecPlot$New_count[VecPlot$Date<=IndexDate])
    CumuSum_Count_t_minus_1=sum(VecPlot$New_count[VecPlot$Date<=(IndexDate-5)])
    
    #R0_upper=3.9
    R0_median=2.2
    #R0_lower=1.4
    Serial_period=5
    
    Y_t=VecPlot$New_count[VecPlot$Date==IndexDate]
    Increased_cumulative=c(R0_median)*(CumuSum_Count-CumuSum_Count_t_minus_1)
    Y_R0=(2*Increased_cumulative/Serial_period)-Y_t
     
    
    RO_predict=data.frame(New_count=Y_R0,Date=IndexDate+Serial_period)
    RO_predict$Type="R0_Predicts"
    print(paste("R0_count",RO_predict$New_count))
    RO_predict$Date=as.Date(RO_predict$Date,origin = "1970-01-01")
    #RO_predict=rbind(RO_predict,RO_predict)
    # 
    # names(R0_predict)=c("Date","New_suspected_count")
    # R0_predict$Type="R0_predict"
    # R0_predict=R0_predict[,c("New_suspected_count","Date","Type")]
    # 
    # 
    VecPlot=data.frame(Date=c(VecPlot$Date,VecPlot2$Date,RO_predict$Date,VecPlot_0$Date),
                       New_count=c(VecPlot$New_count,VecPlot2$New_count,RO_predict$New_count,VecPlot_0$New_count),
                       Type=c(VecPlot$Type,VecPlot2$Type,RO_predict$Type,VecPlot_0$Type))
    VecPlot$Date=as.Date(VecPlot$Date,origin="1970-01-01")
    # 
    # VecPlot$Date=as.Date(VecPlot$Date)
    # VecPlot=VecPlot[!is.na(VecPlot$New_suspected_count),]
    # VecPlot$Type=as.factor(VecPlot$Type)
    # VecPlot$Date=as.Date(as.character(VecPlot$Date))
    # class(VecPlot$Date
    
    AllPlots[[length(AllPlots)+1]]=ggplot(data=VecPlot,aes(x=Date, y=New_count,colour=Type)) + 
      geom_line(aes(linetype=Type)) +
      scale_linetype_manual(values=c("dashed", "solid","solid","solid"))+
      ggtitle(paste("prediction made \n on date: ",as.character(Results$Dates[i])))+
      ylim(0,max(VecPlot$New_count))+
      xlim(min(VecPlot$Date),max(VecPlot$Date)+7)+
      xlab('Dates') +
      ylab('New count')+
      theme_bw() +
      geom_point(data=RO_predict,aes(x=Date,y=New_count), colour="blue")+
      scale_color_manual(values=c('black','red','black','blue'))+
      geom_vline(xintercept=c(min(VecPlot2$Date)-1), linetype="dotted",color='blue')+
      geom_vline(xintercept=c(min(VecPlot2$Date)), linetype="dotted",color='brown')+
      geom_vline(xintercept=c(max(VecPlot2$Date)), linetype="dotted",color='red')
    
    
  }
  
  
  return(AllPlots)
}


#Get performace 

Get_performance=function(Results,X,TargetColumn="New_suspected"){
  #install.packages("MLmetrics")
  library("MLmetrics")
  Dates=Results$Dates[Results$DaysAhead==1]+1
  Predictions=Results$Predictions[Results$DaysAhead==1]
  Observed=X[,TargetColumn][match(Dates,X$Date)]
  BaseLine=X[,TargetColumn][match(Dates-1,X$Date)]
  
  Vec=data.frame(Dates=Dates,Predictions=Predictions,
                 Observed=Observed,BaseLine=BaseLine )
  Vec=Vec[(!is.na(Vec$Observed))&((!is.na(Vec$Predictions))),]
  
  RMSE=mean(sqrt((Vec$Predictions-Vec$Observed)^2),na.rm = T)
  Cor=cor(Vec$Predictions,Vec$Observed)
  Mape=MAPE(Vec$Predictions,Vec$Observed)
  
  Baseline_RMSE=mean(sqrt((Vec$BaseLine-Vec$Observed)^2))
  Baseline_Cor=cor(Vec$BaseLine,Vec$Observed)
  Baseline_Mape=MAPE(Vec$BaseLine,Vec$Observed)
  
  
  Output=data.frame(RMSE=RMSE,Cor=Cor,Mape=Mape,
                    Baseline_RMSE=Baseline_RMSE,Baseline_Cor=Baseline_Cor,Baseline_Mape=Baseline_Mape)
  return(Output)
}







Get_Performance_each_province=function(Results,X,daysahead=1,Aggregation=1,Print=TRUE,GroundTruthName){
  
  Results=Results[!is.na(Results$Y_test),]
  Results$Location=as.character(Results$Location)
  library("MLmetrics")
  AllRMSE=NULL
  AllCor=NULL
  ALLMape=NULL
  
  AllBaseline_RMSE=NULL
  AllBaseline_Cor=NULL
  ALLBaseline_Mape=NULL
  
  AllMechanistic_RMSE= NULL
  AllMechanistic_Cor=NULL
  AllMechanistic_Mape=NULL
  
  AllDates=NULL
  
  AllPredction=NULL
  
  
  Results$Dates=as.Date(Results$Dates,origin="1970-01-01")
  X$Date=as.Date(X$Date,origin="1970-01-01")
  for (each_location in unique(Results$Location)){
    Predictions=Results$Predictions[Results$Location==each_location]
    
    #T + delta T
    Dates=Results$Date[Results$Location==each_location]+daysahead*Aggregation
    
    
    Observed=Results$Y_test[Results$Location==each_location]
    X_vec=X[X$Location==each_location,]
    BaseLine=X_vec[match(Dates-(daysahead*Aggregation),X_vec$Date),GroundTruthName]
    
    MechanisticPrediction=X_vec$Mechanistic_pred[match(Dates,X_vec$Date)]
    
    
    Vec=data.frame(Dates=Dates,Predictions=Predictions,
                   Observed=Observed,BaseLine=BaseLine,
                   MechanisticPrediction=MechanisticPrediction)
    Vec=Vec[(!is.na(Vec$Observed))&((!is.na(Vec$Predictions))),]
    
    RMSE=mean(sqrt((Vec$Predictions-Vec$Observed)^2),na.rm = T)
    Cor=cor(Vec$Predictions,Vec$Observed)
    Mape=MAPE(Vec$Predictions,Vec$Observed)
    
    Baseline_RMSE=mean(sqrt((Vec$BaseLine-Vec$Observed)^2))
    Baseline_Cor=cor(Vec$BaseLine,Vec$Observed)
    Baseline_Mape=MAPE(Vec$BaseLine,Vec$Observed)
    
    Mechanistic_RMSE=mean(sqrt((Vec$MechanisticPrediction-Vec$Observed)^2))
    Mechanistic_Cor=cor(Vec$MechanisticPrediction,Vec$Observed)
    Mechanistic_Mape=MAPE(Vec$MechanisticPrediction,Vec$Observed)
    
    
    
    AllRMSE=c(AllRMSE,RMSE)
    AllCor=c(AllCor,Cor)
    ALLMape=c(ALLMape,Mape)
    
    AllBaseline_RMSE=c(AllBaseline_RMSE,Baseline_RMSE)
    AllBaseline_Cor=c(AllBaseline_Cor, Baseline_Cor)
    ALLBaseline_Mape=c(ALLBaseline_Mape,Baseline_Mape)
    
    AllMechanistic_RMSE= c(AllMechanistic_RMSE,Mechanistic_RMSE)
    AllMechanistic_Cor=c(AllMechanistic_Cor,Mechanistic_Cor)
    AllMechanistic_Mape=c(AllMechanistic_Mape,Mechanistic_Mape)
    
    #AllDates=c(AllDates,as.character(Dates))
    
    
    
    if(Print==TRUE){
    #plot
    plot(X_vec[,GroundTruthName]~X_vec$Date,type='l',
         ylim=c(0,max(c(Vec$Observed,Vec$Predictions))),
         main=each_location
         ,xlab="dates",ylab="Counts")
    lines(Vec$Predictions~Vec$Date,col='red')
    lines(Vec$BaseLine~Vec$Date,col='brown',lty=2)
    }
    Vec$Location=each_location
    names(Vec)=c("Date_T_plus_deltaT",'ModelPrediction',
                 "obsered_Y_test","Persistence_baseline",
                 "Mechanistic_only_Prediction","Location" )
    
    AllPredction=rbind(AllPredction,Vec)
    
  }
  
  Performance=data.frame(Locations=unique(Results$Location),
                    RMSE=AllRMSE,Cor=AllCor,Mape=ALLMape,
                    Baseline_RMSE=AllBaseline_RMSE,Baseline_Cor=AllBaseline_Cor,Baseline_Mape=ALLBaseline_Mape,
                    Mechanistic_RMSE=AllMechanistic_RMSE,Mechanistic_Cor=AllMechanistic_Cor,Mechanistic_Mape=AllMechanistic_Mape)
  return(list(Performance=Performance,
              Prediction=AllPredction))
}


#Function to get R0

Get_R0=function(X){
  #calculate R0 based prediction 
  Serial_periods=c(5,6,7)
  
  CumuSum_Count=cumsum(X$New_confirmed)
  CumuSum_Count=CumuSum_Count
  CumuSum_Count=data.frame(CumuSum_Count=CumuSum_Count,date=X$Date)
  #CumuSum_Count=CumuSum_Count[CumuSum_Count$date>="2020-01-25",]#two days after seal of wuhan
  
  #estimate R0
  VecR0=NULL
  
  for (Serial_period in Serial_periods){
  for (i in nrow(CumuSum_Count):(2*Serial_period+1)){
    Vec1=CumuSum_Count$CumuSum_Count[i]-CumuSum_Count$CumuSum_Count[CumuSum_Count$date==(CumuSum_Count$date[i]-Serial_period)]
    Vec2=CumuSum_Count$CumuSum_Count[CumuSum_Count$date==(CumuSum_Count$date[i]-Serial_period)]-CumuSum_Count$CumuSum_Count[CumuSum_Count$date==(CumuSum_Count$date[i]-2*Serial_period)]
    VecR0=c(VecR0,Vec1/Vec2)
  }
  }
  VecR0=VecR0[!is.na(VecR0)]
  VecR0=VecR0[!is.infinite(VecR0)]
  R0_median=median(VecR0)
  AllR0=VecR0
  
  #prediction use R0
  Y_t=X$New_confirmed[which.max(X$Date)]
  Increased_cumulative=c(R0_median)*(CumuSum_Count$CumuSum_Count[which.max(CumuSum_Count$date)]-CumuSum_Count$CumuSum_Count[nrow(CumuSum_Count)-median(Serial_periods)])
  Y_R0=(2*Increased_cumulative/median(Serial_periods))-Y_t
  
  #calulate the days in between 
  Gradient=(Y_R0-Y_t)/median(Serial_periods)
  MiddleValues=(1:(median(Serial_periods)-1))*Gradient+Y_t
    
  RO_predict=data.frame(New_count=c(MiddleValues,Y_R0),Date=max(CumuSum_Count$date)+median(Serial_periods)-c((median(Serial_periods)-1):0))
  RO_predict$Type="R0_Predicts"
  #print(paste("R0_count",RO_predict$New_count))
  RO_predict$Date=as.Date(RO_predict$Date,origin = "1970-01-01")
  
return(list(AllR0=AllR0,RO_predict=RO_predict))
  
}




#get performance binary 
Get_confusion=function(Y_test,Pred){
confusionMatrix=matrix(data = 0,nrow = 2,ncol = 2)
rownames(confusionMatrix)=c("Predicted increase","Predicted decrease")
colnames(confusionMatrix)=c("Real increase","Real decrease")
confusionMatrix[[1,1]]=sum(Y_test==1&Pred==1)
confusionMatrix[[1,2]]=sum(Y_test==0&Pred==1)
confusionMatrix[[2,1]]=sum(Y_test==1&Pred==0)
confusionMatrix[[2,2]]=sum(Y_test==0&Pred==0)

Precision=confusionMatrix[1,1]/sum(confusionMatrix[1,])
Recall=confusionMatrix[1,1]/sum(confusionMatrix[,1])
Accuracy=sum(Y_test==Pred,na.rm = T)/(length(Y_test))
return(list(confusionMatrix=confusionMatrix,
            Precision=Precision,
            Recall=Recall,
            Accuracy=Accuracy))
}



GetPerformance_binary=function(Results,X,Aggregation = Aggregation,daysahead=daysahead,GroundTruthName){
  

  
  # Y_test=as.numeric(Results$Y_test)
  # Pred=Results$Predictions
  # Y_test= Results$Y_test 
  # Pred=Pred[!is.na(Y_test)]
  # Y_test=Y_test[!is.na(Y_test)]
  # 
  # Output_all=Get_confusion(Y_test=Y_test, Pred= Pred)
  # 
  AllConfusionModel=list()
  AllPrecision_Model=NULL
  AllRecall_Model=NULL
  AllAccuracy_Model=NULL
  
  AllConfusionBaseline=list()
  AllPrecision_baseline=NULL
  AllRecall_baseline=NULL
  AllAccuracy_baseline=NULL
  #Baseline of persistency 
  for (Location in unique(as.character(Results$Location))){
    Location_Results=Results[Results$Location==Location,]
    
    Location_x=X[X$Location==Location,]
    Increase=Location_x[,GroundTruthName]-Location_x[match(Location_x$Date-1,Location_x$Date),GroundTruthName]
    Increase=Increase>0
    
    Baseline_pred=Increase[match(Location_Results$Dates+Location_Results$DaysAhead*Location_Results$Aggregation-1*Aggregation,Location_x$Date)]
    
    Model_pred=as.numeric(as.character(Location_Results$Predictions))
    Y_test=Location_Results$Y_test
    
    Baseline_pred=Baseline_pred[!is.na(Y_test)]
    Model_pred=Model_pred[!is.na(Y_test)]
    Y_test=Y_test[!is.na(Y_test)]
    
    ModelOutput=Get_confusion(Y_test,Model_pred)
    BaselineOutput=Get_confusion(Y_test, Baseline_pred)
    
    AllConfusionModel[[length( AllConfusionModel)+1]]=ModelOutput$confusionMatrix
    AllPrecision_Model=c(AllPrecision_Model,ModelOutput$Precision)
    AllRecall_Model=c(AllRecall_Model,ModelOutput$Recall)
    AllAccuracy_Model=c(AllAccuracy_Model,ModelOutput$Accuracy)
    
    AllConfusionBaseline[[length(AllConfusionBaseline)+1]]=BaselineOutput$confusionMatrix
    AllPrecision_baseline=c(AllPrecision_baseline,BaselineOutput$Precision)
    AllRecall_baseline=c(AllRecall_baseline,BaselineOutput$Recall)
    Accuracy_baseline=BaselineOutput$Accuracy
    AllAccuracy_baseline=c(AllAccuracy_baseline,BaselineOutput$Accuracy)
    
    
  }
  
  
  return(list(AllConfusionModel=AllConfusionModel,
         AllPrecision_Model=AllPrecision_Model,
         AllRecall_Model=AllRecall_Model,
         AllAccuracy_Model=AllAccuracy_Model,
         AllConfusionBaseline=AllConfusionBaseline,
         AllPrecision_baseline=AllPrecision_baseline,
         AllRecall_baseline=AllRecall_baseline,
         AllAccuracy_baseline=AllAccuracy_baseline,
         Location= unique(as.character(Results$Location)))
         )
}



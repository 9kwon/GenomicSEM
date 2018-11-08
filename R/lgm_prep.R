
lgm_prep<-function(rawdata,miss_flag = "",waves = "",name = "Run", sex=TRUE,base_age=TRUE,age_waves=TRUE){

  require(psych)
  require(lavaan)
  require(Hmisc)
  
  log.file <- file(paste0(name, ".log"),open="wt")

  begin.time <- Sys.time()
  
  cat(print(paste0("Analysis started at ",begin.time), sep = ""),file=log.file,sep="\n",append=TRUE)
  
  t<-colnames(rawdata[1:2])
  cat(print(paste0(t[1], " is interpreted as a Family ID column. Please ensure this is correct."), sep = ""),file=log.file,sep="\n",append=TRUE)
  cat(print(paste0(t[2], " is interpreted as a User ID column. Please ensure this is correct."), sep = ""),file=log.file,sep="\n",append=TRUE)
  
  if(base_age == TRUE){
  t2<-colnames(rawdata[3+waves])
  cat(print(paste0(t2, " is interpreted as the baseline age column. Please ensure this is correct."), sep = ""),file=log.file,sep="\n",append=TRUE)
  }else{cat(print(paste0("Age at baseline not provided in data file."), sep = ""),file=log.file,sep="\n",append=TRUE)}
  
  if(sex == TRUE){
    if(base_age == TRUE){
      if(age_waves == TRUE){
        t2<-colnames(rawdata[3+waves+waves])
        cat(print(paste0(t2, " is interpreted as the participant sex column. Please ensure this is correct."), sep = ""),file=log.file,sep="\n",append=TRUE)
        if(range(rawdata[3+waves+waves])[1] != 0){cat(print(paste0("It seems that the participant sex column is not coded as 0/1. Please dummy code the sex column."), sep = ""),file=log.file,sep="\n",append=TRUE)}else{cat(print(paste0("It seems that the participant sex column is coded as 0/1. Great! This is what the function is expecting."), sep = ""),file=log.file,sep="\n",append=TRUE)}
      }
    if(age_waves == FALSE){
      t2<-colnames(rawdata[4+waves])
      cat(print(paste0(t2, " is interpreted as the participant sex column. Please ensure this is correct."), sep = ""),file=log.file,sep="\n",append=TRUE)
      if(range(rawdata[4+waves])[1] != 0){cat(print(paste0("It seems that the participant sex column is not coded as 0/1. Please dummy code the sex column."), sep = ""),file=log.file,sep="\n",append=TRUE)}else{cat(print(paste0("It seems that the participant sex column is coded as 0/1. Great! This is what the function is expecting."), sep = ""),file=log.file,sep="\n",append=TRUE)}
    }}
    if(base_age == FALSE){
      t2<-colnames(rawdata[3+waves])
      cat(print(paste0(t2, " is interpreted as the participant sex column. Please ensure this is correct."), sep = ""),file=log.file,sep="\n",append=TRUE)
      if(range(rawdata[3+waves])[1] != 0){cat(print(paste0("It seems that the participant sex column is not coded as 0/1. Please dummy code the sex column."), sep = ""),file=log.file,sep="\n",append=TRUE)}else{cat(print(paste0("It seems that the participant sex column is coded as 0/1. Great! This is what the function is expecting."), sep = ""),file=log.file,sep="\n",append=TRUE)}
    }}else{cat(print(paste0("Participant sex is not provided in data file. This was determined based on the fact that sex = FALSE was specified for the function. Please ensure this was intended."), sep = ""),file=log.file,sep="\n",append=TRUE)}
  
  if(base_age == TRUE){
    s<-3+waves
    t<-quantile(rawdata[[s]],c(.05,.95))
    range2<-t[2]-t[1]
    
    if(range2 >= 20){
      t2<-quantile(rawdata[[s]],c(.50))
      rawdata_y<-subset(rawdata, rawdata[[s]] <= t2)
      y<-data.frame(describe(rawdata_y))
      y$Group<-paste0("Younger Group; <= ", t2, "Years", sep = "")
      y$Count_Floor<-sapply(rawdata_y, function(x) length(which(x== min(x,na.rm=T))))
      y$Count_Ceiling<-sapply(rawdata_y, function(x) length(which(x== max(x,na.rm=T))))
      
      rawdata_o<-subset(rawdata, rawdata[[s]] > t2)
      o<-data.frame(describe(rawdata_o))
      o$Group<-paste0("Older Group; > ", t2, "Years", sep = "")
      o$Count_Floor<-sapply(rawdata_o, function(x) length(which(x== min(x,na.rm=T))))
      o$Count_Ceiling<-sapply(rawdata_o, function(x) length(which(x== max(x,na.rm=T))))
      
      descript<-rbind(y,o)
      #write.csv(descript, file = paste0("Descriptive", name, ".csv", sep =""))
      
      ##putting it in table format is tough to decipher in the log file in terms of formatting
      #x<-knitr::kable(y)
      #dump("x",file=log.file,append=TRUE)
      
      rawdatalist<-list(Y = rawdata_y,O = rawdata_o)
      n<-2
      
    }
    if(range2 >= 30){
      t3<-quantile(rawdata[[s]],c(.33,.66))
      rawdata_y<-subset(rawdata, rawdata[[s]] <= t3[1])
      y<-data.frame(describe(rawdata_y))
      y$Group<-paste0("Younger Group; <= ", t3[1], "Years", sep = "")
      y$Count_Floor<-sapply(rawdata_y, function(x) length(which(x== min(x,na.rm=T))))
      y$Count_Ceiling<-sapply(rawdata_y, function(x) length(which(x== max(x,na.rm=T))))
      
      rawdata_m<-subset(rawdata,(rawdata[[s]] > t3[1] & rawdata[[s]] <= t3[2]))
      m<-data.frame(describe(rawdata_m))
      m$Group<-paste0("Middle Group; <= ", t3[2], "Years", sep = "")
      m$Count_Floor<-sapply(rawdata_m, function(x) length(which(x== min(x,na.rm=T))))
      m$Count_Ceiling<-sapply(rawdata_m, function(x) length(which(x== max(x,na.rm=T))))
      
      rawdata_o<-subset(rawdata, rawdata[[s]] > t3[2])
      o<-data.frame(describe(rawdata_o))
      o$Group<-paste0("Older Group; > ", t3[2], "Years", sep = "")
      o$Count_Floor<-sapply(rawdata_o, function(x) length(which(x== min(x,na.rm=T))))
      o$Count_Ceiling<-sapply(rawdata_o, function(x) length(which(x== max(x,na.rm=T))))
      
      descript<-rbind(y,m,o)
      #write.csv(descript, file = paste0("Descriptive", name, ".csv", sep =""))
      rawdatalist<-list(Y = rawdata_y, M = rawdata_m, O = rawdata_o)
      n<-3
    }
    if(range2 >= 40){
      t4<-quantile(rawdata[[s]],c(.25, .50, .75))
      rawdata_y<-subset(rawdata, rawdata[[s]] <= t4[1])
      y<-data.frame(describe(rawdata_y))
      y$Group<-paste0("Youngest Group; <= ", t4[1], "Years", sep = "")
      y$Count_Floor<-sapply(rawdata_y, function(x) length(which(x== min(x,na.rm=T))))
      y$Count_Ceiling<-sapply(rawdata_y, function(x) length(which(x== max(x,na.rm=T))))
      
      rawdata_m1<-subset(rawdata,(rawdata[[s]] > t4[1] & rawdata[[s]] <= t4[2]))
      m1<-data.frame(describe(rawdata_m1))
      m1$Group<-paste0("Middle Group 1; <= ", t4[2], "Years", sep = "")
      m1$Count_Floor<-sapply(rawdata_m1, function(x) length(which(x== min(x,na.rm=T))))
      m1$Count_Ceiling<-sapply(rawdata_m1, function(x) length(which(x== max(x,na.rm=T))))
      
      rawdata_m2<-subset(rawdata,(rawdata[[s]] > t4[2] & rawdata[[s]] <= t4[3]))
      m2<-data.frame(describe(rawdata_m2))
      m2$Group<-paste0("Middle Group 2; <= ", t4[3], "Years", sep = "")
      m2$Count_Floor<-sapply(rawdata_m2, function(x) length(which(x== min(x,na.rm=T))))
      m2$Count_Ceiling<-sapply(rawdata_m2, function(x) length(which(x== max(x,na.rm=T))))
      
      rawdata_o<-subset(rawdata, rawdata[[s]] > t4[3])
      o<-data.frame(describe(rawdata_o))
      o$Group<-paste0("Oldest Group; > ", t4[3], "Years", sep = "")
      o$Count_Floor<-sapply(rawdata_o, function(x) length(which(x== min(x,na.rm=T))))
      o$Count_Ceiling<-sapply(rawdata_o, function(x) length(which(x== max(x,na.rm=T))))
      
      descript<-rbind(y,m1,m2,o)
      #write.csv(descript, file = paste0("Descriptive", name, ".csv", sep =""))
      
      rawdatalist<-list(Y = rawdata_y, M1 = rawdata_m1, M2 = rawdata_m2, O = rawdata_o)
      n<-4
    }
  }else{
    descript<-data.frame(describe(rawdata))
    descript$Count_Floor<-sapply(rawdata, function(x) length(which(x== min(x,na.rm=T))))
    descript$Count_Ceiling<-sapply(rawdata, function(x) length(which(x== max(x,na.rm=T))))
    
    #write.csv(descript, file = paste0("Descriptive", name, ".csv", sep =""))
    rawdatalist<-list(Y = rawdata)
    n<-1}
  
  lav_results<-list()
  cov_List<-list()
  mean_List<-list()
  nn_List<-list()
  cat((paste0("For missing data patterns below a 1 denotes the data is present and a 0 denotes the data is missing at the particular wave.")),file=log.file,sep="\n",append=TRUE)
  t<-1
  r<-1
  for(t in 1:n){
    
    cat(print(paste0("Running Analyses for Age Group ", t), sep = ""),file=log.file,sep="\n",append=TRUE)
    rawdata_run<-rawdatalist[[t]]

  ##determine initial number of rows
  initial<-nrow(rawdata_run)
  
  ##make NA equal to NA in all cases
  rawdata_run[rawdata_run == miss_flag]<-NA
  
  ##remove any cases with outcome missing across all waves
  rawdata_run<-rawdata_run[rowSums(is.na(rawdata_run[,3:(waves+2)])) != waves, ]

  after<-nrow(rawdata_run)
  
  ##determine number of cases removed for missing data and print to screen 
  diff<-initial-after
  
  cat(print(paste0(diff, " cases were removed due to missing data across all waves for Age Group ", t), sep = ""),file=log.file,sep="\n",append=TRUE)
  
  cat(print(paste0(after, " cases are left after removing participants with missing data across all waves for Age Group ", t), sep = ""),file=log.file,sep="\n",append=TRUE)
  
  ##save names of covariates: (sex [when present] + PCs)
  if(base_age == TRUE){
    if(age_waves == TRUE){
  covs<-colnames(rawdata_run)[(waves+3+waves):ncol(rawdata_run)]}
    if(age_waves == FALSE){
      covs<-colnames(rawdata_run)[(waves+4):ncol(rawdata_run)]}}else{covs<-colnames(rawdata_run)[(waves+3):ncol(rawdata_run)]}

  ##save names of waves: location in file doesnt depend on inclusion of age/sex as waves are listed first prior to both
  wave_names<-colnames(rawdata_run)[3:(waves+2)]
  
  ##standardize the PC covariates and subtract .5 from sex (0, 1)
  if(base_age == TRUE){
    if(age_waves == TRUE){
      if(sex == FALSE){
      rawdata_run[,(waves+3+waves):ncol(rawdata_run)] <- scale(rawdata_run[,(waves+3+waves):ncol(rawdata_run)])}
      if(sex == TRUE){
        rawdata_run[,(waves+4+waves):ncol(rawdata_run)] <- scale(rawdata_run[,(waves+4+waves):ncol(rawdata_run)])
        rawdata_run[,(waves+3+waves)]<-rawdata_run[,(waves+3+waves)]-.5}}
    if(age_waves == FALSE){
        if(sex == FALSE){
          rawdata_run[,(waves+4):ncol(rawdata_run)] <- scale(rawdata_run[,(waves+4):ncol(rawdata_run)])}
        if(sex == TRUE){
          rawdata_run[,(waves+5):ncol(rawdata_run)] <- scale(rawdata_run[,(waves+5):ncol(rawdata_run)])
          rawdata_run[,(waves+4)]<-rawdata_run[,(waves+4)]-.5}}}
    if(base_age == FALSE){
      if(sex == FALSE){
        rawdata_run[,(waves+3):ncol(rawdata_run)] <- scale(rawdata_run[,(waves+3):ncol(rawdata_run)])}
      if(sex == TRUE){
        rawdata_run[,(waves+4):ncol(rawdata_run)] <- scale(rawdata_run[,(waves+4):ncol(rawdata_run)])  
        rawdata_run[,(waves+3)]<-rawdata_run[,(waves+3)]-.5
        }}

  ##model that regresses waves on all covariates, models residual covariances among the waves
  ##and models covariance between PCs and sex covariate (assumed to be first in the covariate columns)
  write.Model1 <- function(k, j) {  
    Model1 <- ""
    for(i in 1:k){
      for (p in 1:j){
        linestart <- paste(wave_names[i], " ~ ", covs[p], sep = "")  
        Model1 <- paste(Model1, linestart, " \n ", sep = "")
      }}
    
    Model2<-""
    for(i in 1:(j-1)){
      linestart2<-paste(covs[i], "~~", covs[i+1], sep="")
    
    if(j-i >= 2){
      linemid2<-""
      for(f in (i+2):j){
        linemid2<-paste(linemid2, " + ", covs[f], sep = "")
      }
    } else {linemid2<- ""}
    Model2<-paste(Model2, linestart2, linemid2, " \n ", sep = "")
    }
    
    Modelsat<-""
    for (i in 1:(k-1)) {
      linestartc <- paste(wave_names[i], "~~", wave_names[i+1],  sep = "")
      if (k-i >= 2) {
        linemidc <- ""
        for (l in (i+2):k) {
          linemidc <- paste(linemidc, " + ", wave_names[l], sep = "")
        }
      } else {linemidc <- ""}
      Modelsat <- paste(Modelsat, linestartc, linemidc, " \n ", sep = "")
    }
    
    Model3<-paste(Model1,Model2,Modelsat, sep = "")
    return(Model3)
  } 

  model<-write.Model1(waves,length(covs))

  ##run the model in lavaan
  fit <- sem(model, data = rawdata_run, missing="FIML",meanstructure=TRUE)
  
  ##save lavaan output into list object
  lav_results[[t]]<-fit

  ##pull vector of betas for covariate effects
  b<-waves*length(covs)
  betas<-as.vector(partable(fit)$est[1:b])
  
  ##pull vector of intercepts
  intercept<-fitted(fit)$mean[1:waves]
  
  if(base_age == TRUE){
    if(age_waves == TRUE){
      covs<-as.matrix(rawdata_run[,(waves+3+waves):ncol(rawdata_run)])}
    if(age_waves == FALSE){
      covs<-as.matrix(rawdata_run[,(waves+4):ncol(rawdata_run)])}}  
  if(base_age == FALSE){
    covs<-as.matrix(rawdata_run[,(waves+3):ncol(rawdata_run)])}  

  ##counters for loop below
  g<-1 
  u<-1
  p<-ncol(covs)
  p2<-ncol(covs)
  
  for(g in 1:waves){
    ##pull set of covariate betas for an individual wave  
    betas1<-as.matrix(betas[u:p])
    
    ##do the matrix algebra of observed covariates %*% vector of betas + the intercept 
    ##and subtract from observed score in the data file for the corresponding age group
    rawdata_run[,g+2]<-rawdata_run[,g+2]-((covs%*%betas1)+intercept[g])
    
    u<-u+p2
    p<-p2+p
  }
  
  ##create data.frame coding missing participants as missing
  data_miss<-data.frame(ifelse(is.na(rawdata_run[,3:(waves+2)]),0,1))
  
  ##create table of different missing data patterns
  x<-data.frame(table(data_miss))

  ##percentage of cases with this missing data pattern
  x$proportion_of_data<-x$Freq/after
  
  initial2<-nrow(x)
  
  subse=as.data.frame(matrix(NA,ncol=waves,nrow=waves))
  colnames(subse)=colnames(x)[1:waves]
  p<-1
  for(p in 1:waves){
    subse[,p]<-c(rep(1,p), rep(0,(waves-p)))  
  }
  subse<-t(subse)
  colnames(subse)<-colnames(x)[1:waves]
  subse<-data.frame(subse)
 
  ##write removed missing data patterns to log file
  x3<-subset(x, x$proportion_of_data <= .02 & !(interaction(x[,1:waves]) %in% (interaction(subse[,1:waves]))))
  
  cat("Below are the missing data patterns that were excluded from the file for Age Group ", t, ": ", file=log.file,sep="\n",append=TRUE)
  capture.output(x3, file = log.file)
  
  ##subset to missing data patterns with at least the threshold frequency in the data or a pattern of subsequent dropout
  x<-subset(x, x$proportion_of_data >= .02 | interaction(x[,1:waves]) %in% (interaction(subse[,1:waves])))
 
  cat("Below are the missing data patterns that were included from the file for Age Group ", t, ": ",file=log.file,sep="\n",append=TRUE)
  capture.output(x, file = log.file)
  
  ##determine number of missing data patterns with freq < 2% and print to screen
  after2<-nrow(x)
  diff2<-initial2-after2
  
  print(paste0(diff2, " missing data patterns were excluded due to < 2% frequency in the data for Age Group ", t, ". See log file
               for specific patterns removed."), sep = "")
  
  cat(paste0(diff2, " missing data patterns were excluded due to < 2% frequency in the data for Age Group ", t, sep = ""),file=log.file,sep="\n",append=TRUE)
  
  cat(print(paste0(after2, " missing data patterns were retained with > 2% frequency or a missing data pattern of subsequent dropout for Age Group ", t, sep = ""),file=log.file,sep="\n",append=TRUE))
  
  lvls <- c(0, 1)
  data_miss2<-data.frame(lapply(data_miss,factor,levels=lvls))
  
  x2<-x[,1:waves]
  x2[] <-  lapply(x2, factor, levels=lvls)

  data_List<-vector(mode ="list",length=after2)
  
  phenotype<-paste(name, 'AgeBin',t, 'MissingPattern', 1:nrow(x2), sep="")

  p<-1
  row.names(rawdata_run)<-1:nrow(rawdata_run)

for(p in 1:nrow(x2)){
  
    ##subset data_miss2 (full dataset with missing data patterns) to one of the missing patterns retained with > 2% frequency
    data_miss3<-subset(data_miss2,interaction(data_miss2[,1:waves]) %in% interaction(x2[p,1:waves]))
     
    ##save the row.names to subset out rawdata
    f<-as.vector(row.names(data_miss3))

    rawdata2<-subset(rawdata_run[,3:(waves+2)], row.names(rawdata_run) %in% f)
    cov_List[[r]]<-stats::cov(rawdata2)
    mean_List[[r]]<-colMeans(rawdata2)
    nn_List[[r]]<-nrow(rawdata2)
    r<-r+1
    
    data_List[[p]]<-subset(rawdata_run[,1:(waves+2)], row.names(rawdata_run) %in% f)
    
    colnames(data_List[[p]])<-colnames(rawdata_run[1:(waves+2)])
    
    ##add in bogus data for missing cells
    #cov_List[[p]][is.na(cov_List[[p]])]=0 
    #diag(cov_List[[p]])<-ifelse(diag(cov_List[[p]]) == 0, 1, diag(cov_List[[p]]))
    #mean_List[[p]][is.na(mean_List[[p]])]=0
    #t<-ifelse(paste(data_miss2[p,]) == "2", 1, 0)
    
    ##output the data file for plink
    write.table(data_List[[p]], file =  paste0(phenotype[[p]],".txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE, col.names=FALSE)
    
  }
  }

  ##write lavaan results to .RData file
  #save(lav_results,file=paste0("LavaanOutput", name, ".RData",sep=""))
  end.time <- Sys.time()
  
  total.time <- difftime(time1=end.time,time2=begin.time,units="sec")
  mins <- floor(floor(total.time)/60)
  secs <- total.time-mins*60

  cat(print(paste0("Analysis ended at ",end.time), sep = ""),file=log.file,sep="\n",append=TRUE)
  
  cat(print(paste0("Analysis took ",mins," minutes and ",secs," seconds"), sep = ""),file=log.file,sep="\n",append=TRUE)
  
  results<-(list(cov_List=cov_List,mean_List=mean_List,nn=nn_List,descript=descript,lavoutput=lav_results))
  return(list(cov_List=cov_List,mean_List=mean_List,nn=nn_List,descript=descript,lavoutput=lav_results))
  save(results,file=paste0("Results", name, ".RData",sep=""))
  
}


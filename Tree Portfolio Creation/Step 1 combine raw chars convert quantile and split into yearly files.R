rm(list = ls())
setwd("E:/Research/Random Forest/R_source")

feats_list = c('LME','BEME','r12_2','OP','Investment','ST_Rev','LT_Rev','AC','IdioVol',"LTurnover")

# Loop through all 36 combination of LME + feat1 + feat2
for (feat1 in 2:(length(feats_list)-1)){
  for (feat2 in (feat1+1):length(feats_list)){
    print(feat1)
    print(feat2)
    feats = c('LME',feats_list[feat1], feats_list[feat2])
    
    dir.create(file.path(paste("../data/data_chunk_files_quantile/",paste(feats,collapse = '_'),'/',sep="")), showWarnings = FALSE)
    year_min=1963
    y_min = 1963
    y_max = 2016
    
    RET <- read.csv("../data/ret_characteristics/RET.csv", stringsAsFactors=FALSE)
    
    features = list()
    for (i in 1:length(feats)){
      features[[i]] <- read.csv(paste("../data/ret_characteristics/",feats[i],".csv",sep=''), stringsAsFactors=FALSE)
    }
    
    date=RET$date
    RET=RET[,-1]
    colnames(RET)=substring(colnames(RET),5)
    
    convert_quantile<-function(x){
      x[!is.na(x)]=(rank(na.omit(x))-1)/(length(na.omit(x))-1)
      return(x)
    }
    
    for (i in 1:length(feats)){
      features[[i]]=features[[i]][,-1]
      colnames(features[[i]])=substring(colnames(features[[i]]),nchar(feats[i])+2)
    }
    
    # Convert raw characteristics to Quantile numbers
    for (j in 1:nrow(features[[i]])){
      print(j)
      for (i in 1:length(feats)){
        features[[i]][j,]=convert_quantile(as.numeric(features[[i]][j,]))
      }
    }
    
    # Add the raw size info for weighting purpose
    lme <- read.csv(paste("../data/ret_characteristics/",'lme',".csv",sep=''), stringsAsFactors=FALSE)
    lme=lme[,-1]
    colnames(lme)=substring(colnames(lme),5)
    
    # Combine the info and save the data into yearly files
    for (year in y_min:y_max){
      data_train=data.frame(yy=numeric(),mm=numeric(),date=numeric(),permno=character(),
                            ret=numeric())
      for (i in 1:length(feats)){
        data_train[,feats[i]]=numeric(0)
      }
      data_train[,'size']=numeric(0)
      
      for (month in 1:12){
        i=12*(year-year_min)+month
        retm=(RET[i,!is.na(RET[i,])])
        feature_m=list()
        for (j in 1:length(feats)){
          feature_m[[j]]=features[[j]][i,!is.na(features[[j]][i,])]
        }
        lmem=lme[i,!is.na(lme[i,])]
        
        inter=intersect(colnames(retm),colnames(feature_m[[1]]))
        for (j in 2:length(feats)){
          inter=intersect(inter,colnames(feature_m[[j]]))
        }
        inter=intersect(inter,colnames(lmem))
        
        data_m = data.frame(yy=rep(year,length(inter)),
                            mm=rep(month,length(inter)),
                            date=rep(date[i],length(inter)),
                            permno=inter,
                            ret=as.numeric(retm[,inter]))
        for (j in 1:length(feats)){
          data_m[,feats[[j]]]=as.numeric(feature_m[[j]][,inter])
        }
        data_m[,'size']=as.numeric(lmem[,inter])
        
        data_train=rbind(data_train, data_m)
        
      }
      write.csv(data_train,file=paste("../data/data_chunk_files_quantile/",paste(feats,collapse = '_'),"/y",year,".csv",sep=""),row.names = FALSE)
    }
  }
}


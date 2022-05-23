rm(list = ls())
setwd("D:/Research/Random Forest/R_source")
library(dplyr)
library(doSNOW)
library(foreach)
library(tcltk)

# Generate value-weighted returns and min/max of characteristics of tree portfolios for one tree
tree_portfolio = function(data_path, feat_list, tree_depth, q_num, y_min, y_max, file_prefix){
  ret_table = array(0,dim=c((y_max-y_min+1)*12,q_num^(tree_depth+1)-1))
  feat_min_table = list()
  feat_max_table = list()
  for (f in 1:n_feats){
    feat_min_table[[f]] = array(0,dim=c((y_max-y_min+1)*12,q_num^(tree_depth+1)-1))
    feat_max_table[[f]] = array(0,dim=c((y_max-y_min+1)*12,q_num^(tree_depth+1)-1))
  }
  
  for(y in c(y_min:y_max)){
    if(y%%5==0){print(y)}
    data_filenm = paste(data_path,file_prefix,toString(y),'.csv',sep='')
    df_m = read.csv(data_filenm)
    df_m = tree_portfolio_y(df_m, feat_list, tree_depth, q_num)
    
    for (i in 1:tree_depth){
      for(k in c(1:i)){
        df_m[paste('port',i,sep='')] = df_m[paste('port',i,sep='')] + (df_m[toString(k)]-1)*(q_num^(i-k))
      }
    }
    
    for (i in 0:tree_depth){
      for(m in c(1:12)){
        for(k in c(1:q_num^i)){
          mask_port = (df_m['mm']==m)&(df_m[paste('port',i,sep='')]==k)
          company_val = df_m['size'][mask_port]
          ret_mon = df_m['ret'][mask_port]
          feat_m = list()
          
          ret_table[12*(y-y_min)+m,2^i-1+k] = ret_mon%*%company_val/sum(company_val)
          
          for (f in 1:n_feats){
            feat_m[[f]] = df_m[feats[f]][mask_port]
            feat_min_table[[f]][12*(y-y_min)+m,2^i-1+k] = min(feat_m[[f]])
            feat_max_table[[f]][12*(y-y_min)+m,2^i-1+k] = max(feat_m[[f]])
          }
        }
      }
    }
  }  
  ret_list = list()
  ret_list[[1]]=ret_table
  for (f in 1:n_feats){
    ret_list[[2*f]]=feat_min_table[[f]]
    ret_list[[2*f+1]]=feat_max_table[[f]]
  }
  return(ret_list)
}

tree_portfolio_y = function(df_tmp, feat_list, tree_depth, q_num){
  for(k in c(1:tree_depth)){
    df_tmp[toString(k)] = 0
  }
  for (i in 0:tree_depth){
    df_tmp[paste('port',i,sep='')] = 1
  }
  
  for(m in c(1:12)){
    mask_m = (df_tmp['mm']==m)
    df_m = df_tmp[mask_m,]
    df_m[toString(1)] = ntile(df_m[feat_list[1]], q_num)
    for(val in c(1:q_num)){
      k = 1
      mask_m_tmp = (df_m[toString(1)] == val)
      df_m_recurse = df_m[mask_m_tmp,]
      df_m[mask_m_tmp,] = tree_portfolio_y_helper(df_m_recurse, feat_list, k, tree_depth, q_num)
    }
    df_tmp[mask_m,] = df_m
  }
  return(df_tmp)
}

tree_portfolio_y_helper = function(df_m_recurse, feat_list, k, tree_depth, q_num){
  #print(k)
  k = k+1
  df_m_recurse[toString(k)] = ntile(df_m_recurse[feat_list[k]], q_num)
  if(k<tree_depth){
    for(val in c(1:q_num)){
      mask_recurse = (df_m_recurse[toString(k)]==val)
      df_m_recurse[mask_recurse,] = tree_portfolio_y_helper(df_m_recurse[mask_recurse,], 
                                                            feat_list, k, tree_depth, q_num)
    }
  }
  
  return(df_m_recurse)
}

#################
### Main code ###
#################

# Parameters to change
tree_depth = 4

feats_list = c('LME','BEME','r12_2','OP','Investment','ST_Rev','LT_Rev','AC','IdioVol',"LTurnover")


for (feat1 in 2:(length(feats_list)-1)){
  for (feat2 in (feat1+1):10){
    print(feat1)
    print(feat2)
    feats = c('LME', feats_list[feat1], feats_list[feat2])
    
    n_feats = length(feats)
    main_dir = '../data/tree_portfolio_quantile'
    sub_dir = paste(feats,collapse = '_')
    dir.create(file.path(main_dir, sub_dir), showWarnings = FALSE)
    data_path = paste('../data/data_chunk_files_quantile/',paste(feats,collapse = '_'),'/', sep='')
    
    y_min = 1964
    y_max = 2016
    q_num = 2
    
    feat_list_base = feats
    feat_list_id_k = expand.grid(rep(list(1:n_feats),tree_depth))
    
    # Note here I am using CPU parallel computing. You can just replace the foreach loop to for loop to go back to single thread
    cl<-makeCluster(16)
    registerDoSNOW(cl)
    foreach(k=c(1:n_feats^tree_depth),.packages='dplyr') %dopar% {
      print(k)
      file_id = paste(feat_list_id_k[k,],collapse = '')
      feat_list = feat_list_base[as.numeric(feat_list_id_k[k,])]
      
      ret = tree_portfolio(data_path, feat_list, tree_depth, q_num, y_min, y_max, 'y')
      
      ret_table = ret[[1]]
      
      if (tree_depth == 3){
        colnames(ret_table)=c(1,11,12,111,112,121,122,1111,1112,1121,1122,1211,1212,1221,1222)
      }else if (tree_depth == 4){
        colnames(ret_table)=c(1,11,12,111,112,121,122,1111,1112,1121,1122,1211,1212,1221,1222,11111,11112,11121,11122,11211,11212,11221,11222,12111,12112,12121,12122,12211,12212,12221,12222)
      }else if (tree_depth == 5){
        colnames(ret_table)=c(1, 11, 12, 111, 112, 121, 122, 1111, 1112, 1121, 1122, 1211, 1212, 1221, 1222, 11111, 11112, 11121, 11122, 11211, 11212, 11221, 11222, 12111, 12112, 12121, 12122, 12211, 12212, 12221, 12222, 111111, 111112, 111121, 111122, 111211, 111212, 111221, 111222, 112111, 112112, 112121, 112122, 112211, 112212, 112221, 112222, 121111, 121112, 121121, 121122, 121211, 121212, 121221, 121222, 122111, 122112, 122121, 122122, 122211, 122212, 122221, 122222) 
      }
      
      write.table(ret_table, paste(file.path(main_dir, sub_dir),'/', file_id,'ret.csv', sep=''), sep=',',row.names=F) 
      
      feat_min_table = list()
      feat_max_table = list()
      for (f in 1:n_feats){
        feat_min_table[[f]] = ret[[2*f]]
        
        if (tree_depth == 3){
          colnames(feat_min_table[[f]])=c(1,11,12,111,112,121,122,1111,1112,1121,1122,1211,1212,1221,1222)
        }else if (tree_depth == 4){
          colnames(feat_min_table[[f]])=c(1,11,12,111,112,121,122,1111,1112,1121,1122,1211,1212,1221,1222,11111,11112,11121,11122,11211,11212,11221,11222,12111,12112,12121,12122,12211,12212,12221,12222)
        }else if (tree_depth == 5){
          colnames(feat_min_table[[f]])=c(1, 11, 12, 111, 112, 121, 122, 1111, 1112, 1121, 1122, 1211, 1212, 1221, 1222, 11111, 11112, 11121, 11122, 11211, 11212, 11221, 11222, 12111, 12112, 12121, 12122, 12211, 12212, 12221, 12222, 111111, 111112, 111121, 111122, 111211, 111212, 111221, 111222, 112111, 112112, 112121, 112122, 112211, 112212, 112221, 112222, 121111, 121112, 121121, 121122, 121211, 121212, 121221, 121222, 122111, 122112, 122121, 122122, 122211, 122212, 122221, 122222) 
        }
        write.table(feat_min_table[[f]], paste(file.path(main_dir, sub_dir),'/', file_id,feats[f],'_min.csv', sep=''), sep=',',row.names=F) 
        
        feat_max_table[[f]] = ret[[2*f+1]]
        
        if (tree_depth == 3){
          colnames(feat_max_table[[f]])=c(1,11,12,111,112,121,122,1111,1112,1121,1122,1211,1212,1221,1222)
        }else if (tree_depth == 4){
          colnames(feat_max_table[[f]])=c(1,11,12,111,112,121,122,1111,1112,1121,1122,1211,1212,1221,1222,11111,11112,11121,11122,11211,11212,11221,11222,12111,12112,12121,12122,12211,12212,12221,12222)
        }else if (tree_depth == 5){
          colnames(feat_max_table[[f]])=c(1, 11, 12, 111, 112, 121, 122, 1111, 1112, 1121, 1122, 1211, 1212, 1221, 1222, 11111, 11112, 11121, 11122, 11211, 11212, 11221, 11222, 12111, 12112, 12121, 12122, 12211, 12212, 12221, 12222, 111111, 111112, 111121, 111122, 111211, 111212, 111221, 111222, 112111, 112112, 112121, 112122, 112211, 112212, 112221, 112222, 121111, 121112, 121121, 121122, 121211, 121212, 121221, 121222, 122111, 122112, 122121, 122122, 122211, 122212, 122221, 122222) 
        }
        
        write.table(feat_max_table[[f]], paste(file.path(main_dir, sub_dir),'/', file_id,feats[f],'_max.csv', sep=''), sep=',',row.names=F) 
      }
    }
    stopCluster(cl)
  }
}




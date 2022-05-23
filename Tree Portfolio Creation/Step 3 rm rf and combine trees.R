rm(list = ls())
library("lattice")

setwd("E:/Research/Random Forest/R_source")

# minus the risk free rates from portfolio returns
remove_rf = function(port_ret, factor_path){
  file_nm = paste(factor_path,'rf_factor.csv',sep='')
  r_f = read.table(file_nm, header = F, sep=',')
  for(i in c(1:ncol(port_ret))){
    port_ret[,i] = port_ret[,i]-(as.numeric(as.matrix(r_f)))/100
  }
  return(port_ret)
}

feats_list = c('LME','BEME','r12_2','OP','Investment','ST_Rev','LT_Rev','AC','IdioVol',"LTurnover")

# Combine portfolios from different trees and dedup the higher level trees
for (feat1 in 1:1){
  for (feat2 in (feat1+1):(length(feats_list)-1)){
    for (feat3 in (feat2+1):length(feats_list)){
      print(feat1)
      print(feat2)
      print(feat3)
      feats = c(feats_list[feat1], feats_list[feat2], feats_list[feat3])
      n_feats=length(feats)
      tree_depth = 4
      
      factor_path = '../data/factor/'
      data_path = '../data/tree_portfolio_quantile/'
      
      tree_sort_path_base = '../data/tree_portfolio_quantile/'
      tree_sort_path = paste(tree_sort_path_base,paste(feats,collapse = '_'),'/',sep='')
      
      
      # result for tree sort
      feat_list_id_k = expand.grid(rep(list(1:n_feats),tree_depth))
      
      k=1
      file_name_id = paste(paste(feat_list_id_k[k,],collapse = ''),'ret.csv',sep='')
      file_name = paste(tree_sort_path, file_name_id, sep='')
      port_ret0 = read.table(file_name, header=T, sep=',')
      colnames(port_ret0) = paste(paste(feat_list_id_k[k,],collapse = ''),substring(colnames(port_ret0),2),sep=".")
      port_ret = port_ret0
      
      for (k in 2:n_feats^tree_depth){
        file_name_id = paste(paste(feat_list_id_k[k,],collapse = ''),'ret.csv',sep='')
        file_name = paste(tree_sort_path, file_name_id, sep='')
        port_ret0 = read.table(file_name, header=T, sep=',')
        colnames(port_ret0) = paste(paste(feat_list_id_k[k,],collapse = ''),substring(colnames(port_ret0),2),sep=".")
        port_ret = cbind(port_ret,port_ret0)
      }
      
      port_transpose=t(as.matrix(port_ret))
      keep = !duplicated(port_transpose)
      port_dedup = port_ret[,keep] 
      
      port_ret = remove_rf(port_dedup, factor_path)
      print(ncol(port_ret))
      write.table(port_ret, paste('../data/tree_portfolio_quantile/', paste(feats,collapse = '_'),'/level', '_all_','excess_combined.csv', sep=''), sep=',',row.names=F) 
      
      
      
      for (i in 1:n_feats){
        k=1
        file_name_id = paste(paste(feat_list_id_k[k,],collapse = ''),feats[i],'_min.csv',sep='')
        file_name = paste(tree_sort_path, file_name_id, sep='')
        port_ret0 = read.table(file_name, header=T, sep=',')
        colnames(port_ret0) = paste(paste(feat_list_id_k[k,],collapse = ''),substring(colnames(port_ret0),2),sep=".")
        port_ret = port_ret0
        
        for (k in 2:n_feats^tree_depth){
          file_name_id = paste(paste(feat_list_id_k[k,],collapse = ''),feats[i],'_min.csv',sep='')
          file_name = paste(tree_sort_path, file_name_id, sep='')
          port_ret0 = read.table(file_name, header=T, sep=',')
          colnames(port_ret0) = paste(paste(feat_list_id_k[k,],collapse = ''),substring(colnames(port_ret0),2),sep=".")
          port_ret = cbind(port_ret,port_ret0)
        }
        port_ret = port_ret[,keep] 
        print(ncol(port_ret))
        write.table(port_ret, paste('../data/tree_portfolio_quantile/', paste(feats,collapse = '_'),'/level', '_all_',feats[i],'_min.csv', sep=''), sep=',',row.names=F)
        
        k=1
        file_name_id = paste(paste(feat_list_id_k[k,],collapse = ''),feats[i],'_max.csv',sep='')
        file_name = paste(tree_sort_path, file_name_id, sep='')
        port_ret0 = read.table(file_name, header=T, sep=',')
        colnames(port_ret0) = paste(paste(feat_list_id_k[k,],collapse = ''),substring(colnames(port_ret0),2),sep=".")
        port_ret = port_ret0
        
        for (k in 2:n_feats^tree_depth){
          file_name_id = paste(paste(feat_list_id_k[k,],collapse = ''),feats[i],'_max.csv',sep='')
          file_name = paste(tree_sort_path, file_name_id, sep='')
          port_ret0 = read.table(file_name, header=T, sep=',')
          colnames(port_ret0) = paste(paste(feat_list_id_k[k,],collapse = ''),substring(colnames(port_ret0),2),sep=".")
          port_ret = cbind(port_ret,port_ret0)
        }
        port_ret = port_ret[,keep] 
        print(ncol(port_ret))
        write.table(port_ret, paste('../data/tree_portfolio_quantile/', paste(feats,collapse = '_'),'/level', '_all_',feats[i],'_max.csv', sep=''), sep=',',row.names=F)
        
      
      }
    }
  }
}
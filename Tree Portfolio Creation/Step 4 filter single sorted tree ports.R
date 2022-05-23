rm(list = ls())
setwd("D:/Research/Random Forest/R_source")


filter = c("X1111", "X2222", "X3333")

for (feat1_n in 1:8){
  for (feat2_n in (feat1_n+1):9){
    for (feat3_n in (feat2_n+1):10){
      feats_chosen = c(feats[feat1_n],feats[feat2_n],feats[feat3_n])
      
      sub_dir = paste(feats_chosen,collapse = '_')
      
      ports_path = paste('../data/tree_portfolio_quantile/', sub_dir, '/', sep='')
      port_ret = read.table(paste(ports_path, 'level_all_excess_combined.csv', sep=''), header=T, sep=',')
      
      filt = (substring(colnames(port_ret), 1, 5) %in% filter) & (sapply(colnames(port_ret), nchar) == 11)
      port_ret = port_ret[,!filt]
      
      for (f in 1:3){
        f_min = read.table(paste(ports_path, 'level_all_',feats_chosen[f],'_min.csv', sep=''), header=T, sep=',')
        f_max = read.table(paste(ports_path, 'level_all_',feats_chosen[f],'_max.csv', sep=''), header=T, sep=',')
        f_min = f_min[,!filt]
        f_max = f_max[,!filt]
        write.csv(f_min, paste(ports_path, 'level_all_',feats_chosen[f],'_min_filtered.csv', sep=''), row.names = F)
        write.csv(f_max, paste(ports_path, 'level_all_',feats_chosen[f],'_max_filtered.csv', sep=''), row.names = F)
      }
      write.csv(port_ret, paste(ports_path, 'level_all_excess_combined_filtered.csv', sep=''), row.names = F)
    }
  }
}

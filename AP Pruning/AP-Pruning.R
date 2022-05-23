rm(list = ls())
setwd(substring(dirname(rstudioapi::getSourceEditorContext()$path), 1, nchar(dirname(rstudioapi::getSourceEditorContext()$path)) - 6))
source(paste(dirname(rstudioapi::getSourceEditorContext()$path), '/lasso_valid_par_full.R', sep=''))

feats = c('lme','beme','r12_2','op','Investment','st_rev','lt_rev','ac','idiovol','lturnover')

excludes = c()

n_train_valid = 360
cvN = 3

for (f1 in 1:1){
  for (f2 in (f1+1):9){
    for (f3 in (f2+1):10){

      chars = c(feats[f1], feats[f2], feats[f3])
      maindir = '../data/lasso_full_valid/'
      subdir = paste(chars, collapse = '_')
      print(subdir)

      # if (subdir %in% excludes){
      #   next
      # }

      ports <- read.csv(paste("../data/tree_portfolio_quantile/", subdir, "/level_all_excess_combined_filtered.csv", sep=""), stringsAsFactors=FALSE)
      depths = nchar(colnames(ports)) - 7

      coln = colnames(ports)
      for (i in 1:length(coln)){
        coln[i] = paste(substring(coln[i],2,nchar(substring(coln[i], 7, 11))), substring(coln[i], 7, 11), sep='.')
      }
      colnames(ports) = coln

      adj_w = 1/sqrt(2^depths)

      adj_ports = ports
      for (i in 1:length(adj_w)){
        adj_ports[,i] = ports[,i] * adj_w[i]
      }

      lambda0 = seq(0.0, 0.9, 0.05)
      lambda2 = 0.1^seq(5, 8, 0.25)
  
      lasso_valid_full(adj_ports, lambda0, lambda2, maindir, subdir, adj_w, n_train_valid, cvN, parallel = 19)
    }
  }
}


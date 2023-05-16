#readData <- function() {

print("reading data")
  rho_percentile <- readRDS('data/rho_percentile.RDS')
  rho_percentile<-as.data.frame(rho_percentile)
  
  
  
  orf_info<- readRDS("data/orf_info.RDS")
  gsea_df <- readRDS("data/gsea_df.RDS")
  
print("data loaded")
# con = DBI::dbConnect(RMariaDB::MariaDB(), groups='mariaDB')
# 
  # translation_info 
  
# orf_info = DBI::dbGetQuery(con, "select * from omer.coexpressionOrfList_blaste4")
# 
# saveRDS(orf_info, 'data/orf_info.RDS')
# 
# gsea_df <- DBI::dbGetQuery(con, "select * from omer.fgsea_go_slim")
# 
# saveRDS(gsea_df, 'data/gsea_df.RDS')

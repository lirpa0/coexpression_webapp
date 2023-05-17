#readData <- function() {

print("reading data")
  rho_percentile <- readRDS('data/rho_percentile.RDS')
  rho_percentile<-as.data.frame(rho_percentile)
  
  
  
  orf_info<- readRDS("data/orf_info.RDS")
  gsea_df <- readRDS("data/gsea_df.RDS")
  sequence_info <- readRDS('data/sequence_info.RDS')
  
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

# THIS LACKS INFO FOR SPLICED GENES!!
# sequence_info <- DBI::dbGetQuery(con, "select * from carly.carly_orf_seqs")
# 
# sequence_info_sub <- sequence_info%>%inner_join(orf_info[,c('transcript','orf_id','gene')])
# 
# saveRDS(sequence_info_sub, 'data/sequence_info.RDS')

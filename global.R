#readData <- function() {
library(magrittr)
library(stringr)
library(dplyr)

print("reading data")
  rho_percentile <- readRDS('data/rho_percentile.RDS')
  rho_percentile<-as.data.frame(rho_percentile)



  orf_info<- readRDS("data/orf_info.RDS")%>%mutate(orf_id_or_sys_name = ifelse(is.na(gene)==F, gene, orf_id))
  orf_ids <- as.list(orf_info$orf_id_or_sys_name)
  names(orf_ids) =orf_info$transcript
  is_canonical <- orf_info$is_canonical
  names(is_canonical)<-orf_info$orf_id_or_sys_name

  gsea_df <- readRDS("data/gsea_df.RDS")
  sequence_info <- readRDS('data/sequence_info.RDS')

print("data loaded")
# con = DBI::dbConnect(RMariaDB::MariaDB(), groups='mariaDB')
#
  # translation_info


# orf_info = DBI::dbGetQuery(con, "select * from omer.coexpressionOrfList_blaste4")
#
# orf_info%>%left_join(
#   org.Sc.sgd.db::org.Sc.sgd.db%>%AnnotationDbi::select(keys=AnnotationDbi::keys(.),
#                                                        columns = 'GENENAME')%>%tidyr::drop_na(),
#   by=c('gene'='ORF')
# )%>%saveRDS('data/orf_info.RDS')
# library(GO.db)
#
# gsea_df <- DBI::dbGetQuery(con, "select * from omer.fgsea_go_slim")%>%left_join(
# AnnotationDbi::select(GO.db::GO.db, keys=gsea_df$pathway%>%unique, columns = c('TERM')),
# by=c('pathway'='GOID'))
# saveRDS(gsea_df, 'data/gsea_df.RDS')

# THIS LACKS INFO FOR SPLICED GENES!!
 # sequence_info <- DBI::dbGetQuery(con, "select * from april.orf_sequences")
#
# sequence_info_sub <- sequence_info%>%inner_join(orf_info[,c('transcript','orf_id','gene')], by=c('seq_name'='transcript'))%>%
#     rename("transcript" = "seq_name")
#
# saveRDS(sequence_info_sub, 'data/sequence_info.RDS')

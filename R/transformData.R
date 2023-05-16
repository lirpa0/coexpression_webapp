
getSeqData<-function(orf_name){
  sequence_info_df <- data.frame(transcript = c("chr1_43730",'chr1_67378'),
                                 gene = c(NA,'gene1'),
                                 cds_seq = c("ATG...", "ATGagsgas..."),
                                 aa_seq = c("M...", "M..."))
  seq_info <- sequence_info_df%>%dplyr::filter(transcript == orf_name | gene ==orf_name)
  
  seq_info
}

getGseaData<-function(orf_name){
  
  gsea_data <- gsea_df %>%
    filter(transcript == orf_name) %>%
    filter(padj < 0.05) %>%
    mutate(
      padj = formatC(padj, format = "e", digits = 1),
      NES = round(NES, 2)
    )
  
  gsea_data
  
}


getCoexpData <- function(orf_name) {
  
  coexp_data<-data.frame('coexpression'=rho_percentile[,which(colnames(rho_percentile)==orf_name)],
                         'transcript'=rownames(rho_percentile))
  
  coexp_data <- coexp_data %>%
    left_join(orf_info, by = "transcript") %>%
    mutate(coexpression_percentile = round(coexpression, 3)) %>%
    arrange(desc(coexpression_percentile))
  coexp_data_display <- coexp_data[,c("transcript", 'gene', "is_canonical", "coexpression_percentile")]
  colnames(coexp_data_display)<-c('transcript','gene','is_canonical','coexpression percentile')
  
  
  coexp_data_display
}


convertOrfName <- function(orf_name) {
  if (str_detect(orf_name, "Y") & orf_name %in% orf_info$gene) {
    transcript <- orf_info %>%
      filter(gene == orf_name) %>%
      select(transcript) %>%
      pull()
  } else if (str_detect(orf_name, "orf") & orf_name %in% orf_info$orf_id) {
    transcript <- orf_info %>%
      filter(orf_id == orf_name) %>%
      select(transcript) %>%
      pull()
  } else if (orf_name %in% orf_info$transcript) {
    transcript <- orf_name
  }else{
    transcript <- NULL
  }
  transcript
}
getSeqData <- function(orf_name) {
  seq_info <- sequence_info %>% dplyr::filter(transcript == orf_name)

  seq_info
}

getNetwork <- function(orf_name, coexpression_df, thr) {
  orfs_in_subnet <- coexpression_df$transcript[coexpression_df$`coexpression percentile` >= thr]
  weighted_adj <- rho_percentile[orfs_in_subnet, orfs_in_subnet]

  adj <- round(weighted_adj, 3) >= thr
  rownames(adj) <- map_chr(rownames(adj), function(x) orf_ids[[x]])
  colnames(adj) <- rownames(adj)
  g <- igraph::graph_from_adjacency_matrix(as.matrix(adj),
    mode = "undirected"
  ) %>% simplify()
  
  groups<-ifelse(V(g)$name == orf_ids[[orf_name]],orf_ids[[orf_name]] ,
         ifelse(
         is_canonical[V(g)$name] == 'canonical' ,"Canonical",
         "Noncanonical" ))
  gD3 <- igraph_to_networkD3(g, group = groups)

  gD3
}



getGseaData <- function(orf_name) {
  gsea_data <- gsea_df %>%
    filter(transcript == orf_name) %>%
    arrange(padj)%>%
    mutate(
      padj = formatC(padj, format = "e", digits = 1),
      NES = round(NES, 2)
    )

  gsea_data
}


getCoexpData <- function(orf_name) {
  coexp_data <- data.frame(
    "coexpression" = rho_percentile[, which(colnames(rho_percentile) == orf_name)],
    "transcript" = rownames(rho_percentile)
  )

  coexp_data <- coexp_data %>%
    left_join(orf_info, by = "transcript") %>%
    mutate(coexpression_percentile = round(coexpression, 3)) %>%
    arrange(desc(coexpression_percentile))
  coexp_data_display <- coexp_data[, c("orf_id","transcript", "gene", "is_canonical", "coexpression_percentile")]
  colnames(coexp_data_display) <- c("orf_id","transcript", "gene", "is_canonical", "coexpression percentile")


  coexp_data_display
}

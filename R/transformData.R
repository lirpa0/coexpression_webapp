
convertOrfName <- function(orf_name) {
  if (str_detect(orf_name, "Y")) {
    transcript <- orf_info %>%
      filter(gene == orf_name) %>%
      select(transcript) %>%
      pull()
  } else if (str_detect(orf_name, "orf")) {
    transcript <- orf_info %>%
      filter(orf_id == orf_name) %>%
      select(transcript) %>%
      pull()
  } else {
    transcript <- orf_name
  }

  transcript
}
getSeqData <- function(orf_name) {
  seq_info <- sequence_info %>% dplyr::filter(transcript == orf_name | gene == orf_name)

  seq_info
}

getNetwork <- function(orf_name, coexpression_df, thr) {
  orfs_in_subnet <- coexpression_df$transcript[coexpression_df$`coexpression percentile` >= thr]
  weighted_adj <- rho_percentile[orfs_in_subnet, orfs_in_subnet]

  adj <- weighted_adj >= thr
  g <- igraph::graph_from_adjacency_matrix(as.matrix(adj),
    mode = "undirected"
  ) %>% simplify()
  gD3 <- igraph_to_networkD3(g, group = ifelse(V(g)$name == orf_name, 1, 10))

  gD3
}



getGseaData <- function(orf_name) {
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
  coexp_data <- data.frame(
    "coexpression" = rho_percentile[, which(colnames(rho_percentile) == orf_name)],
    "transcript" = rownames(rho_percentile)
  )

  coexp_data <- coexp_data %>%
    left_join(orf_info, by = "transcript") %>%
    mutate(coexpression_percentile = round(coexpression, 3)) %>%
    arrange(desc(coexpression_percentile))
  coexp_data_display <- coexp_data[, c("transcript", "gene", "is_canonical", "coexpression_percentile")]
  colnames(coexp_data_display) <- c("transcript", "gene", "is_canonical", "coexpression percentile")


  coexp_data_display
}


convertOrfName <- function(orf_name) {
  if ((str_detect(orf_name, "Y") | str_detect(orf_name, "y")) & toupper(orf_name) %in% orf_info$gene) {
    orf_name <- toupper(orf_name)
    transcript <- orf_info %>%
      filter(gene == orf_name) %>%
      select(transcript) %>%
      pull()
  } else if ((str_detect(orf_name, "orf") | str_detect(orf_name, "ORF") )& tolower(orf_name) %in% orf_info$orf_id) {
    orf_name <- tolower(orf_name)
    transcript <- orf_info %>%
      filter(orf_id == orf_name) %>%
      select(transcript) %>%
      pull()
  } else if (tolower(orf_name) %in% orf_info$transcript) {
    orf_name <- tolower(orf_name)
    transcript <- orf_name
  }else{
    transcript <- NULL
  }
  transcript
}

constructGenomeBrowserLink <- function(orf_coord_info){
  chr<- orf_coord_info$chromosome
  coor1 <- orf_coord_info$coor1-100
  coor2 <- orf_coord_info$coor2+100
  
  
   link <-glue::glue("https://jbrowse-new.dev.yeastgenome.org/?loc={chr}%3A{coor1}..{coor2}&tracks=DNA%2CAll%20Annotated%20Sequence%20Features%2CWacholder_2023_translated_orfs%2CWacholder_2023_riboseq_plus%2CWacholder_2023_riboseq_minus&highlight=")
   
   link
}

getSeqData <- function(orf_name) {
  seq_info <- sequence_info %>% dplyr::filter(transcript == orf_name)

  seq_info
}

getNetwork <- function(orf_name, coexpression_df, thr) {
  orfs_in_subnet <- coexpression_df$transcript[coexpression_df$`coexpression percentile` >= thr]

  #check if the network is empty, there will always be the selected ORF in the network so it is == 1.
  if(length(orfs_in_subnet)==1){
    # create an empty graph and just add the selected ORF as the node
    g<-igraph::make_empty_graph()
    g <- g + vertex(orf_ids[[orf_name]]) + path(orf_ids[[orf_name]],orf_ids[[orf_name]])
    
  }else{
    weighted_adj <- rho_percentile[orfs_in_subnet, orfs_in_subnet]
    
    adj <- round(weighted_adj, 3) >= thr
    rownames(adj) <- map_chr(rownames(adj), function(x) orf_ids[[x]])
    colnames(adj) <- rownames(adj)
    diag(adj) <- FALSE
    g <- igraph::graph_from_adjacency_matrix(as.matrix(adj),
                                             mode = "undirected"
    )
    
  }
  
  # groups are assigned here but colors are defined in server.R at the related rendering function
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

getOrfCoordInfo <- function(orf_name){
  orf_info%>%filter(transcript==orf_name)%>%
    dplyr::select(chromosome, coor1, coor2, strand)
  
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

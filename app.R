library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyjs)

source("ui.R")
orf_info<- data.frame('transcript'=c("chr1_43730", "chr1_200860",'chr1_180543','chr1_200946','chr1_67378'),
                      classification = c("canonical", "noncanonical",'noncanonical','canonical','canonical'))

rho_percentile <- readRDS('/home/aar75/coexpression/20221110_rho/spqn_raw5_sample400.RDS')[1:5,1:5]
rho_percentile<-as.data.frame(rho_percentile)

gsea_df <- data.frame(transcript = c("chr1_67378", "chr1_67378",'chr1_180543'),
                      gene = c("TFC3", "TFC3",NA),
                      pathway_id = c("GO:0006355", "GO:0006996","GO:0006996"),
                      pathway_name = c("Regulation of transcription, DNA-templated", "Organelle organization","Organelle organization"),
                      padj = c(0.038, 0.017,0.0001),
                      NES = c(1.23, 2.56,4.5),
                      size = c(115, 240, 240))
sequence_info_df <- data.frame(transcript = c("chr1_43730 ",'chr1_67378'),
                               gene = c(NA,'gene1'),
                               cds_seq = c("ATG...", "ATG..."),
                               aa_seq = c("M...", "M..."))




shinyApp(ui, server)
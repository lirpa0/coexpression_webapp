library(magrittr)
library(stringr)
library(dplyr)
library(networkD3)
library(igraph)
library(purrr)

server <- function(input, output, session) {

  observeEvent(input$submit_orf, {
    
    orf_name <- eventReactive(input$submit_orf,
                              {
                              convertOrfName(input$orf_name)
                              })
    if (is.null(orf_name())) {
      showModal(modalDialog(
        title = "Error",
        "ORF name not found. Please check your input and try again.",
        easyClose = TRUE
      ))
    } else {
     
      output$orf_name_seq <- renderText(orf_ids[[orf_name()]])
      output$orf_name_coexp<-renderText(orf_ids[[orf_name()]])
      seq_info = reactive(getSeqData(orf_name()))()
      
      output$nt_seq <- renderText(seq_info$nt_seq)
      output$aa_sequence <- renderText(seq_info$aa_seq)
    
  
    coexp_data<- reactive(getCoexpData(orf_name()))()
    gsea_data <- reactive(getGseaData(orf_name()))()
    
    
    orf_class_filter <- reactive({
      case_when(
        input$orf_class_filter == "all" ~ 'all',
        input$orf_class_filter == "canonical" ~ 'canonical',
        input$orf_class_filter == "noncanonical" ~ 'noncanonical'
      )
    })
    
    output$coexp_table <- DT::renderDataTable({
      if (orf_class_filter() == 'all') {
        coexp_data_filtered <- coexp_data
      } else {
        coexp_data_filtered <- coexp_data %>%
          filter(is_canonical == orf_class_filter())
      }
      
      
      
      DT::datatable(
        coexp_data_filtered%>%
          rename("ORF ID" = "orf_id",
                 "Systematic name"="gene",
                 "ORF classification" = "is_canonical")%>%
          select(-transcript),
        rownames = FALSE,
        filter = "none",
        options = list(pageLength = 10, autoWidth = TRUE, dom= "lfrtip"))
      
    })
    output$gsea_table <- DT::renderDataTable({
      gsea_data_filtered <- gsea_data #%>%
      DT::datatable(
        gsea_data_filtered[, c("pathway","TERM","padj", "NES")],
        rownames = FALSE,
        filter = "none",
        options = list(pageLength = 10, autoWidth = TRUE,dom= "lfrtip")
      )
    })
    d3_compatible_network <- reactive({
      
      d3_compatible_network<-getNetwork(orf_name(), coexp_data, input$thr)
      
      })
    
    # d3_compatible_network<-getNetwork('chr1_43730',coexp_data_display, .9)
    output$force <- renderForceNetwork({
      forceNetwork(Links = d3_compatible_network()$links, Nodes = d3_compatible_network()$nodes, Source = "source",
                   Target = "target", NodeID = "name",
                   Group = "group",zoom=TRUE, opacity=1,fontSize=15,
                   colourScale = JS('d3.scaleOrdinal(["#fdc086", "#7570b3", "#1b9e77"])'), legend=TRUE)
    })
    }
    })

}
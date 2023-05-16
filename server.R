library(magrittr)
library(dplyr)

server <- function(input, output, session) {

  observeEvent(input$submit_orf, {
    output$orf_name_seq <- renderText(input$orf_name)
    
    seq_info = reactive(getSeqData(input$orf_name))()
    if (nrow(seq_info) == 0) {
      showModal(modalDialog(
        title = "Error",
        "ORF name not found. Please check your input and try again.",
        easyClose = TRUE
      ))
    } else {
      
      output$cds_sequence <- renderText(seq_info$cds_seq)
      output$aa_sequence <- renderText(seq_info$aa_seq)
    }
  
    coexp_data<- reactive(getCoexpData(input$orf_name))()
    gsea_data <- reactive(getGseaData(input$orf_name))()
    
    
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
        coexp_data_filtered,
        rownames = FALSE,
        filter = "none",
        options = list(pageLength = 25, autoWidth = TRUE, dom= "lfrtip"))
      
    })
    output$gsea_table <- DT::renderDataTable({
      gsea_data_filtered <- gsea_data #%>%
      DT::datatable(
        gsea_data_filtered[, c("pathway","padj", "NES", "size")],
        rownames = FALSE,
        filter = "none",
        options = list(pageLength = 25, autoWidth = TRUE,dom= "lfrtip")
      )
    })
    })

}
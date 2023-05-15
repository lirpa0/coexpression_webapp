
getSeqData<-function(orf_name){
  sequence_info_df <- data.frame(transcript = c("chr1_43730",'chr1_67378'),
                                 gene = c(NA,'gene1'),
                                 cds_seq = c("ATG...", "ATGagsgas..."),
                                 aa_seq = c("M...", "M..."))
  seq_info <- sequence_info_df%>%dplyr::filter(transcript == orf_name | gene ==orf_name)
  
  seq_info
}

server <- function(input, output, session) {

  observeEvent(input$submit_orf, {
    #updateTabsetPanel(inputId = "params", selected = input$dist)
   #orf_name <- input$orf_name
    output$orf_name_seq <- renderText(input$orf_name)
    
    seq_info = reactive(getSeqData(input$orf_name))()
    print(seq_info)
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
  })
  #seq_data <- observe({getSeqData(orf_name,output)})
#  coexp_data <- observe({getCoexpData(orf_name,output)})
  
  #output$seq_data <- seq_data
  #output$coexp_data <- coexp_data
  
  
  
  
  # 

}
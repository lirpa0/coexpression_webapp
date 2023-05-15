
server <- function(input, output, session) {
  
  selected_page <- reactiveVal("")
  
  observeEvent(input$seq_info, {
    if (input$seq_info %% 2 == 1) {
      disable("coexp_info")
      addClass("seq_info", "selected-btn")
      selected_page("seq")
    } else {
      enable("coexp_info")
      removeClass("seq_info", "selected-btn")
    }
  })
  
  
  observeEvent(input$coexp_info, {
    if (input$coexp_info %% 2 == 1) {
      disable("seq_info")
      addClass("coexp_info", "selected-btn")
      selected_page("coexp")
    } else {
      enable("seq_info")
      removeClass("coexp_info", "selected-btn")
    }
  })
  
  reset_buttons <- function() {
    enable("seq_info")
    enable("coexp_info")
    removeClass("seq_info", "selected-btn")
    removeClass("coexp_info", "selected-btn")
    runjs("$('#seq_info').attr('value', 0);")
    runjs("$('#coexp_info').attr('value', 0);")
  }
  
  
  observeEvent(input$submit_orf, {
    if (selected_page() == "seq") {
      updateRadioButtons(session, "view", selected = "seq_info_page")
      output$orf_name_seq <- renderText(input$orf_name)
      
      seq_info <- sequence_info_df %>%
        filter(transcript == input$orf_name | gene == input$orf_name)
      
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
      
    }  else if (selected_page() == "coexp") {
      updateRadioButtons(session, "view", selected = "coexp_info_page")
      output$orf_name_coexp <- renderText(input$orf_name)
      orf_class_filter <- reactive({
        case_when(
          input$orf_class_filter == "all" ~ 'all',
          input$orf_class_filter == "canonical" ~ 'canonical',
          input$orf_class_filter == "noncanonical" ~ 'noncanonical'
        )
      })
      
      coexp_data<-data.frame('coexpression'=rho_percentile[,which(colnames(rho_percentile)==input$orf_name)], 'transcript'=rownames(rho_percentile))
      coexp_data <- coexp_data %>%
        # mutate(ORF_id = rownames(rho_percentile)) %>%
        # select(ORF_id, input$orf_name) %>%
        # filter(!is.na(input$orf_name)) %>%
        # rename(coexpression_percentile = input$orf_name) %>%
        left_join(orf_info, by = "transcript") %>%
        mutate(coexpression_percentile = round(coexpression, 3)) %>%
        arrange(desc(coexpression_percentile))
      
      output$coexp_table <- DT::renderDataTable({
        if (orf_class_filter() == 'all') {
          coexp_data_filtered <- coexp_data
        } else {
          coexp_data_filtered <- coexp_data %>%
            filter(classification == orf_class_filter())
        }
        
        coexp_data_display <- coexp_data_filtered[,c("transcript", "classification", "coexpression_percentile")]
        colnames(coexp_data_display)<-c('transcript','classification','coexpression percentile')
        
        DT::datatable(
          coexp_data_display,
          rownames = FALSE,
          filter = "none",
          options = list(pageLength = 25, autoWidth = TRUE, dom= "lfrtip"))
        
      })
      
      gsea_data <- gsea_df %>%
        filter(transcript == input$orf_name | gene == input$orf_name) %>%
        filter(padj < 0.05) %>%
        mutate(
          padj = formatC(padj, format = "e", digits = 1),
          NES = round(NES, 2)
        )
      
      output$gsea_table <- DT::renderDataTable({
        gsea_data_filtered <- gsea_data #%>%
        # filter(grepl(input$search_pathway, pathway_id, ignore.case = TRUE))
        
        DT::datatable(
          gsea_data_filtered[, c("pathway_id", "pathway_name","padj", "NES", "size")],
          rownames = FALSE,
          filter = "none",
          options = list(pageLength = 25, autoWidth = TRUE,dom= "lfrtip")
        )
      })
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please select either Sequence Information or Coexpression Information.",
        easyClose = TRUE
      ))
    }
  })
  
  
  
  observeEvent(input$return_main, {
    updateRadioButtons(session, "view", selected = "main")
    reset_buttons()
  })
  
  observeEvent(input$return_main2, {
    updateRadioButtons(session, "view", selected = "main")
    reset_buttons()
  })
  
}
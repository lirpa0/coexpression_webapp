library(magrittr)
library(stringr)
library(dplyr)
library(networkD3)
library(igraph)
library(purrr)
library(rclipboard)


server <- function(input, output, session) {
  
  observeEvent(input$submit_orf, {
    updateSliderInput(session, "thr", value = 1)
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
      
      orf_coord_info <- reactive(getOrfCoordInfo(orf_name()))
      
      output$orf_info <- DT::renderDataTable({
        
        DT::datatable(
          orf_coord_info(),
          rownames = FALSE,
          filter = "none",
          options = list(scrollX = TRUE, paging =FALSE, searching=FALSE , 
                         autoWidth = FALSE,dom = 't')
        )
      })
      browser_link <- reactive(a("Genome Browser", href=constructGenomeBrowserLink(orf_coord_info()),
                                 target="_blank"))
      
      output$browser <- renderUI({
        tagList("", browser_link())
      })
      coexp_data<- reactive(getCoexpData(orf_name()))
      if(is.null(coexp_data())){
        output$coexp_table <- NULL
      }else{
        gsea_data <- reactive(getGseaData(orf_name()))
        
        orf_class_filter <- reactive({
          case_when(
            input$orf_class_filter == "all" ~ 'all',
            input$orf_class_filter == "canonical" ~ 'canonical',
            input$orf_class_filter == "noncanonical" ~ 'noncanonical'
          )
        })
        
        output$coexp_table <- DT::renderDataTable({
          if (orf_class_filter() == 'all') {
            coexp_data_filtered <- coexp_data()
          } else {
            coexp_data_filtered <- coexp_data() %>%
              filter(is_canonical == orf_class_filter())
          }
          
          coexp_data_show<-coexp_data_filtered%>%
            rename("ORF ID" = "orf_id",
                   "Systematic name"="gene",
                   "Gene name" = "GENENAME",
                   "ORF classification" = "is_canonical",
                   "Coexpression (rho)" = "coexpression_percentile")%>%
            filter(transcript!=orf_name())%>%
            select(-transcript)
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste0(input$orf_name,"_coexpression.csv")
            },
            content = function(con) {
              write.csv(coexp_data_show, con)
            }
          )
          
          DT::datatable(
            coexp_data_show,
            rownames = FALSE,
            filter = "none",
            options = list(
              scrollX = TRUE, pageLength = 10, autoWidth = F, dom= "lfrtip"))
          
        })
        output$gsea_table <- DT::renderDataTable(server=T,{
          DT::datatable(
            gsea_data()[, c("pathway","TERM","padj", "NES")]%>%rename("GO ID" = "pathway"),
            rownames = FALSE,
            filter = "none",
            extensions = c('Buttons'),
            options = list(
              buttons=list(
                list(extend = "csv", text = "Download GSEA Results", filename = paste0(input$orf_name,"_gsea"),
                     exportOptions = list(
                       modifier = list(page = "all")
                     )
                )
              ),
              scrollX = TRUE, pageLength = 10, 
              autoWidth = F,dom= "Blfrtip"),
            class='display'
          )
        })
        thr = quantile(coexp_data()$coexpression_percentile,.999)
        updateSliderInput(session, "thr", value = as.numeric(thr))
        
        observeEvent(input$thr, {
          d3_compatible_network <- reactive({
            
            d3_compatible_network<-getNetwork(orf_name(), coexp_data(), input$thr)
            
          })
          
          myColors <- paste0('d3.scaleOrdinal()
                  .domain(["' ,  d3_compatible_network()$nodes[1,2], '", "Canonical", "Noncanonical"])
                  .range(["#fdc086",  "#7570b3", "#1b9e77"])')
          
          output$force <- renderForceNetwork({
            forceNetwork(Links = d3_compatible_network()$links, Nodes = d3_compatible_network()$nodes, Source = "source",
                         Target = "target", NodeID = "name",
                         Group = "group",zoom=TRUE, opacity=1,fontSize=15,fontFamily = "Source Sans Pro",
                         colourScale = JS(myColors), legend=TRUE)
          })
          
        })
        
        
      }
    }
    
    output$clip_nt <- renderUI({
      output$clip_nt <- renderUI({
        rclipButton(
          inputId = "clipbtn",
          label = "Copy Nucleotide sequence",
          clipText = seq_info$nt_seq , 
          icon = icon("clipboard")
        )
      })
    })
    output$clip_aa <- renderUI({
      output$clip_aa <- renderUI({
        rclipButton(
          inputId = "clipbtn",
          label = "Copy AA sequence",
          clipText = seq_info$aa_seq , 
          icon = icon("clipboard")
        )
      })
    })
  })
  
}
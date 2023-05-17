
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = 'stylesheet', type="text/css", href = "styles.css")
  ),
  titlePanel("ORF Information App"),
  sidebarLayout(
    sidebarPanel(
    textInput("orf_name", "Enter your ORF name here", width = "50%"),
    selectInput("view", "Result Type",
                c(Sequence = "seq_info_page", Coexpression = "coexp_info_page")
    ),
    actionButton("submit_orf", "Submit")),
  mainPanel(
    conditionalPanel(condition = 'input.submit_orf',
    conditionalPanel(
      condition = "input.view == 'seq_info_page'",
      h2("Sequence Information for ORF ", tags$span(textOutput("orf_name_seq"), style = "font-weight: bold;")),
      h3("CDS Sequence"),
      verbatimTextOutput("nt_seq"),
      h3("Amino Acid Sequence"),
      verbatimTextOutput("aa_sequence")#,
    ),
    conditionalPanel(
      condition = "input.view == 'coexp_info_page'",
      h2("Coexpression Info for ORF ", tags$span(textOutput("orf_name_coexp"), style = "font-weight: bold;")),
      fluidRow(
        column(6,
               h3("Coexpression"),
               selectInput("orf_class_filter", "Filter by ORF classification:", c("all", "canonical", "noncanonical")),
               DT::dataTableOutput("coexp_table")
        ),
        column(6,
               h3("Gene Set Enrichment Analysis"),
               DT::dataTableOutput("gsea_table")
        )),
      tabsetPanel(
        # tabPanel("Simple Network", simpleNetworkOutput("simple")),
        tabPanel("Force Network", 
                 sliderInput("thr", "Coexpression threshold:",
                             min = 0.8, max = 1, value = .9
                 ),
                 networkD3::forceNetworkOutput("force"))
      ),
  )
)
)))
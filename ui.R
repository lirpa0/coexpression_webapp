ui <- fluidPage(
  
  tags$head(
    tags$link(rel = 'stylesheet', type="text/css", href = "styles.css")
  ),
  titlePanel("ORF Information App"),
  conditionalPanel(
    condition = "input.view == 'main'",
    fluidRow(
      column(6, actionButton("seq_info", "Get Sequence Information", width = "100%", style = "font-size: 24px;")),
      column(6, actionButton("coexp_info", "Get Coexpression Information", width = "100%", style = "font-size: 24px;"))
    ),
    br(),
    textInput("orf_name", "Enter your ORF name here", width = "50%"),
    actionButton("submit_orf", "Submit")
  ),
  conditionalPanel(
    condition = "input.view == 'seq_info_page'",
    h2("Sequence Information for ORF ", tags$span(textOutput("orf_name_seq"), style = "font-weight: bold;")),
    h3("CDS Sequence"),
    verbatimTextOutput("cds_sequence"),
    h3("Amino Acid Sequence"),
    verbatimTextOutput("aa_sequence"),
    actionButton("return_main", "Return to Main Page", style = "position: absolute; top: 0; right: 0;")
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
      )
    ),
    actionButton("return_main2", "Return to Main Page", style = "position: absolute; top: 0; right: 0;")
  ),
  hidden(
    radioButtons("view", "", choices = c("main", "seq_info_page", "coexp_info_page"), selected = "main", inline = TRUE)
  )
)
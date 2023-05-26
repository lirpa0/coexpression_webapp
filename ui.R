
ui <- function(req) {
  fluidPage(
  
  tags$head(
    tags$link(rel = 'stylesheet', type="text/css", href = "styles.css")
  ),
  navbarPage("ORF Information App",
             tabPanel("Search",
 # titlePanel(),
  sidebarLayout(
    sidebarPanel(
    textInput("orf_name", "Enter your ORF name here", width = "100%"),
    selectInput("view", "Result Type",
                c(Sequence = "seq_info_page", Coexpression = "coexp_info_page")
    ),
    actionButton("submit_orf", "Submit"),
    width=3),
  mainPanel(
    conditionalPanel(condition = 'input.submit_orf',
    conditionalPanel(
      rclipboard::rclipboardSetup(),
      
      condition = "input.view == 'seq_info_page'",
      h2("Sequence Information for ORF ", tags$span(textOutput("orf_name_seq"), style = "font-weight: bold;")),
      h3("CDS Sequence"),
      verbatimTextOutput("nt_seq"),
      uiOutput("clip_nt"),
      
      h3("Amino Acid Sequence"),
      verbatimTextOutput("aa_sequence"),
      # UI ouputs for the copy-to-clipboard buttons
      uiOutput("clip_aa"),
      
      h3("Coordinates"),
      DT::dataTableOutput("orf_info"),
      h3(uiOutput("browser")),
      

      

    ),
    conditionalPanel(
      condition = "input.view == 'coexp_info_page'",
      h2("Coexpression Info for ORF ", tags$span(textOutput("orf_name_coexp"), style = "font-weight: bold;")),
      tabsetPanel(
        tabPanel("Coexpression Network",
                 fluidRow(
                   column(6,
                          h3("Coexpression"),
                          selectInput("orf_class_filter", "Filter by ORF classification:", c("all", "canonical", "noncanonical")),
                          DT::dataTableOutput("coexp_table")
                   ),
                   column(6,
                          sliderInput("thr", "Coexpression threshold:",
                                      min = 0.8, max = 1, value = .9
                          ),
                          networkD3::forceNetworkOutput("force"))
                   
                 )),
        # tabPanel("Simple Network", simpleNetworkOutput("simple")),
        tabPanel("Gene set enrichment", 
                 h3("Gene Set Enrichment Analysis"),
                # h3("foo"),
                 
                 DT::dataTableOutput("gsea_table") 
        ),
),
      
  )
)
))),
tabPanel("About",
         
         p("This website aims to accompany //paper. All relevant supplementary data could be found at \n"),
         a("figshare", target="_blank",href="https://figshare.com/articles/dataset/Exploring_the_noncanonical_translatome_using_massively_integrated_coexpression_analysis/22289614")

)

))
}
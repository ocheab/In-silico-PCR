options(shiny.maxRequestSize = 5000 * 1024^2)  # 5 GB upload limit

library(shiny)
library(shinybusy)
library(shinythemes)
library(Biostrings)
library(DT)
library(shinyWidgets)
library(readr)

ui <- navbarPage(
  title = div(
    img(src = "logo.png", height = "40px", style = "margin-right:10px"),
    "Centre for Malaria and Other Tropical Diseases Care, UITH"
  ),
  theme = shinytheme("flatly"),
  
  tabPanel("🏠 Home",
           fluidPage(
             tags$head(
               tags$style(HTML("
        .home-container {
          text-align: center;
          margin-top: 30px;
        }
        .home-header {
          font-size: 28px;
          font-weight: bold;
          color: #2c3e50;
          margin-bottom: 15px;
        }
        .home-subtext {
          font-size: 18px;
          color: #34495e;
          margin-bottom: 25px;
        }
        .home-logo {
          max-height: 250px;
          margin-bottom: 30px;
        }
        .home-highlight {
          font-size: 16px;
          background-color: #e3f2fd;
          padding: 15px;
          border-radius: 10px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          display: inline-block;
        }
      "))
             ),
             
             div(class = "home-container",
                 img(src = "pcr.jpg", class = "home-logo"),
                 div(class = "home-header", "Welcome to the In Silico PCR Tool"),
                 div(class = "home-subtext", "Developed by the Centre for Malaria and Other Tropical Diseases Care, University of Ilorin Teaching Hospital"),
                 div(class = "home-highlight",
                     p("This application helps you simulate PCR amplification in silico using uploaded or custom sequences and primers."),
                     p("🔬 Supports high-throughput analysis of primer binding"),
                     p("📁 Upload multiple sequences in FASTA format"),
                     p("🧬 Analyze primer binding specificity, product sizes, and export results.")
                 )
             )
           )
  ),
  
  tabPanel("🧪 Run Simulation",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 fileInput("fasta_upload", "Upload FASTA File(s)", multiple = TRUE, accept = ".fasta"),
                 fileInput("primer_csv", "Upload Primer CSV File (Optional)", accept = ".csv"),
                 tags$hr(),
                 h5("Or Manually Enter Primer Pairs"),
                 uiOutput("primer_inputs"),
                 actionButton("add_pair", "Add Primer Pair", icon = icon("plus")),
                 tags$hr(),
                 numericInput("mismatch", "Max Mismatches", value = 0, min = 0),
                 numericInput("min_product_size", "Min Product Size (bp)", value = 50, min = 1),
                 numericInput("max_product_size", "Max Product Size (bp)", value = 3000, min = 1),
                 actionButton("run_pcr", "Run In Silico PCR", icon = icon("play-circle")),
                 downloadButton("download_results", "Download CSV", class = "btn btn-success"),
                 downloadButton("download_selected_fasta", "Download Selected FASTA", class = "btn btn-info")
               ),
               mainPanel(
                 h4("Predicted Amplification Results"),
                 DTOutput("pcr_results")
               )
             )
           )
  ),
  
  tabPanel("❓ Help",
           fluidPage(
             h3("How to Use This Tool"),
             tags$ul(
               tags$li("Upload one or more FASTA files containing DNA sequences."),
               tags$li("Optionally, upload a CSV file with 'Forward' and 'Reverse' primer columns."),
               tags$li("Or enter primer pairs manually using the input fields."),
               tags$li("Set mismatch tolerance and desired product size range."),
               tags$li("Click 'Run In Silico PCR' to view predicted amplicons."),
               tags$li("Download results as a CSV or selected sequences as FASTA file."),
               tags$li("Use the table filters to find specific or best primer-product matches.")
             ),
             tags$hr(),
             p("For technical support, contact: cemtrod.ilorin@gmail.com")
           )
  )
)

server <- function(input, output, session) {
  results <- reactiveVal()
  fasta_sequences <- reactiveVal("")
  fasta_storage <- reactiveVal(list())
  primer_counter <- reactiveVal(1)
  
  output$primer_inputs <- renderUI({
    n <- primer_counter()
    lapply(1:n, function(i) {
      fluidRow(
        column(6, textInput(paste0("fwd_", i), label = paste("Forward Primer", i), placeholder = "e.g. ATGCGTAC")),
        column(6, textInput(paste0("rev_", i), label = paste("Reverse Primer", i), placeholder = "e.g. CGTACGTA"))
      )
    })
  })
  
  observeEvent(input$add_pair, {
    primer_counter(primer_counter() + 1)
  })
  
  observeEvent(input$run_pcr, {
    req(input$fasta_upload)
    show_modal_spinner(text = "Running in silico PCR...")
    
    all_results <- list()
    all_fasta <- ""
    fasta_store <- list()
    
    fasta_list <- lapply(input$fasta_upload$datapath, readDNAStringSet)
    names(fasta_list) <- basename(input$fasta_upload$name)
    
    primer_pairs <- list()
    
    if (!is.null(input$primer_csv)) {
      primers_df <- read_csv(input$primer_csv$datapath, show_col_types = FALSE)
      if (all(c("Forward", "Reverse") %in% names(primers_df))) {
        for (i in seq_len(nrow(primers_df))) {
          primer_pairs[[length(primer_pairs) + 1]] <- c(primers_df$Forward[i], primers_df$Reverse[i])
        }
      }
    } else {
      n <- primer_counter()
      for (i in 1:n) {
        fwd <- input[[paste0("fwd_", i)]]
        rev <- input[[paste0("rev_", i)]]
        if (!is.null(fwd) && !is.null(rev) && fwd != "" && rev != "") {
          primer_pairs[[length(primer_pairs) + 1]] <- c(fwd, rev)
        }
      }
    }
    
    for (file_name in names(fasta_list)) {
      genome <- fasta_list[[file_name]]
      
      for (i in seq_along(genome)) {
        seq_name <- names(genome)[i]
        seq <- genome[[i]]
        
        for (pair in primer_pairs) {
          fwd_primer <- pair[1]
          rev_primer <- pair[2]
          
          fwd_match <- matchPattern(fwd_primer, seq, max.mismatch = input$mismatch)
          rev_match <- matchPattern(reverseComplement(DNAString(rev_primer)), seq, max.mismatch = input$mismatch)
          
          valid_matches <- list()
          
          if (length(fwd_match) > 0 && length(rev_match) > 0) {
            for (fwd in start(fwd_match)) {
              for (rev in end(rev_match)) {
                if (rev > fwd) {
                  product_size <- rev - fwd + 1
                  if (product_size >= input$min_product_size && product_size <= input$max_product_size) {
                    valid_matches[[length(valid_matches) + 1]] <- list(fwd = fwd, rev = rev)
                  }
                }
              }
            }
          }
          
          if (length(valid_matches) == 1) {
            fwd <- valid_matches[[1]]$fwd
            rev <- valid_matches[[1]]$rev
            seq_id <- paste0(file_name, ":", seq_name, "|Fwd:", fwd, "|Rev:", rev, "|Size:", rev - fwd + 1)
            amp_seq <- as.character(subseq(seq, fwd, rev))
            fasta_store[[seq_id]] <- amp_seq
            
            all_results[[length(all_results) + 1]] <- data.frame(
              File = file_name,
              Sequence = seq_name,
              Forward_Primer = fwd_primer,
              Reverse_Primer = rev_primer,
              Forward_Pos = fwd,
              Reverse_Pos = rev,
              Product_Size = rev - fwd + 1,
              Specific = "Yes",
              FASTA_ID = seq_id
            )
          } else if (length(valid_matches) > 1) {
            for (match in valid_matches) {
              seq_id <- paste0(file_name, ":", seq_name, "|Fwd:", match$fwd, "|Rev:", match$rev, "|Size:", match$rev - match$fwd + 1)
              amp_seq <- as.character(subseq(seq, match$fwd, match$rev))
              fasta_store[[seq_id]] <- amp_seq
              
              all_results[[length(all_results) + 1]] <- data.frame(
                File = file_name,
                Sequence = seq_name,
                Forward_Primer = fwd_primer,
                Reverse_Primer = rev_primer,
                Forward_Pos = match$fwd,
                Reverse_Pos = match$rev,
                Product_Size = match$rev - match$fwd + 1,
                Specific = "No",
                FASTA_ID = seq_id
              )
            }
          }
        }
      }
    }
    
    final_result <- do.call(rbind, all_results)
    fasta_storage(fasta_store)
    
    if (is.null(final_result) || nrow(final_result) == 0) {
      output$pcr_results <- renderDT({
        datatable(data.frame(Message = "No PCR products found."))
      })
    } else {
      results(final_result)
      
      output$pcr_results <- renderDT({
        datatable(final_result, options = list(pageLength = 10),
                  filter = "top", selection = "multiple") %>%
          formatStyle("Specific",
                      target = "row",
                      backgroundColor = styleEqual("Yes", "#d4edda"))
      })
    }
    
    remove_modal_spinner()
  })
  
  observeEvent(input$generate_fasta, {
    output$fasta_output <- renderText({
      paste(unlist(lapply(names(fasta_storage()), function(name) {
        paste0(">", name, "\n", fasta_storage()[[name]])
      })), collapse = "\n")
    })
  })
  
  output$download_selected_fasta <- downloadHandler(
    filename = function() {
      paste0("selected_pcr_products_", Sys.Date(), ".fasta")
    },
    content = function(file) {
      selected_rows <- input$pcr_results_rows_selected
      all_data <- results()
      selected_ids <- all_data[selected_rows, "FASTA_ID"]
      lines <- unlist(lapply(selected_ids, function(id) {
        paste0(">", id, "\n", fasta_storage()[[id]])
      }))
      writeLines(lines, file)
    }
  )
  
  output$download_fasta <- downloadHandler(
    filename = function() {
      paste0("pcr_products_", Sys.Date(), ".fasta")
    },
    content = function(file) {
      lines <- unlist(lapply(names(fasta_storage()), function(id) {
        paste0(">", id, "\n", fasta_storage()[[id]])
      }))
      writeLines(lines, file)
    }
  )
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("pcr_filtered_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      all_data <- results()
      visible_rows <- input$pcr_results_rows_all
      if (!is.null(visible_rows)) {
        filtered_data <- all_data[visible_rows, ]
      } else {
        filtered_data <- all_data
      }
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
options(shiny.maxRequestSize = 5000 * 1024^2)  # 5 GB upload limit

library(shiny)
library(shinybusy)
library(shinythemes)
library(Biostrings)
library(DT)
library(shinyWidgets)
library(readr)
library(XML)

ui <- navbarPage(
  title = div(
    img(src = "www/logo.png", height = "40px", style = "margin-right:10px"),
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
      ")),
        tags$script(HTML("
  Shiny.addCustomMessageHandler('bindBlastButtons', function(message) {
    setTimeout(function() {
      $('.blast-btn').off('click').on('click', function() {
        Shiny.setInputValue('blast_row', this.id, {priority: 'event'});
      });
    }, 500);
  });
"))
             ),
             
             div(class = "home-container",
                 img(src = "www/pcr.png", class = "home-logo"),
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
             p("For technical support, contact: cemtrod.ilorin@gmail.com"),
             tags$style(HTML("
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    background: #f0f0f0;
    border: 1px solid #ccc;
    margin: 2px;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
    background: #007bff;
    color: white !important;
  }
"))
             
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
        df <- results()
        df$BLAST <- paste0(
          "<button class='btn btn-sm btn-primary blast-btn' id='blast_",
          1:nrow(df), "'>BLAST</button>"
        )
        datatable(df, escape = FALSE, selection = 'multiple', options = list(pageLength = 10)) %>%
          formatStyle("Specific", backgroundColor = styleEqual("Yes", "#d4edda"))
      })
      
      # ⬇️ Add this right after rendering
      observe({
        req(results())
        session$sendCustomMessage("bindBlastButtons", list())
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
  observeEvent(input$blast_selected, {
    selected_rows <- input$pcr_results_rows_selected
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      output$blast_status <- renderText("No rows selected.")
      return(NULL)
    }
    
    all_data <- results()
    fasta_store <- fasta_storage()
    selected_ids <- all_data[selected_rows, "FASTA_ID"]
    first_seq <- fasta_store[[selected_ids[1]]]
    fasta_seq <- paste0(">Query\n", first_seq)
    
    output$blast_status <- renderText("Submitting to NCBI BLAST...")
    
    submit <- httr::POST(
      url = "https://blast.ncbi.nlm.nih.gov/Blast.cgi",
      body = list(
        CMD = "Put",
        PROGRAM = "blastn",
        DATABASE = "nt",
        QUERY = fasta_seq
      ),
      encode = "form"
    )
    
    submit_text <- content(submit, "text")
    if (!grepl("RID =", submit_text)) {
      output$blast_status <- renderText("BLAST submission failed. Please try again.")
      return(NULL)
    }
    
    rid <- sub(".*RID = (\\w+).*", "\\1", submit_text)
    output$blast_status <- renderText(paste("RID:", rid, "Polling NCBI for results..."))
    
    Sys.sleep(15)
    
    results_response <- httr::GET(
      url = "https://blast.ncbi.nlm.nih.gov/Blast.cgi",
      query = list(
        CMD = "Get",
        FORMAT_TYPE = "XML",
        RID = rid
      )
    )
    
    raw_text <- content(results_response, as = "text", encoding = "UTF-8")
    
    # Check if content looks like XML
    if (!grepl("^<\\?xml", raw_text)) {
      output$blast_status <- renderText("NCBI returned non-XML output. BLAST failed.")
      return(NULL)
    }
    
    parsed <- tryCatch({
      XML::xmlParse(raw_text)
    }, error = function(e) {
      output$blast_status <- renderText("BLAST XML parsing failed.")
      return(NULL)
    })
    
    if (is.null(parsed)) return(NULL)
    
    hits <- xpathSApply(parsed_xml, "//Hit", function(hit) {
      get_safe <- function(node, tag) {
        if (!is.null(node[[tag]])) xmlValue(node[[tag]]) else NA
      }
      
      hsp <- hit[["Hit_hsps"]][["Hsp"]]
      
      data.frame(
        Name = get_safe(hit, "Hit_def"),
        Accession = get_safe(hit, "Hit_accession"),
        Length = as.numeric(get_safe(hit, "Hit_len")),
        Identity = as.numeric(get_safe(hsp, "Hsp_identity")),
        AlignLen = as.numeric(get_safe(hsp, "Hsp_align-len")),
        EValue = as.numeric(get_safe(hsp, "Hsp_evalue")),
        stringsAsFactors = FALSE
      )
    })
    if (length(hits) == 0 || all(sapply(hits, function(x) all(is.na(x))))) {
      showModal(modalDialog("BLAST completed, but no significant hits were found.", easyClose = TRUE))
      return()
    }
    
    df_hits <- tryCatch({
      df <- do.call(rbind, hits)
      df$`% Identity` <- round(df$Identity / df$AlignLen * 100, 2)
      df$Score <- sapply(df$`% Identity`, function(score) {
        col <- if (score >= 95) '#28a745' else if (score >= 85) '#ffc107' else '#dc3545'
        sprintf("<div style='background-color:%s;width:%s%%;padding:2px;color:white;border-radius:4px'>%s%%</div>", col, score, score)
      })
      df
    }, error = function(e) {
      cat("⚠️ Could not build hits data frame:", e$message, "\n")
      NULL
    })
    
    if (is.null(df_hits) || nrow(df_hits) == 0) {
      showModal(modalDialog("BLAST returned no usable hits.", easyClose = TRUE))
      return()
    }
    
    
    if (length(hits) == 0) {
      output$blast_status <- renderText("No hits found in BLAST results.")
      return(NULL)
    }
    
    df <- do.call(rbind, lapply(hits, as.data.frame))
    output$blast_results <- renderDT(df)
    output$blast_status <- renderText("BLAST results retrieved successfully.")
  })
  
  observeEvent(input$blast_row, {
    show_modal_spinner(text = "Running BLAST...")
    
    row_num <- as.numeric(sub("blast_", "", input$blast_row))
    df <- results()
    fasta_id <- df[row_num, "FASTA_ID"]
    selected_seq <- fasta_storage()[[fasta_id]]
    fasta_seq <- paste0(">Query\n", selected_seq)
    
    cat("🔍 Selected row:", row_num, "\n")
    cat("🧬 FASTA ID:", fasta_id, "\n")
    cat("🧬 Seq Preview:", substr(selected_seq, 1, 60), "\n")
    
    rid <- tryCatch({
      resp <- httr::POST(
        "https://blast.ncbi.nlm.nih.gov/Blast.cgi",
        body = list(CMD = "Put", PROGRAM = "blastn", DATABASE = "nt", QUERY = fasta_seq),
        encode = "form"
      )
      resp_txt <- httr::content(resp, "text", encoding = "UTF-8")
      cat("📨 BLAST SUBMIT RESPONSE:\n", substr(resp_txt, 1, 300), "\n")
      
      if (grepl("RID =", resp_txt)) {
        rid <- sub(".*RID = (\\w+).*", "\\1", resp_txt)
        cat("✅ RID found:", rid, "\n")
        rid
      } else {
        cat("❌ RID not found.\n")
        NULL
      }
    }, error = function(e) {
      cat("❌ ERROR submitting BLAST:", e$message, "\n")
      NULL
    })
    
    if (is.null(rid)) {
      remove_modal_spinner()
      showModal(modalDialog("BLAST submission failed. NCBI may be temporarily unavailable or rejecting the request.", easyClose = TRUE))
      return()
    }
    
    # ⏳ Wait for BLAST to process
    # 🔁 Poll for BLAST XML result instead of static wait
    blast_xml <- NULL
    for (attempt in 1:12) {
      cat(sprintf("⏳ Polling attempt %d for RID: %s...\n", attempt, rid))
      Sys.sleep(5)  # Wait 5 seconds between attempts
      
      blast_xml <- tryCatch({
        res <- httr::GET("https://blast.ncbi.nlm.nih.gov/Blast.cgi", query = list(
          CMD = "Get", RID = rid, FORMAT_TYPE = "XML"
        ))
        raw <- httr::content(res, "text", encoding = "UTF-8")
        cat("📥 BLAST XML response preview:\n", substr(raw, 1, 300), "\n")
        raw
      }, error = function(e) {
        cat("❌ ERROR retrieving BLAST results:", e$message, "\n")
        ""
      })
      
      if (grepl("^<\\?xml", blast_xml)) {
        cat("✅ XML successfully retrieved on attempt", attempt, "\n")
        break
      } else {
        cat("⏳ Still waiting for BLAST result to be ready...\n")
      }
    }
    
    # 🚨 If no valid XML after polling, abort
    if (!grepl("^<\\?xml", blast_xml)) {
      remove_modal_spinner()
      showModal(modalDialog("❌ BLAST job timed out or returned malformed data after multiple attempts.", easyClose = TRUE))
      return()
    }
    
    if (!grepl("^<\\?xml", blast_xml)) {
      remove_modal_spinner()
      showModal(modalDialog("BLAST returned non-XML data. Likely an error page due to overload or malformed request.", easyClose = TRUE))
      return()
    }
    
    parsed_xml <- tryCatch(XML::xmlParse(blast_xml), error = function(e) {
      cat("❌ XML parse error:", e$message, "\n")
      NULL
    })
    
    if (is.null(parsed_xml)) {
      remove_modal_spinner()
      showModal(modalDialog("❌ Could not parse BLAST XML output.", easyClose = TRUE))
      return()
    }
    
    # Safely parse <Hit> nodes
    hits <- xpathApply(parsed_xml, "//Hit", function(hit) {
      safe_xml_value <- function(node, tag) {
        val <- tryCatch(xmlValue(node[[tag]]), error = function(e) NA)
        if (is.null(val) || val == "") NA else val
      }
      
      hsp <- hit[["Hit_hsps"]][["Hsp"]]
      if (is.null(hsp)) return(NULL)  # Skip hits with no HSP block
      
      data.frame(
        Name      = safe_xml_value(hit, "Hit_def"),
        Accession = safe_xml_value(hit, "Hit_accession"),
        Length    = as.numeric(safe_xml_value(hit, "Hit_len")),
        Identity  = as.numeric(safe_xml_value(hsp, "Hsp_identity")),
        AlignLen  = as.numeric(safe_xml_value(hsp, "Hsp_align-len")),
        EValue    = as.numeric(safe_xml_value(hsp, "Hsp_evalue")),
        stringsAsFactors = FALSE
      )
    })
    
    # Remove NULLs and bind
    hits <- hits[!sapply(hits, is.null)]
    
    if (length(hits) == 0) {
      showModal(modalDialog("BLAST completed, but no usable hits were returned.", easyClose = TRUE))
    } else {
      df <- do.call(rbind, hits)
      df$`% Identity` <- round(df$Identity / df$AlignLen * 100, 2)
      
      # Add colored score bars
      df$Score <- sapply(df$`% Identity`, function(score) {
        col <- if (score >= 95) '#28a745' else if (score >= 85) '#ffc107' else '#dc3545'
        sprintf("<div style='background-color:%s;width:%s%%;padding:2px;color:white;border-radius:4px'>%s%%</div>", col, score, score)
      })
      
      # Display modal
      # Limit to top 5 hits by lowest EValue
      top_hits <- df[order(df$EValue), ][1:min(5, nrow(df)), ]
      
      # Render modal with top hits
      showModal(modalDialog(
        title = "Top 5 BLAST Hits",
        DT::renderDataTable({
          datatable(top_hits[, c("Name", "Accession", "Length", "EValue", "% Identity", "Score")],
                    escape = FALSE, rownames = FALSE, options = list(pageLength = 5))
        }),
        size = "l", easyClose = TRUE
      ))
    }
    
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

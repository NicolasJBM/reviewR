#' Gadget for the documentation of various sources of inforamtion.
#' @return A RData file.
#' @import miniUI
#' @import shiny
#' @import shinythemes
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr rename
#' @importFrom dplyr top_n
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stats na.omit
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @importFrom DT JS
#' @importFrom glue glue
#' @importFrom utils head
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_context_menu
#' @importFrom pdftools pdf_text
#' @importFrom wordcloud wordcloud
#' @importFrom viridis inferno
#' @export


document_source <- function() {
  
  options(shiny.maxRequestSize=500*1024^2)
  
  ui <- miniPage(
    theme = shinytheme("spacelab"),
    
    gadgetTitleBar("Document Sources"),
    miniTabstripPanel(
      
      # Panel where the author selects references in the filtered list
      miniTabPanel(
        "Link",
        icon = icon("search"),
        miniContentPanel(
          fluidRow(
            rhandsontable::rHandsontableOutput('edit_paths'),
            actionButton("update_links","Update links"),
            tags$hr(),
            DT::dataTableOutput('references')
          )
        )
      ),
      
      # Panel where the author checks references in the filtered list
      miniTabPanel(
        "Document",
        icon = icon("list"),
        miniContentPanel(
          sidebarLayout(
            sidebarPanel(
                uiOutput("slctkey"), 
                uiOutput("pdfviewer"),
              width = 4
            ),
            mainPanel(
              fillRow(
                flex = c(5,1),
                fillCol(
                  uiOutput("edit")
                ),
                column(
                  checkboxGroupInput(
                    "displedit",
                    "Select what to display or edit",
                    choices = c("Wordcloud","Source",
                                "Concepts","Relations","Moderations",
                                "Definitions","Explanations (relations)","Explanations (moderations)",
                                "Operationalizations","Observations (relations)","Observations (moderations)",
                                "Notes"),
                    selected = "Wordcloud"
                  ),
                  width = 4
                )
              )
            )
          )
        )
      )
    )
  )
  
  
  
  server <- function(input, output, session) {
    
    # Bind variables
    author <- NULL
    jnl <- NULL
    key <- NULL
    path <- NULL
    title <- NULL
    term <- NULL
    count <- NULL
    
    # Load data
    load(paste0(find.package("bibliogR"),"/references.RData"))
    references <- references %>%
      dplyr::select(key, title, author, journal = jnl)
    files <- list.files(path = "Sources", recursive = TRUE, include.dirs = TRUE)
    files <- files[stringr::str_detect(files, ".pdf")]
    load("Review/srcrev.RData")
    
    if (file.exists("Review/lexana.RData")){
      load("Review/lexana.RData")
    } else {
      lexana = list(
        semrel = list(),
        bow = list()
      )
    }
    
    addResourcePath("Sources", paste0(getwd(), "/Sources"))
    addResourcePath("Review", paste0(getwd(), "/Review"))
    
    # Create reactive values
    tables <- reactiveValues()
    tables$paths <- paths
    tables$levels <- levels
    
    tables$sources <- sources
    tables$concepts <- concepts
    tables$relations <- relations
    tables$moderations <- moderations
    
    tables$definitions <- definitions
    tables$explanations_rel <- explanations_rel
    tables$explanations_mod <- explanations_mod
    
    tables$operationalizations <- operationalizations
    tables$observations_rel <- observations_rel
    tables$observations_mod <- observations_mod
    
    tables$notes <- notes
    
    missing_paths <- paths
    missing_paths[!(paths$path %in% files),"path"] <- NA # remove broken paths
    missing_paths <- missing_paths[is.na(missing_paths$path),]
    missing_ref <- tibble::tibble(
      path = dplyr::filter(paths, is.na(key))$path, # document not tied to reference
      key = as.character(NA)
    )
    edit_paths <- dplyr::bind_rows(missing_paths, missing_ref) %>%
      dplyr::mutate(remove = FALSE)
    tables$edit_paths <- edit_paths
    
    tables$lexana <- lexana
    
    output$edit_paths <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(tables$edit_paths, height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    observeEvent(input$update_links,{
      updates <- suppressWarnings(rhandsontable::hot_to_r(input$edit_paths))
      remove <- dplyr::filter(updates, remove == TRUE)
      updates <- dplyr::select(dplyr::filter(na.omit(updates), remove == FALSE), -remove)
      tables$paths <- tables$paths %>%
        dplyr::filter(!(path %in% updates$path), !(path %in% remove$path)) %>%
        dplyr::bind_rows(updates)
      
      new_missing <- tables$edit_paths %>%
        dplyr::filter(!(path %in% updates$path))
      
      tables$edit_paths <- new_missing
    })
    
    output$references <- DT::renderDataTable(DT::datatable(references, editable = FALSE))
    
    
    output$slctkey <- renderUI({
      base <- na.omit(tables$paths)
      keys <- base$key
      titles <- strsplit(base$path, "/")
      titles <- gsub(".pdf","", unlist(lapply(titles, function(x) x[length(x)])))
      names(keys) <- titles
      selectInput("slctkey", "Paper to display", choices = keys, selected = keys[[1]], width = "100%")
    })
    
    output$pdfviewer <- renderUI({
      if (!is.null(input$slctkey)) pdfurl <- dplyr::filter(tables$paths, key == input$slctkey)$path[1] else
        pdfurl <- files[1]
      tags$iframe(style="height:1000px; width:100%; scrolling=yes", src=paste0("Sources/", pdfurl))
    })
    
    
    output$main_words <- renderPlot({
      
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      if (is.null(input$wrdnbr)) wrdnbr <- 50 else wrdnbr <- input$wrdnbr
      
      if (is.null(tables$lexana$semrel[[slctkey]])){
        
        withProgress(message = "References", value = 0, {
          
          incProgress(0 / 3, detail = "Import text")
          file <- filter(tables$paths, key == slctkey)$path[[1]]
          text <- pdftools::pdf_text(paste0("Sources/", file))
          
          incProgress(0 / 3, detail = "Analyze semantic relationships")
          semrel <- text %>%
            paste(collapse = " ") %>%
            buildR::text_clean() %>%
            buildR::text_txt2semrel(model = "Review/english-ewt-ud-2.4-190531.udpipe") %>%
            tibble::as_tibble()
          
          tables$lexana$semrel[[slctkey]] <- semrel
          
          incProgress(0 / 3, detail = "Create bag of words")
          bow <- semrel%>%
            buildR::text_semrel2bow()
          
          tables$lexana$bow[[slctkey]] <- bow
        })
      }
      
      wrdcld <- tables$lexana$bow[[slctkey]] %>%
        dplyr::filter(!(term %in% tm::stopwords("english")), nchar(term) >= 3)
      
      wordcloud::wordcloud(wrdcld$term, wrdcld$count, scale = c(4,0.5),
                           colors = viridis::inferno(5, direction = -1, end = 0.9),
                           random.order = FALSE, rot.per = 0,
                           max.words = wrdnbr, min.freq = 1)
    })
    
    
    
    
    output$edit_source <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$sources %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_concepts <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$concepts %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_relations <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$relations %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_moderations <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$moderations %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_definitions <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$definitions %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_explanations_rel <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$explanations_rel %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_explanations_mod <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$explanations_mod %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_operationalizations <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$operationalizations %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_observations_rel <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$observations_rel %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_observations_mod <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$observations_mod %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    output$edit_notes <- rhandsontable::renderRHandsontable({
      if (is.null(input$slctkey)) slctkey <- na.omit(tables$paths)$key[[1]] else slctkey <- input$slctkey
      tables$notes %>%
        tmp_add_row(key = slctkey) %>%
        dplyr::filter(key == slctkey) %>%
        rhandsontable::rhandsontable(height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    
    
    
    output$edit <- renderUI({
      
      ui <- list()
      j <- 1
      if ("Wordcloud" %in% input$displedit) {
        ui[[j]] <- h3("Wordcloud")
        j <- j+1
        ui[[j]] <- numericInput("wrdnbr", "Number of words", min = 10, max = 100, value = 50)
        j <- j+1
        ui[[j]] <- plotOutput("main_words", width = "100%")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Source" %in% input$displedit) {
        ui[[j]] <- h3("Source")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_source")
        j <- j+1
        ui[[j]] <- actionButton("apply_source", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Concepts" %in% input$displedit) {
        ui[[j]] <- h3("Concepts")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_concepts")
        j <- j+1
        ui[[j]] <- actionButton("apply_concepts", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Relations" %in% input$displedit) {
        ui[[j]] <- h3("Relations")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_relations")
        j <- j+1
        ui[[j]] <- actionButton("apply_relations", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Moderations" %in% input$displedit) {
        ui[[j]] <- h3("Moderations")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_moderations")
        j <- j+1
        ui[[j]] <- actionButton("apply_moderations", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Definitions" %in% input$displedit) {
        ui[[j]] <- h3("Definitions")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_definitions")
        j <- j+1
        ui[[j]] <- actionButton("apply_definitions", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Explanations (relations)" %in% input$displedit) {
        ui[[j]] <- h3("Explanations (relations)")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_explanations_rel")
        j <- j+1
        ui[[j]] <- actionButton("apply_explrel", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Explanations (moderations)" %in% input$displedit) {
        ui[[j]] <- h3("Explanations (moderations)")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_explanations_mod")
        j <- j+1
        ui[[j]] <- actionButton("apply_explmod", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Operationalizations" %in% input$displedit) {
        ui[[j]] <- h3("Operationalizations")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_operationalizations")
        j <- j+1
        ui[[j]] <- actionButton("apply_operarionalizations", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Observations (relations)" %in% input$displedit) {
        ui[[j]] <- h3("Observations (relations)")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_observations_rel")
        j <- j+1
        ui[[j]] <- actionButton("apply_obsrel", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Observations (moderations)" %in% input$displedit) {
        ui[[j]] <- h3("Observations (moderations)")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_observations_mod")
        j <- j+1
        ui[[j]] <- actionButton("apply_obsmod", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      if ("Notes" %in% input$displedit) {
        ui[[j]] <- h3("")
        j <- j+1
        ui[[j]] <- rHandsontableOutput("edit_notes")
        j <- j+1
        ui[[j]] <- actionButton("apply_notes", "Apply changes")
        j <- j+1
        ui[[j]] <- tags$hr()
        j <- j+1
      }
      
      ui
    })
    
    
    
    
    
    
    #apply_concepts
    #apply_relations
    #apply_moderations
    #apply_definitions
    #apply_explrel
    #apply_explmod
    #apply_operarionalizations
    #apply_obsrel
    #apply_obsmod
    #apply_notes
    
    observeEvent(input$apply_source, {
      toappend <- rhandsontable::hot_to_r(input$edit_source) %>%
        dplyr::mutate_if(is.factor, function(x) factor(x, levels = levels(x), ordered = FALSE)) %>%
        as_tibble()
      
      tables$sources <- tables$sources %>%
        dplyr::filter(key != input$slctkey) %>%
        dplyr::bind_rows(toappend)
    })
    
    
    
    
    
    
    
    
    
    observeEvent(input$done, {
      
      paths <- tables$paths
      levels <- tables$levels
      sources <- tables$sources
      concepts <- tables$concepts
      relations <- tables$relations
      moderations <- tables$moderations
      definitions <- tables$definitions
      explanations_rel <- tables$explanations_rel
      explanations_mod <- tables$explanations_mod
      operationalizations <- tables$operationalizations
      observations_rel <- tables$observations_rel
      observations_mod <- tables$observations_mod
      notes <- tables$notes
      lexana <- tables$lexana
      
      save(paths, levels,
           sources,concepts,relations,moderations,
           definitions, explanations_rel, explanations_mod,
           operationalizations, observations_rel,observations_mod,
           notes, file = "Review/srcrev.RData")
      
      save(lexana, file = "Review/lexana.RData")
      
      stopApp()
    })
  }
  runGadget(ui, server, viewer = shiny::browserViewer())
}


tmp_add_row <- function(x, key){
  x[nrow(x)+1,"key"] <- key
  x
}
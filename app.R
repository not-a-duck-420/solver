#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require('shiny')) install.packages('shiny'); library(shiny)
if (!require('shinyjs')) install.packages('shinyjs'); library(shinyjs)
if (!require('DT')) install.packages('DT'); library(DT)
if (!require('googlesheets4')) install.packages('googlesheets4'); library(googlesheets4)

solve.cypher = function(text) {
  text = toupper(gsub(" ","",paste(text,collapse="")))
  text = gsub("G","0",text)
  text = gsub("H","1",text)
  text = gsub("I","2",text)
  text = gsub("J","3",text)
  text = gsub("K","4",text)
  text = gsub("L","5",text)
  text = gsub("M","6",text)
  text = gsub("N","7",text)
  text = gsub("O","8",text)
  text = gsub("P","9",text)
  text = gsub("Q","A",text)
  text = gsub("R","B",text)
  text = gsub("S","C",text)
  text = gsub("T","D",text)
  text = gsub("U","E",text)
  text = gsub("V","F",text)
  text = gsub("W","0",text)
  text = gsub("X","1",text)
  text = gsub("Y","2",text)
  text = gsub("Z","3",text)
  tolower(text)
}
test.ideas = function(temp, already.tried = NULL) {
  t.lens = sapply(temp,length)
  temp = temp[which(t.lens != 0)]

  pos.results = prod(sapply(temp,length))
  result = temp[[1]]
  for (i in 2:length(temp)) {
    new.new.result = NULL
    for (j in 1:length(temp[[i]])) {
      new.result = NULL
      for (k in result) {
        new.result = c(new.result, paste(k, temp[[i]][j]))
      }
      new.new.result = c(new.new.result, new.result)
    }
    result = new.new.result
  }
  #print(result)
  solved = sapply(result, solve.cypher)
  solved.count = sapply(solved, nchar)

  correct = which(solved.count == 64)
  untested = which(!solved %in% already.tried)
  if (length(correct) != 0) {
    correct = correct[which(correct %in% untested)]
  }

  if (length(correct) == 0) {
    NULL
  } else {
    result = data.frame(cypher = as.character(solved[correct]),
                        idea = names(solved[correct]))
    result$wallet.ID = sapply(result$cypher, FUN = function(cypher) {
      temp = system(paste("python retrieve_wallet.py",cypher), intern=TRUE)
      temp = strsplit(temp," ")[[1]]
      temp[length(temp)]
    })
    result
  }
}
row_col_to_cell <- function(row, col) {
  # Convert column number to letters (e.g., 1 -> A, 2 -> B, ...)
  col_letter <- LETTERS[col]

  # Combine row and column to form the cell specification
  cell_spec <- paste0(col_letter, row)

  return(cell_spec)
}

google.sheet <- read_sheet('https://docs.google.com/spreadsheets/d/1YNH8akuTMSgwjsUoISUfxJhK4aYamlG7Firm1-YXrvg/edit#gid=0',
                           sheet = 1)
google.sheet <- as.data.frame(google.sheet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),

    # Application title
    titlePanel("Goose cypher solver"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          actionButton("load_sheet_button","Reload data"),
          actionButton("clear_button","Clear ideas"),
          actionButton("test_button","Test ideas"),
          selectInput("clue_selector","Select clue:","none","none"),
          verbatimTextOutput("full_clue_text"),
          htmlOutput("part_1_label"),
          verbatimTextOutput("part_1_text"),
          textAreaInput("input_1",""),
          htmlOutput("part_2_label"),
          verbatimTextOutput("part_2_text"),
          textAreaInput("input_2",""),
          htmlOutput("part_3_label"),
          verbatimTextOutput("part_3_text"),
          textAreaInput("input_3",""),
          htmlOutput("part_4_label"),
          verbatimTextOutput("part_4_text"),
          textAreaInput("input_4",""),
          htmlOutput("part_5_label"),
          verbatimTextOutput("part_5_text"),
          textAreaInput("input_5",""),
          htmlOutput("part_6_label"),
          verbatimTextOutput("part_6_text"),
          textAreaInput("input_6",""),
          htmlOutput("part_7_label"),
          verbatimTextOutput("part_7_text"),
          textAreaInput("input_7",""),
          htmlOutput("part_8_label"),
          verbatimTextOutput("part_8_text"),
          textAreaInput("input_8",""),
          htmlOutput("part_9_label"),
          verbatimTextOutput("part_9_text"),
          textAreaInput("input_9",""),
          htmlOutput("part_10_label"),
          verbatimTextOutput("part_10_text"),
          textAreaInput("input_10",""),
          textAreaInput("tested_answers","Already tested answers"),
          actionButton("clear_tested_button","Clear tested keys")
        ),

        mainPanel(
          textOutput("output_text"),
          actionButton("mark_tested_button","Mark as tested"),
          DTOutput("output_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ### Hiding some elements to begin
  hide("full_clue_text")
  hide("part_1_text")
  hide("part_2_text")
  hide("part_3_text")
  hide("part_4_text")
  hide("part_5_text")
  hide("part_6_text")
  hide("part_7_text")
  hide("part_8_text")
  hide("part_9_text")
  hide("part_10_text")
  hide("mark_tested_button")

  ### Initializing labels
  output$part_1_label = renderText("<b>Part 1</b>")
  output$part_2_label = renderText("<b>Part 2</b>")
  output$part_3_label = renderText("<b>Part 3</b>")
  output$part_4_label = renderText("<b>Part 4</b>")
  output$part_5_label = renderText("<b>Part 5</b>")
  output$part_6_label = renderText("<b>Part 6</b>")
  output$part_7_label = renderText("<b>Part 7</b>")
  output$part_8_label = renderText("<b>Part 8</b>")
  output$part_9_label = renderText("<b>Part 9</b>")
  output$part_10_label = renderText("<b>Part 10</b>")

  ### Initializing text
  output$output_text = renderText("")
  output$full_clue_text = renderText("")
  output$part_1_text = renderText("")
  output$part_2_text = renderText("")
  output$part_3_text = renderText("")
  output$part_4_text = renderText("")
  output$part_5_text = renderText("")
  output$part_6_text = renderText("")
  output$part_7_text = renderText("")
  output$part_8_text = renderText("")
  output$part_9_text = renderText("")
  output$part_10_text = renderText("")

  ### Initializing output table
  output$output_table = renderDT({
    datatable(NULL)
  })

  ### Initializing spreedsheet data
  updateSelectInput(inputId = "clue_selector",
                    choices = c("none",paste("Clue",substring(colnames(google.sheet)[grep("-clue",colnames(google.sheet))],1,1))),
                    selected = "none")

  ### Reload data from the google spreadsheet
  observeEvent(input$load_sheet_button, {
    google.sheet <- read_sheet('https://docs.google.com/spreadsheets/d/1YNH8akuTMSgwjsUoISUfxJhK4aYamlG7Firm1-YXrvg/edit#gid=0',
                    sheet = 1)
    google.sheet <- as.data.frame(google.sheet)
    assign("google.sheet",google.sheet,".GlobalEnv")

    # keep selected clue the same unless it has been removed from the google doc
    if (input$clue_selector %in% c("none",paste("Clue",substring(colnames(google.sheet)[grep("-clue",colnames(google.sheet))],1,1)))) {
      t.selected = input$clue_selector
    } else {
      t.selected = "none"
    }
    updateSelectInput(inputId = "clue_selector",
                      choices = c("none",paste("Clue",substring(colnames(google.sheet)[grep("-clue",colnames(google.sheet))],1,1))),
                      selected = t.selected)
  })

  ### Clear inputs button
  observeEvent(input$clear_button, {
    updateTextAreaInput(inputId = "input_1", value = "")
    updateTextAreaInput(inputId = "input_2", value = "")
    updateTextAreaInput(inputId = "input_3", value = "")
    updateTextAreaInput(inputId = "input_4", value = "")
    updateTextAreaInput(inputId = "input_5", value = "")
    updateTextAreaInput(inputId = "input_6", value = "")
    updateTextAreaInput(inputId = "input_7", value = "")
    updateTextAreaInput(inputId = "input_8", value = "")
    updateTextAreaInput(inputId = "input_9", value = "")
    updateTextAreaInput(inputId = "input_10", value = "")
  })

  ### Test ideas button
  observeEvent(input$test_button,{
    options = list()
    options[[1]] = strsplit(input$input_1,"\n",TRUE)[[1]]
    options[[2]] = strsplit(input$input_2,"\n",TRUE)[[1]]
    options[[3]] = strsplit(input$input_3,"\n",TRUE)[[1]]
    options[[4]] = strsplit(input$input_4,"\n",TRUE)[[1]]
    options[[5]] = strsplit(input$input_5,"\n",TRUE)[[1]]
    options[[6]] = strsplit(input$input_6,"\n",TRUE)[[1]]
    options[[7]] = strsplit(input$input_7,"\n",TRUE)[[1]]
    options[[8]] = strsplit(input$input_8,"\n",TRUE)[[1]]
    options[[9]] = strsplit(input$input_9,"\n",TRUE)[[1]]
    options[[10]] = strsplit(input$input_10,"\n",TRUE)[[1]]

    selected.clue = paste(substring(input$clue_selector,nchar(input$clue_selector)),"-clue",sep="")
    selected.clue = which(colnames(google.sheet) == selected.clue)
    correct.wallet = google.sheet[2,selected.clue]

    if (length(unlist(options)) == 0) {
      output$output_text = renderText("No ideas input.")
      hide("output_table")
      hide("mark_tested_button")
    } else {
      temp.table <- test.ideas(options,
                              already.tried = strsplit(input$tested_answers,"\n",TRUE)[[1]])
      if (is.null(temp.table)) {
        output$output_text = renderText("No new correct length solutions found.")
        hide("output_table")
        hide("mark_tested_button")
      } else {
        show("output_table")
        show("mark_tested_button")
        if (correct.wallet %in% temp.table$wallet.ID) {
          output$output_text = renderText(paste("Congratulations! You found the goose! The correct private key is:",
                                                temp.table$cypher[which(temp.table$wallet.ID == correct.wallet)]))
          output$output_table = renderDataTable({
            temp.table
          }, selection = list(mode = 'multiple', selected = which(temp.table$wallet.ID == correct.wallet)))
        } else {
          output$output_text = renderText("Correct wallet ID has not been found.")
          output$output_table = renderDT({
            datatable(temp.table)
          })
        }
      }

    }



  })

  ### Select clue
  observeEvent(input$clue_selector, {
    if (input$clue_selector != "none") {
      selected.clue = paste(substring(input$clue_selector,nchar(input$clue_selector)),"-clue",sep="")
      selected.clue = which(colnames(google.sheet) == selected.clue)

      output$full_clue_text = renderText(google.sheet[1,selected.clue])
      output$part_1_text = renderText(google.sheet[3,selected.clue])
      output$part_2_text = renderText(google.sheet[4,selected.clue])
      output$part_3_text = renderText(google.sheet[5,selected.clue])
      output$part_4_text = renderText(google.sheet[6,selected.clue])
      output$part_5_text = renderText(google.sheet[7,selected.clue])
      output$part_6_text = renderText(google.sheet[8,selected.clue])
      output$part_7_text = renderText(google.sheet[9,selected.clue])
      output$part_8_text = renderText(google.sheet[10,selected.clue])
      output$part_9_text = renderText(google.sheet[11,selected.clue])
      output$part_10_text = renderText(google.sheet[12,selected.clue])

      updateTextAreaInput(inputId = "input_1", value = google.sheet[3,selected.clue+1])
      updateTextAreaInput(inputId = "input_2", value = google.sheet[4,selected.clue+1])
      updateTextAreaInput(inputId = "input_3", value = google.sheet[5,selected.clue+1])
      updateTextAreaInput(inputId = "input_4", value = google.sheet[6,selected.clue+1])
      updateTextAreaInput(inputId = "input_5", value = google.sheet[7,selected.clue+1])
      updateTextAreaInput(inputId = "input_6", value = google.sheet[8,selected.clue+1])
      updateTextAreaInput(inputId = "input_7", value = google.sheet[9,selected.clue+1])
      updateTextAreaInput(inputId = "input_8", value = google.sheet[10,selected.clue+1])
      updateTextAreaInput(inputId = "input_9", value = google.sheet[11,selected.clue+1])
      updateTextAreaInput(inputId = "input_10", value = google.sheet[12,selected.clue+1])

      updateTextAreaInput(inputId = "tested_answers", value = google.sheet[13,selected.clue+1])

      show("full_clue_text")

      for (i in 1:10) {
        if (i %in% which(is.na(google.sheet[3:12,selected.clue]))) {
          hide(paste("part",i,"label",sep="_"))
          hide(paste("part",i,"text",sep="_"))
          hide(paste("input",i,sep="_"))
        } else {
          show(paste("part",i,"label",sep="_"))
          show(paste("part",i,"text",sep="_"))
          show(paste("input",i,sep="_"))
        }
      }


    } else {
      output$full_clue_text = renderText("")
      output$part_1_text = renderText("")
      output$part_2_text = renderText("")
      output$part_3_text = renderText("")
      output$part_4_text = renderText("")
      output$part_5_text = renderText("")
      output$part_6_text = renderText("")
      output$part_7_text = renderText("")
      output$part_8_text = renderText("")
      output$part_9_text = renderText("")
      output$part_10_text = renderText("")

      hide("full_clue_text")
      hide("part_1_text")
      hide("part_2_text")
      hide("part_3_text")
      hide("part_4_text")
      hide("part_5_text")
      hide("part_6_text")
      hide("part_7_text")
      hide("part_8_text")
      hide("part_9_text")
      hide("part_10_text")

      for (i in 1:10) {
        show(paste("part",i,"label",sep="_"))
        show(paste("part",i,"text",sep="_"))
        show(paste("input",i,sep="_"))
      }
    }
  })

  ### Mark as tested
  observeEvent(input$mark_tested_button, {

    options = list()
    options[[1]] = strsplit(input$input_1,"\n",TRUE)[[1]]
    options[[2]] = strsplit(input$input_2,"\n",TRUE)[[1]]
    options[[3]] = strsplit(input$input_3,"\n",TRUE)[[1]]
    options[[4]] = strsplit(input$input_4,"\n",TRUE)[[1]]
    options[[5]] = strsplit(input$input_5,"\n",TRUE)[[1]]
    options[[6]] = strsplit(input$input_6,"\n",TRUE)[[1]]
    options[[7]] = strsplit(input$input_7,"\n",TRUE)[[1]]
    options[[8]] = strsplit(input$input_8,"\n",TRUE)[[1]]
    options[[9]] = strsplit(input$input_9,"\n",TRUE)[[1]]
    options[[10]] = strsplit(input$input_10,"\n",TRUE)[[1]]

    temp.table <- test.ideas(options,
                             already.tried = strsplit(input$tested_answers,"\n",TRUE)[[1]])

    selected.clue = paste(substring(input$clue_selector,nchar(input$clue_selector)),"-clue",sep="")
    selected.clue = which(colnames(google.sheet) == selected.clue)
    correct.wallet = google.sheet[2,selected.clue]

    tested.cypher = temp.table$cypher[input$output_table_rows_selected]
    new.val = paste(input$tested_answers,paste(tested.cypher,collapse="\n"),sep="\n")
    while (substring(new.val,1,1) == "\n") {new.val = substring(new.val,2)}

    ## update in app
    updateTextAreaInput(inputId = "tested_answers",
                        value = new.val)

    ## re-load table
    temp.table <- test.ideas(options,
                             already.tried = strsplit(new.val,"\n",TRUE)[[1]])

    if (is.null(temp.table)) {
      output$output_text = renderText("No new correct length solutions found.")
      hide("output_table")
      hide("mark_tested_button")
    } else {
      show("output_table")
      show("mark_tested_button")
      if (correct.wallet %in% temp.table$wallet.ID) {
        output$output_text = renderText(paste("Congratulations! You found the goose! The correct private key is:",
                                              temp.table$cypher[which(temp.table$wallet.ID == correct.wallet)]))
        output$output_table = renderDataTable({
          temp.table
        }, selection = list(mode = 'multiple', selected = which(temp.table$wallet.ID == correct.wallet)))
      } else {
        output$output_text = renderText("Correct wallet ID has not been found.")
        output$output_table = renderDT({
          datatable(temp.table)
        })
      }
    }
  })

  ### Hide mark as tested if any changes made to input boxes
  observeEvent(c(input$input_1,
                 input$input_2,
                 input$input_3,
                 input$input_4,
                 input$input_5,
                 input$input_6,
                 input$input_7,
                 input$input_8,
                 input$input_9,
                 input$input_10),{
    hide("mark_tested_button")
  })

  ### Clear list of tested keys
  observeEvent(input$clear_tested_button, {
    updateTextAreaInput(inputId = "tested_answers", value = "")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

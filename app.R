#
# useful link for getting python to run correctly:
# https://github.com/ranikay/shiny-reticulate-app#deploying-to-shinyapps.io
###################################
### Installing/loading packages ###
###################################
# reticulate::virtualenv_create(envname = 'python39_env')
reticulate::virtualenv_create(envname = 'python3_env')
reticulate::virtualenv_install('python3_env',
                               packages = c('numpy'))  # <- Add other packages here, if needed

if (!require('shiny')) install.packages('shiny'); library(shiny)
if (!require('shinyjs')) install.packages('shinyjs'); library(shinyjs)
if (!require('DT')) install.packages('DT'); library(DT)
if (!require('reticulate')) install.packages('reticulate'); library(reticulate)

### load python stuff
use_virtualenv('python3_env', required = TRUE)
py_install(c('web3','eth_account'))
source_python("py_functions.py")

#################
### Functions ###
#################
### convert string into a vector of characters
s2c = function(x) {
  strsplit(x,"")[[1]]
}

### convert vector of characters into a string
c2s = function(x) {
  paste(x,collapse="")
}

### return last element of a vector
last = function(x) {
  x[length(x)]
}

### removes all non-hex characters from a string (keeping a:)
rm.hex = function(x) {
  x = s2c(x)
  c2s(x[which(x %in% c(0:9,LETTERS[1:6],letters[1:6]))])
}

### convert a string or vector of strings into a private key using the cipher
solve.cipher = function(text) {
  text = tolower(gsub(" ","",paste(text,collapse="")))
  text = gsub("g","0",text)
  text = gsub("h","1",text)
  text = gsub("i","2",text)
  text = gsub("j","3",text)
  text = gsub("k","4",text)
  text = gsub("l","5",text)
  text = gsub("m","6",text)
  text = gsub("n","7",text)
  text = gsub("o","8",text)
  text = gsub("p","9",text)
  text = gsub("q","a",text)
  text = gsub("r","b",text)
  text = gsub("s","c",text)
  text = gsub("t","d",text)
  text = gsub("u","e",text)
  text = gsub("v","f",text)
  text = gsub("w","0",text)
  text = gsub("x","1",text)
  text = gsub("y","2",text)
  text = gsub("z","3",text)
  rm.hex(text)
}

### input ideas should be a list with each item of the list corresponding to a
### part of the clue and containing a vector of strings (each an idea for that part)
### input 'already.tried' should be a vector of strings corresponding to tested
### ciphers to exclude from the results
test.ideas = function(ideas, already.tried = NULL) {
  t.lens = sapply(ideas,length)          # number of ideas for each part
  ideas = ideas[which(t.lens != 0)]      # remove parts for which no ideas are supplied
  if (length(ideas) == 0) {return(NULL)} # if no ideas, return NULL

  ### create vector of all possible combinations of ideas
  result = ideas[[1]]
  if (length(ideas) > 1) {
    for (i in 2:length(ideas)) {
      new.new.result = NULL
      for (j in 1:length(ideas[[i]])) {
        new.result = NULL
        for (k in result) {
          new.result = c(new.result, paste(k, ideas[[i]][j]))
        }
        new.new.result = c(new.new.result, new.result)
      }
      result = new.new.result
    }
  }

  ### solve for keys using the cipher
  solved = sapply(result, solve.cipher)
  solved.count = sapply(solved, nchar)

  ### subset for correct lengths and untested ciphers
  correct = which(solved.count == 64)
  untested = which(!solved %in% already.tried)
  if (length(correct) != 0) {
    correct = correct[which(correct %in% untested)]
  }
  if (length(correct) == 0) {return(NULL)}

  result = data.frame(key = as.character(solved[correct]),
                      idea = names(solved[correct]))
  result$wallet = sapply(result$key, get_ethereum_address)
  result
}

### load table from the google spreadsheet
load.google.sheet = function() {
  temp = read.csv("https://docs.google.com/spreadsheets/d/1YNH8akuTMSgwjsUoISUfxJhK4aYamlG7Firm1-YXrvg/export?format=csv")
  colnames(temp) = gsub("X","",gsub(".","-",colnames(temp),fixed=TRUE))
  assign("google.sheet",temp,envir = globalenv())
}

################################
### Loading the google sheet ###
################################
load.google.sheet()

######################
### Application UI ###
######################
ui <- fluidPage(
  useShinyjs(),

    # Application title
    titlePanel("Goose cipher solver"),

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
          verbatimTextOutput("output_text"),
          DTOutput("output_table")
        )
    )
)

##########################
### Application server ###
##########################
server <- function(input, output) {

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
    load.google.sheet()

    # keep selected clue the same unless it has been removed from the google doc
    if (input$clue_selector %in% c("none",paste("Clue",substring(colnames(google.sheet)[grep("-clue",colnames(google.sheet))],1,1)))) {
      t.selected = input$clue_selector
    } else {
      t.selected = "none"
    }
    updateSelectInput(inputId = "clue_selector",
                      choices = c("none",paste("Clue",substring(colnames(google.sheet)[grep("-clue",colnames(google.sheet))],1,1))),
                      selected = "none")
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
    correct.wallet = gsub(" ","",google.sheet[2,selected.clue])

    if (length(unlist(options)) == 0) {
      output$output_text = renderText("No ideas input.")
      hide("output_table")
      hide("mark_tested_button")
    } else {
      temp.table <- test.ideas(options, already.tried = strsplit(input$tested_answers,"\n",TRUE)[[1]])

      ### Add incorrect ciphers to the list of tested ciphers
      tested.cipher = temp.table$key[which(temp.table$wallet != correct.wallet)]
      new.val = paste(input$tested_answers,paste(tested.cipher,collapse="\n"),sep="\n")
      while (substring(new.val,1,1) == "\n") {new.val = substring(new.val,2)}
      while (gsub("\n\n","\n",new.val,fixed=TRUE) != new.val) {new.val = gsub("\n\n","\n",new.val,fixed=TRUE)}
      updateTextAreaInput(inputId = "tested_answers", value = new.val)

      if (is.null(temp.table)) {
        output$output_text = renderText("No new correct length solutions found.")
        hide("output_table")
        hide("mark_tested_button")
      } else {
        show("output_table")
        show("mark_tested_button")
        if (correct.wallet %in% temp.table$wallet) {
          congrats.message = sample(c("Honk! Honk!","duck... duck... GOOSE!!!",
                                      "Goose on the loose!","Congratulations!",
                                      "Take a gander..."),1)

          output$output_text = renderText(paste(congrats.message,"You found the goose! The correct private key is:\n",
                                                temp.table$key[which(temp.table$wallet == correct.wallet)]))
          output$output_table = renderDataTable({
            temp.table},selection = list(mode = 'multiple', selected = which(temp.table$wallet == correct.wallet)))
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
    output$output_text = renderText("")
    hide("mark_tested_button")
    hide("output_table")

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

      show("full_clue_text")

      for (i in 1:10) {
        if (i %in% which(google.sheet[3:12,selected.clue] == "")) {
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

  ### Clear list of tested keys
  observeEvent(input$clear_tested_button, {
    updateTextAreaInput(inputId = "tested_answers", value = "")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

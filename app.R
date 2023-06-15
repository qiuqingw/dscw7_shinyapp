#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tm)
library(purrr)
library(ngram)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Science Capstone Final Project at Week 7"),
    sidebarLayout(
        sidebarPanel(
            textInput("inputText", "This apps can predict the next word to your input",value = ""),
            br(), 
            submitButton("Predict"),
            hr(),
            helpText("Click on the Predict button above to predict the next word", 
                     hr(),
                     "The brief output from R.")
        ),
        mainPanel(
            strong("My input:"),
            strong(code(textOutput('inputsentence'))),
            br(),
            strong("The projected word:"),
            strong(code(textOutput('thenextword'))),
            hr(),
            h2("R output"),
            verbatimTextOutput("prediction")
        )
    )
)

# Read three matrixs containing two-word, three-word and four-word patterns.
word2_matrix <- readRDS("twoword_matrix.Rda")
word3_matrix <- readRDS("threeword_matrix.Rda")
word4_matrix <- readRDS("fourword_matrix.Rda")

returnPredVal <- function(headval, numericval) {
    strsplititem <- unlist(strsplit(headval," "))
    returnval <- strsplititem[numericval]
}

# Create a new function "projectnextword", which is used to predict the most frequent word after your input.
projectnextword <- function(inputword) {
    
    #Clean the extra characters
    checkword <- Corpus(VectorSource(inputword))
    inspect(checkword)
    checkword <- tm_map(checkword, content_transformer(tolower))
    checkword <- tm_map(checkword, content_transformer(removeNumbers))
    checkword <- tm_map(checkword, content_transformer(removePunctuation))
    removespecial <- function(x) gsub("[^( )[:alnum:]]","",x)
    checkword <- tm_map(checkword, content_transformer(removespecial))
    checkword <- tm_map(checkword, stripWhitespace)
    checkword <- checkword[[1]]$content[1]
    
    #Check the number of words in the input.  
    words <- unlist(strsplit(checkword," ")) 
    wordcount <- length(words)
    inputWord <- ""
    returnVal <- "N/A"
    if (wordcount>=3){ 
        inputWord <- words[(wordcount-2):wordcount]
    } else if(wordcount == 2){ 
        inputWord <- c("", words)
    } else 
        if(wordcount == 1){
        inputWord <- c("", "", words)
    } else {
        stop("Please enter a word.") 
    }
    catchword <- paste(inputWord[1], inputWord[2], inputWord[3])
    catchword <- paste("^", catchword, sep="")
    fourpred <- word4_matrix[grep(catchword, word4_matrix$Word), ]
    
    #Find the projected word based on the frequency list and the input we enter in the box.
    if(nrow(fourpred) > 0) {
        headval <- as.character(head(fourpred$Word,1))
        returnVal <<- returnPredVal(headval, 4)
    } else {
        
        #If there's no frequency found, Ngram3 will be used.
        catchword <- paste(inputWord[2], inputWord[3])
        catchword <- paste("^", catchword, sep="")
        threepred <- word3_matrix[grep(catchword, word3_matrix$Word), ]
        if(nrow(threepred) > 0) {
            headval <- as.character(head(threepred$Word,1))
            returnVal <<- returnPredVal(headval, 3)
        } else {
            
            #If there's no frequency found, Ngram2 will be used.
            catchword <- paste(inputWord[3])
            catchword <- paste("^", catchword, sep="")
            twopred <- word2_matrix[grep(catchword, word2_matrix$Word), ]
            if (nrow(twopred) > 0) {
                headval <- as.character(head(twopred$Word,1))
                returnVal <<- returnPredVal(headval, 2)
            } else {
                
                #Finally, if there's no frequency found, unknown will be uses.
                returnVal <<- "Unknown"
            }
        }
    }
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$inputsentence <- renderText({
        input$inputText});
    
    output$prediction <- renderPrint({
        result <- projectnextword(input$inputText)
        output$thenextword <- renderText({returnVal})
        result
    });
}

# Run the application 
shinyApp(ui = ui, server = server)

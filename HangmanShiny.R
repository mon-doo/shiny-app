#Extra credit
#Hangman Game
#Monica Dooley

library(shiny)
library(tidyverse)
library(plotrix)
library(htmlTable)
dictionary <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/google-10000-english-usa-no-swears.txt")
dictionary <- subset(dictionary, nchar(dictionary$words) > 4)
n <- 0
word.display <-""
number.mistakes <- 0
result.recorded <- FALSE
guess.messages <- ""
used.letters <- c()
first_call <- TRUE
is.letter <- function(x) (is.character(x) & grepl("[[:alpha:]]", x) & (nchar(x)==1))
answer <- ""

new.game <- function(){
  answer <<- sample(dictionary$words, 1)
  n <<- nchar(answer)
  word.display <<- ""
  number.mistakes <<- 0
  result.recorded <<- FALSE
  guess.messages <<- ""
  used.letters <<- c()
  first_call <<- TRUE
  for (i in seq(1:n)) {    
    if (!is.letter(substr(answer,i,i))) {
      word.display <<- paste(word.display, substr(answer,i,i), sep="")
    }
    else {word.display <<- paste(word.display,"?", sep="") }
  }
}

guess <- function(letter) {
  guess.messages <<- ""
  if (!is.letter(letter)) {
    if(letter != " ")
      guess.messages <<-"Please enter a single letter"
  }
  else if (toupper(letter) %in% used.letters) {
    guess.messages <<-paste("You've already guessed ",toupper(letter))
  }
  else {
    word.display.new <<- ""
    found <<- FALSE
    for (i in seq(1:n)) {
      if (tolower(substr(answer,i,i)) == tolower(letter)) {
        word.display.new <- paste(word.display.new, substr(answer,i,i), sep="")
        found <<- TRUE
      }
      else {word.display.new <- paste(word.display.new, substr(word.display,i,i), sep="") }
    }
    word.display <<- word.display.new
    used.letters <<- c(used.letters,toupper(letter))
    if (found==FALSE) {
      number.mistakes <<- number.mistakes + 1
    }
  }
  
  if (number.mistakes >= 7) {
    guess.messages <<- "You lose, he was hanged :("
  }
  else if (word.display == answer) {
    guess.messages <<- "You win!! He lives!"
  }
}

new.game()

# Define UI for application 
ui <- fluidPage(titlePanel(h1("Hang Man")),
                fluidRow(
                  column(4,
                         textInput("text", label = h2("Guess:"), 
                                   value = ""),
                         actionButton("new_game", "new Game")), 
                  column(8,
                         htmlOutput("wordDisplay"),
                         plotOutput("distPlot"),
                         htmlOutput("gameMessage")),
               fluidRow(column(12,
                                h3("How to play"),
                                p("Choose one letter at a time to guess the word below. Each wrong answer adds one part to the hangman. If he is fully hanged before you solve the word, you lose :("),    
                  ))
                  ))



# Define server logic required 
server <- function(input, output, session) {

   observeEvent(input$text, {
    if(input$text != "" || first_call == TRUE) {
      guess(input$text)
      first_call <<- FALSE
      updateTextInput(session, "text", value = "")
      
      output$wordDisplay <- renderText({
        if (number.mistakes < 7) { 
          paste(word.display, sep="") 
        }
        else { 
          paste(answer, sep="")
        }
      })
      
      output$gameMessage <- renderText( {
        guess.messages
      })
      
      output$distPlot <- renderPlot({
        plot(1:100,1:100, type="n", axes=F, xlab="", ylab="")
        rectFill(0,0,2,90,bg="brown",pch=NULL, fg="brown")
        rectFill(0,88,25,90,bg="brown",pch=NULL, fg="brown")
        segments(2,70,15,89,lwd=4, col = "brown")
        
        
        if(number.mistakes > 0) {
          segments(25,81,25,90,lwd=2, col = "beige")
        }
        if(number.mistakes > 1) {
          draw.circle(25,70,5, border=1, lwd=2)
          segments(22, 74.5, 24, 72.5)
          segments(22, 72.5, 24, 74.5)
          segments(26, 74.5, 28, 72.5)
          segments(26, 72.5, 28, 74.5)
          draw.arc(25, 65, 2, deg1 = 0,deg2=180)
        }
        if(number.mistakes > 2) {
          segments(25,61,25,30,lwd=2)
        }
        if(number.mistakes > 3) {
          segments(25,55,30,40,lwd=2)
        }
        if(number.mistakes > 4) {
          segments(25,55,20,40,lwd=2)
        }
        if(number.mistakes > 5) {
          
          segments(25,30,30,10,lwd=2)
        }
        if(number.mistakes > 6) {
          segments(25,30,20,10,lwd=2)
        }
        
      })
    }
  })
  
  observeEvent(input$new_game, {
    new.game()
    updateTextInput(session, "text", value = " ")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

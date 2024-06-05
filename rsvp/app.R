library(shiny)
library(shinyRadioMatrix)
library(shinycssloaders)
library(googlesheets4)
library(stringr)

# setwd(r"(C:\Users\student\Documents\GitHub\wedding-site-rsvp\rsvp)")
# 
# options(gargle_oauth_cache = ".secrets")
# gs4_auth()
# list.files(".secrets")
# gs4_deauth()
# gs4_auth(cache=".secrets", email="aaronweinstock22@gmail.com")
gs4_auth(cache=".secrets", email=TRUE, use_oob=TRUE)

# Data =========================================================================

# Globals
SHEET_ID = "1a2uoCM9fMm5gDgv-BLgsrVhcVeIGhAMNivF0OzQLIT8"
SHEET_NAME = "RSVP"
ATTENDING_COL = "D"
VEGETARIAN_COL = "E"

# Build invites list: names are passwords, each contains a household name
# and a list of guests
rsvp = read_sheet(
  ss = SHEET_ID,
  sheet = SHEET_NAME
)
passwords = unique(rsvp$Password)
passwords = passwords[passwords != "sepwhat"]
INVITES = lapply(passwords, function(p){
  list(
    household = rsvp$Household[rsvp$Password == p][1],
    people = rsvp$Guest[rsvp$Password == p]
  )
})
names(INVITES) = passwords

# UI ===========================================================================

ui = fluidPage(
  # Title
  titlePanel(
    "Wedding RSVP"
  ),
  # Password input
  fluidRow(
    div(
      h4(
        strong(
          "Please enter the password on your RSVP and click 'Go!'"  
        )
      ),
      style = "display:inline-block;vertical-align:center;margin-left:15px;margin-right:5px"
    ),
    div(
      textInput(
        inputId = "PASSWORD",
        label = NULL,
        value = "",
        width = "100%"
      ),
      style = "display:inline-block;vertical-align:center;margin:5px"
    ),
    div(
      actionButton(
        inputId = "GO",
        label = "Go!"
      ),
      style = "display:inline-block;vertical-align:center;margin:5px"
    )
  ),
  # RSVP submission
  fluidRow(
    column(
      12,
      uiOutput(
        outputId = "INTRO"
      )
    ),
    column(
      12,
      uiOutput(
        outputId = "BUTTONS"
      )
    ),
    # column(
    #   12,
    #   uiOutput(
    #     outputId = "SUBMIT_RSVP"
    #   ),
    # ),
    column(
      12,
      fluidRow(
        div(
          uiOutput(
            outputId = "SUBMIT_RSVP"
          ),
          style = "display:inline-block;vertical-align:center;margin-left:15px;margin-right:5px"
        ),
        div(
          uiOutput(
            outputId = "SUBMIT_INSTRUCTIONS"
          ),
          style = "display:inline-block;vertical-align:center;margin:5px"
        )
      )
    ),
    column(
      12,
      uiOutput(
        outputId = "PASSWORD_ERROR"
      )
    ),
    column(
      12,
      uiOutput(
        outputId = "SUBMISSION"
      )  
    )
  )
)

# Server =======================================================================

server = function(input, output){
  # Response to password input
  observeEvent(input$GO, {
    if(input$PASSWORD %in% names(INVITES)){
      people = INVITES[[input$PASSWORD]]$people
      n = length(people)
      output$INTRO = renderUI({
        paste0(
          "Hi ", INVITES[[input$PASSWORD]]$household, "! Thanks for RSVPing! ",
          "Please read the instructions and respond accordingly.",
          "<br><br>",
          "Please select 'No' for all members of your party who will not be ",
          "attending -- we will miss you!",
          "<br><br>",
          "For those who will be attending, the main course for our wedding ",
          "will be buffet-style Texas barbecue, with options of pulled pork, pork ribs, ",
          "and/or beef brisket. We will have to order the vegetarian option ",
          "(barbecue-style cauliflower) separately. ",
          "If any attending member of your party will eat NONE of the three ",
          "meat options, please select 'Yes - vegetarian'. Only select this ",
          "option if you will eat no meat. For those attending who will eat ",
          "at least one of the meats, select 'Yes - meat'. This is crucial ",
          "to ensure we order the right amount of the vegetarian ",
          "option, and we appreciate your attention to this detail!",
          "<br><br>"
        ) %>%
          HTML()
      })
      output$BUTTONS = renderUI({
        shinyRadioMatrix::radioMatrixInput(
          inputId = "RSVP",
          rowIDs = people,
          rowLLabels = rep("", n),
          choiceNames = list("Yes! - meat","Yes! - vegetarian","No :("),
          choiceValues = list("YES","YESV","NO"),
          selected = rep(list(character(0)), n),
          rowIDsName = "Guest"
        )
      })
      output$SUBMIT_RSVP = renderUI({
        actionButton(
          inputId = "SUBMIT",
          label = "Submit your RSVP"
        )
      })
      output$SUBMIT_INSTRUCTIONS = renderUI({
        h5(
          paste(
            "Please click Submit only once per submission. ",
            "A message will appear to confirm the RSVP is recorded; ",
            "don't close the page until you see this message."
          )
        )
      })
      output$PASSWORD_ERROR = renderUI({
        NULL
      })
    } else{
      output$INTRO = renderUI({
        NULL
      })
      output$BUTTONS = renderUI({
        NULL
      })
      output$SUBMIT_RSVP = renderUI({
        NULL
      })
      output$PASSWORD_ERROR = renderUI({
        h5(
          strong(
            paste(
              "Your entry doesn't match a password given to any household :( ",
              "Please try again, and remember that these passwords are case sensitive.",
              "If there are continued issues, get in touch with Aaron or Steph."
            )
          ),
          style = "color:red"
        )
      })
    }
  })
  # Response to submission
  observeEvent(input$SUBMIT, {
    missing = sum(
      unlist(
        lapply(input$RSVP, function(name){
          is.null(name)
        })
      )
    )
    if(missing > 0){
      output$SUBMISSION = renderUI({
        h5(
          strong(
            paste(
              "RSVP was not made for at least one guest in the household;",
              "please ensure all guests are accounted for and resubmit."
            ) 
          ),
          style = "color:red"
        )
      })  
    } else{
      for(name in names(input$RSVP)){
        sheet_range = paste0(
          c(ATTENDING_COL, VEGETARIAN_COL),
          which(rsvp$Guest == name)+1
        ) %>%
          paste(collapse=":")
        if(str_detect(input$RSVP[[name]], "^YES")){
          is_attending = "YES"
        } else{
          is_attending = "NO"
        }
        if(str_detect(input$RSVP[[name]], "V$")){
          is_vegetarian = "YES"
        } else{
          is_vegetarian = "NO"
        }
        new_data = data.frame(
          Attending = is_attending,
          Vegetarian = is_vegetarian
        )
        googlesheets4::range_write(
          ss = SHEET_ID,
          data = new_data,
          sheet = SHEET_NAME,
          range = sheet_range,
          col_names = FALSE,
          reformat = FALSE
        )
      }
      output$SUBMISSION = renderUI({
        h4(
          paste(
            "Thanks! Your RSVP has been recorded. ",
            "If anything changes, please re-submit your RSVP as soon as you know.",
            "Hopefully we'll be celebrating with you in September, but if not, you'll be missed!"
          ),
          style = "color:green"
        )
      })
    }
  })
}

# Run ==========================================================================

shinyApp(ui = ui, server = server)


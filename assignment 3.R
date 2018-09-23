best <- function(state, outcome) {

  outcome_range <- c("heart attack", "heart failure", "pneumonia")
  input <- read.csv("outcome-of-care-measures.csv")

  try(if (!(state %in% input$State)) stop("invalid state"))
  try(if (!(outcome %in% outcome_range)) stop("invalid outcome"))

  if(outcome == "heart attack") {
    dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    dat <- dat[which(dat$x == state,),]
    cleandat <- data.frame()
    cleandat <- dat[which(dat[, "y"] != "Not Available"),]
    print(cleandat)
  }

} #end best()

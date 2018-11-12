best <- function(state, outcome) {

  outcome_range <- c("heart attack", "heart failure", "pneumonia")
  input <- read.csv("outcome-of-care-measures.csv")

  try(if (!(state %in% input$State)) stop("invalid state"))
  try(if (!(outcome %in% outcome_range)) stop("invalid outcome"))

  if(outcome == "heart attack") {
    dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, z = input$Hospital.Name)
    dat <- dat[which(dat$x == state,),]
    cleandat <- data.frame()
    cleandat <- dat[which(dat[, "y"] != "Not Available"),]
    cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

    lowest_mortality_rate <- min(cleandat[, "y"])

    hospital_name <- cleandat[cleandat$y == lowest_mortality_rate, "z"]
    print(hospital_name)
  }

  else if(outcome == "heart failure") {
      dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, z = input$Hospital.Name)
      dat <- dat[which(dat$x == state,),]
      cleandat <- data.frame()
      cleandat <- dat[which(dat[, "y"] != "Not Available"),]

      cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

      lowest_mortality_rate <- min(cleandat[, "y"])

      hospital_name <- cleandat[cleandat$y == lowest_mortality_rate, "z"]
      print(hospital_name)

    }

  else if(outcome == "pneumonia") {
      dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, z = input$Hospital.Name)
      dat <- dat[which(dat$x == state,),]
      cleandat <- data.frame()
      cleandat <- dat[which(dat[, "y"] != "Not Available"),]

      cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

      lowest_mortality_rate <- min(cleandat[, "y"])

      hospital_name <- cleandat[cleandat$y == lowest_mortality_rate, "z"]
      print(hospital_name)

    }

} #end best()

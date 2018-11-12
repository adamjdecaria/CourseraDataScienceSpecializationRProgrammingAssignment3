best <- function(state, outcome) {
  outcome.range <- c("heart attack", "heart failure", "pneumonia")
  input <- read.csv("outcome-of-care-measures.csv")

  try (if (!(state %in% input$State)) stop("invalid state"))
  try (if (!(outcome %in% outcome.range)) stop("invalid outcome"))

  if (outcome == "heart attack") {
    dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, z = input$Hospital.Name)
    dat <- dat[which(dat$x == state,), ]
    cleandat <- data.frame()
    cleandat <- dat[which(dat[, "y"] != "Not Available"), ]
    cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

    lowest.mortality.rate <- min(cleandat[, "y"])

    hospital.name <- cleandat[cleandat$y == lowest.mortality.rate, "z"]
    print(hospital.name)
  } else if(outcome == "heart failure") {
      dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, z = input$Hospital.Name)
      dat <- dat[which(dat$x == state, ), ]
      cleandat <- data.frame()
      cleandat <- dat[which(dat[, "y"] != "Not Available"), ]

      cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

      lowest.mortality.rate <- min(cleandat[, "y"])

      hospital.name <- cleandat[cleandat$y == lowest.mortality.rate, "z"]
      print(hospital.name)

    } else if(outcome == "pneumonia") {
      dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, z = input$Hospital.Name)
      dat <- dat[which(dat$x == state,), ]
      cleandat <- data.frame()
      cleandat <- dat[which(dat[, "y"] != "Not Available"), ]

      cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

      lowest.mortality.rate <- min(cleandat[, "y"])

      hospital.name <- cleandat[cleandat$y == lowest.mortality.rate, "z"]
      print(hospital.name)
    }
} #end best()

rankhospital <- function(state, outcome, num = "best") {
  outcome.range <- c("heart attack", "heart failure", "pneumonia")
  input <- read.csv("outcome-of-care-measures.csv")

  try (if (!(state %in% input$State)) stop("invalid state"))
  try (if (!(outcome %in% outcome.range)) stop("invalid outcome"))

  if (outcome == "heart attack") {
    dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, z = input$Hospital.Name)
    dat <- dat[which(dat$x == state,), ]
    cleandat <- data.frame()
    cleandat <- dat[which(dat[, "y"] != "Not Available"), ]
    cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

    sorted.data <- data.frame()
    sorted.data <- order(cleandat$y, cleandat$z)
    sorted.data <- cleandat[sorted.data, ]

    if (num == "best") {
      print(sorted.data$z[1])
    }  else {
      print(sorted.data$z[num])
    }
  } else if (outcome == "heart failure") {
    dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, z = input$Hospital.Name)
    dat <- dat[which(dat$x == state,), ]
    cleandat <- data.frame()
    cleandat <- dat[which(dat[, "y"] != "Not Available"), ]
    cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

    sorted.data <- data.frame()
    sorted.data <- order(cleandat$y, cleandat$z)
    sorted.data <- cleandat[sorted.data, ]

    print(sorted.data)

    if (num == "best") {
      print(sorted.data$z[1])
    }  else {
      print(sorted.data$z[num])
    }
  } else if (outcome == "pneumonia") {
    dat <- data.frame(x = input$State, y = input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, z = input$Hospital.Name)
    dat <- dat[which(dat$x == state,), ]
    cleandat <- data.frame()
    cleandat <- dat[which(dat[, "y"] != "Not Available"), ]
    cleandat[, "y"] <- as.numeric(as.character(cleandat[, "y"]))

    sorted.data <- data.frame()
    sorted.data <- order(cleandat$y, cleandat$z)
    sorted.data <- cleandat[sorted.data, ]

    if (num == "best") {
      print(sorted.data$z[1])
    }  else {
      print(sorted.data$z[num])
    }
  }
} # end rankhospital()

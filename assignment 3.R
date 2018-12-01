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
    }  else if (num == "worst"){
        print(sorted.data$z[length(sorted.data$z)])
    } else {
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

    if (num == "best") {
      print(sorted.data$z[1])
    } else if (num == "worst"){
        print(sorted.data$z[length(sorted.data$z)])
    } else {
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
    } else if (num == "worst"){
        print(sorted.data$z[length(sorted.data$z)])
    } else {
      print(sorted.data$z[num])
    }
  }
} # end rankhospital()

rankall <- function(outcome, num) {
  outcome.range <- c("heart attack", "heart failure", "pneumonia")
  input <- read.csv("outcome-of-care-measures.csv")

  try (if (!(outcome %in% outcome.range)) stop("invalid outcome"))

  if (outcome == "pneumonia") {
    dat <- data.frame("state" = input$State, "rate" = input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, "hospital" = input$Hospital.Name)

    cleandat <- data.frame()
    cleandat <- dat[which(dat[, "rate"] != "Not Available"), ]
    cleandat[, "rate"] <- as.numeric(as.character(cleandat[,"rate"]))

    sorted.data <- data.frame()
    sorted.data <- order(cleandat$rate, cleandat$hospital)
    sorted.data <- cleandat[sorted.data, ]

    split.sorted.data <- split(sorted.data, f = sorted.data$state)
    print(split.sorted.data)
  }

} # end rankall()

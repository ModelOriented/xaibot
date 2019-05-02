library("plumber")
library("DALEX")
library("ingredients")
library("iBreakDown")
library("randomForest")

get_observation <- function(class = "X", gender = "X", age = "X", sibsp = "X", parch = "X", fare = "X",
                            embarked = "X") {
  class_ok <- c("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")
  gender_ok <- c("female", "male")
  embarked_ok <- c("Belfast", "Cherbourg", "Queenstown", "Southampton")

  new_passanger <- data.frame(
    class = factor("1st", levels = class_ok),
    gender = factor("male", levels = gender_ok),
    age = 8,
    sibsp = 0,
    parch = 0,
    fare = 72,
    embarked = factor("Southampton", levels = embarked_ok)
  )
  subtitle = ""
  if (class != "X" & class %in% class_ok) {
    new_passanger$class <- factor(class, levels = class_ok)
    subtitle <- paste(subtitle, "  class:", class)
  }
  if (gender != "X" & gender %in% gender_ok) {
    new_passanger$gender <- factor(gender, levels = gender_ok)
    subtitle <- paste(subtitle, "  gender:", gender)
  }
  if (age != "X") {
    new_passanger$age <- as.numeric(as.character(age))
    subtitle <- paste(subtitle, "  age:", age)
  }
  if (sibsp != "X") {
    new_passanger$sibsp <- as.numeric(as.character(sibsp))
    subtitle <- paste(subtitle, "  sibsp:", sibsp)
  }
  if (parch != "X") {
    new_passanger$parch <- as.numeric(as.character(parch))
    subtitle <- paste(subtitle, "  parch:", parch)
  }
  if (fare != "X") {
    new_passanger$fare <- as.numeric(as.character(fare))
    subtitle <- paste(subtitle, "  fare:", fare)
  }
  if (embarked != "X" & embarked %in% embarked_ok) {
    new_passanger$embarked <- factor(embarked, levels = embarked_ok)
    subtitle <- paste(subtitle, "  embarked:", embarked)
  }

  list(new_passanger = new_passanger,
       subtitle = subtitle)
}

#* @apiTitle API for Titanic Survival Model. Use either predict / break_down / ceteris_paribus hooks

#* Predict survival of passenger from Titanic

#* @param class passenger class. X if missing. One of ("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")
#* @param gender passenger gender. X if missing. One of ("female", "male")
#* @param age passenger age. X if missing
#* @param sibsp passenger sibsp. X if missing
#* @param parch passenger parch. X if missing
#* @param fare passenger fare. X if missing
#* @param embarked passenger embarked. X if missing. One of ("Belfast", "Cherbourg", "Queenstown", "Southampton")
#* @get /predict
#* @post /predict
function(req, class = "X", gender = "X", age = "X", sibsp = "X", parch = "X", fare = "X",
  embarked = "X") {

  tmp <- get_observation(class, gender, age, sibsp, parch, fare, embarked)
  new_passanger <- tmp$new_passanger

  load("explain_titanic_rf.rda")
  pr <- predict(explain_titanic_rf, new_passanger)

  list(
    result_text = paste("Predicted survival:", pr),
    result = pr,
    raw_body = req$postBody
  )
}



#* Plot break down survival of passenger from Titanic

#* @param class passenger class. X if missing. One of ("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")
#* @param gender passenger gender. X if missing. One of ("female", "male")
#* @param age passenger age. X if missing
#* @param sibsp passenger sibsp. X if missing
#* @param parch passenger parch. X if missing
#* @param fare passenger fare. X if missing
#* @param embarked passenger embarked. X if missing. One of ("Belfast", "Cherbourg", "Queenstown", "Southampton")
#* @get /break_down
#* @post /break_down
#* @png (width = 420, height = 250)
function(req, class = "X", gender = "X", age = "X", sibsp = "X", parch = "X", fare = "X",
  embarked = "X") {

  tmp <- get_observation(class, gender, age, sibsp, parch, fare, embarked)
  new_passanger <- tmp$new_passanger
  subtitle <- tmp$subtitle

  load("explain_titanic_rf.rda")

  order <- c("class", "age", "gender", "fare", "sibsp", "parch", "embarked")
  order <- intersect(order, c(strsplit(subtitle, split = "[^A-Za-z0-9]")[[1]]))
  if (length(order) < 1)  order <- "age"
  sp_rf <- break_down(explain_titanic_rf, new_passanger,
                      order = order)
  sp_rf[nrow(sp_rf) - 1,"contribution"] = sp_rf[nrow(sp_rf) - 1,"contribution"] + sp_rf[nrow(sp_rf),"cummulative"] - sp_rf[nrow(sp_rf) - 1,"cummulative"]
  sp_rf[nrow(sp_rf) - 1,"cummulative"] = sp_rf[nrow(sp_rf),"cummulative"]
  pr <- predict(explain_titanic_rf, new_passanger)
  title = paste0("Your chances of survival are ", round(pr, 3))
  subtitle = "And here is how I get it"
  print(plot(sp_rf) + ingredients::theme_drwhy() + facet_null() +
          theme(legend.position = "none") + 
          ggtitle(title, subtitle))
}


#* Plot ceteris paribus down survival of passenger from Titanic

#* @param variable variable to be explained
#* @param class passenger class. X if missing. One of ("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")
#* @param gender passenger gender. X if missing. One of ("female", "male")
#* @param age passenger age. X if missing
#* @param sibsp passenger sibsp. X if missing
#* @param parch passenger parch. X if missing
#* @param fare passenger fare. X if missing
#* @param embarked passenger embarked. X if missing. One of ("Belfast", "Cherbourg", "Queenstown", "Southampton")
#* @get /ceteris_paribus
#* @post /ceteris_paribus
#* @png (width = 420, height = 250)
function(req, variable = "age", class = "X", gender = "X", age = "X", sibsp = "X", parch = "X", fare = "X",
  embarked = "X") {

  tmp <- get_observation(class, gender, age, sibsp, parch, fare, embarked)
  new_passanger <- tmp$new_passanger
  subtitle <- tmp$subtitle

  load("explain_titanic_rf.rda")

  if (!(variable %in% c("age", "sibsp", "parch", "fare", "class", "gender", "embarked"))) {
    variable = "age"
  }

  if (variable %in% c("age", "sibsp", "parch", "fare")) {
    pr <- predict(explain_titanic_rf, new_passanger)
    title = paste0("Your chances of survival for ", variable, " = ", new_passanger[[variable]], " are ", round(pr, 3))
    subtitle = paste0("But keeping everything else constant \nchances for different values of ", variable, " are...")
    grids = list()
    grids[[variable]] = sort(unique(explain_titanic_rf$data[,variable]))
    cp_titanic_rf <- ceteris_paribus(explain_titanic_rf, new_passanger,
                                     variables = variable, variable_splits = grids)
    pl <- plot(cp_titanic_rf) +
            show_observations(cp_titanic_rf, variables = variable, size = 5) +
          ylab(paste0("Survival after change in ", variable)) + facet_null() +
          xlab(variable) +
      ggtitle(title, subtitle) + 
      theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 12))
  }
  if (variable %in% c("class", "gender", "embarked")) {
    pr <- predict(explain_titanic_rf, new_passanger)
    title = paste0("Your chances of survival for ", variable, " = ", new_passanger[[variable]], " are ", round(pr, 3))
    subtitle = paste0("But keeping everything else constant \nchances for different values of ", variable, " are...")
    grids = list()
    grids[[variable]] = sort(unique(explain_titanic_rf$data[,variable]))
    cp_titanic_rf <- ceteris_paribus(explain_titanic_rf, new_passanger,
                                     variables = variable, variable_splits = grids)
    pl <- plot(cp_titanic_rf, only_numerical = FALSE) +
            ylab(paste0("Survival after change in ", variable)) + facet_null() +
            xlab(variable) +
            ggtitle(title, subtitle) + 
            theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 12))
  }

  print(pl)
}


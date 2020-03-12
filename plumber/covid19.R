library("plumber")
library("DALEX")
library("ingredients")
library("iBreakDown")
library("randomForest")

get_observation <- function(country = "X", gender = "X", age = "X") {
  country_ok <- c("China", "Other")
  gender_ok <- c("female", "male")

  new_passanger <- data.frame(
    class = factor("Other", levels = country_ok),
    gender = factor("male", levels = gender_ok),
    age = 50
  )
  subtitle = ""
  if (country != "X" & country %in% class_ok) {
    new_passanger$country <- factor(country, levels = country_ok)
    subtitle <- paste(subtitle, "  country:", country)
  }
  if (gender != "X" & gender %in% gender_ok) {
    new_passanger$gender <- factor(gender, levels = gender_ok)
    subtitle <- paste(subtitle, "  gender:", gender)
  }
  if (age != "X") {
    new_passanger$age <- as.numeric(as.character(age))
    subtitle <- paste(subtitle, "  age:", age)
  }

  list(new_passanger = new_passanger,
       subtitle = subtitle)
}

#* @apiTitle API for COVID Survival Model. Use either predict / break_down / ceteris_paribus hooks

#* Predict survival of COVID19 infected persons

#* @param country country of COVID19 infected person. X if missing. One of ("China", "Other")
#* @param gender gender of COVID19 infected person. X if missing. One of ("female", "male")
#* @param age age of COVID19 infected person. X if missing
#* @get /predict
#* @post /predict
function(req, country = "X", gender = "X", age = "X") {

  tmp <- get_observation(country, gender, age)
  new_case <- tmp$new_passanger

  load("sumodelcovid.rda")
  pr <- predict(sumodelcovid, new_case)

  list(
    result_text = paste("Predicted survival:", pr),
    result = pr,
    raw_body = req$postBody
  )
}


#* Plot break down survival of COVID19 infected persons

#* @param country country of COVID19 infected person. X if missing. One of ("China", "Other")
#* @param gender gender of COVID19 infected person. X if missing. One of ("female", "male")
#* @param age age of COVID19 infected person. X if missing
#* @get /break_down
#* @post /break_down
#* @png (width = 420, height = 250)
function(req, country = "X", gender = "X", age = "X") {

  tmp <- get_observation(country, gender, age)
  new_case <- tmp$new_passanger
  subtitle <- tmp$subtitle

  load("sumodelcovid.rda")
  
  order <- c("class", "age", "gender", "fare", "sibsp", "parch", "embarked")
  order <- intersect(order, c(strsplit(subtitle, split = "[^A-Za-z0-9]")[[1]]))
  if (length(order) < 1)  order <- "age"
  sp_rf <- break_down(sumodelcovid, new_case,
                      order = order)
  sp_rf[nrow(sp_rf) - 1,"contribution"] = sp_rf[nrow(sp_rf) - 1,"contribution"] + sp_rf[nrow(sp_rf),"cummulative"] - sp_rf[nrow(sp_rf) - 1,"cummulative"]
  sp_rf[nrow(sp_rf) - 1,"cummulative"] = sp_rf[nrow(sp_rf),"cummulative"]
  pr <- predict(sumodelcovid, new_case)
  title = paste0("Your chances of survival are ", round(pr, 3))
  subtitle = "And here is how I get it"
  print(plot(sp_rf) + ingredients::theme_drwhy() + facet_null() +
          theme(legend.position = "none") + 
          ggtitle(title, subtitle))
}


#* Plot ceteris paribus down survival of COVID19 infected persons

#* @param variable variable to be explained
#* @param country country of COVID19 infected person. X if missing. One of ("China", "Other")
#* @param gender gender of COVID19 infected person. X if missing. One of ("female", "male")
#* @param age age of COVID19 infected person. X if missing
#* @get /ceteris_paribus
#* @post /ceteris_paribus
#* @png (width = 420, height = 250)
function(req, variable = "age", country = "X", gender = "X", age = "X") {
  
  tmp <- get_observation(country, gender, age)
  new_case <- tmp$new_passanger
  subtitle <- tmp$subtitle

  load("sumodelcovid.rda")
  
  if (!(variable %in% c("age", "country", "gender"))) {
    variable = "age"
  }

  if (variable %in% c("age")) {
    pr <- predict(sumodelcovid, new_case)
    title = paste0("Your chances of survival for ", variable, " = ", new_passanger[[variable]], " are ", round(pr, 3))
    subtitle = paste0("But keeping everything else constant \nchances for different values of ", variable, " are...")
    grids = list()
    grids[[variable]] = sort(unique(sumodelcovid$data[,variable]))
    cp_titanic_rf <- ceteris_paribus(sumodelcovid, new_case,
                                     variables = variable, variable_splits = grids)
    pl <- plot(cp_titanic_rf) +
            show_observations(cp_titanic_rf, variables = variable, size = 5) +
          ylab(paste0("Survival after change in ", variable)) + facet_null() +
          xlab(variable) +
      ggtitle(title, subtitle) + 
      theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 12))
  }
  if (variable %in% c("country", "gender")) {
    pr <- predict(sumodelcovid, new_case)
    title = paste0("Your chances of survival for ", variable, " = ", new_case[[variable]], " are ", round(pr, 3))
    subtitle = paste0("But keeping everything else constant \nchances for different values of ", variable, " are...")
    grids = list()
    grids[[variable]] = sort(unique(sumodelcovid$data[,variable]))
    cp_titanic_rf <- ceteris_paribus(sumodelcovid, new_case,
                                     variables = variable, variable_splits = grids)
    pl <- plot(cp_titanic_rf, only_numerical = FALSE) +
            ylab(paste0("Survival after change in ", variable)) + facet_null() +
            xlab(variable) +
            ggtitle(title, subtitle) + 
            theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 12))
  }

  print(pl)
}


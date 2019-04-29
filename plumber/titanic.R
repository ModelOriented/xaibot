library("plumber")
library("DALEX")
library("randomForest")

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
#* @post /Predict
function(req, class = "X", gender = "X", age = "X", sibsp = "X", parch = "X", fare = "X",
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
  if (class != "X" & class %in% class_ok) new_passanger$class <- class
  if (gender != "X" & gender %in% gender_ok) new_passanger$gender <- gender
  if (age != "X") new_passanger$age <- age
  if (sibsp != "X") new_passanger$sibsp <- sibsp
  if (parch != "X") new_passanger$parch <- parch
  if (fare != "X") new_passanger$fare <- fare
  if (embarked != "X" & embarked %in% embarked_ok) new_passanger$embarked <- embarked

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
#* @png
function(req, class = "X", gender = "X", age = "X", sibsp = "X", parch = "X", fare = "X",
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
  if (class != "X" & class %in% class_ok) new_passanger$class <- class
  if (gender != "X" & gender %in% gender_ok) new_passanger$gender <- gender
  if (age != "X") new_passanger$age <- age
  if (sibsp != "X") new_passanger$sibsp <- sibsp
  if (parch != "X") new_passanger$parch <- parch
  if (fare != "X") new_passanger$fare <- fare
  if (embarked != "X" & embarked %in% embarked_ok) new_passanger$embarked <- embarked

  load("explain_titanic_rf.rda")

  sp_rf <- single_prediction(explain_titanic_rf, new_passanger)
  print(plot(sp_rf))
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
#* @png
function(req, variable = "age", class = "X", gender = "X", age = "X", sibsp = "X", parch = "X", fare = "X",
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
  if (class != "X" & class %in% class_ok) new_passanger$class <- class
  if (gender != "X" & gender %in% gender_ok) new_passanger$gender <- gender
  if (age != "X") new_passanger$age <- age
  if (sibsp != "X") new_passanger$sibsp <- sibsp
  if (parch != "X") new_passanger$parch <- parch
  if (fare != "X") new_passanger$fare <- fare
  if (embarked != "X" & embarked %in% embarked_ok) new_passanger$embarked <- embarked

  load("explain_titanic_rf.rda")

  vr_embarked  <- variable_response(explain_titanic_rf, variable =  variable)
  print(plot(vr_embarked))
}

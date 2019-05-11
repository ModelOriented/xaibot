# xaibot-titanic
XAI chatbot explaining your chance of survival on Titanic.
Bot will be released soon.

## Sources
Chatbot is developed in the platform Dialogflow.
Both NLU training files and code for the dialogue management might be found [here](bot).
The Machine Learning model is accessed via `plumber` API described below.

## start plumber

You need to have files `titanic.R` and `explain_titanic_rf.rda` in the working folder.

Then in order to execute the `plumber` api you need to call in the screen.

```
library("plumber")
pmodel <- plumb("titanic.R")
pmodel$run(host = "0.0.0.0", port = 8787)
```

## Try it out

Find a demo here: http://52.31.27.158:8787/__swagger__/

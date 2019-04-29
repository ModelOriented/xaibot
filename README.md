# xaibot-titanic

XAI chat bot for Titanic model - created with plumber

## start plumber

You need to have files `titanic.R` and `explain_titanic_rf.rda` in the working folder.

Then in order to execute the `plumber` api you need to call in the screen.

```
library("plumber")
pmodel <- plumb("titanic.R")
pmodel$run(host = "0.0.0.0", port = 8787)
```


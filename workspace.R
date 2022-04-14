library(tercen)
library(dplyr, warn.conflicts = FALSE)

options("tercen.workflowId" = "1c421ac85832bc0e153d047644b2304c")
options("tercen.stepId"     = "c0993539-c54d-408a-839e-640024fd4ca7")

getOption("tercen.workflowId")
getOption("tercen.stepId")

ctx = tercenCtx()

ctx %>%
  select(.y, .ci, .ri) %>%
  group_by(.ci, .ri) %>%
  summarise(mean = mean(.y)) %>%
  ctx$addNamespace() %>%
  ctx$save()
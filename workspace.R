library(tercen)
library(dplyr, warn.conflicts = FALSE)

options("tercen.serviceUri"= "https://tercen.com/api/v1/")
options("tercen.username"= "agouy")
options("tercen.password"= "4/g0tercen")

options("tercen.workflowId" = "1c421ac85832bc0e153d047644b2304c")
options("tercen.stepId"     = "a24610ab-08ff-45b8-bb0c-d7b59207d04e")

getOption("tercen.workflowId")
getOption("tercen.stepId")

ctx = tercenCtx()

ctx %>%
  select(.y, .ci, .ri) %>%
  group_by(.ci, .ri) %>%
  summarise(mean = mean(.y)) %>%
  ctx$addNamespace() %>%
  ctx$save()
library(tercen)
library(dplyr)
library(openCyto)
library(data.table)
library(flowWorkspace)
library(ncdfFlow)
library(ggcyto)

ctx <- tercenCtx()

dims <- paste0(ctx$rselect()[[1]], collapse = ",")

# Parameters
pop                   <- ctx$op.value('pop', as.character, '+')
gating_method         <- ctx$op.value('gating_method', as.character, 'singletGate')
gating_args           <- ctx$op.value('gating_args', as.character, '')
collapseDataForGating <- ctx$op.value('collapseDataForGating', as.character, '')
groupBy               <- ctx$op.value('groupBy', as.character, '')
preprocessing_method  <- ctx$op.value('preprocessing_method', as.character, '')
preprocessing_args    <- ctx$op.value('preprocessing_args', as.character, '')

data <- ctx %>% 
  as.matrix() %>%
  t()

dim(data)

colnames(data) <- ctx$rselect()[[1]]

data <- cbind(data, .ci =seq_len(nrow(data)) - 1)

flow.dat <- flowCore::flowFrame(as.matrix(data))
flow.set <- flowCore::flowSet(flow.dat)

gs <- GatingSet(flow.set)

gs_add_gating_method(
  gs,
  alias = 'gating_step',
  pop = pop,
  parent = "root",
  dims = dims,
  gating_method = gating_method,
  gating_args = gating_args,
  collapseDataForGating = collapseDataForGating,
  preprocessing_method = preprocessing_method,
  preprocessing_args = preprocessing_args
)

data_get <- gh_pop_get_data(gs, alias)

filter_data <- data[, ".ci"] %in% exprs(data_get)[, ".ci"]

df_out <- tibble(
  flag = ifelse(filter_data, "pass", "fail"),
  .ci = as.integer(data[, ".ci"])
) %>%
  ctx$addNamespace() %>%
  ctx$save()

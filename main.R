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

alias <- 'gating_step'

data <- ctx %>% 
  as.matrix() %>%
  t()

colnames(data) <- ctx$rselect()[[1]]

data <- cbind(data, .ci = seq_len(nrow(data)) - 1)

flow.dat <- flowCore::flowFrame(as.matrix(data))
flow.set <- flowCore::flowSet(flow.dat)

gs <- GatingSet(flow.set)

gs_add_gating_method(
  gs,
  alias = alias,
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
?gh_pop_get_data
filter_data <- data[, ".ci"] %in% exprs(data_get)[, ".ci"]

df_out1 <- tibble(
  flag = ifelse(filter_data, "pass", "fail"),
  .ci = as.integer(data[, ".ci"])
) %>%
  ctx$addNamespace()

gate_info <- gh_pop_get_gate(gs, alias)
if(class(gate_info) == "rectangleGate") {
  df_out2 <- tibble(
    min = gate_info@min,
    max = gate_info@max,
    .ri = seq_len(ncol(data) - 1) - 1
  ) %>%
    ctx$addNamespace()
  ctx$save(list(df_out1, df_out2))
} else {
  ctx$save(df_out1)
}


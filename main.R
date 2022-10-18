suppressPackageStartupMessages({
  library(tercen)
  library(dplyr)
  library(openCyto)
  library(data.table)
  library(flowWorkspace)
  library(ncdfFlow)
  library(ggcyto)
  library(flowStats)
})
tim::set_workflow_step_ids("https://tercen.com/tercen/w/af62f6ddd40f1682214d21ae900c18c5/ds/e40b0c62-e8a5-4737-874c-42b39cf1d9bd")
ctx <- tercenCtx()

rnames <- ctx$rselect()[[1]]
dims <- paste0(rnames, collapse = ",")

# Parameters
pop                   <- ctx$op.value('pop', as.character, '+')
gating_method         <- ctx$op.value('gating_method', as.character, 'singletGate')
gating_args           <- ctx$op.value('gating_args', as.character, '')

alias <- 'gating_step'

data <- ctx %>% 
  as.matrix() %>%
  t()

colnames(data) <- rnames

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
  gating_args = gating_args
)

## plot gating results
if(length(rnames) == 1) {
  p <- ggcyto(gs, aes_string(x = rnames[1]), subset = alias)
  p <- p +
    geom_density() +
    geom_gate(alias) +
    geom_stats() +
    theme_minimal()
} else {
  p <- autoplot(gs, gate = alias, bins = 100) + theme_minimal()
}

fname <- tim::save_plot(
  p,
  type = "png",
  width = 750,
  height = 750,
  units = "px",
  dpi = 144,
  device = "png"
) 

plts <- tibble(filename = fname) %>%
  mutate(.ri = 0)

df_plot <- tim::plot_file_to_df(plts$filename, filename = "Gating_step.png") %>% 
  bind_cols(plts %>% select(.ri)) %>%
  ctx$addNamespace() # %>%

data_get <- gh_pop_get_data(gs, alias)
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
  ctx$save(list(df_out1, df_out2, df_plot))
} else {
  ctx$save(list(df_out1, df_plot))
}

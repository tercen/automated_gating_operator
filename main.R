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

ctx <- tercenCtx()

# Parameters
method <- ctx$op.value('gating_method', as.character, '2D - Ellipsoid gate')
gate_name <- ctx$op.value('gate_name', as.character, 'Singlets')
stringency <- 1 - ctx$op.value('stringency', as.numeric, 0.05)
plot.width <- ctx$op.value('plot.width', as.numeric, 750)
plot.height <- ctx$op.value('plot.height', as.numeric, 750)

gating_method <- switch(
  method,
  "1D - Tail gate" = "quantile",
  "1D - Largest peak" = "mindensity",
  "2D - Singlet gate" = "singletGate",
  "2D - Ellipsoid gate" = "flowClust.2d",
  "2D - Quadrant gate" = "gate_quad_tmix"
)

data <- ctx$as.matrix() %>% t()
channels <- ctx$rselect()[[1]]
colnames(data) <- channels

data <- data %>% 
  as_tibble() %>%
  mutate(.ci = 1:nrow(.) - 1L)

files <- ctx$cselect() %>% 
  select(contains(c("filename","Barcodes")))

flow.frames <- data %>%
  bind_cols(files) %>%
  group_by(across(select(contains(c("filename","Barcodes"))))) %>% 
  group_map(~tim::matrix_to_flowFrame(as.matrix(.x))) 

names(flow.frames) <- levels(as.factor(files[[1]]))

flow.set <- flow.frames %>%
  flowCore::flowSet()

if(method == "1D - Largest peak") {
  global_max <- max(fsApply(flow.set, function(x) range(x, type = "data")[2, 1]))
}

gating_args <- switch(
  method,
  "1D - Tail gate" = paste0("probs=", stringency),
  "1D - Largest peak" = paste0("max=", global_max * stringency),
  "2D - Singlet gate" = paste0("prediction_level=", stringency),
  "2D - Ellipsoid gate" = paste0("quantile=", stringency),
  "2D - Quadrant gate" = paste0("quantile1=", stringency, "quantile3=", stringency)
)

gs <- GatingSet(flow.set)

gs_add_gating_method(
  gs,
  alias = gate_name,
  parent = "root",
  dims = paste0(channels, collapse = ","),
  gating_method = gating_method,
  gating_args = gating_args
)

## plot gating results
if(length(channels) == 1) {
  p <- ggcyto(gs, aes(x = !!sym(channels[1]))) +
    geom_density()
} else {
  p <- ggcyto(gs, aes(x = !!sym(channels[1]), y = !!sym(channels[2]))) +
    geom_hex(bins = 100)
}

p <- p +
  geom_gate(gate_name) +
  geom_stats(fill = alpha(c("steelblue"), 0.2)) +
  theme_minimal() + 
  labs_cyto("marker") +
  ggcyto_par_set(limits = "data") +
  labs(title = NULL)

fname <- tim::save_plot(
  p,
  type = "png",
  width = plot.width,
  height = plot.height,
  units = "px",
  dpi = 144,
  device = "png"
)

plts <- tibble(filename = fname) %>%
  mutate(.ri = 0)

df_plot <- tim::plot_file_to_df(plts$filename, filename = "Gating_step.png") %>% 
  bind_cols(plts %>% select(.ri)) %>%
  ctx$addNamespace() # %>%

filter_data <- lapply(seq_len(length(gs)), function(x) {
  dat <- gh_pop_get_data(gs[[x]], gate_name)
  exprs(flow.set[[x]])[, ".ci"] %in% exprs(dat)[, ".ci"]
}) %>%
  unlist(use.names = FALSE)

df_out1 <- tibble(
  !!gate_name := ifelse(filter_data, 1, 0),
  .ci = as.integer(data[[".ci"]])
) %>%
  ctx$addNamespace()

ctx$save(list(df_out1, df_plot))


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
gate_name <- ctx$op.value('gate_name', as.character, 'Gate')
stringency <- 1 - ctx$op.value('stringency', as.numeric, 0.05)
K <- 4
plot.width <- ctx$op.value('plot.width', as.numeric, 750)
plot.height <- ctx$op.value('plot.height', as.numeric, 750)
seed <- ctx$op.value('seed', as.numeric, 42)
if(seed > 0) set.seed(seed)

gating_method <- switch(
  method,
  "1D - Tail gate" = "quantileGate",
  "1D - Largest peak" = "mindensity",
  "2D - Singlet gate" = "singletGate",
  "2D - Ellipsoid gate" = "flowClust.2d",
  "2D - Quadrant gate" = "quadGate.tmix"
)

if(length(ctx$labels) == 0) stop("A lebel containing Event IDs should be specified.")

if(ctx$hasNumericXAxis) {
  data <- ctx$select(c(".y", ".x", ".ci", ctx$labels[[1]])) %>%
    rename(.ev_id = ctx$labels[[1]])
  channels <- c(ctx$yAxis[[1]], ctx$xAxis[[1]], ".ci", ".ev_id")
} else {
  data <- ctx$select(c(".y", ".ci", ctx$labels[[1]])) %>%
    rename(.ev_id = ctx$labels[[1]])
  channels <- c(ctx$yAxis[[1]], ".ci", ".ev_id")
}

colnames(data) <- channels
data <- data %>% as_tibble()

files <- ctx$cselect() %>% 
  select(contains(c("filename", "Barcodes"))) %>%
  mutate(.ci = 1:nrow(.) - 1L)

if(ncol(files) == 1) {
  files <- files %>%
    mutate(filename = "File") %>%
    relocate(filename)
}

flow.frames <- data %>%
  left_join(files, by = ".ci") %>% 
  group_by(across(contains(c("filename", "Barcodes")))) %>% 
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
  "2D - Quadrant gate" = paste0("quantile1=", stringency, ", quantile3=", stringency, ", K=", 4, ", usePrior = 'no'")
)

gs <- GatingSet(flow.set)

chans <- channels[!channels %in% c(".ci", ".ev_id")]

gs_add_gating_method(
  gs,
  alias = gate_name,
  pop = ifelse(method == "2D - Quadrant gate", "+/-+/-", "+"),
  parent = "root",
  dims = paste0(chans, collapse = ","),
  gating_method = gating_method,
  gating_args = gating_args
)

## plot gating results
if(length(chans) == 1) {
  p <- ggcyto(gs, aes(x = !!sym(channels[1]))) +
    geom_density()
} else {
  p <- ggcyto(gs, aes(x = !!sym(channels[1]), y = !!sym(channels[2]))) +
    geom_hex(bins = 100)
}

gates <- gh_pop_get_descendants(gs[[1]], "root")
for(gate in gates) {
  p <- p + geom_gate(gate)
}

p <- p +
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
  mutate(.ri = 0L)

df_plot <- tim::plot_file_to_df(plts$filename, filename = "Gating_step.png") %>% 
  bind_cols(plts %>% select(.ri)) %>%
  ctx$addNamespace() # %>%

filter_data <- list()
for(gate in gates) {
  filter_data[[gate]] <- lapply(seq_len(length(gs)), function(x) {
    dat <- gh_pop_get_data(gs[[x]], gate)
    exprs(flow.set[[x]])[, ".ev_id"] %in% exprs(dat)[, ".ev_id"]
  }) %>%
    unlist(use.names = FALSE) %>%
    as.numeric()
}

df_out1 <- as_tibble(filter_data) %>% 
  mutate(.ev_id = as.integer(data[[".ev_id"]])) %>%
  ctx$addNamespace()

df_out1 <- df_out1 %>%
  as_relation() %>%
  as_join_operator(ctx$labels, list(".ev_id"))

df_out2 <-  df_plot %>%
  as_relation() %>%
  as_join_operator(list(), list())

save_relation(list(df_out1, df_out2), ctx)

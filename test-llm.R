source("llm-gbif.R")

bench::bench_time({
  txt_to_taxa("frogs", "qwen3")
})


bench::bench_time({
  txt_to_taxa("frogs")
})

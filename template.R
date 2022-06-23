# metaGTx_processing.R
# This is a template for processing shotgun metagenomic/metatranscriptomic sequences
# using tools from the BioBakery ( https://github.com/biobakery/biobakery )
#
# make sure you have kneaddata and humann installed on your system, can be done with pip
library(metaGTx.processing)
library(magrittr)
library(stringr)

###### MODIFY THESE VARIABLES AS NEEDED
###### VARIABLES YOU DON'T NEED/WANT AT ALL SET TO NULL
###### see ?create.processing.env for help
create.processing.env(
  interactive = FALSE,
  job.queue = "name@jobqueue",
  jobs.dir = "PATH/TO/STORING/COMMANDS/FILES",
  max.cores = 50,
  max.concurrent.jobs = 5,
  base.dir = "PATH/TO/WORKING/DIRECTORY",
  raw.seq.dirs = "PATH/TO/RAW/SEQUENCES/DIRECTORY",
  store.dir = "PATH/TO/FINAL/OUTPUT/DIRECTORY",
  link.dir = file.path(base.dir, "WorkingFiles"),
  temp.dir = file.path(base.dir, "Results"), # optional, a directory for writing immediate output that will then get, e.g. zipped and moved to the store.dir
  max.memory = "50G",
  knead.host.db = "PATH/TO/HOST/GENOME/BOWTIE/FILES",
  save.env.dir = "Run_envs" # for saving this processing environment for future use or to share with others.
)
# Alternatively
# load("PATH/TO/SAVED/ENVIRONMENT") # should be located in `save.env.dir` above

###### THESE VARIABLES INHERIT FROM ABOVE
concurrent.jobs <- ifelse(run.env$interactive, 1, run.env$max.concurrent.jobs)
cores.per.job <- floor(run.env$max.cores / concurrent.jobs)
for (dir in c(run.env$link.dir, run.env$temp.dir, run.env$store.dir)) {
  if (!dir.exists(dir)) { dir.create(dir) }
}

###### BEGIN PROCESSING
setwd(run.env$link.dir)
symlink.raw.fastqs( # this function makes symlinks from raw fastq files into the link.dir and also sets sample names in run.env
  fastq.dirs = run.env$raw.seq.dirs,
  delim = ___, # get this from raw fastq file names
  sample.field = ___ # get this from raw fastq file names
)
setwd(run.env$base.dir)

###### KNEADDATA
knead.output <- file.path(run.env$store.dir, "KneadData_output") # path to store QC'd sequences
knead.other <- file.path(run.env$store.dir, "KneadData_other") # path for other kneaddata output files
knead.array.file <- file.path(run.env$jobs.dir, "kneaddata_array_commands.txt")
for (dir in c(knead.output, knead.other)) {
  if (!dir.exists(dir)) { dir.create(dir) }
}
commands <- generate.full.commands(
  tool = "kneaddata",
  paired = TRUE,
  input.dir = run.env$link.dir,
  output.dir = run.env$store.dir,
  tmp.dir = run.env$temp.dir,
  reference.db = run.env$knead.host.db,
  threads = cores.per.job,
  zip.output = TRUE,
  write.to = knead.array.file
)
head(commands) # inspect commands for accuracy
if (run.env$interactive) {
  execute.commands(commands)
} else {
  generate.SGE.commmand(
    c = knead.array.file,
    q = run.env$job.queue,
    P = cores.per.job,
    b = concurrent.jobs,
    f = run.env$max.memory,
    qsub_options = run.env$qsub.options
  )
  ## Submit job with printed command from appropriate machine
}

move.files(
  move.from = run.env$store.dir,
  move.to = knead.output,
  match.pattern = "_paired_[12].fastq.gz"
)
move.files(
  move.from = run.env$store.dir,
  move.to = knead.other,
  match.pattern = "\\.gz$"
)

###### HUMANN
setwd(run.env$link.dir)
file.remove(list.files())
symlink.files(
  file.dir = knead.output,
  pattern = "_1.fastq.gz"
)
setwd(run.env$base.dir)

humann.output <- file.path(run.env$store.dir, "Humann_output")
humann.array.file <- file.path(run.env$jobs.dir, "humann_array_commands.txt")

if (!dir.exists(humann.output)) { dir.create(humann.output) }

### If need to install full chocophlan database before running humann. Run the following (change path name!)
# install.chocophlan("PATH/TO/INSTALL/DATABASE")

commands <- generate.full.commands(
  tool = "humann",
  paired = FALSE,
  input.dir = run.env$link.dir,
  output.dir = humann.output,
  tmp.dir = run.env$temp.dir,
  threads = cores.per.job,
  write.to = humann.array.file
)
head(commands) # inspect commands for accuracy
if (run.env$interactive) {
  execute.commands(commands)
} else {
  generate.SGE.commmand(
    c = humann.array.file,
    q = run.env$job.queue,
    P = cores.per.job,
    b = concurrent.jobs,
    f = run.env$max.memory
  )
  ## Submit job with printed command from appropriate machine
}

get.pct.unalign(
  location = humann.output,
  out.file = file.path(humann.output, "pct_unaligned.csv")
)
extract.metaphlan2.files(location = humann.output, move.to = humann.output)

setwd(run.env$link.dir)
file.remove(list.files())
gunzip.files(location = humann.output, match.pattern = "tsv.gz")
symlink.files(
  file.dir = humann.output,
  pattern = "tsv"
)
setwd(run.env$base.dir)

## renormalize abundances
curr.tool <- "humann_renorm_table"
show.tool.help(curr.tool)
new.abund.type <- "relab"
output.dir <- run.env$temp.dir
abund.files <- list.files(
  path = run.env$link.dir,
  pattern = "families|abundance",
  full.names = TRUE
)
for (sample in run.env$samples) {
  files <- str_subset(abund.files, pattern = sample)
  for (in.file in files) {
    out.file <- file.path(
      output.dir,
      str_replace(
        basename(in.file), "(families|abundance)", paste0("\\1_", new.abund.type)
      )
    )
    generate.tool.commmand(
      tool = curr.tool,
      input = in.file,
      units = new.abund.type,
      output = out.file,
      update.snames = "flag"
    ) %>% execute.commands()
  }
}
file.remove(
  list.files(path = run.env$link.dir, pattern = "families|abundance", full.names = T)
)
file.copy(
  from = list.files(path = run.env$temp.dir, pattern = "relab", full.names = T),
  to = run.env$link.dir
)

## unpack pathways
curr.tool <- "humann_unpack_pathways"
show.tool.help(curr.tool)
for (sample in run.env$samples) {
  sample.files <- list.files(path = run.env$link.dir, pattern = sample, full.names = T)
  genes.file <- str_subset(sample.files, pattern = "genefamilies")
  paths.file <- str_subset(sample.files, pattern = "coverage")
  out.file <- str_replace(genes.file, "genefamilies_relab", "unpacked_pathways_relab")
  generate.tool.commmand(
    tool = curr.tool,
    input.genes = genes.file,
    input.pathways = paths.file,
    output = out.file
  ) %>% execute.commands()
}
file.copy(
  from = list.files(path = run.env$link.dir, pattern = "unpacked", full.names = T),
  to = run.env$temp.dir
)

## join like output tables
curr.tool <- "humann_join_tables"
show.tool.help(curr.tool)
file.types <- c("genefamilies", "pathabundance", "pathcoverage", "metaphlan_bugs_list")
input.dir <- run.env$link.dir
output.dir <- run.env$temp.dir
for (file.type in file.types) {
  out.file <- file.path(output.dir, paste0("all_", file.type, ".tsv"))
  if (file.type == "metaphlan_bugs_list") {
    join.metaphlan2.tables(input.dir = input.dir, output.file = out.file)
  } else {
    generate.tool.command(
      tool = curr.tool,
      input = input.dir,
      file_name = file.type,
      output = out.file
    ) %>% execute.commands()
  }
  header <- readLines(con = out.file, n = 1) %>% str_remove_all("--R1_kneaddata_paired_1")
  body <- readLines(con = out.file)[-1]
  writeLines(text = c(header, body), con = out.file)
}

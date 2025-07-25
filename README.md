# Citation
If you use `asmidx` in a publication, please cite:
Morinaga, Gen, Darío Balcazar, Athanase Badolo, et al. “From Macro to Micro: De Novo Genomes of Aedes Mosquitoes Enable Comparative Genomics among Close and Distant Relatives.” Genome Biology and Evolution, 2025, evaf142. https://doi.org/10.1093/gbe/evaf142.

# Install
To install `asmidx`, paste the following into the R console.
```
if (!requireNamespace("devtools", quietly = TRUE))
library(devtools)
devtools::install_github('genmor/asmidx')
```
## `asmidx`
`asmidx` is a package written for R to assess draft genome quality. The main function call `asmidx()` is a Shiny GUI where the user can upload a dataframe of assembly metrics—this can be anything the user thinks is relevant to assessing assembly quality. In `asmidx()` the user can choose any set of numerical metrics to create a ranking. To create a ranking, the user can choose a metric to be either maximized (e.g., N50, number of BUSCO complete) or minimized (e.g., L50, number of BUSCO missing). Each metric is then feature normalized from 0–1 (worst–best) and a row mean is calculated from these values and multiplied by 100 to output a normalized score. Each draft assembly is ranked by this normalized score.

Each metric can also be weighted by any positive value defined by the user, and a weighted output can also be created. `asmidx()` will create a lollipop plot and output a table (normalized and weighted), which the user can save to their device.

## Why would (or should) anyone use `asmidx`?
`asmidx` is a tool for comparing draft assemblies output from a single set of reads. "Compare" implies that multiple draft assemblies have been made, but why would anyone bother making multiple draft assemblies from a single set of reads?

 1. Numerous assembler programs have been written (or re-written) to handle new types of sequencing reads (e.g., PacBio HiFi). Most (if not all of them) are rigorously tested using test data sets and generally output high quality draft assemblies. However, their performance can vary with "real" data sets, requiring users to fine-tune settings or try different assemblers altogether.
 2. If an assembly requires additional bioinformatic touches (e.g., purging haplotigs), it's not always clear what effects they have on the overall assembly and whether this effect is consistent across different program combinations.

With these two reasons in mind, we sought to look for ways to compare different draft assemblies against one another and realized that it's not always exactly clear what constitutes a "good" draft genome—highly contiguous (e.g., high N50; low L50); highly complete (e.g., high BUSCO single-copy); low number of assembly errors? All of these things are important, but what if no one assembly out-performs all others in all of these metrics? What if any one of the drafts trade high scores in one metric for low scores in another? Should each metric be equally be important, or are some metrics less important than others?

`asmidx` attempts to make this decision process easier and more transparent. While we mention some metrics here that we consider generally important for assessing genome quality, they are by no means an attempt to define what constitutes a "high" quality genome. We leave that up to the user, hence the flexible implementation allowing users to define the specific metrics to consider and how important each are relative to one another.

## What's in this package?
`asmidx()` will invoke a local Shiny GUI and will let the user import data and rank assemblies. The only real requirements for use are that the data are in tabular format with an identifier column and at least one genome assembly metric.

We include helper functions that will format non-tabular output from Inspector and BUSCO into a tabular dataframe. Users could merge these dataframes together by assembly to create a master dataframe with assembly metrics. Such a table can be written to disk and used in `asmidx()`. If you already have a tabular dataset with assembly metrics, then you can just use that—just make sure there's a column with assembly IDs.

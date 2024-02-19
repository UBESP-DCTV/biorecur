SPARE.bed <- function(
  bedfile,
  gIDs,
  obj.null,
  output.file = NULL,
  chr = NULL,
  missing.cutoff = 0.05,
  min.maf = 0.05,
  p.cutoff = 0.001,
  memory = 512,
  maxchunksize = 5e4,
  verbose = TRUE
) {
  bim.file <- paste0(bedfile, ".bim")
  fam.file <- paste0(bedfile, ".fam")
  bed.file <- paste0(bedfile, ".bed")

  if (!file.exists(bim.file)) {
    stop("Could not find paste0(bedfile,'.bim')")
  }
  if (!file.exists(bed.file)) {
    stop("Could not find paste0(bedfile,'.bed')")
  }
  if (!file.exists(fam.file)) {
    stop("Could not find paste0(bedfile,'.fam')")
  }
  if (is.null(output.file)) {
    stop("please provide name of output file")
  }

  fam.data <- utils::read.table(fam.file, stringsAsFactors = FALSE)
  bim.data <- utils::read.table(bim.file, stringsAsFactors = FALSE)

  N <- nrow(fam.data)
  M <- nrow(bim.data)

  if (verbose) {
    usethis::ui_info("Totally {M} markers in plink files.")
    if (!any(obj.null$IDs %in% fam.data$V2)) {
      usethis::ui_stop(stringr::str_c(
        "None of the subject IDs from null model were found in the ",
        ".fam file."
      ))
    } else {
      usethis::ui_info(stringr::str_c(
        "In total, {length(intersect(obj.null$IDs, fam.data$V2))} ",
        "samples found in genotype and phenotype."
      ))
    }

  }

  total.samples <- intersect(obj.null$IDs, fam.data$V2)

  size <- dim(bim.data)[1]

  # 3 values of 8 bytes, + 4 bytes for ploidy
  bytes_per_genotype <- (3 * 8) + 4
  bytes_per_variant <- bytes_per_genotype * N
  memory_available <- memory * 1024^2
  chunksize <- floor(
    min(maxchunksize, memory_available / bytes_per_variant)
  )

  reps <- ceiling(size / chunksize)
  outcome <- NULL

  if (verbose) {
    usethis::ui_info(stringr::str_c(
      "Split all markers into {reps} chunks.",
      "Each chunk includes less than {chunksize} markers."
    ))
  }

  outcome <- NULL

  for (r in seq_len(reps)) {
    if (verbose) {
      usethis::ui_todo("Reading chunk {r}/{reps}...")
    }

    indices <- c(((r - 1) * chunksize + 1):min(r * chunksize, size))

    Geno.mtx <- seqminer::readPlinkToMatrixByIndex(
        bedfile,
        seq_len(N),
        indices
      )

    colnames(Geno.mtx) <- bim.data$V2[indices]

    outcome <- SPARE(
      obj.null = obj.null,
      Geno.mtx = Geno.mtx,
      missing.cutoff = missing.cutoff,
      min.maf = min.maf,
      p.cutoff = p.cutoff,
      verbose = verbose
    )
    new_outcome <- cbind(
      Chr = bim.data[match(outcome$SNP, bim.data[, 2]), 1],
      SNP = outcome$SNP,
      A1 = bim.data[match(outcome$SNP, bim.data[, 2]), 5],
      A2 = bim.data[match(outcome$SNP, bim.data[, 2]), 6],
      BP = bim.data[match(outcome$SNP, bim.data[, 2]), 4],
      outcome[, -1]
    )

    if (r == 1) {
      data.table::fwrite(
        new_outcome,
        paste0(output.file, ".txt"),
        sep = "\t",
        append = FALSE,
        row.names = FALSE,
        col.names = TRUE,
        verbose = verbose
      )
    } else {
      data.table::fwrite(
        new_outcome,
        paste0(output.file, ".txt"),
        sep = "\t",
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE,
        verbose = verbose
      )
    }
  }
}

SPARE.bgen <- function(bgenfile, gIDs,
                       obj.null,
                       output.file = NULL,
                       chr = NULL,
                       missing.cutoff = 0.05,
                       min.maf = 0.05,
                       p.cutoff = 0.001,
                       memory = 512,
                       maxchunksize = 5e4,
                       backingdir = "Connections",
                       backingfile = "backfile") {
  bgenfile <- paste0(bgenfile, ".bgen")
  bgifile <- paste0(bgenfile, ".bgi")

  if (!file.exists(bgenfile)) stop("Could not find .bgen file")
  if (!file.exists(bgifile)) stop("Could not find .bgen.bgi file")
  if (!dir.exists(backingdir)) {
    print(paste0(
      "Creating directory ",
      paste0(getwd(), "/", backingdir),
      " for the backingfiles of the .bgen file"
    ))
    dir.create(backingdir)
  }
  if (is.null(output.file)) stop("please provide name of output file")

  ### Create 'myid' variable for reading .bgen SNPs
  db_con <- RSQLite::dbConnect(RSQLite::SQLite(), bgifile)
  on.exit(RSQLite::dbDisconnect(db_con), add = TRUE)
  infos <- dplyr::collect(dplyr::tbl(db_con, "Variant"))
  infos$myid <- with(infos, paste(chromosome, position, allele1,
                                  allele2,
                                  sep = "_"
  ))
  info <- list()
  info[[1]] <- infos$myid

  ### Set up chunk sizes
  size <- dim(infos)[1]

  # 3 values of 8 bytes, + 4 bytes for ploidy
  bytes_per_genotype <- (3 * 8) + 4
  bytes_per_variant <- bytes_per_genotype * length(gIDs)
  memory_available <- memory * 1024^2
  chunksize <- floor(
    min(maxchunksize, memory_available / bytes_per_variant)
  )

  reps <- ceiling(size / chunksize)
  outcome <- NULL

  print(paste0("Split all markers into ", reps, " chunks."))
  print(
    paste0("Each chunk includes less than ", chunksize, " markers.")
  )

  ### Analyze .bgen files per chunk
  for (r in 1:reps) {
    print(paste0("Reading chunk ", r, " of ", reps))
    indices <- c(((r - 1) * chunksize + 1):min(r * chunksize, size))

    if (is.null(chr)) chr <- as.numeric(infos$chromosome[indices[1]])
    backfile <- paste0(
      getwd(), "/", backingdir, "/", backingfile, "_", chr, "_", r
    )

    snps <- list()
    # The names of the SNPs in this chunk
    snps[[1]] <- infos$myid[indices]
    bgen <- bigsnpr::snp_readBGEN(
      bgenfiles = bgenfile,
      list_snp_id = snps,
      backingfile = backfile,
      ncores = 1
    )

    file <- paste0(backfile, ".rds")
    genotype <- bigsnpr::snp_attach(file)
    G <- genotype$genotypes

    Geno.mtx <- G[, ]
    colnames(Geno.mtx) <- infos$myid[indices]

    if (length(gIDs) != dim(Geno.mtx)[1]) {
      stop(
        "Length of gIDs not equal to number of samples in .bgen file."
      )
    }
    rownames(Geno.mtx) <- gIDs

    # Executing the linear regression
    outcome <- SPARE(
      obj.null = obj.null,
      Geno.mtx = Geno.mtx,
      missing.cutoff = missing.cutoff,
      min.maf = min.maf,
      p.cutoff = p.cutoff
    )


    new_outcome <- cbind(
      SNP = outcome$SNP,
      t(matrix(unlist(strsplit(outcome$SNP, split = "_")), nrow = 4)),
      outcome[, -1]
    )
    colnames(new_outcome)[2:5] <- c("Chr", "BP", "A1", "A2")


    if (r == 1) {
      data.table::fwrite(
        new_outcome,
        paste0(output.file, ".txt"),
        sep = "\t",
        append = FALSE,
        row.names = FALSE,
        col.names = TRUE
      )
    } else {
      data.table::fwrite(
        new_outcome,
        paste0(output.file, ".txt"),
        sep = "\t",
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
    }

    ### Clean up directory by removing previous connections
    for (k in 1:r) {
      prev.file <- paste0(getwd(), "/Connections/tmpfile", chr, "_", k)
      unlink(prev.file)
      prev.file <- paste0(getwd(), "/Connections/tmpfile", chr, "_", k)
      unlink(prev.file)
    }
  }
}

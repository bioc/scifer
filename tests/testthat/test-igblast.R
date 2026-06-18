test_that("Database directory inputs", {
    expect_error(
        igblast(database = NA, fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), ), 
        "The database directory does not exist.")
    expect_error(
        igblast(database = "/invalidpath", fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), ),
        "The database directory does not exist.")
    expect_error(
        igblast(database = "", fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), threads = 1),
        "The database directory does not exist.")
    expect_error(
        igblast(database = NULL, fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), threads = 1),
        "The database directory does not exist.")
    expect_error(
        igblast(database = character(0), fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), threads = 1),
        "The database directory does not exist.")
    skip_if(isMacOSXArm(), message = "This test is not supported on MacOS with arm architecture.")
    skip_if(Sys.info()[["sysname"]] == "Linux" && Sys.info()[["machine"]] == "aarch64", message = "This test is not supported on Linux with arm architecture.")
    if(isWindows()){
      if(system("makeblastdb") %in% c(127, "Exit Code 127")){
        skip_on_os(os = "windows")
      }
    }
})
  
test_that("Fasta file directory inputs", {    
      expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), fasta = NA, threads = 1),
        "The fasta file directory does not exist.")
    expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), fasta = "/invalidpath", threads = 1),
        "The fasta file directory does not exist.")  
    expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), fasta = "", threads = 1),
        "The fasta file directory does not exist.")
    expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), fasta = NULL, threads = 1),
        "The fasta file directory does not exist.")
    skip_if(isMacOSXArm(), message = "This test is not supported on MacOS with arm architecture.")
    skip_if(Sys.info()[["sysname"]] == "Linux" && Sys.info()[["machine"]] == "aarch64", message = "This test is not supported on Linux with arm architecture.")
    if(isWindows()){
      if(system("makeblastdb") %in% c(127, "Exit Code 127")){
        skip_on_os(os = "windows")
      }
    }
})

test_that("Threads argument inputs", {    
    expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), 
            fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), 
            threads = "H"), "The threads argument should be a numeric value.")
    expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), 
            fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), 
            threads = NULL), "The threads argument should be a numeric value.")
    expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), 
            fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), 
            threads = NA), "The threads argument should be a numeric value.")
    expect_error(
        igblast(database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"), 
            fasta = system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"), 
            threads = character(0)), "The threads argument should be a numeric value.")
    skip_if(isMacOSXArm(), message = "This test is not supported on MacOS with arm architecture.")
    skip_if(Sys.info()[["sysname"]] == "Linux" && Sys.info()[["machine"]] == "aarch64", message = "This test is not supported on Linux with arm architecture.")
    if(isWindows()){
      if(system("makeblastdb") %in% c(127, "Exit Code 127")){
        skip_on_os(os = "windows")
      }
    }
})

test_that("returns a data.frame object", {
  skip_if(isMacOSXArm(), message = "This test is not supported on MacOS with arm architecture.")
  skip_if(Sys.info()[["sysname"]] == "Linux" && Sys.info()[["machine"]] == "aarch64", message = "This test is not supported on Linux with arm architecture.")
    if(isWindows()){
      if(system("makeblastdb") %in% c(127, "Exit Code 127")){
        skip_on_os(os = "windows")
      }
    }
    result <- igblast(
                    database = system.file("extdata/test_fasta/KIMDB_rm", package = "scifer"),
                    system.file("extdata/test_fasta/test_igblast.txt", package = "scifer"),
                    threads = 1
                    )
    # igblast() returns NULL when the conda environment or the igblast/
    # makeblastdb binaries are not available in this environment (e.g. on the
    # build machine). In that case skip rather than fail the whole build; the
    # data.frame contract is only meaningful when igblast actually ran.
    skip_if(is.null(result),
            "igblast could not run in this environment; skipping output check.")
    expect_s3_class(result, "data.frame")

})

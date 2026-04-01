usethis::use_description()
usethis::use_namespace()
usethis::use_rstudio()
usethis::use_roxygen_md()

# dependencies -----------------------------------------------------------------
# usethis::use_version("0.1.0")
usethis::use_description(
  fields = list(
    Title = "R Interface to MultiChain Blockchain RPC API",
    Description = "Provides a comprehensive R interface to the MultiChain blockchain JSON-RPC API. Users can manage blockchain nodes, create and subscribe to data streams, issue assets, and manage network permissions directly from the R console.",
    `Authors@R` = person("Aliaksandr", "Martsinkevich", email = "alex@capsula.by",
                         role = c("aut", "cre"), comment = c(ORCID = "0000-0003-3655-4489"))
  ),
  check_name = TRUE
)
usethis::use_package("httr2")    # Для HTTP-запросов
usethis::use_package("jsonlite") # Для работы с JSON
usethis::use_testthat()
usethis::use_pipe()
usethis::use_mit_license()



# files ------------------------------------------------------------------------
usethis::use_r("connection")
usethis::use_r("node_management")
usethis::use_r("commands")
usethis::use_r("streams")
usethis::use_r("utils")
usethis::use_r("assets")
usethis::use_r("permissions")
usethis::use_r("addresses")




usethis::use_testthat()
usethis::use_test("connection")
usethis::use_test("node_management")
usethis::use_test("assets")
usethis::use_test("streams")

#usethis::use_build_ignore("pdf")
#usethis::use_build_ignore(c("pdf", "..pdf", "copy.R", "start.R", "test.R"))




# test -------------------------------------------------------------------------
devtools::document()
devtools::test()
devtools::install()





# check ------------------------------------------------------------------------
devtools::load_all()
devtools::document()
devtools::check()


devtools::check(args = c("--as-cran"), manual = TRUE, remote = TRUE)
devtools::check(args = "--as-cran")

# devtools::build_manual(path = ".")
# tinytex::reinstall_tinytex(repository = "illinois")
# tinytex::use_tinytex()
# tinytex::tlmgr_update()
# tinytex::tlmgr_install("makeindex")
# R CMD Rd2pdf .
# R CMD Rd2pdf . --output=multichainr.pdf

# fs::dir_ls(recurse = TRUE) |> fs::file_touch()

files <- list.files(recursive = TRUE, full.names = TRUE)
Sys.setFileTime(files, Sys.time())

covr::report()

lintr::lint_package() # проверить warnings()

# проверить @seealso, @family!

# сделать в виньетке тестовую ноду и в примерах повторять так же создание?
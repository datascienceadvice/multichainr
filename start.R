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

usethis::edit_r_environ("project")
Sys.setenv(MULTICHAIN_PATH = "E:/multichain")
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))


usethis::use_testthat()
usethis::use_test("connection")
usethis::use_test("node_management")
usethis::use_test("assets")
usethis::use_test("streams")

#usethis::use_build_ignore("pdf")
#usethis::use_build_ignore(c("pdf", "..pdf", "copy.R", "start.R", "test.R"))


usethis::use_vignette("getting-started", "Introduction to multichainr")
usethis::use_vignette("working-with-assets", "Managing Assets and Tokens")
usethis::use_vignette("data-streams", "Storing Data in Streams")
usethis::use_vignette("permissions-and-governance", "Network Permissions and Governance")
usethis::use_vignette("advanced-transactions", "Atomic Swaps and Raw Transactions")

# виньетки в том же стиле (init/cleanup/summary)
usethis::use_vignette("multi-signature-wallets", "Multi-Signature Wallets and Security")
usethis::use_vignette("handling-large-payloads", "Handling Large Payloads with Binary Cache")
usethis::use_vignette("global-variables", "Global Variables and State History")
usethis::use_vignette("smart-filters", "Smart Filters and On-Chain Logic")
usethis::use_vignette("", "")
usethis::use_vignette("", "")

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

devtools::check_win_devel()
devtools::check_win_release()

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

разобраться с путями - как все таки установить путь к mc

# проверить @seealso, @family!

# сделать в виньетке тестовую ноду и в примерах повторять так же создание?
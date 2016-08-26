.onAttach <- function(libname, pkgname) {
    library('Biobase')

    packageStartupMessage(
        sprintf("\nThis is EuPathDB version %s\n Read 'EuPathDB()' to get started.\n", 
                packageVersion("EuPathDB"))

    if (interactive() && .Platform$OS.type == "windows" &&
        .Platform$GUI == "Rgui") {
        addVigs2WinMenu("EuPathDB")
    }
}

get_vault_name <- function(x) {
    UseMethod("get_vault_name")
}

get_vault_name.default <- function(x) {
    x
}

get_vault_name.character <- function(x) {
    x
}

get_vault_name.aws_glacier_vault <- function(x) {
    x$VaultName
}

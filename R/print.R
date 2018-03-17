print.aws_glacier_vault <- 
function(
  x,
  ...
) {
    cat(sprintf("Glacier Vault:  %s\n", x$VaultName))
    cat(sprintf("Creation Date:  %s\n", x$CreationDate))
    cat(sprintf("Last Inventory: %s\n", if (is.null(x$LastInventoryDate)) "never" else x$LastInventoryDate))
    cat(sprintf("# of Archives:  %s\n", x$NumberOfArchives))
    cat(sprintf("Size (bytes):   %s\n", x$SizeInBytes))
    cat(sprintf("ARN: %s\n", x$VaultARN))
    invisible(x)
}

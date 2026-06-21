# zzz.R -- Load-time hooks.
#
# Registers the built-in optimiser engines into the package registry when the
# namespace loads. User-supplied engines are added afterwards with
# register_optimiser().

.onLoad <- function(libname, pkgname) {
  .register_builtin_engines()
  invisible(NULL)
}

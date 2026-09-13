# Shiny recognizes this filename and skips automatic R/*.R sourcing.
# global.R owns the ordered library loading (compiled or source fallback),
# and ui.R explicitly loads ui_desktop_downloads.R. Automatic sourcing would
# create a second shared environment that shadows those runtime functions
# and caches with uncompiled copies. Keep new helpers in the explicit loaders.

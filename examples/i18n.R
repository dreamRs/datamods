library(datamods)

# Use with an objet
my.translations <- list(
  "Hello" = "Bonjour"
)
i18n("Hello", my.translations)

# Use with options()
options("i18n" = list(
  "Hello" = "Bonjour"
))
i18n("Hello")

# With a package
options("datamods.i18n" = "fr")
i18n("Browse...", translations = i18n_translations("datamods"))
# If you call i18n() from within a function of your package
# you don't need second argument, e.g.:
# i18n("Browse...")

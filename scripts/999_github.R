# upload this to github repository
usethis::use_git()
usethis::use_github(auth_token = "cd0fee4ccebbdd388bf52eed764f1a6ea729d60e")

# create README file
usethis::use_readme_rmd()
build_readme()

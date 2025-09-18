# https://biostats-r.github.io/biostats/github/1_Git_Tutorial.html#set-up
install.packages("usethis") 
library(usethis)

#Set-up
#Now you have to configure your name and email associated with your GitHub account. 
#Go to RStudio and, using your own identification, type:
use_git_config(
  user.name = "marijokerud", 
  user.email = "mari.jokerud@gmail.com"
)

#Connect RStudio and GitHub
#GitHub needs to validate who you are before you can connect it and RStudio. 
#We can do this by generating and saving a Personal Access Token (PAT). You need to do this once for every RStudio project.
usethis::create_github_token()

# This will ask you for your PAT: paste it at the prompt and press return. 
# This will save the PAT so that it can be used to access GitHub. 
#Treat your PAT as a password - never save it in a script.
gitcreds::gitcreds_set() 


#will add various files to your global .gitignore file (Section 2.9) to reduce the chance of you leaking passwords, making git safer to use.
git_vaccinate()


# Making a repo

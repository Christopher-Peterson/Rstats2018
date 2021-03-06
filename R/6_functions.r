# Functions! 
library(tidyverse)
library(cowplot) 
theme_set(theme_cowplot())
lizards = read_csv("data/anoles.csv")
# Most things you use in R are functions.  Example:

### Function Intro ####
mean(1:5) # The mean function takes the arguments c(1,2,3,4,5) 
        # and returns a single value, the mean
mean(lizards$SVL)
lizards %>% summarize(mean_SVL = mean(SVL))

# How could we write our own mean function?
mean2 <- # function name
  function(x) { # defines a function with argument X
    # Everything within the braces is part of the BODY of the function
  sum(x) / length(x) # Evaluates this expression; 
  # the last expression evaluated is returned (a.k.a., the output)
  }
mean2(1:5)
mean2(lizards$SVL)
lizards %>% summarize(mean_SVL = mean2(SVL))

# Let's have another example:
normalize = function(x, ...) { # We have one argument (x) and dots (...)
  # This function centers a vector around its mean and then divides it by its standard deviation
  # The function scale() does this already, but it doesn't always play nicely with mutate()
  mean_val = mean(x, ...) # The dots allow the user to pass extra arguments to functions called INSIDE another function
  stdev = sd(x, ...)
  (x - mean_val) / stdev # this is the last command evaluated, so it's returned as the output
  # I could have simplified this as (x - mean(x, ...))/sd(x, ...)
}
normalize(1:5)

# What do dots do? They add flexibility.  For example: 
missing_vector = c(1:5, NA) # This has a missing value in it
mean(missing_vector) # NA's can cause problems in a lot of functions.
mean(missing_vector, na.rm = TRUE) # the mean() function (and sd()) have an optional argument to remove them

normalize(missing_vector) # The NA screws things up
normalize(missing_vector, na.rm = TRUE) 
# The na.rm argument wasn't in the function definition; 
# it was intercepted by the dots (...) and passed to mean() and sd()
# This can give you a lot of felxibility when writing functions.

# Alternatively, we can set default arguments
normalize2 = function(x, na.rm = TRUE, ...) {
  mean_val = mean(x, na.rm = na.rm, ...) 
  stdev = sd(x, na.rm = na.rm,...)
  (x - mean_val) / stdev
}
normalize2(missing_vector)
normalize2(missing_vector, FALSE) # Now we have to specifically set it as false
# Note that we haven't been including argument names in these function calls
normalize2(x = missing_vector, na.rm = FALSE) # R will assume the arguments based on their position
# You have to use names for dots and everything that comes after the dots in the argument list

# Putting an argument after the dots prevents you from accidentally calling it by position
normalize3 = function(x, ..., na.rm = TRUE) {
  mean_val = mean(x, na.rm = na.rm, ...) 
  stdev = sd(x, na.rm = na.rm,...)
  (x - mean_val) / stdev
}
normalize3(missing_vector, na.rm = FALSE) # You'll get weird errors if this happens

# Let's add a bit more flexibility to our normalize function.  
  # In some cases, it's a good idea to normalize data by dividing by the mean instead of the sd
  # Other times, people just want to center around the mean without affecting the scale
normalize_general = 
  function(x, method = c("sd", "mean", "center"), # this lists the possible options for method
           ..., na.rm = TRUE) {
  method = match.arg(method) # This ensures that method is one of the options defined in the arugments
  # If method isn't explicitly declared, match.arg() sets it to the first option.
  # Using the match.arg command is a great way to make sure you avoid headaches later on.
  mean_val = mean(x, na.rm = na.rm, ...)
  if(method == "sd") { ## Note the double equal sign; that returns a logical comparison
    stdev = sd(x, na.rm = na.rm,...)
    out = (x - mean_val) / stdev
  } else if (method == "mean") { 
    out = x / mean_val
  } else if (method == "center") { # The only other option is method == "center"
    out = x - mean_val
  }
  out # return the output variable
  }

normalize_general(missing_vector) # no argument defaults to method == "sd"
normalize_general(missing_vector)== normalize3(missing_vector) # same result
normalize_general(missing_vector, method = "mean") # Goes with the other option
normalize_general(missing_vector, method = "m") # match.arg() finds the best match for incomplete options
normalize_general(missing_vector, method = "center")

#### Advice on function writing #####

# Functions let you avoid errors from copy/paste duplications
  # If you need to change something, you only need to modify the code in one location, 
  # which reduces errors
  # Giving a function a good name makes the code easier to read

# For example, let's say I wanted to estimate the diversity of 
  # lizard colors at each site 
  # using Shannon diversity: exp( - sum(prop_i * log(prop_i))), 
  # where prop_i is the proportion of individuals of a given type

# We could arguably include this code as part of a summarize command:

lizards %>% group_by(Site, Color_morph) %>% 
  summarize(n_i = n()) %>% 
  mutate(prop_i = n_i / sum(n_i)) %>% 
  summarize(diversity = exp(-sum(prop_i * log(prop_i))))
# It's not particularly clear what's happening here, though
# And good luck sharing this w/ a collaborator who isn't familiar with the math

# Alternatively, you could define a function
shannon_diversity <- function(x) { # x is an argument of the function
  # This is the body of a function;
  # you can define local variables here that will disappear after the function is run.
  counts <- table(x)  # table counts the number of occurences of each type of x
  proportions <- counts / sum(counts)
  out <- exp(-sum(proportions * log(proportions))) 
  out # The final value that's evalulated or printed in a function is returned
}
lizards %>% group_by(Site) %>% # This bit of code is much more readable 
  summarize(diversity = shannon_diversity(Color_morph))
# This also makes it easy to re-use and expand
lizards %>% group_by(Site) %>% # This bit of code is much more readable 
  summarize(color_diversity = shannon_diversity(Color_morph),
            perch_diversity = shannon_diversity(Perch_type))

# Make your function names descriptive,
  # RStudio can tab-complete function names, so don't make it too short to avoid typing
  # Generally, function names should be verbs and arguments should be nouns
# It's a good idea to have your function's main data input 
  # (whether a simple vector or a data frame)
  # be the first argument, since this will let you pipe ( %>% ) data into it.
# The less important / more optional an argument is, the later you should put it.
  # consider adding default values for the arguments that aren't essential

# Other notes:
  # You can see the code of a function if you type it without parentheses
normalize_general  # this works for built-in functions too, though they may be a bit cryptic
args(normalize_general) # list the arguments of a function
formals(normalize_general) # returns the arguments in a list

#### Local variable names and scoping #####

# Environments: a conceptual space where values are associated with names

# When you define a variable:
important_variable = -1
# You associate it with an environment
# By default we're working in the global environment
# when you try to use that variable's name
important_variable
# R looks for it in the current environment

# Functions have their own internal environment
scope_function1 = function(x) {
  local_var = 1
  x * important_variable + local_var
}
scope_function1(1)
# If your code calls for a variable that isn't in the current environment, 
  # R looks for the parent environment (where it was created)
  # In this case, that's the global environment

scope_function2 = function(x) {
  local_var = 1
  important_var = 10
  x * important_var + local_var
}
# But the global value is hidden if there's a local variable that matches it first
scope_function2(1)

scope_function3 = function(x) {
  important_var = 10
  x * scope_function1(1)
}
scope_function3(1)
# In this case, scope_function1 doesn't have a local variable named important_var,
  # so it searches its parent environment (the global environment, where it was created),
  # not the environment in which it was called

important_function = function(x) {
  important_varaible = 10
  x * important_variable
}
important_function(1:10)
# What's happening here?

# Generally, it's a good idea to try to keep your local variable names different from the ones you use outside of functions

#### Let's write some functions! ####

lizards = read_csv("data/anoles.csv")

# Let's see how the Limb Length : Height relationship varies by site:

lizards %>% 
  filter(Site == "A") %>%
  ggplot(aes(x = Limb, y = Height)) +
  facet_wrap(~Site, scale = "free")+
  geom_smooth(method = "lm") + geom_point()
# This is fine, but it's a lot of information on one plot
# What if we wanted to make separate plots for each site? 

# Use a filter() command to make it work for only a single site:
lizards %>% 
  filter(Site == "A") %>%
  ggplot(aes(x = Limb, y = Height)) +
  facet_wrap(~Site, scale = "free")+
  geom_smooth(method = "lm") + geom_point()

###########
# Exercise:
###########
# Modify this block of code into a function that could plot arbitrary sites?
# Allow the function to have optional arguments for 
  # geom_point and geom_smooth

plot_site = function(site){
  lizards %>% 
    filter(Site == site) %>%
    ggplot(aes(x = Limb, y = Height)) +
    facet_wrap(~Site, scale = "free")+
    geom_smooth(method = "lm") + geom_point()
}
plot_site("R")


#### Functions with tidyverse variables ####
# a.k.a., tidy evaluation

# How would you generalize the above code to show a plot of any two variables?

variable_plot = function(site, x, y, ...) {
  lizards %>% 
    filter(Site == site) %>%
    ggplot(aes(x = x, y = y)) +
    facet_wrap(~Site, scale = "free")+
    geom_smooth(method = "lm", ...) + geom_point(...)
}
variable_plot(site = "A", x = SVL, y = Tail)
# This causes an error.  Why?

# The variables SVL and Tail don't exist on their own; they only make sense relative
  # to the lizards data, and the function doesn't know that.
  # It's trying to find free variables with those names, and when it can't it 
  # throws an error.
  # We need a way to tell the function to trust that these names are real until 
  # we can pass them to ggplot

# For many tidyverse functions, you can do that like this:

variable_plot_good = function(site, x, y, ...) {
  lizards %>% 
    filter(Site == site) %>%
    ggplot(aes(x = {{x}}, y = {{y}} )) + ## Note the double braces
    facet_wrap(~Site, scale = "free")+
    geom_smooth(method = "lm", ...) + geom_point(...)
}
variable_plot_good(site = "A", x = SVL, y = Tail)
# These double braces tell the aes() function to use the names of x and y, 
  # not the values they (don't) point to

# Create a function to filter data that's above the mean of one variable
above_mean = function(data, variable) {
  data %>% filter({{variable}} > mean({{variable}}, na.rm = TRUE))
}
lizards %>% above_mean(SVL)

# Passing a new column name (for mutate and summarize) 
add_one_to_col = function(data, variable, var_name) {
  data %>% mutate( {{var_name}} := {{variable}} + 1) # Note the := instead of =
}
lizards %>% add_one_to_col(SVL, var_name = SVL_plus_one) %>% View
lizards %>% add_one_to_col(SVL, var_name = "SVL_plus_one") # You can pass the new name as a character vector too


# To see if a function plays nicely with {{ }}, look at the help page.
?aes
  # The page should mention that the function supports 
  # "quoting," "quasiquotation," "unquoting," or something similar. 


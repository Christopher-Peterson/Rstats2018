# Functions! 
library(tidyverse)
library(cowplot) # Not sure if we need this, but why not?
lizards = read_csv("data/anoles.csv")
# Most things you use in R are functions.  Example:

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
normalize3(missing_vector, FALSE) # You'll get weird errors if this happens

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

# A different way to pass arguments to functions
extra_args = list(method = "mean", na.rm = FALSE)
extra_args
normalize_general(x = 1:5, !!!extra_args) 
# Placing !!! before a list will splice the elements of that list into the function call as extra arguments
normalize_general(x = 1:5, !!!extra_args)  ==  normalize_general(x = 1:5, method = "mean", na.rm = FALSE)

# This is incredibly useful when you're passing arguments to specific functions;
  # it allows a lot more flexibility than dots

normalize_general_splice = 
  function(x, method = c("sd", "mean", "center"), 
           mean_args = list(), sd_args = list()) {
    method = match.arg(method) 
    mean_val = mean(x, !!!mean_args) # argument splicing
    if(method == "sd") { 
      stdev = sd(x, !!!sd_args) # argument splicing
      out = (x - mean_val) / stdev
    } else if (method == "mean") { 
      out = x / mean_val
    } else if (method == "center") { # The only other option is method == "center"
      out = x - mean_val
    }
    out 
  }
normalize_general_splice(missing_vector, method = "sd", 
                         mean_args = list(na.rm = TRUE, trim = 0.05),
                         se_args = list(na.rm = FALSE))

#### Advice on function writing #####

# You can write your own functions.
# Functions let you avoid errors from copy/paste duplications
# If you need to change something, you only need to modify the code in one location, which reduces errors
# Giving a function a good name makes the code easier to read

# For example, let's say I wanted to estimate the diversity of lizard colors at each site 
# using Shannon diversity: exp( - sum(prop_i * log(prop_i))), 
# where prop is the proportion of individuals of a given type

# We could arguably include this code as part of a summarize command:

lizards %>% group_by(Site, Color) %>% summarize(n_i = n()) %>% 
  group_by(Site) %>% mutate(prop_i = n_i / sum(n_i)) %>% 
  summarize(diversity = exp(-sum(prop_i * log(prop_i))))
# It's not particularly clear what's happening here, though
# And good luck sharing this w/ a collaborator who isn't familiar with the math

# Alternatively, you could define a function
shannon_diversity <- function(types) { # types is an argument of the function
  # This is the body of a function;
  # you can define local variables here that will disappear after the function is run.
  counts <- table(types)  # table counts the number of occurencesof each t
  proportions <- counts / sum(counts)
  out <- exp(-sum(proportions * log(proportions))) 
  out # The final value that's evalulated or printed in a function is returned
}
lizards %>% group_by(Site) %>% # This bit of code is much more readable 
  summarize(diversity = shannon_diversity(Color))
# This also makes it easy to re-use and expand
lizards %>% group_by(Site) %>% # This bit of code is much more readable 
  summarize(color_diversity = shannon_diversity(Color),
            perch_diversity = shannon_diversity(Perch_type))



# Make your function names descriptive, but not irritatingly long
  # Generally, function names should be verbs and arguments should be nouns
# It's a good idea to have your function's main data input (whether a simple vector or a data frame)
  # be the first argument, since this will let you pipe ( %>% ) data into it.
# The less important / more optional an argument is, the later you should put it.
  # consider adding default values for the arguments that aren't essential
  # 

# Other notes:
  # You can see the code of a function if you type it without parentheses
normalize_general  # this works for built-in functions too, though they may be a bit cryptic
args(normalize_general) # list the arguments of a function

#### Local variable names and scoping #####

important_variable = -1

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
  # filter(Site == "A") %>% 
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

# How could you make this into a function that could plot arbitrary sites?
# Modify this function to allow for optional arguments to 
  # geom_point and geom_smooth



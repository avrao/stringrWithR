# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}
# Some vectors of numbers
percent_change <- c(4, -1.91, 3.00, -5.002)
income <- c(72.19, 1030.18, 10291.93, 1189192.18)
p_values <- c(0.12, 0.98, 0.0000191, 0.00000000002)

# Format c(0.0011, 0.011, 1) with digits = 1
format(c(0.0011, 0.011, 1), digits = 1)

# Format c(1.0011, 2.011, 1) with digits = 1
format(c(1.0011, 2.011, 1), digits = 1)

# Format percent_change to one place after the decimal point
format(percent_change, digits = 2)

# Format income to whole numbers
format(income, digits = 2)

# Format p_values in fixed format
format(p_values, scientific = FALSE)

formatted_income <- format(income, digits = 2)
formatted_income
# Print formatted_income
print(formatted_income)

# Call writeLines() on the formatted income
writeLines(formatted_income)

# Define trimmed_income
trimmed_income <- format(income,digit=2,trim=TRUE)

# Call writeLines() on the trimmed_income
writeLines(trimmed_income)

# Define pretty_income
pretty_income <- format(income, digits=2, big.mark=",")

# Call writeLines() on the pretty_income
writeLines(pretty_income)

# From the format() exercise
x <- c(0.0011, 0.011, 1)
y <- c(1.0011, 2.011, 1)

# formatC() on x with format = "f", digits = 1
formatC(x, format="f", digits=1)

# formatC() on y with format = "f", digits = 1
formatC(y, format="f", digits=1)

# Format percent_change to one place after the decimal point
formatC(percent_change, format="f", digits=1)

# percent_change with flag = "+"
formatC(percent_change, format="f", flag="+", digits=1)

# Format p_values using format = "g" and digits = 2
formatC(p_values, format="g", digits=2)

# Add $ to pretty_income
paste("$", pretty_income, sep="")

# Add % to pretty_percent
paste(pretty_percent, "%", sep="")

# Create vector with elements like 2010: +4.0%`
years
pretty_income
pretty_percent

year_percent <- paste(years, ": ", pretty_percent, "%", sep = "")

# Collapse all years into single string
paste(year_percent, collapse=", ")

# Define the names vector
income_names <- c("Year 0", "Year 1", "Year 2", "Project Lifetime")

# Create pretty_income
pretty_income <- format(income, digits=2, big.mark=",")
pretty_income
# Create dollar_income
dollar_income <- paste("$", pretty_income,sep="")
dollar_income
# Create formatted_names
formatted_names <- format(income_names, justify="right")

# Create rows
rows <- paste(formatted_names, dollar_income, sep="   ")

# Write rows
writeLines(rows)

# Randomly sample 3 toppings
my_toppings <- sample(toppings, size = 3)

# Print my_toppings
my_toppings

# Paste "and " to last element: my_toppings_and
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep="")
my_toppings_and
# Collapse with comma space: these_toppings
these_toppings <- paste(my_toppings_and, collapse=", ")
these_toppings
# Add rest of sentence: my_order
my_order <- paste("I want to order a pizza with ", these_toppings,sep="", ".")

# Order pizza with writeLines()
writeLines(my_order)

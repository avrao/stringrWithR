library(stringr)
library(babynames)
library(dplyr)
library(rebus)
library(stringi)

my_toppings <- c("cheese", NA, NA)
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep = "")

# Print my_toppings_and
my_toppings_and

# Use str_c() instead of paste(): my_toppings_str
my_toppings_str <- str_c(c("", "", "and "), my_toppings)

# Print my_toppings_str
my_toppings_str

# paste() my_toppings_and with collapse = ", "
paste(my_toppings_and, collapse=", ")

# str_c() my_toppings_str with collapse = ", "
str_c(my_toppings_str, collapse=", ")


# Extracting vectors for boys' and girls' names
babynames_2014 <- filter(babynames, year == 2014)
boy_names <- filter(babynames_2014, sex == "M")$name
girl_names <- filter(babynames_2014, sex == "F")$name

# Take a look at a few boy_names
head(boy_names)

# Find the length of all boy_names
boy_length <- str_length(boy_names)

# Take a look at a few lengths
head(boy_length)

# Find the length of all girl_names
girl_length <- str_length(girl_names)

# Find the difference in mean length
mean(girl_length) - mean(boy_length)

# Confirm str_length() works with factors
head(str_length(factor(boy_names)))


# Extract first letter from boy_names
boy_first_letter <- str_sub(boy_names, 1, 1)

# Tabulate occurrences of boy_first_letter
table(boy_first_letter)

# Extract the last letter in boy_names, then tabulate
boy_last_letter <- str_sub(boy_names, -1,-1)
table(boy_last_letter)
# Extract the first letter in girl_names, then tabulate
girl_first_letter <- str_sub(girl_names, 1,1)
table(girl_first_letter)
# Extract the last letter in girl_names, then tabulate
girl_last_letter <- str_sub(girl_names, -1,-1)
table(girl_last_letter)

# Look for pattern "zz" in boy_names
contains_zz <- str_detect(boy_names, fixed("zz"))

# Examine str() of contains_zz
str(contains_zz)

# How many names contain "zz"?
sum(contains_zz)

# Which names contain "zz"?
boy_names[contains_zz]

# Which rows in boy_df have names that contain "zz"?
boy_df[contains_zz, ]

# Find boy_names that contain "zz"
str_subset(boy_names, fixed("zz"))

# Find girl_names that contain "zz"
str_subset(girl_names, fixed("zz"))

# Find girl_names that contain "U"
starts_U <- str_subset(girl_names, fixed("U"))
starts_U

# Find girl_names that contain "U" and "z"
str_subset(starts_U, fixed("z"))

# Count occurrences of "a" in girl_names
number_as <- str_count(girl_names, fixed("a"))

# Count occurrences of "A" in girl_names
number_As <- str_count(girl_names, fixed("A"))

# Histograms of number_as and number_As
hist(number_as)
hist(number_As)

# Find total "a" + "A"
total_as <- number_as +number_As

# girl_names with more than 4 a's
girl_names[total_as>4]

both_names <- c("Box, George", "Cox, David")

# Split both_names into first_names and last_names
both_names_split <- str_split(both_names, fixed(", "), n=2, simplify=TRUE)
both_names_split
# Get first names
first_names <- both_names_split[,2]
first_names
# Get last names
last_names <- both_names_split[,1]

# Split lines into words
words <- str_split(lines, " ")

# Number of words per line
lapply(words, length)

# Number of characters in each word
word_lengths <- lapply(words,str_length)

# Average word length per line
lapply(word_lengths, mean)

# Some (fake) phone numbers
phone_numbers <- c("510-555-0123", "541-555-0167")

# Use str_replace() to replace "-" with " "
str_replace(phone_numbers, fixed("-"), "")

# Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers, fixed("-"), "")

# Turn phone numbers into the format xxx.xxx.xxxx
str_replace_all(phone_numbers, fixed("-"), ".")


# Find the number of nucleotides in each sequence
str_length(genes)
str(genes)
# Find the number of A's occur in each sequence

findc <- str_count(genes, fixed("A"))
findc

# Return the sequences that contain "TTTTTT"
findseq <- str_subset(genes, fixed("TTTTTT"))
findseq

# Replace all the "A"s in the sequences with a "_"
replacestr <- str_replace_all(genes, pattern=fixed("A"), replacement="_")
replacestr

# Define some full names
names <- c("Diana Prince", "Clark Kent")

# Split into first and last names
names_split <- str_split(names, fixed(" "), n=2, simplify=TRUE)
names_split
# Extract the first letter in the first name
abb_first <- str_sub(names_split[,1], 1,1)
abb_first
# Combine the first letter ". " and last name
combinestr <- str_c(abb_first, ".", names_split[,2])
combinestr

# Use all names in babynames_2014
all_names <- babynames_2014$name

# Get the last two letters of all_names
last_two_letters <- str_sub(all_names, -2,-1)

# Does the name end in "ee"?
ends_in_ee <- str_detect(last_two_letters, "ee")
ends_in_ee
# Extract rows and "sex" column
sex <- babynames_2014$sex[ends_in_ee]
sex
# Display result as a table
table(sex)

#requires rebus package

# Some strings to practice with
x <- c("cat", "coat", "scotland", "tic toc")

# Print END
END

# Run me
str_view(x, pattern = START %R% "c")

# Match the strings that start with "co"
str_view(x, pattern = START %R% "co")

# Match the strings that end with "at"
str_view(x, pattern = "at" %R% END)
# Match the string that is exactly "cat"
str_view(x, pattern = START %R% "cat" %R% END)
# Match two characters, where the second is a "t"
str_view(x, pattern = ANY_CHAR %R% "t")
# Match a "t" followed by any character
str_view(x, pattern = "t" %R% ANY_CHAR)
# Match two characters
str_view(x, pattern = ANY_CHAR %R% ANY_CHAR)
# Match a string with exactly three characters
str_view(x, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% END)

# Find part of name that matches pattern
part_with_q <- str_extract(boy_names, pattern)
# Get a table of counts
table(part_with_q)
# Did any names have the pattern more than once?
count_of_q <- str_count(boy_names, pattern)

# Get a table of counts
table(count_of_q)

# Which babies got these names?
with_q <- str_detect(boy_names, pattern)
with_q
# What fraction of babies got these names?
mean(with_q)

# Match Jeffrey or Geoffrey
whole_names <- or("Jeffrey", "Geoffrey")
str_view(boy_names, pattern = whole_names, match = TRUE)

# Match Jeffrey or Geoffrey, another way
common_ending <- or("Je" , "Geo") %R% "ffrey"
str_view(boy_names, pattern = common_ending, match = TRUE)

# Match with alternate endings
by_parts <- or("Je" , "Geo") %R% "ff" %R% or("ry", "ery", "rey", "erey")
str_view(boy_names, pattern = by_parts, match = TRUE)

# Match names that start with Cath or Kath
ckath <- START %R% or("Cath", "Kath")
str_view(girl_names, pattern = ckath, match = TRUE)

# Create character class containing vowels
vowels <- char_class("aeiouAEIOU")

# Print vowels
vowels

# See vowels in x with str_view()
str_view(x, pattern=vowels)

# See vowels in x with str_view_all()
str_view_all(x, pattern=vowels)

# Number of vowels in boy_names
num_vowels <- str_count(boy_names, pattern=vowels)

# Proportion of vowels in boy_names
name_length <- str_length(boy_names)

# Calc mean number of vowels
mean(num_vowels)

# Calc mean fraction of vowels per name
mean(num_vowels/ name_length)

# Vowels from last exercise
vowels <- char_class("aeiouAEIOU")

# See names with only vowels
str_view(boy_names,
         pattern = exactly(one_or_more(vowels)),
         match = TRUE)

# Use `negated_char_class()` for everything but vowels
not_vowels <- negated_char_class("aeiouAEIOU")

# See names with no vowels
str_view(boy_names,
         pattern = exactly(one_or_more(not_vowels)),
         match = TRUE)

# Create a three digit pattern
three_digits <- DGT %R% DGT %R% DGT

# Test it
str_view_all(contact, pattern = three_digits)

# Create a separator pattern
separator <- char_class('-', '.', '(', ')', ' ')

# Test it
str_view_all(contact, pattern = separator)

# Use these components
three_digits <- DGT %R% DGT %R% DGT
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")

# Create phone pattern
phone_pattern <- optional(OPEN_PAREN) %R%
  three_digits %R%
  zero_or_more(separator) %R%
  three_digits%R%
  zero_or_more(separator) %R%
  four_digits

# Test it
str_view_all(contact, pattern = phone_pattern)

# Use this pattern
three_digits <- DGT %R% DGT %R% DGT
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")
phone_pattern <- optional(OPEN_PAREN) %R%
  three_digits %R%
  zero_or_more(separator) %R%
  three_digits %R%
  zero_or_more(separator) %R%
  four_digits

# Extract phone numbers
str_extract(contact, pattern=phone_pattern)

# Extract ALL phone numbers
str_extract_all(contact, pattern=phone_pattern)

# Pattern to match one or two digits
age <- one_or_more(DGT)

# Test it
str_view(narratives, pattern = age)

# Use this pattern
age <- DGT %R% optional(DGT)

# Pattern to match units
unit <- optional(or("YO", "YR", "MO"))

# Test pattern with age then units
str_view(narratives, pattern = age %R% unit)

# Use these patterns
age <- DGT %R% optional(DGT)
unit <- optional(SPC) %R% or("YO", "YR", "MO")

# Pattern to match gender
gender <- optional(or("M", "F"))

# Test pattern with age then units then gender
str_view(narratives, pattern = age %R% unit %R% gender)

# Use these patterns
age <- DGT %R% optional(DGT)
unit <- optional(SPC) %R% or("YO", "YR", "MO")
gender <- optional(SPC) %R% or("M", "F")

# Extract age, unit, gender
str_extract(narratives, pattern=age %R% unit %R% gender)

#convert string to numeric
# age_gender, age, gender, unit are pre-defined
ls.str()
str(ls)
# Extract age and make numeric
as.numeric(str_extract(age_gender, pattern=age))

# Replace age and units with ""
genders <- str_remove(age_gender, pattern=age %R% unit)

# Replace extra spaces
str_remove_all(genders, pattern= one_or_more(SPC))

# Numeric ages, from previous step
ages_numeric <- as.numeric(str_extract(age_gender, age))

# Extract units
time_units <- str_extract(age_gender, pattern=unit)

# Extract first word character
time_units_clean <- str_extract(time_units, pattern= WRD)

# Turn ages in months to years
ifelse(time_units_clean == "Y", ages_numeric, ages_numeric/12)

# Capture parts between @ and . and after .
email <- capture(one_or_more(WRD)) %R%
  "@" %R% one_or_more(WRD) %R%
  DOT %R% one_or_more(WRD)

# Check match hasn't changed
str_view(hero_contacts, pattern=email)

# Pattern from previous step
email <- capture(one_or_more(WRD)) %R%
  "@" %R% capture(one_or_more(WRD)) %R%
  DOT %R% capture(one_or_more(WRD))

# Pull out match and captures
email_parts <- str_match(hero_contacts, pattern=email)
email_parts

# Save host
host <- email_parts[,3]
host

# View text containing phone numbers
contact

# Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R%
  capture(three_digits) %R% zero_or_more(separator) %R%
  capture(four_digits)

# Pull out the parts with str_match()
phone_numbers <- str_match(contact, pattern=phone_pattern)

# Put them back together
str_c(
  "(",
  phone_numbers[,2],
  ")",
  phone_numbers[,3],
  "-",
  phone_numbers[,4])

# narratives has been pre-defined
narratives

# Add capture() to get age, unit and sex
pattern <- capture(optional(DGT) %R% DGT) %R%
  optional(SPC) %R% capture(or("YO", "YR", "MO")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Pull out from narratives
str_match(narratives, pattern=pattern)
str_view(narratives, pattern=pattern)

# Edit to capture just Y and M in units
pattern2 <- capture(optional(DGT) %R% DGT) %R%
  optional(SPC) %R% capture(or("Y", "M")) %R% optional(or("O","R")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Check pattern
str_view(narratives, pattern2)

# Pull out pieces
str_match(narratives, pattern2)

# Names with three repeated letters
repeated_three_times <- capture(LOWER) %R% REF1 %R% REF1

# Test it
str_view(boy_names, pattern = repeated_three_times, match = TRUE)

# Names with a pair that reverses
pair_that_reverses <- capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1

# Test it
str_view(boy_names, pattern = pair_that_reverses, match = TRUE)

# Four letter palindrome names
four_letter_palindrome <- exactly(capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1)

# Test it
str_view(boy_names, pattern = four_letter_palindrome, match = TRUE)

# View text containing phone numbers
contact

# Replace digits with "X"
str_replace(contact, DGT, "X")

# Replace all digits with "X"
str_replace_all(contact, DGT, "X")

# Replace all digits with different symbol
str_replace_all(contact, DGT, c("X", ".", "*", "_"))

# Build pattern to match words ending in "ING"
pattern <- one_or_more(WRD) %R% "ING"
str_view(narratives, pattern)

# Test replacement
str_replace(narratives, capture(pattern),
            str_c("CARELESSLY", REF1, sep = " "))

# One adverb per narrative
adverbs_10 <- sample(adverbs, 10)

# Replace "***ing" with "adverb ***ly"
str_replace(narratives,
            capture(pattern),
            str_c(adverbs_10, REF1, sep = " "))

#Unicode characters
# Names with builtin accents
(tay_son_builtin <- c(
  "Nguy\u1ec5n Nh\u1ea1c",
  "Nguy\u1ec5n Hu\u1ec7",
  "Nguy\u1ec5n Quang To\u1ea3n"
))
tay_son_builtin
# Convert to separate accents
tay_son_separate <- stri_trans_nfd(tay_son_builtin)

# Verify that the string prints the same
tay_son_separate

# Match all accents
str_view_all(tay_son_separate, UP_DIACRITIC)

# tay_son_separate has been pre-defined
tay_son_separate

# View all the characters in tay_son_separate
str_view_all(tay_son_separate, ANY_CHAR)

# View all the graphemes in tay_son_separate
str_view_all(tay_son_separate, GRAPHEME)

# Combine the diacritics with their letters
tay_son_builtin <- stri_trans_nfc(tay_son_separate)
tay_son_builtin

# View all the graphemes in tay_son_builtin
str_view_all(tay_son_builtin, GRAPHEME)

#readlines example
# Read play in using stri_read_lines()
earnest <- stri_read_lines(earnest_file, earnest)

# Read play in using stri_read_lines()
earnest <- stri_read_lines(earnest_file)

# Detect start and end lines
start <- str_which(earnest, fixed("START OF THE PROJECT"))
end <- str_which(earnest, fixed("END OF THE PROJECT"))

# Get rid of gutenberg intro text
earnest_sub  <- earnest[(start + 1):(end - 1)]

# Read play in using stri_read_lines()
earnest <- stri_read_lines(earnest_file)

# Detect start and end lines
start <- str_which(earnest, fixed("START OF THE PROJECT"))
end <- str_which(earnest, fixed("END OF THE PROJECT"))

# Get rid of gutenberg intro text
earnest_sub  <- earnest[(start + 1):(end - 1)]

# Detect first act
lines_start <- str_which(earnest_sub, fixed("FIRST ACT"))

# Set up index
intro_line_index <- 1:(lines_start - 1)

# Split play into intro and play
intro_text <- earnest_sub[intro_line_index]
play_text <- earnest_sub[-intro_line_index]

# Read play in using stri_read_lines()
earnest <- stri_read_lines(earnest_file)

# Detect start and end lines
start <- str_which(earnest, fixed("START OF THE PROJECT"))
end <- str_which(earnest, fixed("END OF THE PROJECT"))

# Get rid of gutenberg intro text
earnest_sub  <- earnest[(start + 1):(end - 1)]

# Detect first act
lines_start <- str_which(earnest_sub, fixed("FIRST ACT"))

# Set up index
intro_line_index <- 1:(lines_start - 1)

# Split play into intro and play
intro_text <- earnest_sub[intro_line_index]
play_text <- earnest_sub[-intro_line_index]

# Take a look at the first 20 lines
writeLines(play_text[1:20])

# Pattern for start, word then .
pattern_1 <- START %R% one_or_more(WRD) %R%DOT

# Test pattern_1
str_view(play_lines, pattern_1, match = TRUE)
str_view(play_lines, pattern_1, match = FALSE)

# Pattern for start, capital, word then .
pattern_2 <- START %R% ascii_upper() %R% one_or_more(WRD) %R% DOT

# Test pattern_2
str_view(play_lines, pattern_2, match = TRUE)
str_view(play_lines, pattern_2, match = FALSE)

# Pattern from last step
pattern_2 <- START %R% ascii_upper() %R% one_or_more(WRD) %R% DOT

# Get subset of lines that match
lines <- str_subset(play_lines, pattern_2)

# Extract match from lines
who <- str_extract(lines, pattern_2)

# Let's see what we have
unique(who)

# Create vector of characters
characters <- c("Algernon", "Jack", "Lane", "Cecily", "Gwendolen", "Chasuble",
                "Merriman", "Lady Bracknell", "Miss Prism")

# Match start, then character name, then .
pattern_3 <- START %R% or1(characters) %R% DOT

# View matches of pattern_3
str_view(play_lines, pattern_3, match=TRUE)

# View non-matches of pattern_3
str_view(play_lines, pattern_3, match=FALSE)

# Variables from previous step
characters <- c("Algernon", "Jack", "Lane", "Cecily", "Gwendolen", "Chasuble",
                "Merriman", "Lady Bracknell", "Miss Prism")
pattern_3 <- START %R% or1(characters) %R% DOT

# Pull out matches
lines <- str_subset(play_lines, pattern_3)

# Extract match from lines
who <- str_extract(lines, pattern_3)

# Let's see what we have
unique(who)

# Count lines per character
table(who)

# catcidents has been pre-defined
head(catcidents)

# Construct pattern of DOG in boundaries
whole_dog_pattern <- whole_word("DOG")

# See matches to word DOG
str_view(catcidents, whole_dog_pattern, match=TRUE)

# From previous step
whole_dog_pattern <- whole_word("DOG")

# Transform catcidents to upper case
catcidents_upper <- str_to_upper(catcidents)

# View matches to word "DOG" again
str_view(catcidents_upper, whole_dog_pattern, match=TRUE)

# From previous steps
whole_dog_pattern <- whole_word("DOG")
catcidents_upper <- str_to_upper(catcidents)

# Which strings match?
has_dog <- str_detect(catcidents_upper, whole_dog_pattern)

# Pull out matching strings in original
catcidents[has_dog]

# View matches to "TRIP"
str_view(catcidents, pattern="TRIP", match=TRUE)

# Construct case insensitive pattern
trip_pattern <- regex("TRIP", ignore_case = TRUE)

# View case insensitive matches to "TRIP"
str_view(catcidents, trip_pattern, match=TRUE)

# From previous step
trip_pattern <- regex("TRIP", ignore_case = TRUE)

# Get subset of matches
trip <- str_subset(catcidents, trip_pattern)

# Extract matches
str_extract(trip, trip_pattern)

library(stringi)
catcidents
# Get first five catcidents
cat5 <- catcidents[1:5]

# Take a look at original
writeLines(cat5)

# Transform to title case
writeLines(str_to_title(cat5))

# Transform to title case with stringi
writeLines(stri_trans_totitle(cat5))

# Transform to sentence case with stringi
writeLines(stri_trans_totitle(cat5, type="sentence"))



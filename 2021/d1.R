

# Data prep ----
input = readLines(here::here("2021", "d1_input.txt"))
input = as.integer(input)

# Part 1
depth_increase = diff(input) > 0
sum(depth_increase)

# Part 2
input_window = data.table::frollsum(input, 3, align = "center")
depth_increase_window = diff(input_window) > 0
sum(depth_increase_window, na.rm = TRUE)

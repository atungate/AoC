# Data prep ----
# Data must be in clipboard from website
input = readLines(here::here("2021", "d2_input.txt"))

direction = sub("\\s+\\d+$", "", input)
value = as.integer(sub(".*\\s", "", input))

last = function(x) {
    x[length(x)]
}

# Ensuring sure "up" reduces depth when using cumsum
up_idx = direction == "up"
value[up_idx] = value[up_idx] * -1L 

# Creating an flag for "forward" values
forward_idx = direction == "forward"



# Part 1 ----
# Forward values
forward_vec = cumsum(value[forward_idx])

# Depth values
depth_vec = cumsum(value[!forward_idx])

# Multiplying the last numbers
last(forward_vec) * last(depth_vec)



# Part 2 ----

# Creating a vector for the aim 
aim_vec = value
aim_vec[forward_idx] = 0L # forward does not contribute to aim values
aim_vec = cumsum(aim_vec)

# Forward only vector to determine depth (multiply with aim later)
forward_only = value
forward_only[!forward_idx] = 0L 

depth_aim_vec = cumsum(forward_only * aim_vec) 


last(forward_vec) * last(depth_aim_vec)

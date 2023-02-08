# Generate a vector of random numbers
v <- rnorm(100)

# Find the arithmetic mean of all elements in the vector
mean_v <- mean(v)

# Select elements whose value is less than the mean
v_less_than_mean <- v[v < mean_v]

# Form a 10x10 matrix from v_less_than_mean
matrix_v <- matrix(v_less_than_mean, nrow = 10, ncol = 10)

# Display the even rows of the matrix
even_rows <- matrix_v[seq(2, nrow(matrix_v), by = 2), ]
print("Even Rows:")
print(even_rows)

# Display the odd rows of the matrix
odd_rows <- matrix_v[seq(1, nrow(matrix_v), by = 2), ]
print("Odd Rows:")
print(odd_rows)


# Call str on flowers to see the types of each column
str(flowers)

# Use unique() to see how many possible values Time takes
unique(flowers$Time)

# Build a formula to express Flowers as a function of Intensity and Time: fmla. Print it
(fmla <- as.formula("Flowers ~ Intensity + Time"))

# Use fmla and model.matrix to see how the data is represented for modeling
mmat <- model.matrix(fmla, flowers)

# Examine the first 20 lines of flowers
head(flowers, 20)

# Examine the first 20 lines of mmat
head(mmat, 20)
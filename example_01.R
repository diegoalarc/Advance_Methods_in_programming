# pip3 install tensorflow
# pip3 install keras

library(keras)
library(tensorflow)
library(magick)

# load cifar dataset
cifar10 <- dataset_cifar10()

# extract training and validation data
x_train <- cifar10$train$x
y_train <- cifar10$train$y
x_val <- cifar10$test$x
y_val <- cifar10$test$y

# plot an image
img <- image_read(x_train[1002,,,]/255)
plot(img)

# normalize value space to (0,1)
x_train <- x_train/255
x_val <- x_val/255

# categorize target arrays
# one-hot encoding
y_train <- to_categorical(y_train)
y_val <- to_categorical(y_val)

n.classes <- dim(y_val)[2]

# build architecture
model <- keras_model_sequential(
  layers = list(
    layer_conv_2d(filters = 32,
                  kernel_size = c(3,3),
                  input_shape = dim(x_train)[2:4],
                  activation = "relu",
                  kernel_constraint = constraint_maxnorm(3)),
    layer_flatten(),
    layer_dense(units = 512, activation = "relu", kernel_constraint = constraint_maxnorm(3)),
    layer_dropout(rate = 0.5),
    layer_dense(units = n.classes, activation = "softmax")
  ))

# model compilation
compile(model, 
        loss = "categorical_crossentropy",
        optimizer = optimizer_sgd(lr = 0.01, decay = 0.01/25),
        metrics = 'accuracy')

model.hist <- fit(model, x_train, y_train, validation_data = list(x_val, y_val),
                  epochs = 25, batch_size = 1440)

y_val_hat <- predict_classes(model, x_val)
library(keras)
library(tensorflow)
library(magick)

# load CIFAR data set
cifar10 <- dataset_cifar10()
#save(cifar10, file = "Downloads/ml/cifar.RData")
# 60.000 images, 10 classes (common objects)

# extract raining and validation input (x) and target (y) arrays
x_train <- cifar10$train$x
y_train <- cifar10$train$y
x_val <- cifar10$test$x
y_val <- cifar10$test$y

# have a look at one of the training images using magick
img <- image_read(x_train[5,,,]/255)
plot(img)

# z-score
x_train.mean <- mean(x_train)
x_train.std <- sd(x_train)
x_train <- (x_train-x_train.mean)/(x_train.std+1e-7)
x_val = (x_val-x_train.mean)/(x_train.std+1e-7)

# categorize target arrays
# (one-hot-encoding instaed of multi-value classes)
y_train <- to_categorical(y_train)
y_val <- to_categorical(y_val)

# capture number of classes
n.classes <- dim(y_val)[2]

# build a convolutional neural network
# as a sequential keras model
weight_decay <- 1e-4
model <- keras_model_sequential(
  layers = list(layer_conv_2d(filters = 32, # output dimensions of the first CNN layer (number of feature maps)
                              kernel_size = c(3,3), # the window size of the convolutio
                              padding = "same",
                              kernel_regularizer = regularizer_l2(weight_decay), #regularization preventing overfitting by scaling all weights (penalizing very high weights)
                              input_shape = dim(x_train)[2:4]), # input dimensions
                layer_activation_elu(),
                layer_batch_normalization(),
                layer_conv_2d(filters = 32, # output dimensions of the first CNN layer (number of feature maps)
                              kernel_size = c(3,3), # the window size of the convolutio
                              padding = "same",
                              kernel_regularizer = regularizer_l2(weight_decay)),
                layer_activation_elu(),
                layer_batch_normalization(),
                layer_max_pooling_2d(pool_size = c(2,2)),
                layer_dropout(rate = 0.2), # dropout to prevent overfitting (killing 20% of units and their connections)
                
                layer_conv_2d(filters = 64, # output dimensions of the first CNN layer (number of feature maps)
                              kernel_size = c(3,3), # the window size of the convolutio
                              padding = "same",
                              kernel_regularizer = regularizer_l2(weight_decay)),
                layer_activation_elu(), # Exponential Linear Unit or its widely known name ELU is a function that tend to converge cost to zero faster and produce more accurate results
                layer_batch_normalization(), # speeds up training by applying a transformation to maintain the mean activation close to 0 and the activation standard deviation close to 1 (internal covariate shift)
                layer_conv_2d(filters = 64, # output dimensions of the first CNN layer (number of feature maps)
                              kernel_size = c(3,3), # the window size of the convolutio
                              padding = "same",
                              kernel_regularizer = regularizer_l2(weight_decay)),
                layer_activation_elu(), 
                layer_batch_normalization(),
                layer_max_pooling_2d(pool_size = c(2,2)),
                layer_dropout(rate = 0.3),
                
                layer_conv_2d(filters = 128, # output dimensions of the first CNN layer (number of feature maps)
                              kernel_size = c(3,3), # the window size of the convolutio
                              padding = "same",
                              kernel_regularizer = regularizer_l2(weight_decay)),
                layer_activation_elu(),
                layer_batch_normalization(),
                layer_conv_2d(filters = 128, # output dimensions of the first CNN layer (number of feature maps)
                              kernel_size = c(3,3), # the window size of the convolutio
                              padding = "same",
                              kernel_regularizer = regularizer_l2(weight_decay)),
                layer_activation_elu(),
                layer_batch_normalization(),
                layer_max_pooling_2d(pool_size = c(2,2)),
                layer_dropout(rate = 0.3),
                
                layer_flatten(), # 'unstack' multidimensional tensors to get back to 'normal' dimensions
                layer_dense(units = n.classes, activation = "softmax") # apply softmax to add up probabilties towards 1 and to converge more quickly
  ))

# augmentate images using a generator
datagen <- image_data_generator(rotation_range = 15, width_shift_range = 0.1,
                                height_shift_range = 0.1, horizontal_flip = T)
datagen$fit(x_train)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.001, decay = 1e-6),
  metrics = 'accuracy'
)

# change learning rate with training epoch
lr_timed <- function(epoch){
  lr <- 0.001
  if(epoch > 75) lr <- 0.0005
  if(epoch > 100) lr <- 0.0003
  return(lr)
}

# run model using generator (will run in parallel and feed data to network)
batch_size <- 64
model.hist <- fit_generator(model, generator = datagen$flow(x_train, y_train, batch_size = batch_size),
                            steps_per_epoch = as.integer((dim(x_train)[1] %/% batch_size)), epochs = 125,
                            verbose = 1, validation_data = list(x_val, y_val)) #, callbacks = callback_learning_rate_scheduler(lr_timed))

y_val_hat <- predict_classes(model, x_val)

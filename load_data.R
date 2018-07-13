

database_path <- "C:\\Users\\Dell\\Documents\\UCR_TS_Archive_2015"

#create the function of loading data
load_data <- function(dirname, normalization = FALSE, slice_ratio = 1, idx_valid = 0){
    
    #generate random seed and file path
    rng <- set.seed(23455)
    
    train_file0 <- paste(dirname, "_TRAIN", sep = "")
    test_file1 <- paste(dirname, "_TEST", sep = "")
    
    train_file <- paste(database_path, dirname, sep = "\\")
    test_file <- paste(database_path, dirname, sep = "\\")
    
    #load train set
    data <- read.table(train_file, sep = ",")
    train_x <- data[,-1]
    train_y <- as.integer(data[, 1])
    
    len_data <- dim(train_data)[2]
    
    #restrict slice ratio when data length is too large
    if (len_data > 500) {
        if (slice_ratio > 0.98){
            slice_ratio <- slice_ratio
        } else {
            slice_ratio <- 0.98
        }
    }
    
    #shuffle for splitting train set and dataset
    n <- dim(train_data)[1]
    ind <- sample(1:n, size = n, replace = FALSE)
    
    #split train set into train set and validation set
    if (idx_valid > 0){
        
        index <- as.integer(0.2 * idx_valid * n) : as.integer(0.2 * idx_valid * n)   #这里跟原程序略有不同
        
        valid_x <- train_x[ind[index]]
        valid_y <- train_y[ind[index]]
        
        ind <- ind[-index]
        
        train_x <- train_x[ind]
        train_y <- train_y[ind]
    } else {
        train_x <- train_x[ind]
        train_y <- train_y[ind]
        
        valid_x <- train_x
        valid_y <- train_y
    }
    
    a1 <- slice_data(train_x, train_y, slice_ratio)
    b1 <- slice_data(valid_x, valid_y, slice_ratio)
    train_x <- a1[[1]]
    train_y <- a1[[2]]
    valid_x <- b1[[1]]
    valid_y <- b1[[2]]
    
    #shuffle again
    n <- dim(train_x)[1]
    ind <- sample(1:n, size = n, replace = FALSE)
    
    #load test set
    data <- read.table(test_file, sep = ",")
    test_x <- data[,-1]
    test_y <- as.integer(data[,1])
    
    a1 <- slice_data(test_x, test_y, slice_data)
    test_x <- a1[[1]]
    test_y <- a1[[2]]
    
    #z-normalization
    if (normalization == TRUE){
         mean_x <- apply(train_x, 1, mean)
         std <- apply(train_x, 1, sd)
         train_x <- (train_x - mean_x) / std_x
         valid_x <- (valid_x - mean_x) / std_x
         test_x <- (test_x - mean_x) / std_x
    }
    
    return(c(train_x, train_y, valid_x, valid_y, test_x, test_y, len_data, slice_ratio))
    
}



slice_data <- function(data_x, data_y, slice_ratio = 1){
    
    #return the sliced dataset
    if (slice_ratio == 1){
        return(c(data_x, data_y))
    }
    dim0 <- dim(data_x)
    n <- dim0[1]
    length <- dim0[2]
    length_sliced <- as.integer(length * slice_ratio)
    
    
    increase_num <- as.integer(length - length_sliced + 1) #if increase_num=5, it means one ori becomes 5 new instance
    n_sliced <- n * increase_num
    #print "*increase num", increase_num
    #print "*new length", n_sliced, "orginal len", n
    
    new_x <- array(0, dim = c(n_sliced, length_sliced))
    new_y <- array(0, dim = n_sliced)
    
    for (i in 1:n){
        for (j in 1:increase_num){
            new_x[(i - 1) * increase_num + j,] <- data_x[i, j : j + length_sliced]
            new_y[(i - 1) * increase_num + j] <- as.integer(data_y[i])
        }
    }
    
    return(c(new_x, new_y))
    
}


a_downsample <- function(data_x, sample_rate, offset = 0){
   
    dim0 <- dim(data_x)
    num <- dim0[1]
    length_x <- dim0[2]
    last_one <- 0
    if (length_x %% sample_rate > offset) last_one = 1
    new_length = as.integer(length_x / sample_rate) + last_one
    out_put <- array(0, dim = c(num, new_length))
    for (i in new_length){
        output[,i] <- array(data_x[,offset + sample_rate * (i-1)])
    }
    
    return(output)
    
}

a_movingavrg <- function(data_x, window_size){
    dim0 <- dim(data_x)
    num <- dim0[1]
    length_x <- dim0[2]
    output_len <- length_x - window_size + 1
    output <- array(0, dim = c(num, output_len))
    for (i in 1:output_len){
        output[, i] <- mean(data_x[, i : i + window_size], 2)
    }
    return(output)
}

movingavrg <- function(data_x, window_base, step_size, num){
    if (num == 0){
        return(c(NULL, array(NA)))  #跟原程序不同的地方
    }
    out <- a_movingavrg(data_x, window_base)
    data_lengths <- dim(out)[2]
    for (i in 1:(num - 1)){
        window_size <- window_base + step_size * i
        if (window_size > dim(data_x)[2]){
            next  #跳过本次迭代的剩余部分，并进行下一次的迭代
        }
    new_series <- a_movingavrg(data_x, window_size)
    data_lengths <- c(data_lengths, dim(new_series)[2])
    out <- cbind(out, new_series)
    return(c(out, data_lengths))    
    }
}

batch_movingavrg <- function(train, valid, test, window_base, step_size, num){
    a <- movingavrg(train, window_base, step_size, num)
    new_train <- a[1]
    lengths <- a[2]
    b <- movingavrg(valid, window_base, step_size, num)
    new_valid <- b[1]
    lengths <- b[2]
    c <- movingavrg(test, window_base, step_size, num)
    new_test <- c[1]
    lengths <- c[2]
    
    return(c(new_train, new_valid, new_test, lengths))
}

downsample <- function(data_x, base, step_size, num){
    if (num == 0){
        return(c(NULL, array(NA)))
    }
    out <- a_downsample(data_x, base, 0)
    data_lengths <- as.array(dim(out)[2])
    #for offset in range(1, base): #for the base case
    #    new_series <- a_downsample(data_x, base, offset)
    #    data_lengths <- c(data_lengths, dim(new_series)[2])
    #    out <- cbind(out, new_series)
    for( i in 1:(num-1)){
        sample_rate <- base + step_size * i
        if(sample_rate > dim(data_x)[2]){
            next
        }
        new_series <- a_downsample(date_x, sample_rate, offset = 0)
        data_lengths <- c(data_lengths, dim(new_series)[2])
        out <- cbind(out, new_series)
    }
    return(c(out, data_lengths))
}

batch_downsample<- function(train, valid, test, window_base, step_size, num){
    a <- downsample(train, window_base, step_size, num)
    new_train <- a[1]
    lengths <- a[2]
    b <- downsample(valid, window_base, step_size, num)
    new_valid <- b[1]
    lengths <- b[2]
    c <- downsample(test, window_base, step_size, num)
    new_test <- c[1]
    lengths <- c[2]
    
    return(c(new_train, new_valid, new_test, lengths))
}






















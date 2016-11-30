# handy TensorFlow functions
tf_extract_columns <- function (tensor, col_idx) {
  
  # coerce col_idx to a integer tensor
  col_idx <- as.integer(col_idx)
  stopifnot(is.vector(col_idx))
  col_idx <- tf$convert_to_tensor(col_idx)
  
  # flatten tensors
  x_shape = tf$shape(tensor)
  x_flat = tf$reshape(tensor, shape(-1))
  
  # extract columns in flat state
  i_flat = tf$reshape(tf$reshape(tf$range(0L, x_shape[0]) * x_shape[1],
                                 shape(-1, 1)) + col_idx, shape(-1))
  
  # reform to matrix
  tf$reshape(tf$gather(x_flat, i_flat),
             tf$pack(list(x_shape[0], tf$constant(-1L))))
  
}

# combine a list of placeholders and corresponding list of objects into a dict. 
# This essentially gets at py_dict (unexported in tensorflow) and adds some 
# checking
dictify <- function (placeholders, values) {
  
  if (!is.list(placeholders) | !is.list(values) )
  if (length(placeholders) != length(values))
    stop ('mismatching placeholders and objects')
  
  
  ph_names <- paste0('placeholder_', seq_along(placeholders))
  # dump placeholders here, by name
  for (i in seq_along(placeholders))
    assign(ph_names[i], placeholders[[i]])
  
  # put these as names on values
  names(values) <- ph_names
  
  # create the dict (should then find the placeholders named above)
  do.call(dict, values)
  
}
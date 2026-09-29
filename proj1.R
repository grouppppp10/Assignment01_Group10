
####StatProg_ass1


# Team member contributions:
# Ruojin zhang: 
# Jiahui Liu: 
# Jiayi Tan:

## Import files

#setwd("~/Desktop/statistical programming/p1/Assignment01_Group10")
#setwd("/Users/tianjiuye/ORDS/session1/optional/SP（R）/Practical-1")
music_raw <- readLines("palestrina.txt")
tail(head(music_raw,n=20),n=10)

#preprocess
preprocess <- function(music_raw) {
  
  # (a) Remove first 3 lines
  music_raw <- music_raw[-(1:3)]
  
  # (b) Remove lines beginning with "#"
  music_raw <- music_raw[!grepl("^#", music_raw)]
  
  # (c) and (d) Add "|" and "||"
  for (i in 1:length(music_raw)) {
    
    
    if (music_raw[i] == "") {
      next
    }
    
    if (i == length(music_raw) || music_raw[i + 1] == "") {
      music_raw[i] <- paste0(music_raw[i], " ||")
    } else {
      music_raw[i] <- paste0(music_raw[i], " |")
    }
  }
  
  # (e) Split into individual notes
  music_split <- strsplit(music_raw, " ")
  music_notes <- unlist(music_split)
  music_notes <- music_notes[music_notes != ""]
  return(music_notes)
}

music_clean <- preprocess(music_raw)

#check
tail(music_clean,n=10)

#step 5
# (a) Create a vector containing each unique symbol
notes <- unique(music_clean)

# (b) Convert each element in music_clean into its corresponding
tokens <- match(music_clean, notes)

# Check
length(tokens) == length(music_clean)

#step6
make_matrix <- function(tokens, end, mlag = 4) {
  n <- length(tokens)
  M <- matrix(NA,
              nrow = n - mlag,
              ncol = mlag + 1)
  
  for (i in 1:(n - mlag)) {
    M[i, ] <- tokens[i:(i + mlag)]
  }
  
  M <- M[rowSums(M[, 1:mlag] == end) == 0, ]
  
  return(M)
}

#check
end <- match("||", notes)
M <- make_matrix(tokens, end)
dim(M)
any(M[, 1:4] == end)
head(M)
tail(M)
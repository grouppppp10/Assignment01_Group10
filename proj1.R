
####StatProg_ass1


# Team member contributions:
# Ruojin zhang: 
# Jiahui Liu: 
# Jiayi Tan:

## Import files

#setwd("~/Desktop/statistical programming/p1/Assignment01_Group10")
#setwd("/Users/tianjiuye/ORDS/session1/optional/SP（R）/Practical-1")
setwd("/Users/yolaaaaaa/Documents/PGT/Statistical_Programming/Assignment01_Group10")
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
  # (a) Create the sequence matrix
  n <- length(tokens)
  M <- matrix(NA,
              nrow = n - mlag,
              ncol = mlag + 1)
  
  for (i in 1:(n - mlag)) {
    M[i, ] <- tokens[i:(i + mlag)]
  }
  
  # (b) Remove rows containing end in the history
  M <- M[rowSums(M[, 1:mlag] == end) == 0, ]
  
  return(M)
}

#check
M <- make_matrix(
  tokens = tokens,
  end = which(notes == "||"),
  mlag = 4
)

dim(M)
any(M[, 1:4] == which(notes == "||"))

#step7

# Sample the next token
# M: history columns + a final next-token column
# w[j]: mixture weight for order j 
next_note <- function(key, M, w = rep(1, ncol(M) - 1)) {
  mlag <- ncol(M) - 1
  nr <- nrow(M)
  key <- tail(key, mlag) # Keep only the latest tokens
  probability <- numeric(nr) # Accumulate sampling weights for each row
  if (any(w > 0)) {
    w <- w / max(w)
  }
  
  # Use suffixes of length 1 up to length(key)
  for (j in seq_len(length(key))) {
    
    # Select the current suffix and corresponding history columns
    current_key <- tail(key, j)
    mc <- mlag - j + 1
    
    # Count mismatches in each row; zero means a complete match
    ii <- colSums(
      !(t(M[, mc:mlag, drop = FALSE]) == current_key)
    )
    
    matched <- which(ii == 0)
    count <- length(matched)
    
    # No longer suffix can match if this suffix has no matches
    if (count == 0) {
      break
    }
    
    if (w[j] > 0) {
      probability[matched] <-
        probability[matched] + w[j] / count
    }
  }

  candidates <- which(probability > 0) # Select rows with positive weight 
  
  if (length(candidates) == 0) {
    
    # Fallback: sample uniformly across all rows
    candidates <- seq_len(nr)
    probs <- NULL
    
  } else {
    probs <- probability[candidates]
  }
  
  # A single candidate needs no random sampling
  if (length(candidates) == 1) {
    return(M[candidates, mlag + 1])
  }
  chosen <- sample(
    seq_along(candidates),
    size = 1,
    prob = probs
  )
  M[candidates[chosen], mlag + 1]
}

#check
mlag <- ncol(M) - 1
key <- as.vector(M[1, seq_len(mlag)])
mc <- mlag - length(key) + 1

next_note(key, M)

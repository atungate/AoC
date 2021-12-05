
# Data prep ----
input = readLines(here::here("2021", "d4_input.txt"))

# Getting the bingo cards and numbers ready
numbers = as.integer(strsplit(input[1], ",", fixed = TRUE)[[1]])
cards = input[-c(1:2)]
cards = lapply(cards, function(x) as.integer(strsplit(x, "\\s+")[[1]]))
cards = cards[lengths(cards) != 0]
cards = split(cards, rep(1:100, each = 5))
cards = lapply(cards, function(x) do.call(rbind, x))


# Part 1 ----
# For each number, fill-in every bingo card. break loop if bingo
bingo_simulator = function(cards, numbers, winner = c("first", "last")) {
    
    bingo_time = function(m) {
        any(.rowSums(m, 5L, 5L) == 5) | any(.colSums(m, 5L, 5L) == 5)
    }
    
    cards_idx = lapply(cards, `==`, 999) # Creating list to keep track of bingo numbers
    BINGO_idx = rep(FALSE, length(cards)) # to avoid running bingo_time() on already-bingo cards
    
    for(num in numbers) {
        for(card_n in seq_along(cards)) {
            cards_idx[[card_n]] = pmax(cards_idx[[card_n]], cards[[card_n]] == num) # keeping track of called numbers
            
            if(winner == "first") {
                BINGO = bingo_time(cards_idx[[card_n]])
            } else {
                BINGO_idx[!BINGO_idx] = vapply(cards_idx[!BINGO_idx], bingo_time, logical(1L), USE.NAMES = FALSE)
                BINGO = all(BINGO_idx)
            }

            if(BINGO) {
                print("BINGO")
                winning_card = list(card = cards[[card_n]],
                                    idx = cards_idx[[card_n]],
                                    number = num)
                break
            }  
        }
        if(BINGO) break
    }
    winning_card
}
winning_card = bingo_simulator(cards, numbers, winner = "first")
uncalled_nums = unlist(Map(`[`, winning_card$card, !winning_card$idx))
sum(uncalled_nums) * winning_card$number


# Part 2 ----
last_winning_card = bingo_simulator(cards, numbers, winner = "last")
last_uncalled_nums = unlist(Map(`[`, last_winning_card$card, !last_winning_card$idx))
sum(last_uncalled_nums) * last_winning_card$number





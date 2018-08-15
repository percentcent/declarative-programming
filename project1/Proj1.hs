--Authored by Peishan Li, student ID 905508
--The purpose of the project is to implement the performer part of the ChordProbe game.
--ChordProbe is a two-player logical guessing game, in which the composer selects a three-pitch target chord,and the performer begins to choose a chord as a guess repeatedly with composer's response.
--The code aims to help the performer make good decision in the game, with less guesses needed to find the target chord.

module Proj1 (initialGuess, nextGuess, filterState, count1, fcom, countElse, dropElem, fdelete, intialState, combinations, guessStrategy, calSim, calExpe, GameState) where
import Data.List

--The type GameState is to store the remaining list of possible targets. The type of GameState is a list of three-pitch chord.
--And each chord is a list of three String which represent pitches, like "A1","B2".
type GameState = [[String]]

--The function intialGuess provides a hard-code first guess and the initial GameState with total 1330 candidate chords. 
--It call funcition gameState to implement the initialisation.
initialGuess :: ([String],GameState)
initialGuess = 
	let firstGuess = ["A1","B1","C2"] in
	let gameState = intialState ['A'..'G'] ['1'..'3'] in
	(firstGuess,gameState)

--The function provides every next guess(chord) and updates the latest GaseState(remaining list of possible targets)
--with the input of composer's response. It calls function newState and newGuess to implement detailed strategies.
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (lastGuess,state) (right,rNote,rOctave) =
	let newState = filterState lastGuess state right rNote rOctave in
	let newGuess = guessStrategy newState newState [] in 
	(newGuess,newState)

--fileterState aims to employ the response from composer to filter elements that are inconsistent with any response 
--we have received from previous guesses. filterState serves like the filter function, remove some elements in the list and return an updated list.
filterState :: [String] -> GameState -> Int -> Int -> Int -> GameState
filterState a [] r1 r2 r3 =[]
filterState a (x:xs) r1 r2 r3 =
	if fcom a x r1 r2 r3
		then x:(filterState a xs r1 r2 r3)
	else filterState a xs r1 r2 r3

--count1 function compares two lists and returns the number of common elements(exact the same, like "A1" and "A1") in those two lists
count1 :: (Eq a) => [a] -> [a] -> Int -> Int
count1 list [] num = num
count1 list (x:xs) num =
	if x `elem` list 
		then count1 list xs (num+1)
	else count1 list xs num

--fcom is a function to compare whether the difference between two chords is consistent with the previous response.
--It compares the three pitches in two chords one by one. If consitent, return True.
fcom :: [String] -> [String] -> Int -> Int -> Int -> Bool
fcom aList bList r1 r2 r3 =
	let
		a1 = count1 aList bList 0 
	in
	case a1 == r1 of False -> False
	                 True ->
	                   let
	                      (a2,a3) = countElse aList bList
	                   in
	                   if (a2 == r2 && a3 == r3) then True else False

--countElse and count1 work together, aiming to obtain the multiple occurrences in counting correct notes and octaves. 
--countElse the tuple of notes correctness and octaves correctness.
--getNode and getOctave functions purpose to get the Node or the Octove from a specific Chord(String,like"A1")
--count function is to calculate notes correctness and octaves correctness, and fdelete function guarantees to exclude the exact Chord matches.
countElse :: [String] -> [String] -> (Int,Int)
countElse aList bList =
	let
	    (aNext,bNext)=fdelete aList bList
	    getNode [] = []
	    getNode (y:ys) = (head y):getNode ys
	    getOctave [] = []
	    getOctave (z:zs) = (last z):getOctave zs
	    lNote = getNode aNext
	    rNote = getNode bNext
	    lOctave = getOctave aNext
	    rOctave = getOctave bNext
	    count a [] num = num
	    count a (b:bs) num = 
	    	if b `elem` a then count (dropElem a b) bs (num+1) else count a bs num
	    a2 = count lNote rNote 0 
	    a3 = count lOctave rOctave 0
	in
	(a2,a3)


--dropElem implements a sub-function of countElse, in order to drop a matched element in a list.
dropElem ::(Eq a) => [a] -> a -> [a]
dropElem [] a = []
dropElem (x:xs) a =
	if x == a then xs else x:(dropElem xs a)

--fdelete function guarantees to exclude the exact Chord matches.
fdelete :: (Eq a) => [a] -> [a] -> ([a],[a])
fdelete x y =
	let
	    drop list []=[]
	    drop list (b:bs) =
		    if b `elem` list
			    then drop list bs
		    else b:drop list bs
	in
	((drop y x),(drop x y))

--initialState provides the initial GameState with two main step. Firstly, generate 21 Chords. secondly, generate gamestates with combinations.
intialState :: String -> String -> GameState
intialState aList bList =
	let 
		construct (x:xs) [] = []
		construct (x:xs) (e:es) = (subconstruct (x:xs) e)++(construct (x:xs) es)
		subconstruct [] e = []
		subconstruct (x:xs) e = [x,e]:(subconstruct xs e)
		candidate = construct aList bList
	in
	combinations 3 candidate 

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

--Implement the guess strategy, choosing each guess so that it is most likely to leave a small remaining list of possible targets.
guessStrategy :: [[String]] -> GameState -> [(Double,[String])] -> [String]
guessStrategy [] list rankList = 
	let 
        sortRank = sort rankList
        next = (snd . head) sortRank
	in
	next
guessStrategy (x:xs) list rankList =
	let
		simList = map (calSim x) list 
		d = (calExpe . sort) simList
		newRankList = rankList ++ [(d,x)]
	in
	guessStrategy xs list newRankList

--combine function count1 and countElse explained above to return multiple occurrences in counting correct notes and octaves between 2 chords.
calSim :: [String] -> [String] -> (Int,Int,Int)
calSim aList bList =
	let
		a = count1 aList bList 0
		(b,c) = countElse aList bList
	in
	(a,b,c)

--calculate the Expectation of number of remaining candidates.
calExpe :: [(Int,Int,Int)] -> Double
calExpe list = 
	let
		l = length list
		sum1 p [] count num = num+(count*count)
		sum1 p (x:xs) count num = 
			if x /= p 
				then sum1 x xs 1 num+(count*count)
			else sum1 x xs (count+1) num
		a = sum1 (head list) list 0 0
		answer = (fromIntegral a)/(fromIntegral l)
	in
	answer



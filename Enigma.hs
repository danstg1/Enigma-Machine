{-
ENIGMA PROJECT BY DANIEL ST-GALLAY, ASSISTED CODE FROM DCS - UNIVERSITY OF SHEFFIELD
A functional programming representation of the enigma machine, including encoding any message,
aswell as being able to find the longest menu given a crib and a string
aswell as being able to break an enigma code by deducing the machines settings
USES DOUBLE STEPPING FOR THE STECKERING
FIRST ARGUMENT IN ENIGMA CREATION IS THE FAR RIGHT ROTOR

> SIMPLE ENIGMA ROTOR1, ROTOR2, ROTOR3 0,0,25 = > LR = ROTOR3  (0) , MR = ROTOR2 (0), RR = ROTOR 1 (25)

--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List
  import Debug.Trace
  import System.Random
  import Data.Array.IO
  import Control.Monad
  -- add extra imports if needed, but only standard library functions!



{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char,Char)]
  type Offsets = (Int,Int,Int)
  type Stecker = [(Char,Char)] 
  

  {- USING THE SET UP WHERE THE FIRST ROTOR SPECIFICED, LABELLED ROTOR 1, IS THE ROTOR ON THE FAR RIGHT OF THE ENIGMA-}
  data Enigma = SimpleEnigma {_r1::Rotor, _r2::Rotor, _r3::Rotor, _reflector::Reflector, _offsets::Offsets}
                | SteckeredEnigma {_r1::Rotor, _r2::Rotor, _r3::Rotor, _reflector::Reflector,_offsets::Offsets,_steckerboard:: Stecker}

{-Functions to get different parts of a list -}
  getfst (a,_,_) = a
  getmid (_,a,_) = a
  getlst (_,_,a) =a

  breakDownLetter :: String -> [(Char,Int)]
  {- Takes in a string, and changes it to a list of (CHARACTERS, POSITION IN STRING)-}
  breakDownLetter input = zip input [0..length(input)]


  isSimple:: Enigma -> Bool
  {- A function to detect wether an enigma is simple or steckerered-}
  isSimple(SimpleEnigma _r1 _r2 _r3 _reflector _offsets) = True

  isSimple(SteckeredEnigma _r1 _r2 _r3 _reflector _offsets _steckerboard) = False



  findLetter :: Int -> Rotor -> Enigma -> Int
  {- A function tot find a letter on a rotor-}
  findLetter og_position rotor enigma = ((mod ((head(elemIndices letter (fst rotor)))) 26))
    where letter = chr(og_position+65)



  encodeMessage :: String -> Enigma -> String
  {- The main head of the encoding, mainly just has other functions applied to it -}
  encodeMessage input enigma = (applyStecker (map encodeLetter brokenDown) (enigma)) where

      brokenDown = (breakDownLetter (map (toUpper) (input))) where

      encodeLetter inputLetter = chr((mod (alignCharacters r1ReEnc 0 first_rotor_offset) 26) +65) where
  
        
        r1ReEnc = findLetter (r1bInput) (_r1 enigma) enigma
        r1bInput = (alignCharacters r2ReEnc first_rotor_offset second_rotor_offset)


        r2ReEnc = findLetter (r2bInput) (_r2 enigma) enigma
        r2bInput = (alignCharacters r3ReEnc second_rotor_offset third_rotor_offsets)

      
        r3ReEnc = findLetter (r3bInput) (_r3 enigma) enigma
        r3bInput = (alignCharacters (reflected) third_rotor_offsets 0)


        reflected = alphaPos (applyReflector (chr ((mod (alignCharacters r3Enc 0 third_rotor_offsets) 26)+65)) reflectorB) -- 0 Into Align Characters as no offset is applied for reflector
        --Also must be converted to and from a letter to search through the reflector list


        r3Enc = applyRotor (r3aInput) (snd inputLetter) 3 (_offsets enigma) (_r3 enigma) (enigma)
        r3aInput = (alignCharacters r2Enc third_rotor_offsets second_rotor_offset)
        
        
        r2Enc = applyRotor (r2aInput) (snd inputLetter) 2 (_offsets enigma) (_r2 enigma) (enigma) -- Apply the encryption, getting the next letter
        r2aInput = (alignCharacters r1Enc second_rotor_offset first_rotor_offset) -- Align the characters for the input , default is A->A
        

        r1Enc = applyRotor (alphaPos (switchBoard)) (snd(inputLetter)) 1 (_offsets enigma)(_r1 enigma)(enigma)

        switchBoard
          | (isSimple enigma) =(fst(inputLetter))
          | otherwise = onSteckerBoard (fst(inputLetter)) (_steckerboard enigma)


        first_rotor_offset = getlst(appliedOffsets)
        second_rotor_offset = getmid(appliedOffsets) --These are set here to stop repeated calls to the 'get' functions
        third_rotor_offsets = getfst(appliedOffsets)
        appliedOffsets = calculateOffset enigma (snd(inputLetter))
  

  alignCharacters :: Int -> Int -> Int -> Int
  {- Alligns all the characters to their correct position, only if the rotors are incremented-}
  alignCharacters char positive negative = mod ((char) + (mod positive 26) - (mod negative 26)) 26 -- Mod 26 is applied to ensure that the offset never exceeds the length of alphabet



  applyStecker :: String -> Enigma -> String
  {- Apply steckereing to all the inputted string -}
  applyStecker input enigma
    | (isSimple(enigma)) = input
    | otherwise = map (\inputLetter -> onSteckerBoard (inputLetter) (_steckerboard enigma) ) input



  calculateOffset :: Enigma -> Int -> Offsets
  {- Calculate the offset of rotors using the enigma and amount of button presses-}
  {- Also includes double stepping -}

  calculateOffset enigma presses =(of3 + offset2inc, of2 +of2add+offset2inc+offset1inc, of1) where
    of3
      | of2+of2add < (snd(_r2 enigma)) = (getfst(_offsets enigma))
      | of2+of2add == (snd(_r2 enigma)) = (getfst(_offsets enigma)) + 1
      | otherwise = (getfst(_offsets enigma) + (div (((of2+of2add) - (snd(_r2 enigma)))) 26)) + 1

    of2add --This occurs when the rotor increments itself due to being in the correct position (Double stecker)
      | of2 < (snd(_r2 enigma)-1) =0
      | of2 ==  (snd(_r2 enigma)-1) = 1
      | otherwise = (div (((of2) - (snd(_r2 enigma)))) 26) + 1

    of2
      | ((getlst(_offsets enigma)+presses+1) < (snd(_r1 enigma)))= (getmid(_offsets enigma))
      | of1 == (snd(_r1 enigma)) = (getmid(_offsets enigma)) +1
      | otherwise = (getmid(_offsets enigma) + (div (((of1) - (snd(_r1 enigma)))) 26)) +1

    of1 = (getlst(_offsets enigma)+presses+1)

    offset2inc -- Used to allign rotors as double steckering will mess this up otherwise
      | (getmid(_offsets enigma) < (snd(_r2 enigma))) = 0
      | otherwise = -1


    offset1inc
      | (getlst(_offsets enigma) < (snd(_r1 enigma))) = 0
      | otherwise = -1




  applyRotor :: Int -> Int ->Int -> Offsets -> Rotor -> Enigma -> Int
  {- Apply the rotor jump to the letter, using the positional arguments in the alphabet -}
  applyRotor og_position presses rotnum offset rotor enigma = alphaPos letter where
      letter
        | rotnum == 1 = (fst rotor) !! ((mod ((og_position) + getlst(z)) 26))
        | rotnum == 2 = (fst rotor) !! ((mod ((og_position)) 26))
        | rotnum == 3 = (fst rotor) !! ((mod ((og_position)) 26))
      z = calculateOffset (enigma) (presses)

  applyReflector ::  Char -> Reflector -> Char
  {- Apply the reflector letter-}
  applyReflector og_letter reflector
    |fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) reflector)) == og_letter = snd(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) reflector))
    |otherwise = fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) reflector))
    {-Filter The List Out, If The Original Letter Is The First Letter In The Tuple, Filter That Aswell, If Not Filter The Second-}

  getStecker :: Char -> Stecker -> Char
  {- Get a characters steckered counterpart -}
  getStecker og_letter steckerboard
    | fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard)) == og_letter = snd(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard))
    | snd(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard)) == og_letter = fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard))
    | otherwise = og_letter

  onSteckerBoard:: Char->Stecker->Char
  {- Used to check if a character is on the stckerboard -}
  onSteckerBoard character steckerboard
    |((length ((filter (\letter-> (fst(letter))/= character)) steckerboard)) /= (length steckerboard) || (length ((filter (\letter-> (snd(letter))/= character)) (steckerboard))) /= (length steckerboard)) = getStecker character steckerboard
    |otherwise=character



{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] -- the supplied type is not correct; fix it!
  type Crib = [(Char, Char)] -- the supplied type is not correct; fix it!

  longestMenu :: Crib -> Menu
  {- Main call program-}
  longestMenu crib = getLongest [] (map (\x -> getlst(usingForwardFind x crib)) [0..length(crib)*2])


  findLetterPos :: (Char,Char) -> Crib -> Int 
  {- Calculates the position of any character on a crib and returns the int position-}
  findLetterPos letter crib  =(head ((elemIndices (head(filter (\pair -> fst(pair)==fst(letter)) ((filter (\pair -> snd(pair)==snd(letter))) crib))) crib)))



  forwardFind :: Crib -> (Char,Char,[Int]) -> Crib  -> Int -> (Char,Char, [Int])
  {-Recurisvly attempts to create the longest chain possible for Menu-}
  forwardFind og_crib letter crib counter
          |letterInOriginal (snd(new_letters)) crib== False = (fst(new_letters) , snd(new_letters), (getlst letter)++ [(findLetterPos new_letters og_crib)])
          |otherwise = forwardFind og_crib (fst(new_letters) , snd(new_letters), (getlst letter)++[(findLetterPos new_letters og_crib)]) (delete ((getfst(letter)), (getmid(letter))) crib) ((counter^2)+1)
          where new_letters = (randomElement counter ((filter (\pair -> fst(pair)==getmid(letter)) crib)))



  letterInOriginal :: Char -> Crib -> Bool
  {- Checks if the letter of the encrypted message is in the crib-}
  letterInOriginal letter crib
          | length (filter (\pair -> fst(pair)==letter) crib) > 0 = True
          | otherwise = False



  usingForwardFind :: Int -> Crib -> (Char,Char,[Int])
  {-Applies forward find to every letter of the crib-}
  usingForwardFind startNum crib = (findLongest ('A','B',[]) (map (\letterx -> getLetterLst letterx) crib)) where
          getLetterLst x
                  | (letterInOriginal (snd(x)) (crib)== False) = ('A','A',[0])
                  | otherwise = forwardFind crib (fst x, snd x, [findLetterPos (x) crib]) crib startNum



  findLongest :: (Char,Char,[Int]) -> [(Char,Char, [Int])] -> (Char,Char, [Int])
  {- Gets the longest menu from a select set of letters-}
  findLongest  top [] = top
  findLongest top (new:rest)
    | length(getlst(top)) < length(getlst(new)) = findLongest new rest
    | otherwise = findLongest top rest
  
  getLongest :: [Int] -> [[Int]]-> [Int]
  {-Gets the longest list from a 2D array-}
  getLongest  top [] = top
  getLongest top (new:rest)
    | length(top) < length(new) = getLongest new rest
    | otherwise = getLongest top rest


  randomElement :: Int -> [(Char,Char)] -> (Char,Char)
  {-Used to return a random element-}
  randomElement _ [] = ('A','A')
  randomElement seed inputLst = inputLst !! rand where
    lengthOfLst = length inputLst
    (rand, _) = randomR (0,(lengthOfLst-1)) generator where
          generator = mkStdGen seed





{- Part 3: Simulating the Bombe -}
  
  letters = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','W','X','Y','Z']
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  {- Main call program -}
  breakEnigma crib = (checkOffset (enigma) (crib) 0 (longestMenu crib)) where 
    enigma = (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0)) --THE DEFAULT TEST ENIGMA , CHANGE TO CHECK DIFFERENT ENIGMAS


  checkEveryLetter :: Crib -> Enigma -> Menu -> [Bool]
  {-Goes through every letter in the alphabet, using each one as the starting stecker-}
  checkEveryLetter crib enigma menu = (map (\startStecker -> checkContradictions menu enigma startStecker crib) letters)

  getFinalResult :: Crib -> Enigma -> [Int] -> (Offsets, Stecker)
  {-Used when correct increments are found, to locate the correct stecker-}
  getFinalResult _ _ [] = ((0,0,0),[])
  getFinalResult crib enigma correctPositions = ( offsetIO , nub(map sortTuples ((map (\pair -> createLink (removeFirst menu) (head menu) (enigma) (crib) [(fst(crib !! head(menu)) , pair)]) letters) !! (head(correctPositions))))) where
    menu = longestMenu (crib)
    offsetIO = (mod (getfst(offset)) 26, mod (getmid(offset)) 26, mod (getlst(offset)) 26)
    offset = _offsets enigma


  checkResults :: (Offsets, Stecker) -> Enigma -> Crib -> Bool
  {-This checks the results, as some steckers may be valid and still not work-}
  checkResults results oldenimga crib
    | (encodeMessage (fst(unzip(crib))) newEnigma) == snd(unzip(crib)) = True
    | otherwise = False
    where
      newEnigma = SteckeredEnigma (_r1 oldenimga) (_r2 oldenimga) (_r3 oldenimga) (_reflector oldenimga) (fst(results)) (snd(results))


  checkOffset:: Enigma -> Crib -> Int -> Menu -> Maybe (Offsets, Stecker)
  {- This will check offsets, and if they do not work, increment them and move on-}


  checkOffset _ _ 17576 _ = Nothing
  checkOffset enigma crib counter menu
    | (and checkList) == True = checkOffset newEnigma crib (counter+1) menu
    | (checkResults finalResult newEnigma crib) == True = Just finalResult --If the result is wrong, then continue
    | otherwise = checkOffset newEnigma crib (counter+1) menu
    where
      finalResult = (getFinalResult crib newEnigma (elemIndices False checkList))
      checkList = (checkEveryLetter crib newEnigma menu)
      newEnigma  = resetOffsets counter enigma

  checkContradictions :: Menu -> Enigma -> Char -> Crib -> Bool
  {- Checks if a contracdiction exists in a stecker-}
  checkContradictions menu enigma testLetter crib = testConflict (nub (map sortTuples (createLink (removeFirst menu) (head menu) (enigma) (crib) [starting_stecker]))) where
    starting_stecker = (fst(start_letter), testLetter)
    start_letter = crib !! head(menu)

  resetOffsets :: Int -> Enigma -> Enigma
  {- The function to apply different offsets -}
  resetOffsets counter enigma = (SimpleEnigma (_r1 enigma) (_r2 enigma) (_r3 enigma) (_reflector enigma) (mod (floor (fromIntegral counter/ (26*26))) 26, mod (floor (fromIntegral counter/ 26)) 26, mod counter 26))

  cheatFunction :: Char -> Int -> String
  {- This function takes in a letter and its position in the crib, then creates that amount of fake A's behind the letter-}
  cheatFunction start_letter length = (map (\letter -> chr (letter+65)) [0..length-1]) ++ [start_letter]


  createLink :: Menu -> Int -> Enigma -> Crib -> [(Char,Char)] -> [(Char,Char)]
  {- Recurisvley creates the chain of steckers by searching menu -}
  createLink [] _ _ _ chain = chain
  createLink menu position enigma crib chainLink = createLink (removeFirst menu) (head menu) (enigma) (crib) (chainLink ++ [(last(encodeMessage inputString (enigma)),snd(crib !! position))] ++ [(snd(crib !! position), last(encodeMessage inputString (enigma)))]) where
    inputString = cheatFunction (snd(last(chainLink))) position


  removeFirst :: [a] -> [a]
  {-Removes the first item from a list -}
  removeFirst = \inputLst ->
      case inputLst of
          [] -> []
          x:xs -> xs

  sortTuples :: (Char,Char) -> (Char,Char) 
  {-Sorts all the tuples so that the first letter in the alphabet is first-}
  sortTuples (x,y)
    | alphaPos x < alphaPos y = (x,y)
    | otherwise = (y,x) 


  generateString :: [(Char,Char)] -> String
  {-Generates the string from a crib-}
  generateString letters = fst(unzip letters) ++ snd(unzip letters)

  testConflict :: [(Char,Char)] -> Bool
  {-Tests the contradictions on all the characters-}
  testConflict letters
    | length (nub concatString) /= length (concatString) = True
    | otherwise = False
    where concatString = generateString letters

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)
  rotorLst = [rotor1, rotor2, rotor3, rotor4, rotor5]
  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

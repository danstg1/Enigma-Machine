
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

  type Rotor = (String, Int) -- the supplied type is not correct; fix it!
  type Reflector = [(Char,Char)] -- the supplied type is not correct; fix it!
  type Offsets = (Int,Int,Int) -- the supplied type is not correct; fix it!
  type Stecker = [(Char,Char)] -- the supplied type is not correct; fix it!
  
  data Enigma = SimpleEnigma {_r1::Rotor, _r2::Rotor, _r3::Rotor, _reflector::Reflector, _offsets::Offsets}
                | SteckeredEnigma {_r1::Rotor, _r2::Rotor, _r3::Rotor, _reflector::Reflector,_offsets::Offsets,_steckerboard:: Stecker}
  getfst (a,_,_) = a
  getmid (_,a,_) = a
  getlst (_,_,a) =a

  breakDownLetter :: String -> [(Char,Int)]
  breakDownLetter input = zip input [0..length(input)]

  isSimple:: Enigma -> Bool
  isSimple(SimpleEnigma _r1 _r2 _r3 _reflector _offsets) = True

  isSimple(SteckeredEnigma _r1 _r2 _r3 _reflector _offsets _steckerboard) = False



  findLetter :: Int -> Rotor -> Int -> Int -> Enigma -> Int
  findLetter og_position rotor rotnum presses enigma
    | rotnum == 1 = ((mod ((head(elemIndices j (fst rotor)))) 26))
    | rotnum == 2 = ((mod ((head(elemIndices j (fst rotor))) + getmid(z) - getmid(_offsets enigma)) 26))
    | rotnum == 3 = ((mod ((head(elemIndices j (fst rotor)))  + getfst(z) - getfst(_offsets enigma)) 26))
    | otherwise = 0
    where z = calculateOffset enigma presses
          j = chr(og_position+65)



  encodeMessage :: String -> Enigma -> String
  encodeMessage input enigma = applyStecker (map encodeLetter brokenDown) (enigma) where

      brokenDown = breakDownLetter (map (toUpper) (input)) where

      encodeLetter inputLetter = chr((mod (alignCharacters r1ReEnc 0 first_rotor_offset) 26) +65) where
  
        
        r1ReEnc = findLetter (r1bInput) (_r1 enigma) (1) (snd(inputLetter)) enigma
        r1bInput = (alignCharacters r2ReEnc first_rotor_offset second_rotor_offset)


        r2ReEnc = findLetter (r2bInput) (_r2 enigma) (2) (snd(inputLetter)) enigma
        r2bInput = (alignCharacters r3ReEnc second_rotor_offset third_rotor_offsets)


        r3ReEnc = findLetter (r3bInput) (_r3 enigma) (3) (snd(inputLetter)) enigma
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
        second_rotor_offset = getmid(appliedOffsets)
        third_rotor_offsets = getfst(appliedOffsets)
        appliedOffsets = calculateOffset enigma (snd(inputLetter))
  

  alignCharacters :: Int -> Int -> Int -> Int
  alignCharacters char positive negative = mod ((char) + (mod positive 26) - (mod negative 26)) 26 -- Mod 26 is applied to ensure that the offset never exceeds the length of alphabet



  applyStecker :: String -> Enigma -> String
  applyStecker input enigma
    | (isSimple(enigma)) = input
    | otherwise = map (\inputLetter -> onSteckerBoard (inputLetter) (_steckerboard enigma) ) input


  calculateOffset :: Enigma -> Int -> Offsets
  calculateOffset enigma presses = (of3, of2, of1) where
    of1 = (getlst(_offsets enigma)+presses)
    of2 = (getmid(_offsets enigma) + (floor (fromIntegral presses/ fromIntegral (snd (_r1 enigma)))))
    of3 = (getfst(_offsets enigma)+ (floor (fromIntegral (floor (fromIntegral presses/ fromIntegral (snd (_r1 enigma))))/ fromIntegral (snd (_r2 enigma)))))



  applyRotor :: Int -> Int ->Int -> Offsets -> Rotor -> Enigma -> Int
  applyRotor og_position presses rotnum offset rotor enigma = alphaPos letter where
      letter
        | rotnum == 1 = (fst rotor) !! ((mod ((og_position) + getlst(z)) 26))
        | rotnum == 2 = (fst rotor) !! ((mod ((og_position)) 26))
        | rotnum == 3 = (fst rotor) !! ((mod ((og_position)) 26))
      z = calculateOffset (enigma) (presses)

  applyReflector ::  Char -> Reflector -> Char
  applyReflector og_letter reflector
    |fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) reflector)) == og_letter = snd(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) reflector))
    |otherwise = fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) reflector))
    {-Filter The List Out, If The Original Letter Is The First Letter In The Tuple, Filter That Aswell, If Not Filter The Second-}

  getStecker :: Char -> Stecker -> Char
  getStecker og_letter steckerboard
    |fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard)) == og_letter = snd(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard))
    |snd(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard)) == og_letter = fst(head(filter (\letter-> fst(letter) == og_letter||snd(letter)==og_letter) steckerboard))
    | otherwise = og_letter

  onSteckerBoard:: Char->Stecker->Char
  onSteckerBoard a steckerboard
    |((length ((filter (\letter-> (fst(letter))/= a)) steckerboard)) /= (length steckerboard) || (length ((filter (\letter-> (snd(letter))/= a)) (steckerboard))) /= (length steckerboard)) = getStecker a steckerboard
    |otherwise=a



{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] -- the supplied type is not correct; fix it!
  type Crib = [(Char, Char)] -- the supplied type is not correct; fix it!

  longestMenu :: Crib -> Menu
  longestMenu crib = getLongest(map (\x -> getlst(usingForwardFind x crib)) [0..length(crib)*2])


  findLetterPos :: (Char,Char) -> Crib -> Int --MINUS LENGTH OF LAST ITEM IN OLD LT TO VALUE TO CALCULATE TRUE POSITION
  findLetterPos letter crib  =(head ((elemIndices (head(filter (\x -> fst(x)==fst(letter)) ((filter (\x -> snd(x)==snd(letter))) crib))) crib)))



  forwardFind :: Crib -> (Char,Char,[Int]) -> Crib  -> Int -> (Char,Char, [Int])
  forwardFind og_crib letter crib counter
          |letterInOriginal (snd(new_letters)) crib== False = (fst(new_letters) , snd(new_letters), (getlst letter)++ [(findLetterPos new_letters og_crib)])
          |otherwise = forwardFind og_crib (fst(new_letters) , snd(new_letters), (getlst letter)++[(findLetterPos new_letters og_crib)]) (delete ((getfst(letter)), (getmid(letter))) crib) (counter^2)
          where new_letters = (randomElement counter ((filter (\x -> fst(x)==getmid(letter)) crib)))



  letterInOriginal :: Char -> Crib -> Bool
  letterInOriginal letter crib
          | length (filter (\x -> fst(x)==letter) crib) > 0 = True
          | otherwise = False



  usingForwardFind :: Int -> Crib -> (Char,Char,[Int])
  usingForwardFind startNum crib = (findLongest (map (\letterx -> getLetterLst letterx) crib)) where
          getLetterLst x
                  | (letterInOriginal (snd(x)) (crib)== False) = ('A','A',[0])
                  | otherwise = forwardFind crib (fst x, snd x, [findLetterPos (x) crib]) crib startNum



  findLongest :: [(Char,Char, [Int])] -> (Char,Char, [Int])
  findLongest list = list !! head(elemIndices (maximum (map (\x -> length(getlst(x))) list)) (map (\x -> length(getlst(x))) list))

  getLongest :: [[Int]]-> [Int]
  getLongest list = list !! head(elemIndices (maximum (map (\x -> length((x))) list)) (map (\x -> length((x))) list))

  randomElement :: Int -> [(Char,Char)] -> (Char,Char)
  randomElement seed inputLst = inputLst !! rand where
    lengthOfLst = length inputLst
    (rand, _) = randomR (0,(lengthOfLst-1)) generator where
          generator = mkStdGen seed

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
  rotor0 = ("ABCDEFGHIJKLMNOPQRSTUVWXYZ",0::Int)
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

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

module HW03 where

data Expression =
    Var String
  | Val Int
  | Op Expression Bop Expression
  deriving (Show, Eq)

data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1
extend :: State -> String -> Int -> State
extend oldState var val = (\var2 -> if var2 == var then val else oldState var2)       

empty :: State
empty = (\var -> 0)

-- Exercise 2
convertToNum :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
convertToNum inputFunc = (\x y -> if (inputFunc x y) then 1 else 0)

getOp :: Bop -> (Int -> Int -> Int)
getOp inputBop = case inputBop of 
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> (div)
  Gt -> convertToNum (>)
  Ge -> convertToNum (>=)
  Lt -> convertToNum (<)
  Le -> convertToNum (<=)
  Eql -> convertToNum (==)

evalE :: State -> Expression -> Int
evalE _ (Val int) = int
evalE inputState (Var str) = inputState str
evalE inputState (Op expr1 bop expr2) = (getOp bop) result1 result2
  where
    result1 = evalE inputState expr1
    result2 = evalE inputState expr2

-- Exercise 3

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign inputString inputExpr) = DAssign inputString inputExpr
desugar (Incr inputString) = DAssign inputString (Op (Var inputString) Plus (Val 1))
desugar (If expr st1 st2) = DIf expr (desugar st1) (desugar st2)
desugar (While expr st1) = DWhile expr (desugar st1)
desugar (For initSt expr incrSt actionSt) = DSequence (desugar initSt)
 (DWhile expr (DSequence (desugar actionSt) (desugar incrSt)))
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar (Skip) = DSkip

-- Exercise 4
evalSimple :: State -> DietStatement -> State
evalSimple iState (DAssign str expr) = (extend iState str (evalE iState expr))
evalSimple iState (DIf expr thenStmt elseStmt) = 
  if (evalE iState expr) == 1 
  then (evalSimple iState thenStmt)
  else (evalSimple iState elseStmt)
evalSimple iState while@(DWhile expr ds) = 
  if (evalE iState expr) == 0 
  then iState
  else evalSimple (evalSimple iState ds) while 
evalSimple iState (DSequence ds1 ds2) = evalSimple (evalSimple iState ds1) ds2
evalSimple iState DSkip = iState

run :: State -> Statement -> State
run iState iStatement = evalSimple iState (desugar iStatement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]

-------------------------------------------------
-- Midas Driving problem
-------------------------------------------------


data Stop = Stop { pos :: Int } deriving (Show,Eq,Ord)

driveR :: Order (List Stop)
driveR a b = lengthL a >= lengthL b

driveQ :: Order (List Stop)
driveQ a b = lengthL a >= lengthL b && doko a <= doko b
  where
    doko :: List Stop -> Int
    doko (In (Inl One)) = 0
    doko (In (Inr (Cross a b))) = pos a

gasOK :: Int -> Stop -> Predicate (List Stop)
gasOK full goal = \x -> (fst (foldF f (cons $ Inr (Cross goal x))) <= full)
  where
    f (Inl One) = (0,Stop 0) -- (maximux distance,previous position)
    f (Inr (Cross cur (maxDist,preStop))) = (max maxDist curDist,cur)
      where curDist = pos cur - pos preStop

driveF :: Funs Stop (List Stop)
driveF = (funs1,funs2)
  where
    funs1 = [ Just . nil ]
    funs2 = [ aux cons , aux outr ]
    aux g (x@(Inr (Cross a b))) = test (gasOK 70 a) (g x)



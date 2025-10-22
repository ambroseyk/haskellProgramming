-- greetIfcool1.hs
module GreetIfCool1 where
greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool
        then putStrLn "Hello. What is shaking?"
    else
        putStrLn "No. No, thank you."
    where cool = coolness == "very cool"
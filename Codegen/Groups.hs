module Codegen.Groups where
    import Codegen.Data

    -- Merges the gates and connections of multiple groups to a new group
    mergeGroups :: [Group] -> Group
    mergeGroups = foldl merger (Group [] [])
        where merger i a = a { items = items a ++ items i, connections = connections a ++ connections i }

    relayerGroup :: Integer -> Group -> Group
    relayerGroup l g = g { items = map (\e -> e { layer = layer e + l }) (items g) }

    -- Comparator: Sets signal-0 to (in0 - in1)
    cmp :: String -> Signal -> Signal -> Group
    cmp id intype0 intype1 = Group gates connections
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ AC $ ArithmeticCombinator "+" intype0 (Signal "0" Constant) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "in1") 0 $ AC $  ArithmeticCombinator "+" intype1 (Signal "0" Constant) (Signal "signal-1" Virtual),
                    makeEntity (id ++ "out0") 0 $ AC $  ArithmeticCombinator "-" (Signal "signal-0" Virtual) (Signal "signal-1" Virtual) (Signal "signal-0" Virtual)
                    ]
                connections = [
                    makeRedConnection (id ++ "in0") (id ++ "out0"),
                    makeRedConnection (id ++ "in1") (id ++ "out0")
                    ]

    -- Equals: Sets signal-check to 1 if signal-0 is 0
    eq :: String -> Integer -> Group
    eq id value = Group gates []
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ DC $ DeciderCombinator "=" (Signal "signal-0" Virtual) (Signal (show value) Constant) (Signal "signal-0" Virtual)
                    ]

    -- Not equals: Sets signal-check to 1 if signal-0 is not 0
    neq :: String -> Integer -> Group
    neq id value = Group gates []
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ DC $ DeciderCombinator "≠" (Signal "signal-0" Virtual) (Signal (show value) Constant) (Signal "signal-0" Virtual)
                    ]

    -- And: Sets signal-0 to 1 if signal-0 is 1 on both inputs
    and :: String -> Group
    and id = Group gates connections
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ AC $ ArithmeticCombinator "+" (Signal "signal-0" Virtual) (Signal "0" Constant) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "in1") 0 $ AC $  ArithmeticCombinator "+" (Signal "signal-0" Virtual) (Signal "0" Constant) (Signal "signal-1" Virtual),
                    makeEntity (id ++ "out0") 0 $ AC $  ArithmeticCombinator "AND" (Signal "signal-0" Virtual) (Signal "signal-1" Virtual) (Signal "signal-0" Virtual)
                    ]
                connections = [
                    makeRedConnection (id ++ "in0") (id ++ "out0"),
                    makeRedConnection (id ++ "in1") (id ++ "out0")
                    ]

    -- Comparator: Sets signal-0 to (in0 - const)
    cmpconst :: String -> Signal -> Integer -> Group
    cmpconst id intype0 count = Group gates connections
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ AC $ ArithmeticCombinator "+" intype0 (Signal "0" Constant) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "0") 0 $ CC $ ConstCombinator (Signal "signal-1" Virtual) count,
                    makeEntity (id ++ "out0") 0 $ AC $  ArithmeticCombinator "-" (Signal "signal-0" Virtual) (Signal "signal-1" Virtual) (Signal "signal-0" Virtual)
                    ]
                connections = [
                    makeRedConnection (id ++ "in0") (id ++ "out0"),
                    makeRedConnection (id ++ "0") (id ++ "out0")
                    ]

    -- Does nothing
    input :: String -> Group
    input id = Group gates []
        where   gates = [
                    makeEntity (id ++ "out0") 0 $ AC $ ArithmeticCombinator "+" (Signal "signal-each" Virtual) (Signal "0" Constant) (Signal "signal-each" Virtual)
                    ]

    output :: String -> Signal -> Group
    output id rewrite = Group gates connections
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ AC $ ArithmeticCombinator "+" (Signal "signal-each" Virtual) (Signal "0" Constant) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "out0") 1 $ AC $ ArithmeticCombinator "+" (Signal "signal-0" Virtual) (Signal "0" Constant) rewrite,
                    makeEntity (id ++ "0") 0 $ DC $ DeciderCombinator "=" (Signal "signal-0" Virtual) (Signal "0" Constant) (Signal "signal-red" Virtual),
                    makeEntity (id ++ "1") 0 $ DC $ DeciderCombinator ">" (Signal "signal-0" Virtual) (Signal "0" Constant) (Signal "signal-green" Virtual),
                    makeEntity (id ++ "2") 0 $ AC $ ArithmeticCombinator "+" (Signal "signal-0" Virtual) (Signal "1" Constant) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "3") 1 $ LA $ Lamp (Signal "signal-0" Virtual)
                    ]
                connections = [
                    makeRedConnection (id ++ "in0") (id ++ "out0"),
                    makeRedConnection (id ++ "in0") (id ++ "0"),
                    makeRedConnection (id ++ "in0") (id ++ "1"),
                    makeRedSingleConnection (id ++ "0") (id ++ "3"),
                    makeRedSingleConnection (id ++ "1") (id ++ "3"),
                    makeRedSingleConnection (id ++ "2") (id ++ "3")
                    ]

    -- Sets signal-0 to 0 if 1, otherwise to 0
    not :: String -> Group
    not id = Group gates connections
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ AC $ ArithmeticCombinator "+" (Signal "signal-0" Virtual) (Signal "0" Constant) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "out0") 0 $ DC $ DeciderCombinator "≠" (Signal "signal-0" Virtual) (Signal "1" Constant) (Signal "signal-0" Virtual)
                    ]
                connections = [
                    makeRedConnection (id ++ "in0") (id ++ "out0")
                    ]


module CircComp.Groups where
    import CircComp.Data

    -- Merges the gates and connections of multiple groups to a new group
    mergeGroups :: [Group] -> Group
    mergeGroups = foldl merger (Group [] [])
        where merger i a = a { items = items a ++ items i, connections = connections a ++ connections i }

    relayerGroup :: Integer -> Group -> Group
    relayerGroup l g = g { items = map (\e -> e { layer = layer e + l }) (items g) }

    -- Comparator: Sets signal-0 to (in0 - in1)
    cmpEq :: String -> Signal -> Signal -> Group
    cmpEq id intype0 intype1 = Group gates connections
        where   gates = [
                    makeEntity (id ++ "in0") 0 $ AC $ ArithmeticCombinator "+" intype0 (Signal "0" Constant) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "in1") 0 $ AC $  ArithmeticCombinator "+" intype1 (Signal "0" Constant) (Signal "signal-1" Virtual),
                    makeEntity (id ++ "0") 0 $ AC $  ArithmeticCombinator "-" (Signal "signal-0" Virtual) (Signal "signal-1" Virtual) (Signal "signal-0" Virtual),
                    makeEntity (id ++ "out0") 0 $ DC $  DeciderCombinator "=" (Signal "signal-0" Virtual) (Signal "0" Constant) (Signal "signal-0" Virtual)
                    ]
                connections = [
                    makeRedConnection (id ++ "in0") (id ++ "0"),
                    makeRedConnection (id ++ "in1") (id ++ "0"),
                    makeRedConnection (id ++ "0") (id ++ "out0")
                    ]

    constInput :: String -> Signal -> Integer -> Group
    constInput id sig cnt = Group gates []
        where   gates = [
                    makeEntity (id ++ "out0") 0 $ AC $ ArithmeticCombinator "+" (Signal "signal-0" Virtual) (Signal (show cnt) Constant) sig
                    ]

    -- Does nothing
    input :: String -> Group
    input id = Group gates connections
        where   gates = [
                    makeEntity (id ++ "out0") 0 $ AC $ ArithmeticCombinator "+" (Signal "signal-each" Virtual) (Signal "0" Constant) (Signal "signal-each" Virtual),
                    makeEntity (id ++ "0") (-1) $ CE $ Chest
                    ]
                connections = [
                    makeRedConnectionToSingle (id ++ "0") (id ++ "out0")
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


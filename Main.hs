module Main where

    import CircComp.Data
    import qualified CircComp.Groups as Groups
    import CircComp.Json
    import CircComp.Id
    import CircComp.Layout

    import CircComp.Syntaxtree



    -- main :: IO ()
    -- main = putStrLn $ exportBlueprint $ Blueprint $ layoutGroup $ updateGroupIds $ Groups.mergeGroups [
    --     Groups.input "inp0",
    --     Groups.constInput "inp1" (Signal "copper-plate" Item) 5,

    --     Groups.relayerGroup 1 $Groups.cmpEq "cmp0" (Signal "copper-plate" Item) (Signal "copper-plate" Item),
    --     makeRedConnectionGroup "inp0out0" "cmp0in0",
    --     makeRedConnectionGroup "inp1out0" "cmp0in1",

    --     Groups.relayerGroup 2 $ Groups.output "out0" (Signal "copper-plate" Item),
    --     makeRedConnectionGroup "cmp0out0" "out0in0",

    --     Groups.relayerGroup 2 $ Groups.output "out1" (Signal "copper-plate" Item),
    --     makeRedConnectionGroup "cmp0out0" "out1in0"
    --     ]

    main :: IO ()
    main = putStrLn $ exportBlueprint $ Blueprint $ layoutGroup $ updateGroupIds $ astToGroup $ 
        Ast [
            
            Ast [
                Ast [] (TokenConstItem (TokenVirtualItem "copper-plate") (TokenStaticNumber 69)),
                Ast [] TokenInput
            ]
            (TokenCmpeq (TokenVirtualItem "copper-plate") (TokenVirtualItem "copper-plate"))
        ] TokenOutput
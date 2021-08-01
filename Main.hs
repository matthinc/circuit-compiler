module Main where

    import Codegen.Data
    import qualified Codegen.Groups as Groups
    import Codegen.Json
    import Codegen.Id
    import Codegen.Layout

    main :: IO ()
    main = putStrLn $ exportBlueprint $ Blueprint $ layoutGroup $ updateGroupIds $ Groups.mergeGroups [
        Groups.input "inp0",

        Groups.relayerGroup 1 $Groups.cmpconst "cmp0" (Signal "copper-plate" Item) 10,
        makeRedConnectionGroup "inp0out0" "cmp0in0",

        Groups.relayerGroup 2 $ Groups.output "out0" (Signal "copper-plate" Item),
        makeRedConnectionGroup "cmp0out0" "out0in0",

        Groups.relayerGroup 2 $ Groups.output "out1" (Signal "copper-plate" Item),
        makeRedConnectionGroup "cmp0out0" "out1in0"
        ]
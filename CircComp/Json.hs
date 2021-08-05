module CircComp.Json where
    import qualified CircComp.Data as Data
    import Data.List

    exportBlueprint :: Data.Blueprint -> String
    exportBlueprint bp = "{\"blueprint\": {"
        ++ "\"version\": 281479273775104,"
        ++ "\"item\": \"blueprint\","
        ++ "\"entities\":" ++ exportGroup (Data.group bp)
        ++ "}}"

    exportGroup :: Data.Group -> String
    exportGroup group = "[" ++ exportList ++ "]"
        where exportList = intercalate "," $ map (exportEntity $ Data.connections group) $ Data.items group

    exportEntity :: [Data.Connection] -> Data.Entity  -> String
    exportEntity c e = "{"
        ++ "\"entity_number\":" ++ show (Data.id e) ++ ","
        ++ "\"position\":{\"x\":" ++ show (Data.xpos e) ++ ", \"y\":" ++ show (Data.ypos e) ++ "},"
        ++ "\"connections\":{" ++ exportConnections c e ++ "},"
        ++ exportWrappedEntity (Data.wrapped_entity e)
        ++ "}"

    exportConnections :: [Data.Connection] -> Data.Entity -> String
    exportConnections connections entity = inbound ++ "," ++ outbound
        where   entity_id = Data.id entity
                inbound = "\"1\": {\"red\":[" ++ inboundRed ++ "], \"green\":[]}"
                inboundRed = intercalate ","
                    $ map (\c -> "{\"entity_id\":" ++ show (Data.id_from c) ++ inboundCircuitId c)
                    $ filter (\c -> Data.id_to c == entity_id && Data.id_from c >= 0 && Data.color c == Data.Red ) connections
                inboundCircuitId c = if Data.multiSource c then ", \"circuit_id\":2}" else "}"
                outbound = "\"2\": {\"red\":[" ++ outboundRed ++ "], \"green\":[]}"
                outboundRed = intercalate ","
                    $ map (\c -> "{\"entity_id\":" ++ show (Data.id_to c) ++ outboundCircuitId c)
                    $ filter (\c -> Data.id_from c == entity_id && Data.id_to c >= 0 && Data.color c == Data.Red ) connections
                outboundCircuitId c = if Data.multiTarget c then ", \"circuit_id\":1}" else "}"

    exportWrappedEntity :: Data.EntityType -> String
    exportWrappedEntity e = case e of
        Data.AC c -> exportArithmeticCombinator c
        Data.DC c -> exportDeciderCombinator c
        Data.CC c -> exportConstantCombinator c
        Data.LA c -> exportLamp c
        Data.CE c -> exportChest c

    exportArithmeticCombinator :: Data.ArithmeticCombinator -> String
    exportArithmeticCombinator c = "\"control_behavior\": { \"arithmetic_conditions\": {"
        ++ "\"operation\":\"" ++ Data.operation c ++ "\","
        ++ exportSignal "first_" (Data.first_signal c) ++ ","
        ++ exportSignal "second_" (Data.second_signal c) ++ ","
        ++ exportSignal "output_" (Data.out_signal c)
        ++ "}},"
        ++ "\"name\":\"arithmetic-combinator\""

    exportLamp :: Data.Lamp -> String
    exportLamp c = "\"control_behavior\": { \"circuit_condition\": {"
        ++ "\"comparator\":\">\","
        ++ "\"constant\":0,"
        ++ exportSignal "first_" (Data.lsignal c)
        ++ "},"
        ++ "\"use_colors\":true"
        ++ "},"
        ++ "\"name\":\"small-lamp\""

    exportChest :: Data.Chest -> String
    exportChest c = "\"name\":\"steel-chest\""

    exportDeciderCombinator :: Data.DeciderCombinator -> String
    exportDeciderCombinator c = "\"control_behavior\": { \"decider_conditions\": {"
        ++ "\"comparator\":\"" ++ Data.dcomparator c ++ "\","
        ++ exportSignal "first_" (Data.dfirst_signal c) ++ ","
        ++ exportSignal "" (Data.dsecond_signal c) ++ ","
        ++ exportSignal "output_" (Data.dout_signal c) ++ ","
        ++ "\"copy_count_from_input\":false"
        ++ "}},"
        ++ "\"name\":\"decider-combinator\""

    exportConstantCombinator :: Data.ConstCombinator -> String
    exportConstantCombinator c = exportArithmeticCombinator $
        Data.ArithmeticCombinator "+" (Data.Signal "0" Data.Constant) (Data.Signal (show $ Data.count c) Data.Constant) (Data.signal c)

    exportSignal :: String -> Data.Signal -> String
    exportSignal index sig | Data.stype sig == Data.Virtual = "\"" ++ index ++ "signal\": {\"name\":\"" ++ Data.name sig ++ "\", \"type\": \"virtual\"}"
                           | Data.stype sig == Data.Item = "\"" ++ index ++ "signal\": {\"name\":\"" ++ Data.name sig ++ "\", \"type\": \"item\"}"
                           | Data.stype sig == Data.Constant = "\"" ++ index ++ "constant\": " ++ Data.name sig


module CircComp.Id where
    import Data.Maybe
    import Data.List
    import qualified CircComp.Data as Data

    updateGroupIds :: Data.Group -> Data.Group
    updateGroupIds group = Data.Group entitiesWithIds connectionsWithIds
        where   entitiesWithIds = generateIds $ Data.items group
                connectionsWithIds = updateConnections entitiesWithIds $ Data.connections group

    generateIds :: [Data.Entity] -> [Data.Entity]
    generateIds entities = zipWith (curry updateId) entities [0..]
        where updateId (e, i) = e { Data.id = i }

    findId :: [Data.Entity] -> String -> Integer
    findId e id = maybe (-1) Data.id $ find (\e -> Data.temp_id e == id) e

    updateConnections ::  [Data.Entity] -> [Data.Connection] -> [Data.Connection]
    updateConnections entities connections = filter filterConnections $ map updateConnection connections
        where   updateConnection c = c { Data.id_from = findId entities (Data.id_from_temp c), Data.id_to = findId entities (Data.id_to_temp c) }
                filterConnections c = Data.id_from c >= 0 && Data.id_to c >= 0
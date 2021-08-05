module CircComp.Layout where
    import qualified CircComp.Data as Data
    import Data.List (nub, sort)

    layerHeight :: Integer
    layerHeight = -2

    layoutGroup :: Data.Group -> Data.Group
    layoutGroup group = group { Data.items = layoutEntities (Data.items group)}

    layoutEntities :: [Data.Entity] -> [Data.Entity]
    layoutEntities entities = concat $ zipWith (\e y -> map (\et -> et { Data.ypos = y * layerHeight}) e) layersXPosSet [0..]
        where   layers = sort $ nub $ map Data.layer entities
                layerEntities layer = filter (\e -> Data.layer e == layer) entities
                entitiesByLayer = map layerEntities layers
                layersXPosSet = map (zipWith (\x e -> e { Data.xpos = x}) [0..]) entitiesByLayer

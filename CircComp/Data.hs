module CircComp.Data where

    data Color = Red | Green
        deriving (Show, Eq)

    data Group = Group {
        items :: [Entity],
        connections :: [Connection]
    } deriving (Show)

    data Connection = Connection {
        id_from :: Integer,
        id_to :: Integer,
        id_from_temp :: String,
        id_to_temp:: String,
        color :: Color,
        multiTarget :: Bool,
        multiSource :: Bool
    } deriving (Show)

    makeRedConnection :: String -> String -> Connection
    makeRedConnection inp outp = Connection (-1) (-1) inp outp Red True True

    makeRedSingleConnection :: String -> String -> Connection
    makeRedSingleConnection inp outp = Connection (-1) (-1) inp outp Red False True

    makeRedConnectionToSingle :: String -> String -> Connection
    makeRedConnectionToSingle inp outp = Connection (-1) (-1) inp outp Red True False

    makeRedConnectionGroup :: String -> String -> Group
    makeRedConnectionGroup inp outp = Group [] [Connection (-1) (-1) inp outp Red True True]

    data ArithmeticCombinator = ArithmeticCombinator {
        operation :: String,
        first_signal :: Signal,
        second_signal :: Signal,
        out_signal :: Signal
    } deriving (Show)

    newtype Lamp = Lamp {
        lsignal :: Signal
    } deriving (Show)

    data Chest = Chest deriving (Show)

    data DeciderCombinator = DeciderCombinator {
        dcomparator :: String,
        dfirst_signal :: Signal,
        dsecond_signal :: Signal,
        dout_signal :: Signal
    } deriving (Show)

    data ConstCombinator = ConstCombinator {
        signal :: Signal,
        count :: Integer
    } deriving (Show)

    data SignalType = Virtual | Item | Constant
        deriving (Show, Eq)

    data Signal = Signal {
        name :: String,
        stype :: SignalType
    } deriving (Show)

    data EntityType = AC ArithmeticCombinator | DC DeciderCombinator | CC ConstCombinator | LA Lamp | CE Chest
         deriving (Show)

    data Entity = Entity {
        id :: Integer,
        temp_id :: String,
        layer :: Integer,

        xpos :: Integer,
        ypos :: Integer,

        wrapped_entity :: EntityType
    } deriving (Show)

    makeEntity :: String -> Integer -> EntityType ->  Entity
    makeEntity tempid layer = Entity 0 tempid layer (-1) (-1)

    data Blueprint = Blueprint {
        group :: Group
    } deriving (Show)



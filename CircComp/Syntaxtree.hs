module CircComp.Syntaxtree where

    import qualified CircComp.Data as Data
    import qualified CircComp.Groups as Groups

    data Ast = Ast {
        childNodes :: [Ast],
        token :: Token
    }

    data Token = TokenCmpeq SignalToken SignalToken | TokenConstItem SignalToken TokenStatic | TokenInput | TokenOutput
    data SignalToken = TokenVirtualSignal String | TokenVirtualItem String
    data TokenStatic = TokenStaticNumber Integer

    astToGroup :: Ast -> Data.Group
    astToGroup root = Groups.mergeGroups $ processAstNode root 0 0 "#" "root" []

    processAstNode :: Ast -> Integer -> Integer -> String -> String -> [Data.Group] -> [Data.Group]
    processAstNode node depth index parentid id groups = processChildren
        where   childid n = "#" ++ show n
                newGroups = groups ++ [newGroup] ++ [connection]
                connection = Data.makeRedConnectionGroup (id ++ "out0") (parentid ++ "in" ++ show index) 
                newGroup = Groups.relayerGroup (-depth) $ astNodeToGroup node parentid id
                childNodesWithIndex = zip (childNodes node) [0..]
                processChildren = foldl (\a n -> processAstNode (fst n) (depth + 1) (snd n) id (id ++ "#" ++ show (snd n)) a) newGroups childNodesWithIndex

    astNodeToGroup :: Ast -> String -> String -> Data.Group
    astNodeToGroup node parentid id = case token node of
        TokenOutput -> Groups.output id (Data.Signal "signal-check" Data.Virtual)
        TokenCmpeq s1 s2 -> Groups.cmpEq id (signalTokenToSignal s1) (signalTokenToSignal s2) 
        TokenInput -> Groups.input id
        TokenConstItem s1 n1 -> Groups.constInput id (signalTokenToSignal s1) (staticToken n1)

    signalTokenToSignal :: SignalToken -> Data.Signal
    signalTokenToSignal tok = case tok of
        TokenVirtualSignal name -> Data.Signal name Data.Virtual
        TokenVirtualItem name -> Data.Signal name Data.Item

    staticToken :: TokenStatic -> Integer
    staticToken tok = case tok of
        TokenStaticNumber num -> num
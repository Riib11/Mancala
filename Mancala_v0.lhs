\documentclass{article}
%-------------------------------------------------------------------------------
% SETUP
%-------------------------------------------------------------------------------

\usepackage{verbatim}
\newenvironment{code}{\verbatim}{\endverbatim\normalsize}
\usepackage[margin=1in]{geometry}

\newcommand{\tc}[1]{\texttt{#1}}

%-------------------------------------------------------------------------------
\begin{document}
%-------------------------------------------------------------------------------

\begin{center}
\Huge{ATL Template}
\\[0.75cm]
\end{center}

%///////////////////////////////////////////////
\begin{code}

module Mancala
(
) where

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Status}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

data Player = Player1 | Player2

instance Show Player where
    show Player1 = "player 1"
    show Player2 = "player 2"

get_next_player :: Player -> Player
get_next_player player = case player of
    Player1 -> Player2
    Player2 -> Player1

data Status
    = Turn Player
    | Finish

instance Show Status where
    show (Turn player) = (show player) ++ "'s turn"
    show Finish = "game finished"

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Holes}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

type Hole = Int

data Index = Index Int

instance Show Index where
    show (Index i) = "#" ++ (show i)

ind :: Int -> Index
ind i = Index $ i `mod` 12

inc_index :: Index -> Index
inc_index (Index i) = ind $ i + 1

get_hole :: [Hole] -> Index -> Hole
get_hole (h:hs) (Index i) = case i of
    0 -> h
    _ -> get_hole hs (Index $ i - 1)

set_hole :: Index -> Hole -> [Hole] -> [Hole]
set_hole (Index i) h_new (h:hs) = case i of
    0 -> h_new : hs
    _ -> h : set_hole (Index $ i - 1) h_new hs

add_hole :: Index -> Hole -> [Hole] -> [Hole]
add_hole index h holes = set_hole index (h + get_hole holes index) holes

init_holes =
    [ 4, 4, 4, 4, 4, 4
    , 4, 4, 4, 4, 4, 4 ]

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Stores}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

type Store = Int


init_store = 0

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{State}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

data State = State
    { state_status :: Status
    , state_holes  :: [Hole]
    , state_store1 :: Store
    , state_store2 :: Store }

instance Show State where
    show (State status holes store1 store2) = 
        let [hA, hB, hC, hD, hE, hF, hG, hH, hI, hJ, hK, hL] = map show holes
        in "\n"
            ++ "[#] " ++ (show status) ++ "\n\n"
            ++ "  P2: " ++ (show store2) ++ "\n"
            ++ "/––"       ++ "–+–"       ++ "––\\\n"
            ++ "| "  ++ hJ ++ " | " ++ hI ++  " |\n"
            ++ "|––"       ++ "–+–"       ++ "––|\n"
            ++ "| "  ++ hK ++ " | " ++ hH ++  " |\n"
            ++ "|––"       ++ "–+–"       ++ "––|\n"
            ++ "| "  ++ hL ++ " | " ++ hG ++  " |\n"
            ++ "|––"       ++ "–+–"       ++ "––|\n"
            ++ "| "  ++ hA ++ " | " ++ hF ++  " |\n"
            ++ "|––"       ++ "–+–"       ++ "––|\n"
            ++ "| "  ++ hB ++ " | " ++ hE ++  " |\n"
            ++ "|––"       ++ "–+–"       ++ "––|\n"
            ++ "| "  ++ hC ++ " | " ++ hD ++  " |\n"
            ++ "\\–––+–––//" ++ "\n"
            ++ "  P1: " ++ (show store1) ++ "\n"

extract_current_player :: State -> Player
extract_current_player state = case state_status state of
    Turn player -> player

get_state_store :: Player -> State -> Int
get_state_store player (State st hs s1 s2) = case player of
    Player1 -> s1
    Player2 -> s2

set_state_store :: Player -> Int -> State -> State
set_state_store player x (State st hs s1 s2) = case player of
    Player1 -> State st hs x s2
    Player2 -> State st hs s1 x

get_state_hole :: Index -> State -> Hole
get_state_hole index state = get_hole (state_holes state) index

set_state_holes :: [Hole] -> State -> State
set_state_holes hs (State st _ s1 s2) = State st hs s1 s2

set_state_hole :: Index -> Hole -> State -> State
set_state_hole index hole state = set_state_holes
    (set_hole index hole (state_holes state)) state

add_state_store :: Player -> Int -> State -> State
add_state_store player x state = set_state_store player
    (x + get_state_store player state) state


init_state = State
    (Turn Player1)
    init_holes
    0
    0

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Action}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

data Action = A1 | A2 | A3 | A4 | A5 | A6
    deriving (Show, Enum)

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Update}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

update :: Action -> State -> State
update action state = case (state_status state) of
    Finish -> state
    Turn player ->
        let action_index = get_action_index action player
        in update_empty_side
            $ spread_hole action_index
                state 

update_empty_side :: State -> State
update_empty_side (State status holes store1 store2) =
    let [hA,hB,hC,hD,hE,hF,hG,hH,hI,hJ,hK,hL] = holes
        empty_holes = [0,0,0,0,0,0,0,0,0,0,0,0]
        side1 = (sum $ take 6 holes)
        side2 = (sum $ drop 6 holes)
    in case (side1, side2) of
        -- player 1's side is empty,
        -- so player 1 gets all player 2's side's pieces
        (0, _) -> State
            Finish
            empty_holes
            (store1 + side2)
            store2
        -- player 2's side is empty,
        -- so player 2 gets all player 1's side's pieces
        (_, 0) -> State
            Finish
            empty_holes
            store1
            (store2 + side1)
        -- otherwise, nothing special happens
        _ -> State status holes store1 store2

get_action_index :: Action -> Player -> Index
get_action_index action player =
    let player_offset = case player of
            Player1 -> 0
            Player2 -> 6
    in ind $ player_offset + fromEnum action

spread_hole :: Index -> State -> State
spread_hole target_index state =
    let helper :: Int -> Index -> State -> State
        -- base case: last piece
        helper 0 prev_index state = state
        helper 1 prev_index (State (Turn player) holes store1 store2) =
            case prev_index of
                -- end in player 1's store
                Index 2 -> case player of
                    -- player 1 ended in player 1's store,
                    -- so player 1 takes another turn
                    Player1 -> State
                        (Turn Player1)
                        holes
                        (store1 + 1)
                        store2
                    -- player 2 ended in player 1's store,
                    -- so nothing special happens
                    Player2 -> State
                        (Turn Player1)
                        holes
                        (store1 + 1)
                        store2
                -- end player 2's store
                Index 8 -> case player of
                    -- player 1 ended in player 2's store,
                    -- so nothing special happens
                    Player1 -> State
                        (Turn Player1)
                        holes
                        store1
                        (store2 + 1)
                    -- player 2 ended in player 2's store,
                    -- so player 1 takes another turn
                    Player2 -> State
                        (Turn Player2)
                        holes
                        store1
                        (store2 + 1)
                -- end in normal hole
                Index i ->
                    let next_index = inc_index prev_index
                        next_hole = get_hole holes next_index
                        foll_index = inc_index next_index
                        next_player = get_next_player player
                    in case next_hole of
                        -- next hole is empty,
                        -- so take following hole
                        0 -> add_state_store player next_hole -- score hole
                            $ State
                                (Turn next_player)
                                ( add_hole next_index 1
                                $ set_hole foll_index 0
                                    holes )
                                store1
                                store2
                        -- next hole is not empty,
                        -- so end turn normally
                        _ -> State
                            (Turn next_player)
                            (add_hole next_index 1 holes)
                            store1
                            store2
        helper x prev_index (State (Turn player) holes store1 store2) =
            let next_index = inc_index prev_index
                next_player = get_next_player player
            in case prev_index of
                -- pass player 1's store
                Index 2 -> helper (x - 2) next_index
                    $ State
                        (Turn next_player)
                        (add_hole next_index 1 holes)
                        (store1 + 1) -- player 1 scores
                        store2
                -- pass player 2's store
                Index 8 -> helper (x - 2) next_index
                    $ State
                        (Turn next_player)
                        (add_hole next_index 1 holes)
                        store1
                        (store2 + 1) -- player 2 scores
                -- pass normal hole
                Index i -> helper (x - 1) next_index
                    $ State
                        (Turn next_player)
                        (add_hole next_index 1 holes)
                        store1
                        store2

        target_hole = get_state_hole target_index state -- action-targeted hole
        init_state  = set_state_hole target_index 0 state -- empty target hole
    in helper target_hole target_index init_state

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

%-------------------------------------------------------------------------------
\end{document}
%-------------------------------------------------------------------------------

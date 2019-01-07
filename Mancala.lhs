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

import qualified Data.Vector as V
import Debug

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Status}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

data Player = P1 | P2
    deriving (Show)

get_other_player :: Player -> Player
get_other_player player = case player of
    P1 -> P2
    P2 -> P1

data Status
    = Turn Player
    | Finished

instance Show Status where
    show (Turn player) = (show player) ++ "'s turn"
    show Finished = "game finished"

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Spaces}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

type Spaces = V.Vector (Int, Int) -- (index, pieces)

(+%) :: Int -> Int -> Int
x +% y = (x + y) `mod` 12

-- spaces_init = V.indexed $ V.replicate 12 4
spaces_init = V.indexed $ V.fromList
    [ 4, 4, 4, 4, 4, 4
    , 1, 0, 4, 4, 4, 4 ]


\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


\section{State}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

data State = State
    { status :: Status
    , spaces :: Spaces
    , score1 :: Int
    , score2 :: Int }

instance Show State where
    show state =
        let format s = case length s of { 1 -> s ++ " "; 2 -> s; _ -> error s }
            s i = format $ show $ get_space i state
        in foldl (++) ""
            [ "\n"
            , show $ status state
            , "\n\n"
            , "score2: " ++ (show $ score2 state) ++ "\n"
            , "+----" ++   "+"  ++ "----+\n"
            , "| ", s 9,  " | ", s 8, " |\n"
            , "+----" ++   "+"  ++ "----+\n"
            , "| ", s 10, " | ", s 7, " |\n"
            , "+----" ++   "+"  ++ "----+\n"
            , "| ", s 11, " | ", s 6, " |\n"
            , "+----" ++   "+"  ++ "----+\n"
            , "| ", s 0,  " | ", s 5, " |\n"
            , "+----" ++   "+"  ++ "----+\n"
            , "| ", s 1,  " | ", s 4, " |\n"
            , "+----" ++   "+"  ++ "----+\n"
            , "| ", s 2,  " | ", s 3, " |\n"
            , "+----" ++   "+"  ++ "----+\n"
            , "score1: " ++ (show $ score1 state) ++ "\n"
            , "\n" ] 

state_init = State
    (Turn P1)
    spaces_init
    (0)
    (0)

--
-- setters
--

set_status :: Status -> State -> State
set_status status (State _ ss s1 s2) = State status ss s1 s2

set_spaces :: Spaces -> State -> State
set_spaces spaces (State st _ s1 s2) = State st spaces s1 s2

set_score1 :: Int -> State -> State
set_score1 score1 (State st ss _ s2) = State st ss score1 s2

set_score2 :: Int -> State -> State
set_score2 score2 (State st ss s1 _) = State st ss s1 score2

--
-- useful functions
--

add_score :: Player -> Int -> State -> State
add_score player x state = case player of
    P1 -> set_score1 (x + score1 state) state
    P2 -> set_score2 (x + score2 state) state

get_space :: Int -> State -> Int
get_space index state = snd $ (spaces state) V.! index

set_space :: Int -> Int -> State -> State
set_space index x_new state = let
    spaces_new = V.map
        (\(i, x) -> if i == index then (i, x_new) else (i, x))
        (spaces state)
    in set_spaces spaces_new state

add_space :: Int -> Int -> State -> State
add_space index x_new state =
    set_space index (x_new + get_space index state) state

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Action}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

data Action = Action Int

get_active_index :: Player -> Action -> Int
get_active_index player (Action index) =
    index + case player of
        P1 -> 0
        P2 -> 6

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\section{Update}

% TODO: description

%///////////////////////////////////////////////
\begin{code}

update :: Action -> State -> IO State
update action state_orig = let
    active_player = case status state_orig of
        Turn P1 -> P2
        Turn P2 -> P2

    -- distribute selected pieces to the appropriate places
    updated_action :: State -> IO State
    updated_action state = case active_player of
        P1 -> updated_action_p1 state
        P2 -> updated_action_p2 state
    
    -- (player 1) distribute selected pieces to the appropriate places
    updated_action_p1 :: State -> IO State
    updated_action_p1 state = let
        action_index = get_active_index active_player action
        action_pieces = get_space action_index state
        emptied_action_index = set_space action_index 0 state
        -- distribute action pieces
        helper :: Int -> Int -> State -> IO State
        helper index pieces state = let
            target = get_space (index +% 1) state
            after = get_space (index +% 2) state
            in case (index, pieces, target) of
                -- drop last piece in score1
                (2, 1, _) -> do
                    debug "drop last piece in score1"
                    return
                      $ add_score P1 1            -- P1 scores 1
                      $ set_status (Turn P1)      -- P1 takes extra turn
                            state
                -- drop last piece immediately after score1
                (2, 2, _) -> do
                    debug "drop last piece immediately after score1"
                    return
                        $ add_score P1 1           -- P1 scores 1
                        $ add_space (index +% 1) 1 -- drop 1 in `target`
                        $ set_status (Turn P2)     -- alternate turn to P2
                            state
                -- `target` is empty
                (_, 1, 0) -> do
                    debug "target is empty"
                    return
                      $ add_space (index +% 1) 1  -- drop 1 in `target`
                      $ add_score P1 after        -- P1 scores `after`
                      $ set_space (index +% 2) 0  -- empty `after`
                      $ set_status (Turn P2)      -- alternate turn to P2
                            state
                -- target is non-empty
                (_, 1, _) -> do
                    debug "target is non-empty"
                    return
                      $ add_space (index +% 1) 1  -- drop 1 in `target`
                      $ set_status (Turn P2)      -- alternate turn to P2
                            state
                -- pass score1
                (2, _, _) -> do
                    debug "pass score1"
                    helper (index +% 1) (pieces - 2)
                      $ add_space (index +% 1) 1  -- drop 1 in target-space
                      $ add_score P1 1            -- P1 scores 1
                            state
                -- normal
                (_, _, _) -> do
                    debug "normal"
                    helper (index +% 1) (pieces - 1)
                      $ add_space (index +% 1) 1  -- drop 1 in target-space
                            state
        in helper action_index action_pieces
            $ set_space action_index 0 state
    
    -- (player 2) distribute selected pieces to the appropriate places
    updated_action_p2 :: State -> IO State
    updated_action_p2 state = let
        action_index = get_active_index active_player action
        action_pieces = get_space action_index state
        emptied_action_index = set_space action_index 0 state
        -- distribute action pieces
        helper :: Int -> Int -> State -> IO State
        helper index pieces state = let
            target = get_space (index +% 1) state
            after = get_space (index +% 2) state
            in case (index, pieces, target) of
                -- drop last piece in score2
                (8, 1, _) -> do
                    debug "drop last piece in score1"
                    return
                      $ add_score P2 1            -- P2 scores 1
                      $ set_status (Turn P2)      -- P2 takes extra turn
                            state
                -- drop last piece immediately after score2
                (8, 2, _) -> do
                    debug "drop last piece immediately after score2"
                    return
                        $ add_score P2 1           -- P2 scores 1
                        $ add_space (index +% 1) 1 -- drop 1 in `target`
                        $ set_status (Turn P1)     -- alternate turn to P1
                            state
                -- `target` is empty
                (_, 1, 0) -> do
                    debug "target is empty"
                    return
                      $ add_space (index +% 1) 1  -- drop 1 in `target`
                      $ add_score P2 after        -- P2 scores `after`
                      $ set_space (index +% 2) 0  -- empty `after`
                      $ set_status (Turn P1)      -- alternate turn to P1
                            state
                -- target is non-empty
                (_, 1, _) -> do
                    debug "target is non-empty"
                    return
                      $ add_space (index +% 1) 1  -- drop 1 in `target`
                      $ set_status (Turn P1)      -- alternate turn to P1
                            state
                -- pass score2
                (8, _, _) -> do
                    debug "pass score2"
                    helper (index +% 1) (pieces - 2)
                      $ add_space (index +% 1) 1  -- drop 1 in target
                      $ add_score P2 1            -- P2 scores 1
                            state
                -- normal
                (_, _, _) -> do
                    debug "normal"
                    helper (index +% 1) (pieces - 1)
                      $ add_space (index +% 1) 1  -- drop 1 in target
                            state
        in helper action_index action_pieces
            $ set_space action_index 0 state
    
    -- apply post-action rules
    updated_postaction :: State -> IO State
    updated_postaction state = return state
    
    -- update the status of the game,
    -- including checking if the game is finished
    updated_status :: State -> IO State
    updated_status state = return state
    
    -- foldl
    -- :: (State -> (State -> IO State) -> State)
    -- -> State
    -- -> [State -> IO State]
    -- -> State

    -- apply all updates, in left-right order (opposite of composition)
    apply_updates :: IO State -> [State -> IO State] -> IO State
    apply_updates = foldl (>>=)
    
    in case status state_orig of
        Finished -> return state_orig
        Turn _   -> apply_updates (return state_orig)
                        [ updated_action
                        , updated_postaction
                        , updated_status ]

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\begin{code}

p1 x = update (Action x) (set_status (Turn P1) state_init)
p2 x = update (Action x) (set_status (Turn P2) state_init)

\end{code}

%-------------------------------------------------------------------------------
\end{document}
%-------------------------------------------------------------------------------

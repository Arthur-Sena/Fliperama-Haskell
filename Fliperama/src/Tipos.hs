module Tipos where

import Control.Monad.State
import Control.Monad.Cont

import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)

type Tabuleiro = [[Char]] --Tabuleiro Jogo Da Velha
type Sudoku = [[Int]]
type Coord = (Int, Int)
type Jogada = (Int, Int, Int) --Linha, Coluna, Valor
type Game a = StateT GameState IO a
type Menu = ContT () IO

data GameState = GameState
    { 
        tabuleiro :: Sudoku 
        , jogadaSugerida :: Maybe (Int, Int, Int)
        , tempoRestante :: NominalDiffTime
        , tempoInicial :: UTCTime 
    }

newtype Historico = Historico [Jogada]
    deriving (Show)

vazio, jogadorX, jogadorO :: Char
vazio = ' '
jogadorX = 'X'
jogadorO = 'O'

tamanhoTabuleiro :: Int
tamanhoTabuleiro = 3

newtype SudokuBoard = SudokuBoard { getBoard :: Sudoku }
    deriving (Show)

instance Semigroup SudokuBoard where
    (SudokuBoard b1) <> (SudokuBoard b2) = SudokuBoard (joinTabuleiro b1 b2)

instance Monoid SudokuBoard where
    mempty = SudokuBoard (replicate 9 (replicate 9 0))

joinTabuleiro :: Sudoku -> Sudoku -> Sudoku
joinTabuleiro b1 b2 = [[if x == 0 then y else x | (x, y) <- zip line1 line2] | (line1, line2) <- zip b1 b2]

instance Semigroup Historico where
    (Historico h1) <> (Historico h2) = Historico (h1 ++ h2)

instance Monoid Historico where
    mempty = Historico []
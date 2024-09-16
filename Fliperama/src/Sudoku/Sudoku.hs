module Sudoku.Sudoku where

import Sudoku.Validacao (validarJogada, validarEntradaSudoku)
import Tipos
import System.Random (randomRIO)
import Control.Monad.Trans.State (State, StateT, get, put)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import Control.Monad.IO.Class (liftIO)

geradorTabuleiroVazio :: Sudoku
geradorTabuleiroVazio = replicate 9 (replicate 9 0)

sudokuGenerator :: Int -> IO Sudoku
sudokuGenerator qntCasasPreenchidas = do
    Just fullBoard <- preencheTabuleiro geradorTabuleiroVazio
    limpaPosicao fullBoard (81 - qntCasasPreenchidas)
  where
    limpaPosicao tabuleiro 0 = return tabuleiro
    limpaPosicao tabuleiro n = do
        linha <- randomRIO (0, 8)
        col <- randomRIO (0, 8)
        if tabuleiro !! linha !! col == 0
            then limpaPosicao tabuleiro n
            else limpaPosicao (copySudokuEspacoSemValor tabuleiro linha col 0) (n - 1)
    copySudokuEspacoSemValor tabuleiro linha col val =
        take linha tabuleiro ++
        [take col (tabuleiro !! linha)
        ++ [val]
        ++ drop (col + 1) (tabuleiro !! linha)
        ] ++ drop (linha + 1) tabuleiro

salvarJogada :: Sudoku -> Jogada -> Sudoku
salvarJogada tabuleiro (line, col, val) =
    take line tabuleiro
    ++ [
        take col (tabuleiro !! line)
        ++ [val]
        ++ drop (col + 1) (tabuleiro !! line)
        ] ++ drop (line + 1) tabuleiro

showTabuleiro :: Sudoku -> IO ()
showTabuleiro tabuleiro = do
    putStrLn "    1 2 3 | 4 5 6 | 7 8 9"
    putStrLn "   -----------------------"
    mapM_ putStrLn $ formatarSudoku 1 tabuleiro
  where
    formatarSudoku _ [] = []
    formatarSudoku n (x:xs) =
        (show n ++ " " ++ concatMap separadorQuadrante (zip [1..] x)) : separadorDeLinha ++ formatarSudoku (n + 1) xs
      where
        separadorDeLinha = if n `mod` 3 == 0 && not (null xs) then ["   ---------------------"] else []
        separadorQuadrante (i, x') = (if (i-1) `mod` 3 == 0 then "| " else "") ++ (if x' == 0 then ". " else show x' ++ " ")

preencheTabuleiro :: Sudoku -> IO (Maybe Sudoku)
preencheTabuleiro tabuleiro = preencheTabuleiro' tabuleiro (0, 0)
  where
    preencheTabuleiro' tabuleiro (9, _) = return (Just tabuleiro)
    preencheTabuleiro' tabuleiro (linha, 9) = preencheTabuleiro' tabuleiro (linha + 1, 0)
    preencheTabuleiro' tabuleiro (linha, col) = do
      nums <- randomRIO (1, 9) >>= \n -> return $ [n] ++ [1..9] ++ [1..9]
      let possibilidades = filter (\n -> validarJogada tabuleiro n (linha, col)) nums
      tentativa possibilidades
      where
        tentativa [] = return Nothing
        tentativa (x:xs) = do
          let newTabuleiro = take linha tabuleiro ++ [take col (tabuleiro !! linha) ++ [x] ++ drop (col + 1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
          msudoku <- preencheTabuleiro' newTabuleiro (linha, col + 1)
          case msudoku of
            Just b  -> return (Just b)
            Nothing -> tentativa xs

preenchendoTabuleiro :: Sudoku -> Maybe SudokuBoard
preenchendoTabuleiro sudoku = preenche' sudoku (0, 0)
  where
    preenche' b (9, _) = Just (SudokuBoard b) -- Se chegar na última linha, retorna tabuleiro
    preenche' b (line, 9) = preenche' b (line + 1, 0) -- Avança pra próxima linha
    preenche' b (line, col)
        | b !! line !! col /= 0 = preenche' b (line, col + 1) -- Se a célula já estiver preenchida, avança
        | otherwise = foldr tryMove Nothing [1..9]
      where
        tryMove n acc
            | validarJogada b n (line, col) = case preenche' (salvarJogada b (line, col, n)) (line, col + 1) of
                Just sudokuPreenchido -> Just sudokuPreenchido
                Nothing -> acc
            | otherwise = acc

solucionarSudoku :: Sudoku -> IO (Maybe Sudoku)
solucionarSudoku sudoku = do
    case preenchendoTabuleiro sudoku of
        Just (SudokuBoard tabuleiro) -> return (Just tabuleiro)
        Nothing -> return Nothing

posicoesVazias :: Sudoku -> [(Int, Int)]
posicoesVazias tab = [(x, y) | x <- [0..8], y <- [0..8], tab !! x !! y == 0]

findJogadaValida :: Sudoku -> Maybe (Int, Int, Int)
findJogadaValida sudoku =
    let vazias = posicoesVazias sudoku
        numeros = [1..9]
        jogadasValidas = [(x + 1, y + 1, v) | (x, y) <- vazias, v <- numeros, validarEntradaSudoku sudoku (x + 1, y + 1, v)]
    in case jogadasValidas of
        []       -> Nothing
        (j:_)    -> Just j

sugerirJogada :: Sudoku -> Maybe (Int, Int, Int)
sugerirJogada = findJogadaValida --eta reduce

sugerirJogadaState :: StateT GameState IO ()
sugerirJogadaState = do
    GameState sudokuAtual _ tempoRestante tempoInicial <- get
    let novaSugestao = sugerirJogada sudokuAtual
    put (GameState sudokuAtual novaSugestao tempoRestante tempoInicial)

atualizarTempo :: StateT GameState IO ()
atualizarTempo = do
    now <- liftIO getCurrentTime
    GameState sudokuAtual sugestao  tempoRestante inicio <- get
    let tempoDecorriso = diffUTCTime now inicio
    let novoTempo = tempoRestante - tempoDecorriso
    put (GameState sudokuAtual sugestao novoTempo now)

verificarTempoLimite :: StateT GameState IO Bool
verificarTempoLimite = do
    GameState _ _ tempoRestante _ <- get
    return (tempoRestante <= 0)

module Sudoku.Main where

import Tipos
import Sudoku.Sudoku
import Sudoku.Validacao (validEntrada, validTabuleiroCompleto)
import Control.Monad.Trans.State (State, StateT, evalStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import Tipos (GameState(tempoInicial))
import Data.Time (NominalDiffTime)

{--
    Nível de Díficuldade: o nível de dificuldade varia
    de acordo com a quantidade de casas que começam preenchidas
    no inicio do jogo, por isso a funcao startSUdoku vem acompanhada
    de um inteiro (qntCasasPreenchidasInicialmente)
--}

mainMenu :: IO ()
mainMenu = do
    putStrLn " "
    putStrLn "Escolha uma opção:"
    putStrLn "1. Jogo Fácil"
    putStrLn "2. Jogo Médio"
    putStrLn "3. Jogo Difícil"
    putStrLn "4. Encerrar"
    option <- getLine
    case option of
        "1" -> startSudoku 72 600 -- Jogo Fácil (85% preenchido) && 10 minutos para completar
        "2" -> startSudoku 40 480 -- Jogo Médio (50% preenchido) &&  8 minutos para completar
        "3" -> startSudoku 20 300 -- Jogo Difícil &&  5 minutos para completar
        "4" -> putStrLn "Fim!"
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            mainMenu

--USEI ESSE MOCK SÓ PRA TESTAR O SOLUCIONADOR DE SUDOKU
--CASO QUEIRA TESTAR TAMBEM É SÓ COLOCAR OS NÚMEROS AI E INICIAR O JOGO NORMALMENTE
--EM QUALQUER NIVEL DE DIFICULDADE, DEPOIS DIGITA 10 0 0 PARA VER O "GABARITO"
sudokuMock :: Sudoku
sudokuMock = [[0,0,0,0,0,0,0,0,0], --1
              [0,0,0,0,0,0,0,0,0], --2
              [0,0,0,0,0,0,0,0,0], --3
              [0,0,0,0,0,0,0,0,0], --4
              [0,0,0,0,0,0,0,0,0], --5
              [0,0,0,0,0,0,0,0,0], --6
              [0,0,0,0,0,0,0,0,0], --7
              [0,0,0,0,0,0,0,0,0], --8
              [0,0,0,0,0,0,0,0,0]] --9
            -- 1 2 3 4 5 6 7 8 9


startSudoku :: Int -> NominalDiffTime -> IO ()
startSudoku qntCasasPreenchidasInicialmente tempoLimite = do
    tabuleiroInicial <- sudokuGenerator qntCasasPreenchidasInicialmente
    now <- getCurrentTime
    evalStateT sudokuLoop (GameState tabuleiroInicial Nothing tempoLimite now)

sudokuLoop :: StateT GameState IO ()
sudokuLoop = do
    atualizarTempo
    tempoAcabou <- verificarTempoLimite
    if tempoAcabou
        then liftIO $ do
                putStrLn "Tempo esgotado! O jogo terminou."
                mainMenu
        else do
            GameState tabuleiroAtual jogadaAtual tempoLimite tempoInicial <- get
            liftIO $ showTabuleiro tabuleiroAtual
            if validTabuleiroCompleto tabuleiroAtual
                then liftIO $ putStrLn "Parabéns! Você resolveu o Sudoku!"
                else do
                    liftIO $ do
                        putStrLn " "
                        putStrLn "Digite sua jogada no formato (linha coluna número)"
                        putStrLn "Dica: digite 0 0 0 para uma sugestão de jogada válida."
                        putStrLn ", 0 0 1 para sair ou 10 0 0 para revelar a solução:"
                        maybe (return ()) (\(x, y, v) -> putStrLn $ "Sugestão: (" ++ show x ++ ", " ++ show y ++ ", " ++ show v ++ ")") jogadaAtual

                    jogada <- liftIO getLine
                    let (x:y:v:_) = map read (words jogada)

                    if x == 0 && y == 0 && v == 1
                        then liftIO $ putStrLn "Jogo encerrado."
                        else if x == 10
                            then do
                                liftIO $ putStrLn "Revelando solução..."
                                resolucao <- liftIO $ solucionarSudoku tabuleiroAtual
                                case resolucao of
                                    Just filledBoard -> liftIO $ showTabuleiro filledBoard
                                    Nothing -> liftIO $ putStrLn "Não foi possível revelar a solução."
                                liftIO mainMenu
                            else if x == 0 && y == 0 && v == 0
                                then do
                                    sugerirJogadaState
                                    sudokuLoop
                                else do
                                    valid <- liftIO $ validEntrada tabuleiroAtual (x, y, v)
                                    if valid
                                        then do
                                            let newSudoku = salvarJogada tabuleiroAtual (x-1, y-1, v)
                                            put (GameState newSudoku Nothing tempoLimite tempoInicial)
                                            sudokuLoop
                                        else sudokuLoop
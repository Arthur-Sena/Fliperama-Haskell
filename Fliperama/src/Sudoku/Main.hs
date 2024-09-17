module Sudoku.Main where

import Tipos
import Sudoku.Sudoku
import Sudoku.Validacao (validEntrada, validTabuleiroCompleto)
import Control.Monad.Trans.State (State, StateT, evalStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import Tipos (GameState(tempoInicial, jogadaSugerida), Historico)
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
    evalStateT (sudokuLoop mempty) (GameState tabuleiroInicial Nothing tempoLimite now)

sudokuLoop :: Historico -> StateT GameState IO ()
sudokuLoop history = do
    GameState tabuleiroAtual jogadaSugerida tempoLimite tempoInicial <- get
    atualizarTempo
    tempoAcabou <- verificarTempoLimite
    if tempoAcabou
        then tempoEsgotado
        else do
            showTabuleiro 
            if validTabuleiroCompleto tabuleiroAtual
                then liftIO $ putStrLn "Parabéns! Você resolveu o Sudoku!"
                else processarEntrada jogadaSugerida history

--------------------- FUNÇÕES AUXILIARES DO MENU ---------------------
tempoEsgotado :: StateT GameState IO ()
tempoEsgotado = liftIO $ do
    putStrLn "Tempo esgotado! O jogo terminou."
    mainMenu

processarEntrada :: Maybe Jogada -> Historico -> StateT GameState IO ()
processarEntrada jogadaAtual history = do
    liftIO $ exibirMensagens jogadaAtual
    jogada <- liftIO getLine
    let (x:y:v:_) = map read (words jogada)
    case (x, y, v) of
        (0, 0, 1) -> encerrarJogo
        (10, _, _) -> revelarSolucao
        (0, 0, 0) -> sugerirJogadaState >> sudokuLoop history
        (9, 0, 0) -> desfazerJogadaState history
        _ -> processarJogada (x, y, v) history

exibirMensagens :: Maybe Jogada -> IO ()
exibirMensagens jogadaAtual = do
    putStrLn " "
    putStrLn "Digite sua jogada no formato (linha coluna número)"
    putStrLn "Dica: digite 0 0 0 para uma sugestão de jogada válida"
    putStrLn ", 0 0 1 para sair, 9 0 0 para desfazer a última jogada ou"
    putStrLn " 10 0 0 para revelar a solução:"
    maybe (return ()) (\(x, y, v) -> putStrLn $ "Sugestão: (" ++ show x ++ ", " ++ show y ++ ", " ++ show v ++ ")") jogadaAtual

encerrarJogo :: StateT GameState IO ()
encerrarJogo = liftIO $ do
    putStrLn "Jogo encerrado."
    mainMenu

revelarSolucao :: StateT GameState IO ()
revelarSolucao = do
    GameState tabuleiroAtual jogadaSugerida tLimite tInicial <- get
    liftIO $ putStrLn "Revelando solução..."
    resolucao <- solucionarSudoku
    case resolucao of
        Just tabuleiroCompleto -> put (GameState tabuleiroCompleto jogadaSugerida tLimite tInicial)  
        Nothing -> liftIO $ putStrLn "Não foi possível revelar a solução."
    showTabuleiro 
    liftIO mainMenu

desfazerJogadaState :: Historico -> StateT GameState IO ()
desfazerJogadaState history = do
    GameState tabuleiroAtual _ tempoLimite tempoInicial <- get
    let newSudoku = desfazerJogada tabuleiroAtual history
    let newHistory = Historico (init (unHistorico history))
    put (GameState newSudoku Nothing tempoLimite tempoInicial)
    sudokuLoop newHistory

processarJogada :: Jogada -> Historico -> StateT GameState IO ()
processarJogada (x, y, v) history = do
    GameState tabuleiroAtual _ tempoLimite tempoInicial <- get
    valid <- liftIO $ validEntrada tabuleiroAtual (x, y, v)
    if valid
        then do
            let newSudoku = salvarJogada tabuleiroAtual (x-1, y-1, v)
            let newHistory = adicionarJogadaHistorico (x-1, y-1, v) history
            put (GameState newSudoku Nothing tempoLimite tempoInicial)
            sudokuLoop newHistory
        else sudokuLoop history

module JogoDaVelha.Main where

import JogoDaVelha.Resultados ( ganhou, empatou )
import JogoDaVelha.Jogada
import Tipos ( jogadorO, jogadorX, Tabuleiro, Jogada, Coord )

{----------------------------------\
    Menus de Inicio e Fim
\----------------------------------}

-- Menu inicial para o jogo da velha
menuInicial :: IO ()
menuInicial = do
    putStrLn "Bem-vindo ao Jogo da Velha!"
    putStrLn "Por favor, escolha uma opção: (1, 2 ou 3)"
    putStrLn "1 - Jogar contra o computador"
    putStrLn "2 - Jogar com um amigo"
    putStrLn "3 - Sair"
    escolha <- getLine
    case escolha of
        "1" -> nivelDificuldade
        "2" -> jogoComAmigo
        "3" -> putStrLn "Fim!"
        _   -> do
            putStrLn "Opção inválida. Por favor, escolha '1', '2' ou '3'."
            menuInicial

-- Menu após um jogo
menuFinal :: IO ()
menuFinal = putStrLn "Fim do jogo. Obrigado por jogar!"

--Escolha de dificuldade
nivelDificuldade :: IO ()
nivelDificuldade = do
    putStrLn "Por favor, escolha o nível de dificuldade: (1, 2 ou 3)"
    putStrLn "1 - Fácil"
    putStrLn "2 - Difícil"
    putStrLn "3 - Profissional"
    escolha <- getLine
    case escolha of
        "1" -> jogoContraComputadorFacil
        "2" -> jogoContraComputadorDificil
        "3" -> jogoContraComputadorProfissional
        _   -> do
            putStrLn "Opção inválida. Por favor, escolha '1', '2' ou '3'."
            nivelDificuldade

{-------------------------------------------------------------------------------------}

loopJogo :: Tabuleiro -> Char -> (Tabuleiro -> Char -> IO Tabuleiro) -> IO ()
loopJogo tabuleiro jogadorAtual proximaJogada = do
    putStrLn $ "É a vez do jogador " ++ [jogadorAtual]
    novoTabuleiro <- proximaJogada tabuleiro jogadorAtual
    imprimeTabuleiro novoTabuleiro
    if ganhou novoTabuleiro jogadorAtual
        then putStrLn $ "O jogador " ++ [jogadorAtual] ++ " ganhou!"
        else if empatou novoTabuleiro
            then putStrLn "O jogo empatou!"
            else loopJogo novoTabuleiro (alternarJogador jogadorAtual) proximaJogada

-- Alternar entre jogador X e O
alternarJogador :: Char -> Char
alternarJogador jogador = if jogador == jogadorX then jogadorO else jogadorX

-- Jogada do jogador humano
jogadaHumana :: Tabuleiro -> Char -> IO Tabuleiro
jogadaHumana tabuleiro jogadorAtual = do
    jogada <- obterJogadaValida(tabuleiro)
    return $ fazJogada jogada tabuleiro jogadorAtual

-- Jogada do computador (fácil)
jogadaComputadorFacil :: Tabuleiro -> Char -> IO Tabuleiro
jogadaComputadorFacil tabuleiro jogadorAtual = do
    putStrLn "É a vez do computador."
    jogada <- jogadaComputador tabuleiro
    return $ fazJogada jogada tabuleiro jogadorAtual

-- Jogada do computador (difícil e profissional)
jogadaComputadorInteligente :: (Tabuleiro -> Char -> IO Coord) -> Tabuleiro -> Char -> IO Tabuleiro
jogadaComputadorInteligente estrategia tabuleiro jogadorAtual = do
    putStrLn "É a vez do computador."
    jogada <- estrategia tabuleiro jogadorAtual
    return $ fazJogada jogada tabuleiro jogadorAtual

jogoComAmigo :: IO ()
jogoComAmigo = do
    putStrLn "Você escolheu jogar com um amigo."
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loopJogo tabuleiro jogadorX jogadaHumana
    putStrLn "  "
    menuFinal

jogoContraComputadorFacil :: IO ()
jogoContraComputadorFacil = do
    putStrLn "Você escolheu jogar contra o computador. (Fácil)"
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loopJogo tabuleiro jogadorX jogadaHumanaComputadorFacil
    menuFinal
  where
    jogadaHumanaComputadorFacil tab jogadorAtual = 
        if jogadorAtual == jogadorX 
        then jogadaHumana tab jogadorAtual
        else jogadaComputadorFacil tab jogadorAtual

jogoContraComputadorDificil :: IO ()
jogoContraComputadorDificil = do
    putStrLn "Você escolheu jogar contra o computador. (Difícil)"
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loopJogo tabuleiro jogadorX jogadaHumanaComputadorInteligente
    menuFinal
  where
    jogadaHumanaComputadorInteligente tab jogadorAtual = 
        if jogadorAtual == jogadorX 
        then jogadaHumana tab jogadorAtual
        else jogadaComputadorInteligente jogadaInteligenteComputador tab jogadorAtual

jogoContraComputadorProfissional :: IO ()
jogoContraComputadorProfissional = do
    putStrLn "Você escolheu jogar contra o computador. (Profissional)"
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loopJogo tabuleiro jogadorO jogadaHumanaComputadorInteligente
    menuFinal
  where
    jogadaHumanaComputadorInteligente tab jogadorAtual = 
        if jogadorAtual == jogadorX 
        then jogadaHumana tab jogadorAtual
        else jogadaComputadorInteligente jogadaInteligenteComputador tab jogadorAtual

module Main (main) where

import Sudoku.Main (mainMenu)
import JogoDaVelha.Main (menuInicial)

menu :: IO ()
menu = do
  putStrLn " "
  putStrLn "Escolha o número do jogo: "
  putStrLn "1 - Jogo da Velha"
  putStrLn "2 - Sudoku "
  putStrLn "3 - Sair "
  option <- getLine
  case option of
      "1" -> do
        menuInicial -- Menu Inicial do Jogo da Velha
        menu 
      "2" -> do 
        mainMenu -- Menu Inicial do Sudoku
        menu 
      "3" -> putStrLn "..." 
      _   -> do
          putStrLn "Opção inválida. Tente novamente."

main :: IO ()
main = do
  putStrLn "-- Fliperama --"
  menu
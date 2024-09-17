module Main (main) where

import JogoDaVelha.Main (menuInicial)
import Sudoku.Main (mainMenu)
import Tipos 

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Cont

--callCC (call-with-current-continuation)
menu :: Menu ()
menu = do
  liftIO $ putStrLn " "
  liftIO $ putStrLn "Escolha o número do jogo: "
  liftIO $ putStrLn "1 - Jogo da Velha"
  liftIO $ putStrLn "2 - Sudoku"
  liftIO $ putStrLn "3 - Sair"
  option <- liftIO getLine
  case option of
      "1" -> do
        liftIO menuInicial
        callCC $ \exit -> do
          menu
      "2" -> do
        liftIO mainMenu
        callCC $ \exit -> do
          menu
      "3" -> liftIO $ putStrLn "..."
      _ -> do
          liftIO $ putStrLn "Opção inválida. Tente novamente."
          menu

main :: IO ()
main = do
  putStrLn "-- Fliperama --"
  runContT menu return
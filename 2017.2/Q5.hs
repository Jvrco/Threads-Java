{-
Na preparação de sanduíches em uma lanchonete chamada Pé de Fava, uma pessoa fornece os ingredientes (pão, carne e tomate); duas outras são responsáveis por preparar os sanduíches. 
Porém, a lanchonete dispõe de apenas uma faca para ser utilizada na preparação. Considere que os recipientes de ingredientes são continuamente reabastecidos na capacidade máxima 
de porções (30 para cada ingrediente). Desenvolva uma solução em Haskell que modele o funcionamento desta lanchonete. Utilize memória transacional e variáveis mutáveis.

obs: o resultado final tá meio bugado
-}


import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf

type Faca = TVar Bool

-- Função para reabastecer ingredientes
reabastecer :: MVar Int -> MVar Int -> MVar Int -> IO ()
reabastecer p c t = loop
  where
    loop = do
      paes <- takeMVar p
      carnes <- takeMVar c
      tomates <- takeMVar t
      if paes < 30 || carnes < 30 || tomates < 30
        then do
          printf "Reabastecendo ingredientes...\n"
          putMVar p (min 30 (paes + 1))
          putMVar c (min 30 (carnes + 1))
          putMVar t (min 30 (tomates + 1))
        else do
          printf "Ingredientes no máximo!\n"
          putMVar p paes
          putMVar c carnes
          putMVar t tomates
      loop

-- Função para preparar sanduíches
preparar :: MVar Int -> MVar Int -> MVar Int -> Faca -> IO ()
preparar p c t f = loop
  where
    loop = do
      atomically $ do
        facaDisponivel <- readTVar f
        
        if facaDisponivel
          then writeTVar f False 
          else retry 

      paes <- takeMVar p
      carnes <- takeMVar c
      tomates <- takeMVar t

      if paes > 0 && carnes > 0 && tomates > 0
        then do
          printf "Preparando sanduíche...\n"
          putMVar p (paes - 1)
          putMVar c (carnes - 1)
          putMVar t (tomates - 1)
        else do
          printf "Não foi possível preparar o sanduíche: falta de ingredientes.\n"
          putMVar p paes
          putMVar c carnes
          putMVar t tomates

      atomically $ writeTVar f True
      loop

main :: IO ()
main = do
  -- Inicializa os ingredientes com 30 porções cada
  paes <- newMVar 30
  carnes <- newMVar 30
  tomates <- newMVar 30
  -- Inicializa a faca disponível (True significa que está livre)
  faca <- atomically $ newTVar True

  -- Cria a thread para reabastecimento
  forkIO $ reabastecer paes carnes tomates

  -- Cria duas threads para preparar sanduíches
  forkIO $ preparar paes carnes tomates faca
  forkIO $ preparar paes carnes tomates faca

  -- Mantém o programa rodando para observar o comportamento das threads
  threadDelay 10000000


import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf
import Control.Monad (forever)

type Ingrediente = TVar Int
type Faca = MVar ()

-- Função para reabastecer ingredientes
reabastecer :: Ingrediente -> Ingrediente -> Ingrediente -> IO ()
reabastecer pao carne tomate = forever $ do
    atomically $ do
        paes <- readTVar pao
        carnes <- readTVar carne
        tomates <- readTVar tomate
        if paes < 30 || carnes < 30 || tomates < 30
          then do
            writeTVar pao (min 30 (paes + 1))
            writeTVar carne (min 30 (carnes + 1))
            writeTVar tomate (min 30 (tomates + 1))
          else retry -- Espera até que seja necessário reabastecer

    -- Imprime o estado dos ingredientes após o reabastecimento (fora de 'atomically')
    paes <- atomically $ readTVar pao
    carnes <- atomically $ readTVar carne
    tomates <- atomically $ readTVar tomate
    printf "Reabastecimento: Pão: %d, Carne: %d, Tomate: %d\n" paes carnes tomates

-- Função para preparar sanduíches
preparar :: Ingrediente -> Ingrediente -> Ingrediente -> Faca -> IO ()
preparar pao carne tomate faca = forever $ do
  -- Tenta pegar a faca (bloqueia até que esteja disponível)
  takeMVar faca
  atomically $ do
    paes <- readTVar pao
    carnes <- readTVar carne
    tomates <- readTVar tomate
    if paes > 0 && carnes > 0 && tomates > 0
      then do
        writeTVar pao (paes - 1)
        writeTVar carne (carnes - 1)
        writeTVar tomate (tomates - 1)
      else retry -- Espera até que haja ingredientes suficientes para preparar o sanduíche
  -- Solta a faca
  putMVar faca ()
  printf "Sanduíche preparado!\n"

main :: IO ()
main = do
  -- Inicializa os ingredientes com 30 porções cada
  pao <- atomically $ newTVar 30
  carne <- atomically $ newTVar 30
  tomate <- atomically $ newTVar 30
  -- Inicializa a faca como MVar (liberada inicialmente)
  faca <- newMVar ()

  -- Cria a thread para reabastecimento
  forkIO $ reabastecer pao carne tomate

  -- Cria duas threads para preparar sanduíches
  forkIO $ preparar pao carne tomate faca
  forkIO $ preparar pao carne tomate faca

  -- Mantém o programa rodando para observar o comportamento das threads
  threadDelay 10000000

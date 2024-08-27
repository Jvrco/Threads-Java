{-
Um fabricante de sorvetes contratou você para simular parte do processo de produção deles. Na produção, a mistura de dois ingredientes (o aromatizante e o espessante) 
acontece apenas quando o recipiente de mistura refrigerado (RMR) está disponível, ou seja, eles são retirados de diferentes depósitos quando podem ser efetivamente 
misturados. Para o RMR ficar disponível, é necessário que ele seja esvaziado, o que acontece com o giro do RMR a fim de retirar o sorvete. Assim, as operações de 
retirada do sorvete e de mistura dos ingredientes precisam do RMR de forma exclusiva. Defina as operações para misturar os ingredientes e esvaziar o RMR. Considere 
que os ingredientes ficam guardados em depósitos distintos e que vamos diferenciar a quantidade que utilizamos de cada um deles. Além disso, vamos abstrair o tempo 
necessário para misturar ingredientes e returar sorvete do recipiente.

a) Implemente, em Haskell, o que foi descrito acima, utilizando mutable variables (MVar) e memória transacional (TVar), como você achar necessário.
-}


import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf

type Sorvete = TVar Int

main :: IO ()
main = do
  -- Depósitos de ingredientes
  aromatizante <- newMVar 10 -- quantidade inicial de aromatizante
  espessante <- newMVar 10 -- quantidade inicial de espessante
  
  -- Quantidade total de sorvetes produzidos
  sorvete <- atomically (newTVar 0)

  -- RMR: Recipiente de Mistura Refrigerado
  rmr <- newMVar ()

  -- Indica se o sorvete está pronto para ser retirado
  sorvetePronto <- atomically $ newTVar False

  -- Executa as operações de mistura e retirada em threads separadas
  forkIO $ mistura rmr aromatizante espessante sorvetePronto
  forkIO $ retirada rmr sorvetePronto sorvete

  -- Espera a conclusão (para manter a main rodando)
  threadDelay 10000000

mistura :: MVar () -> MVar Int -> MVar Int -> TVar Bool -> IO ()
mistura rmr a e sorvetePronto = loop
  where loop = do
          takeMVar rmr -- Aguarda o RMR estar disponível
          aromatizante <- takeMVar a
          espessante <- takeMVar e
          if aromatizante > 0 && espessante > 1
            then do
              printf "Misturando 1 aromatizante e 2 espessantes no RMR.\n"
              putMVar a (aromatizante - 1)
              putMVar e (espessante - 2)
              atomically $ writeTVar sorvetePronto True -- Indica que o sorvete está pronto para ser retirado
            else do
              printf "Ingredientes insuficientes para a mistura.\n"
              putMVar a aromatizante
              putMVar e espessante
          putMVar rmr () -- Libera o RMR para a próxima operação
          loop

retirada :: MVar () -> TVar Bool -> TVar Int -> IO ()
retirada rmr sorvetePronto s = loop
  where loop = do
          takeMVar rmr -- Aguarda o RMR estar disponível
          pronto <- atomically $ readTVar sorvetePronto
          if pronto
            then do
              atomically $ do
                sorvetes <- readTVar s
                writeTVar s (sorvetes + 1)
                writeTVar sorvetePronto False -- Reseta o status do sorvete para não pronto
              printf "Sorvete retirado! Quantidade total: %d\n" =<< atomically (readTVar s)
            else do
              printf "Nenhum sorvete pronto para ser retirado.\n"
          putMVar rmr () -- Libera o RMR para a próxima operação
          loop

{-
Um semáforo binário é definido tradicionalmente pelas operações "p" e "v". A primeira adquire um lock, fazendo o seguinte: testa se o lock é verdadeiro, se sim, 
adquire o lock tornando-o falso, senão espera. A segunda operação libera o lock, tornando-o verdadeiro. Defina um semáforo em Haskell usando STM. 
Para isso, defina um novo tipo com uma variável transacional do tipo Bool. Defina as duas funções "p" e "v".
-}

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

type Semaforo = TVar Bool

p :: Semaforo -> STM ()
p semaforo = do
    lock <- readTVar semaforo
    if lock
        then writeTVar semaforo False 
        else retry 

v :: Semaforo -> STM ()
v semaforo = writeTVar semaforo True 

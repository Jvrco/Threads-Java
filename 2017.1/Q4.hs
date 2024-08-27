import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- Definindo o tipo Conta como uma TVar Int
type Conta = TVar Int

-- (a) saque: Realiza a retirada de uma quantia de uma conta
saque :: Conta -> Int -> STM ()
saque conta x = do
    saldo <- readTVar conta
    writeTVar conta (saldo - x)

-- (b) deposito: Realiza um depósito em uma conta, utilizando a função saque
deposito :: Conta -> Int -> STM ()
deposito conta x = saque conta (-x)

-- (c) saque2: Realiza a retirada bloqueando se o saldo for insuficiente
saque2 :: Conta -> Int -> STM ()
saque2 conta x = do
    saldo <- readTVar conta
    if saldo >= x
        then writeTVar conta (saldo - x)
        else retry -- retry bloqueia até que o saldo seja suficiente

-- (d) saque3: Tenta retirar de uma conta A, e se não conseguir, tenta retirar de uma conta B
saque3 :: Conta -> Conta -> Int -> STM ()
saque3 contaA contaB x = saque2 contaA x `orElse` saque2 contaB x

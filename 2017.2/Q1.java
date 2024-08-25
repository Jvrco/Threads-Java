import java.util.Random;

class Conta {
    private int saldo;

    public Conta(int saldoInicial) {
        this.saldo = saldoInicial;
    }

    public synchronized void sacar(int valor) {
        if (valor > saldo) {
            System.out.println(Thread.currentThread().getName() + " tentou sacar " + valor + ", mas o saldo é insuficiente. Saldo atual: " + saldo);
        } else {
            saldo -= valor;
            System.out.println(Thread.currentThread().getName() + " sacou: " + valor + ". Saldo atual: " + saldo);
        }
    }

    public synchronized void depositou(int valor) {
        saldo += valor;
        System.out.println(Thread.currentThread().getName() + " depositou: " + valor + ". Saldo atual: " + saldo);
    }

    public int getSaldo() {
        return this.saldo;
    }
}

class Pessoa implements Runnable {
    private final Conta conta;
    private final Random random;

    public Pessoa(String nome, Conta conta) {
        Thread.currentThread().setName(nome);
        this.conta = conta;
        this.random = new Random();
    }

    public void run() {
        while (!Thread.currentThread().isInterrupted()) { // Roda até a thread ser interrompida
            int acao = random.nextInt(2); // 0 = depósito, 1 = saque
            int valor = random.nextInt(100) + 1; // Valores de 1 a 100

            if (acao == 0) {
                conta.depositou(valor);
            } else {
                conta.sacar(valor);
            }

            try {
                Thread.sleep(100); // Pausa entre as operações
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt(); // Reinterrompe a thread para sair do loop
            }
        }
    }
}

public class Q1 {
    public static void main(String[] args) {
        Conta contaCompartilhada = new Conta(0);

        // Criando as threads para as pessoas Maria, João, José, Fred e outras duas
        Thread maria = new Thread(new Pessoa("Maria", contaCompartilhada));
        Thread joao = new Thread(new Pessoa("João", contaCompartilhada));
        Thread jose = new Thread(new Pessoa("José", contaCompartilhada));
        Thread fred = new Thread(new Pessoa("Fred", contaCompartilhada));
        Thread ana = new Thread(new Pessoa("Ana", contaCompartilhada));
        Thread pedro = new Thread(new Pessoa("Pedro", contaCompartilhada));

        // Iniciando as threads
        maria.start();
        joao.start();
        jose.start();
        fred.start();
        ana.start();
        pedro.start();

        try {
            // Deixa as threads rodarem por 10 segundos
            Thread.sleep(10000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // Interrompendo as threads após 10 segundos
        maria.interrupt();
        joao.interrupt();
        jose.interrupt();
        fred.interrupt();
        ana.interrupt();
        pedro.interrupt();

        // Aguardando a conclusão de todas as threads
        try {
            maria.join();
            joao.join();
            jose.join();
            fred.join();
            ana.join();
            pedro.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // Exibindo o saldo final da conta
        System.out.println("Saldo final: " + contaCompartilhada.getSaldo());
    }
}

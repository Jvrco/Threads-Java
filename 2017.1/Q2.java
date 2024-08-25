/*
 * Implemente em Java as classes Produtor, Consumidor e ProdutorConsumidor. Esta última possui o método main(). 
 * Os dados produzidos são valores do tipo inteiro. Não se pode utilizar classes da API de Java como, por exemplo, interface BlockingQueue, 
 * que possui métodos put e take, utilizando a implementação ArrayBlockingQueue.
 */


import java.util.LinkedList;
import java.util.Queue;

class Buffer {
    private Queue<Integer> buffer;
    private int capacidade;

    public Buffer(int capacidade) {
        this.buffer = new LinkedList<>();
        this.capacidade = capacidade;
    }

    public synchronized void produzir(int x) {
        while (buffer.size() == capacidade) {
            try {
                wait();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        buffer.add(x);
        System.out.println("Produzido: " + x);
        notifyAll();
    }

    public synchronized void consumir() {
        while (buffer.isEmpty()) {
            try {
                wait();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        int valor = buffer.poll();
        System.out.println("Consumido: " + valor);
        notifyAll();
    }
}

class Produtor implements Runnable {
    private Buffer buffer;

    public Produtor(Buffer buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        
        buffer.produzir(1);
        try {
            Thread.sleep(100); // Simula tempo de produção
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        
    }
}

class Consumidor implements Runnable {
    private Buffer buffer;

    public Consumidor(Buffer buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        
        buffer.consumir();
        try {
            Thread.sleep(150); 
        }catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        
    }
}

public class Q2 {
    public static void main(String[] args) {
        Buffer buffer = new Buffer(5); // Capacidade do buffer
        Thread produtorThread = new Thread(new Produtor(buffer));
        Thread consumidorThread = new Thread(new Consumidor(buffer));

        produtorThread.start();
        consumidorThread.start();

        try {
            produtorThread.join();
            consumidorThread.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}

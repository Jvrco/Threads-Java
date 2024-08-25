/*
 * Implemente a classe Semáforo em Java com dois métodos que realizam as seguintes operações:
 * up: incrementa o contador do semáforo e acorda threads que estejam bloqueadas.
 * down: decrementa o contador do semáforo ou, caso seja zero, suspende a thread.
 * class Semaforo extends Object {
	private int contador;

	public Semaforo (int inicial) {
    	contador = inicial;
	}

	public void down() {
    	synchronized (this) {...}
	}

	public void up() {
    	synchronized (this) {...}
	}
 * }
 */

 class Semaforo extends Object {
	int contador;

	public Semaforo (int inicial) {
    	this.contador = inicial;
	}

	public synchronized void up() {
        contador+=1;
        notify();
    	
	}

	public synchronized void down() {
        while(contador == 0){
            try{
                wait();
            }catch(InterruptedException e){
            }
        }
        contador -=1;
    	
	}
}



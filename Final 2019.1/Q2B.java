/*
 * Um fabricante de sorvetes contratou você para simular parte do processo de produção deles. Na produção, a mistura de dois ingredientes (o aromatizante e o espessante) 
 * acontece apenas quando o recipiente de mistura refrigerado (RMR) está disponível, ou seja, eles são retirados de diferentes depósitos quando podem ser efetivamente 
 * misturados. Para o RMR ficar disponível, é necessário que ele seja esvaziado, o que acontece com o giro do RMR a fim de retirar o sorvete. Assim, as operações de 
 * retirada do sorvete e de mistura dos ingredientes precisam do RMR de forma exclusiva. Defina as operações para misturar os ingredientes e esvaziar o RMR. Considere 
 * que os ingredientes ficam guardados em depósitos distintos e que vamos diferenciar a quantidade que utilizamos de cada um deles. Além disso, vamos abstrair o tempo 
 * necessário para misturar ingredientes e returar sorvete do recipiente.
 * 
 * b) Apresente uma solução em Java. Pode-se utilizar classes da API de concorrência de java, não envolvendo tipos primitivos atômicos.
 */

 class RMR{
    private int aromatizante;
    private int espessante;
    private boolean sorvetePronto = false;
    private int sorvetesProduzidos;


    public RMR(int espessante,int aromatizante){
        this.espessante = espessante;
        this.aromatizante = aromatizante;


    }


    public synchronized void  misturar(){
        while(sorvetePronto){
            try{
                wait();
            }catch(InterruptedException e){
                Thread.currentThread().interrupt();               
            }

        }
        if (aromatizante > 0 && espessante > 1) {
            System.out.println("Misturando 1 aromatizante e 2 espessantes no RMR.");
            aromatizante--;
            espessante -= 2;
            sorvetePronto = true;
        } else {
            System.out.println("Ingredientes insuficientes para a mistura.");
        }

        notifyAll();



    }


    public synchronized void retirar(){
        while(!sorvetePronto){
            try{
                wait();
            }catch(InterruptedException e){
                Thread.currentThread().interrupt();               
            }

        }
        sorvetesProduzidos++;
        System.out.println("Sorvete retirado! Quantidade total: " + sorvetesProduzidos);
        sorvetePronto = false;

        notifyAll();
    }
 }

 class Mistura implements Runnable{
    private RMR rmr;
    public Mistura(RMR rmr){
        this.rmr = RMR;
    }

    public void run(){
        while(true){
            rmr.misturar();
            try {
                Thread.sleep(1000); // Simula o tempo necessário para a mistura
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
 }



 class RetiradaTask implements Runnable {
    private RMR rmr;

    public RetiradaTask(RMR rmr) {
        this.rmr = rmr;
    }

    @Override
    public void run() {
        while (true) {
            rmr.retirar();
            try {
                Thread.sleep(1500); // Simula o tempo necessário para a retirada
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}


public class Q2B {
    public static void main(String[] args) {
        RMR rmr = new RMR(10, 10); // Quantidade inicial de aromatizante e espessante

        Thread misturaThread = new Thread(new MisturaTask(rmr));
        Thread retiradaThread = new Thread(new RetiradaTask(rmr));

        misturaThread.start();
        retiradaThread.start();
    }
}
public class Ejercicio2{
  
  static int factorial(int n){
    if(n==1)
      return n;
    return n * factorial(n-1);
  }
  
  public static void main(String[] args){
    //java es glotón porque deberá evaluar el factorial completo y el stack se acabará.
    boolean a = true && (factorial(10000) > 1);
    System.out.println(a);
  }
  
}

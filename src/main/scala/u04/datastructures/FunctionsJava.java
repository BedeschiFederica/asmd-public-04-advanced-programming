package u04.datastructures;

import java.util.function.UnaryOperator;

public class FunctionsJava {

    static int factorial(int n){
        return switch(n){
            case 0 | 1 -> 1;
            default -> n * factorial(n - 1);
        };
    }

    static <X> X applyManyTimes(X initial, int n, UnaryOperator<X> f){
        return switch(n){
            case 0 -> initial;
            default -> applyManyTimes(f.apply(initial), n - 1, f);
        };
    }

    record Point2D(double x, double y){}

    static Point2D multiply(Point2D p, double d){
        return switch(p){
            case Point2D(double x, double y) -> new Point2D(x * d, y * d);
        };
    }

    public static void main(String[] args){
        System.out.println(factorial(5));
        System.out.println(applyManyTimes(0, 10, x -> x + 2));
        System.out.println(multiply(new Point2D(10, 20), 1.5));
    }
}

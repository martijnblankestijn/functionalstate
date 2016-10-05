package nl.java;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

enum Input {
  Coin, Turn
}
// basically just a tuple
class Result<A, S> {
  public final A a;
  public final S s;

  public Result(A a, S s) {
    this.a = a;
    this.s = s;
  }

  @Override
  public String toString() {
    return "State: " + s + ", value: " + a;
  }
}


class State<S, A> {
  public final Function<S, Result<A, S>> run;

  public State(Function<S, Result<A, S>> f) {
    this.run = f;
  }
}

// again, poor substitute for a Tuple.
class CandiesAndCoins {
  public final int candies;
  public final int coins;

  CandiesAndCoins(int candies, int coins) {
    this.candies = candies;
    this.coins = coins;
  }

  @Override
  public String toString() {
    return "candies=" + candies + ", coins=" + coins;
  }
}


public class JavaMachine {
  public final boolean locked;
  public final int candies;
  public final int coins;

  public JavaMachine(boolean locked, int candies, int coins) {
    this.locked = locked;
    this.candies = candies;
    this.coins = coins;
  }

  JavaMachine process(Input input) {
    if (candies == 0) {
      System.out.println("No candies ignoring input: " + input);
      return this;
    } else return (input == Input.Coin) ? insertCoin(1) : turnKnob();
  }


  private JavaMachine insertCoin(int inserted) {
    System.out.println("Coin inserted");
    return locked ? new JavaMachine(false, this.candies, this.coins + inserted) : this;
  }

  private JavaMachine turnKnob() {
    System.out.println("Knob turned");
    return locked ? this : new JavaMachine(true, this.candies - 1, this.coins);
  }

  @Override
  public String toString() {
    return "locked=" + locked + ", candies=" + candies + ", coins=" + coins;
  }

  public static State<JavaMachine, CandiesAndCoins> simulateMachine(List<Input> inputs) {
    Function<JavaMachine, Result<CandiesAndCoins, JavaMachine>> stateFunction = m -> {
      
      System.out.println("Processing input");
      Result<CandiesAndCoins, JavaMachine> z = new Result<>(new CandiesAndCoins(0, 0), m);
      
      return foldLeft(inputs, z, (b, input) -> {
        JavaMachine machine = b.s.process(input);
        return new Result<>(new CandiesAndCoins(machine.candies, machine.coins), machine);

      });
    };
    System.out.println("Creating machine");
    return new State<>(stateFunction);
  }

  /**
   * Sadly the Stream API does not support a foldLeft.
   */
  private static <A, B> B foldLeft(List<A> inputs,
                                   B r,
                                   BiFunction<B, A, B> c) {
    B result = r;
    for (A input : inputs) {
      result = c.apply(result, input);

    }
    return result;
  }
}

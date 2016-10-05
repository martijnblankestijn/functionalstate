package nl.java;

import org.junit.Test;

import java.util.List;

import static java.util.Arrays.asList;
import static nl.java.Input.Coin;
import static nl.java.Input.Turn;
import static nl.java.JavaMachine.simulateMachine;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;


public class JavaMachineTest {
  @Test
  public void unlockWhenCoinInsertedAndCandyAvailable() {
    JavaMachine machine = new JavaMachine(true, 1, 0);
    JavaMachine newMachine = machine.process(Coin);
    
    assertThat(newMachine.candies, is(1));
    assertThat(newMachine.locked, is(false));
    assertThat(newMachine.coins, is(1));
  }
  
  @Test
  public void turnOnUnlockedMachineWillDispenseCandy() {
    JavaMachine machine = new JavaMachine(false, 1, 0);
    JavaMachine newMachine = machine.process(Turn);

    assertThat(newMachine.candies, is(0));
    assertThat(newMachine.locked, is(true));
    assertThat(newMachine.coins, is(0));
  }
  
  
  @Test
  public void testSimulation() {

    JavaMachine initialMachine = new JavaMachine(true, 5, 10);

    List<Input> inputs = asList(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn);
    State<JavaMachine, CandiesAndCoins> state = simulateMachine(inputs);

    System.out.println("Machine created, apply inputs " + inputs);
    //val ((coins, candies), m) = 
    Result<CandiesAndCoins, JavaMachine> result = state.run.apply(initialMachine);

    System.out.println("Result acquired.");
    assertThat(result.s.locked, is(true));
    assertThat(result.a.candies, is(1));
    assertThat(result.a.coins, is(14));


    List<Input> secondInput = asList(Coin, Turn, Coin, Turn);
    State<JavaMachine, CandiesAndCoins> again = simulateMachine(secondInput);
    
    System.out.println("Machine created, apply inputs " + secondInput);
    Result<CandiesAndCoins, JavaMachine> resultAgain = again.run.apply(result.s);

    System.out.println("Again result acquired.");
    assertThat(resultAgain.s.locked, is(true));
    assertThat(resultAgain.a.candies, is(0));
    assertThat(resultAgain.a.coins, is(15));
    
  }
}

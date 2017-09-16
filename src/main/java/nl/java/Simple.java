package nl.java;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Arrays.asList;

public class Simple {
int foo;

  public static void main(String[] args) {

final int foo = 1;

Stream.of(1,2,3)
    .map(x -> x + 1)
    .collect(
        Collectors.toList()
    );

  int x = foo  + 1;
  }
  
  void dummy() {
    foo = 1;
    int y = foo;
  }
  
}

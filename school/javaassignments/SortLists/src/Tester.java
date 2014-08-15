import java.math.BigInteger;
import java.util.Scanner;

public class Tester {

	/**
	 * @param args
	 * This is the main function.
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		BigIntegerLinkedList list = new BigIntegerLinkedList();
		
		long startTime = 0, stopTime = 0;
		
		Scanner kb = new Scanner(System.in);
		
		list.add(new BigInteger(""+5));
		list.add(new BigInteger(""+4));
		list.add(new BigInteger(""+6));
		
		list.selectionSort();
	}

}

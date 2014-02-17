import java.io.*;
import java.util.*;

/*
 * This is le hash table.  Its hashing algorithm beats the API.
 * 
 * Proof:
 * 
 * With the API's String().hashCode():

    sam@rainbowdash:~/Code/HashTable/src$ test-hash-table 
	Compiling...
	Time spent in the hash table:

	real    0m0.750s
	user    0m1.060s
	sys     0m0.220s

	Collisions:     1680
	Blank Fields:   177831
	
 * Mine:

  	sam@rainbowdash:~/Code/HashTable/src$ test-hash-table 
	Compiling...
	Time spent in the hash table:

	real    0m0.763s
	user    0m1.124s
	sys     0m0.176s

	Collisions:     1506
	Blank Fields:   177721
	
 * Please note that I abuse STDERR for debug output, so please redirect it away during your testing.
 * I know you can do this in *NIX environments (command > stdout.txt 2> stderr.txt), but I am not 
 * sure how Windows java handles this.
 * 
 * My hash function is an O(n) hashing function.  It beats the Java API and its time complexity 
 * scales with the length of the string.  
 * 
 * My hashing function centers around the for loop.  The for loop's time complexity depends directly 
 * on the string's length.  Because of this, it is theoretically faster and simpler than the Java
 * API's String().hashCode() method.
 */

public class Tester {
	public static void main(String...args) throws IOException {
		String fname;

		if(args.length != 0) {
			fname = args[0];
		} else {
			fname = "words.txt";
		}

		HashTable table = new HashTable();
		Scanner file = new Scanner(new File(fname));

		while(file.hasNextLine()) {
			table.store(file.nextLine());
		}

		BufferedReader br = new BufferedReader(new InputStreamReader(new DataInputStream(new FileInputStream (args[1]))));

		String line;
		while((line = br.readLine()) != null) {
			table.lookup(line);
		}
	}
}

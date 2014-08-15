import java.util.ArrayList;
import java.util.Collections;

public class HashTable {
	private final int size = 199999;
	private ArrayList<ArrayList <Anagram>> table = new ArrayList<ArrayList <Anagram>>(size);
	private int collisions = 0;
	private int empty = 0;

	public HashTable(){
		for (int i = 0; i < size; i++) {
			table.add(new ArrayList<Anagram>());
		}
	}

	public int genHash(String word) {
		int code = 0;
		char[] ara = word.toCharArray();

		for(int i = 0; i < ara.length; i++) {
			code += (ara[i] * ara[i] * ara[i] + ara[i]*2);
		}

		return code % size;
	}

	public int hashPos(String word) {
		return Math.abs(genHash(new Anagram(word).getKey()) % size);
		//return Math.abs((new Anagram(word).getKey().hashCode())) % size;
	}

	public void store(String word) {
		//table[hashPos(word)].add(new Anagram(word));
		int position = hashPos(word);
		
		word = word.toLowerCase();
		
		if(word.equals("")) {
			return;
		}

		/*try {
			table.get(position);
		} catch (ArrayIndexOutOfBoundsException e) {
			table.add(position, new ArrayList<Anagram>());
		}*/

		/*if(table.get(position) == null) {
			table.add(position, new ArrayList<Anagram>());
		}*/

		Anagram toAdd = new Anagram(word);

		for(Anagram a : table.get(position)) {
			if(!a.getKey().equals(toAdd.getKey())) {
				System.err.printf("Collision detected at %s and %s\n", toAdd.getWord(), a.getWord());
				collisions++;
			}
		}

		table.get(position).add(toAdd);
	}

	private ArrayList<Anagram> lookupList(String word) {
		int position = hashPos(word);

		return table.get(position);
	}

	public void lookup(String word) {
		ArrayList<Anagram> collisions = new ArrayList<Anagram>();
		ArrayList<Anagram> matches = new ArrayList<Anagram>();
		ArrayList<Anagram> results = lookupList(word);
		Anagram correctWord = new Anagram(word);
		int count = 0;

		for(Anagram ana : results) {
			if(ana.getKey().equals(correctWord.getKey())) {
				if(!ana.getWord().equals(correctWord.getWord())) {
					matches.add(ana);
					count++;
				}
			} else {
				collisions.add(ana);
			}
		}

		System.out.printf("%s %d [", word, count);

		for(Anagram ana : matches) {
			if(!ana.getKey().equals(correctWord.getKey())) {
				System.out.printf("\"%s\" ", ana.getWord());
			}
		}
		
		System.out.println("]");
	}

	//Assuming hash table will never be bigger than memory
	public String toString() {
		System.out.println("Hash\tKey\tValue");
//		String buf = "Hash\tkey\tvalue";
		int pos = 0;

		for(ArrayList<Anagram> arr : table) {
			if(arr.size() != 0) {
//				buf = buf + "" + pos + "\t";
				System.out.printf("%d\t", pos);
				for(Anagram ana : arr) {
//					buf = buf + ana.toString();
//					buf = buf + ",\t";
					System.out.printf("%s,\t", ana.toString());
				}

//				buf = buf + "\n";
				System.out.println();
			} else {
				empty++;
			}

			pos++;
		}
		
//		buf = buf + "\nCollisions:\t" + collisions;
//		buf = buf + "\nBlank fields: \t" + empty;
		System.out.printf("\nCollisions:\t%d\nBlank Fields:\t%d\n", collisions, empty);

		return "";
	}
}

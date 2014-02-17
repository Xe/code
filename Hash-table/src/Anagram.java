//import java.util.ArrayList;
import java.util.*;

public class Anagram {
	private String word;
	private String key;

	public Anagram(String word) {
		this.word = word.toLowerCase();

		sortLetters();
	}

	//modifies the word in the anagram
	private void sortLetters() {
		char[] splitWord = this.word.toCharArray();

		Arrays.sort(splitWord);

		this.key = new String(splitWord);
	}

	public String getKey(){ return key; }
	public String getWord(){ return word; }

	public int compareTo(Anagram other) {
		return key.compareTo(other.getKey());
	}

	public boolean equals(Anagram other) {
		return this.compareTo(other) == 0;
	}

	/*public int getHash() {
		return key.hashCode();
	}*/

	public String toString() {
		return this.key + ":\t" + this.word;
	}
}

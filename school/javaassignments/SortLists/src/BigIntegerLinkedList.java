import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigInteger;

public class BigIntegerLinkedList {
	private int size = 0;
	
	public Node head = new Node();
	
	int dataCompares = 0;
	int dataAssigns = 0;
	int loopCompares = 0;
	int loopAssigns = 0;
	int other = 0;
	
	/**
	 * @author sam
	 * Inner Node class for the list
	 */
	class Node {
		Node next, prev;
		BigInteger data;
		
		/**
		 * Full constructor
		 * @param data
		 * @param next
		 * @param prev
		 */
		Node(BigInteger data, Node next, Node prev) {
			this.data = data;
			this.next = next;
			this.prev = prev;
		}
		
		/**
		 * Extremely truncated constructor that just sets the data field.
		 * @param data
		 */
		Node(BigInteger data) {
			this.data = data;
		}
		
		/**
		 * Head constructor
		 */
		Node() {
			this.next = this;
			this.prev = this;
			this.data = null;
		}
	}
	
	public void add(int data) {
		add(new BigInteger("" + data));
	}
	
	/**
	 * Add a new node to the list.
	 * @param data
	 */
	public void add(BigInteger data) {
		Node newNode = new Node(data);
		
		if(size == 0) {
			head.next = newNode;
			head.prev = newNode;
			newNode.next = head;
			newNode.prev = head;
			
			size++;
		} else {
			Node elem = head.prev;
			newNode.next = head;
			elem.next = newNode;
			newNode.prev = elem;
			
			size++;
		}
	}
	
	/**
	 * Nuke the list
	 */
	public void clear() {
		size = 0;
		
		head = new Node();
	}
	
	public void removeAll() {
		clear();
	}
	
	/**
	 * returns the size of the list
	 * @return the size of the list as a number of nodes
	 */
	public int getSize() {
		return this.size;
	}
	
	public void bubbleSort()
	{
		Node cur_next, cur;
		BigInteger temp;
		
		cur = head.next;
		cur_next = cur.next;
		
		boolean switched;
		boolean done = false;
		
		while(!done)
		{
			switched = false;
			while(cur.next != head)
			{
				if(cur_next.data.compareTo(cur.data)<0)
				{
					// swap values
					temp = cur_next.data;
					cur_next.data = cur.data;
					cur.data = temp;
					
					switched = true;
				}
			}
			
			if(switched)
			{
				cur = head.next;
				cur_next = cur.next;
			}
			else
			{
				done = true;
			}
		}
	}
	
	public void insertSortNode()
	{
		Node insert1, cur = null;
		Node temp = null; // hod last insert1 point
		
		for(insert1 = this.head.next.next; insert1 != this.head; insert1 = insert1.next)
		{
		// loop compares and loop assigns
			for( cur = head.next; cur != insert1; cur = cur.next)
			{
				// data comparison
				if(insert1.data.compareTo(cur.data)<0)
				{
					// do the swap
					temp = insert1.prev;
					
					//unlink insert1 node
					insert1.prev.next = insert1.next;
					insert1.next.prev = insert1.prev;
					
					//attach node before cur
					insert1.prev = cur.prev; // first link up unattached node
					insert1.next = cur;
					
					cur.prev.next = insert1; // second attach inserted node
					cur.prev = insert1;
					
					insert1 = temp;
					
					break;
				}
			}
		}
	}
	
	public void insertSortArray()
	{
		Node insert1, cur, tmp1;
		BigInteger temp;
		
		// int dataComapres, dataAssigns;
		// int loopCompares, loopAssigns;
		// int other;
		
		for(insert1 = this.head.next.next; insert1 != this.head; insert1 = insert1.next)
		{
			++loopCompares; ++loopAssigns;
			for( cur = head.next; cur != insert1; cur = cur.next )
			{
				++loopCompares; ++loopAssigns;
				
				++dataCompares;
				if( insert1.data.compareTo(cur.data) < 0 )
				{
					temp = insert1.data;
					++dataAssigns;
					
					tmp1 = insert1;
					
					while( tmp1 != cur.prev )
					{
						++ loopCompares;
						tmp1.data = tmp1.prev.data;
						tmp1 = tmp1.prev;
						dataAssigns+=2;
					}
					++loopCompares;
					
					cur.data = temp;
					++dataAssigns;
					
					break;
				}
			}
			++loopCompares; ++loopAssigns;
		}
		++loopCompares; ++loopAssigns;
	}
	
	/**
	 * As the name implies, this does a selection sort
	 */
	public void selectionSort() {
		BigInteger min;
		
		Node curr;
		Node smaller;
		
		for(curr = head.next; curr.data != null; curr = curr.next) {
			++loopCompares;
			min = curr.data;
			smaller = curr;
			
			for(Node ncurr = curr.next; ncurr.data != null; ncurr = ncurr.next) {
				++loopCompares;
				if(ncurr.data.compareTo(min) > 0) {
					min = ncurr.data;
					smaller = ncurr;
					dataAssigns += 2;
				}
				++dataCompares;
			}
			
			if(!(min.equals(curr.data))) {
				smaller.data = curr.data;
				curr.data = min;
				min = null;
				dataAssigns += 3;
			}
			++dataCompares;
		}
	}
	
	public void zeroStats() {
		dataCompares = 0;
		dataAssigns = 0;
		loopCompares = 0;
		loopAssigns = 0;
		other = 0;
	}

	public String getTotal()
	{
		int total;	
		total = dataCompares + dataAssigns + loopCompares + loopAssigns + other;
		
		return("" + total);
	}
	
 	public String getDataAssign()
 	{
		int total;	
		total = dataCompares + dataAssigns + loopCompares + loopAssigns + other;
		
		return("" + total);
 	}
	
	public void disp6sortsFile(boolean disp, String fileName, String header, String data)
	{
		FileWriter fw = null;
		PrintWriter pw = null;
			
		try
		{
			File file = new File(fileName);
			fw = new FileWriter(file, true); // append = true
			pw = new PrintWriter(fw, true);
		}
		catch(IOException e)
		{
			System.err.println("File open failed " + fileName + "\n" + e);
			System.exit(-1);
		}
		
		if(disp) // disp header
		{
			pw.print(header + "\n");
		}
		
		pw.print(data + "\n");
		
		pw.close(); // close the file
	}
}

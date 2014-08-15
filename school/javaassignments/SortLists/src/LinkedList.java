import java.util.*; //required for List interface
import java.io.*;
import java.math.BigInteger;

public class LinkedList
{
	protected static class Node
	{
		Comparable item;
		Node next;
		Node prev;
		
		public Node(Object data, Node next, Node prev)
		{
			this.item = item;
			this.next = next;
			this.prev = prev;
		}

		public Node(Object item)
		{
			this(item, null, null);
		}

		public Node()
		{
			this(null, null, null);
		}

	}

private Node head;
private int size;

		int dataCompares = 0;
		int dataAssigns = 0;
		int loopCompares = 0;
		int loopAssigns = 0;
		int other = 0;

public boolean add(Comparable newItem)
{
	Node newNode = new Node(newItem);
	Node curr;
	
	if(isEmpty())
	{
		head.next = newNode;
		head.prev = newNode;
		newNode.next = head;
		newNode.prev = head;
	}
	else
	{
		newNode.next = head;
		newNode.prev = head.prev;
			
		head.prev.next = newNode;
		head.prev = newNode;
	}
	size++;
	return false;
}

public void removeAll()
{
	head = null;
	size = 0;
}

public boolean isEmpty()
{
	if(this.head == null)
	{
		return true;
	}
	else
	{
		return false;
	}
}


// public boolean remove(Comparable item)
// {
// 	if(!isEmpty())
// 	{
// 		Node prev = null;			//start at the beginning of the list.
// 		Node curr = head;
// 		
// 		while(curr != null)
// 		{
// 			if(curr.item.compareTo(item) == 0)// if item matches current item
// 			{
// 				if(prev == null)// remove from the front
// 				{
// 					head = curr.next;//delink first time
// 				}
// 				else
// 				{
// 					prev.next = curr.next;//link around found item
// 					curr = curr.next;//continues linking around found item
// 				}
// 				size--;	// Decrement size
// 				return true;
// 			}
// 			else
// 			{
// 				prev = curr;			//maintain previous reference
// 				curr = curr.next;		// move curr to the next node in the list.
// 			}
// 		}
// 	}
// }

public void bubbleSort()
{
	Node cur_next, cur;
	Comparable temp;
	
	cur = head.next;
	cur_next = cur.next;
	
	boolean switched;
	boolean done = false;
	
	while(!done)
	{
		switched = false;
		while(cur.next != head)
		{
			if(cur_next.item.compareTo(cur.item)<0)
			{
				// swap values
				temp = cur_next.item;
				cur_next.item = cur.item;
				cur.item = temp;
				
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
			if(insert1.item.compareTo(cur.item)<0)
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

public void selectionSort() {
	Comparable min;
	
	Node curr;
	Node smaller;
	
	for(curr = head.next; curr.item != null; curr = curr.next) {
		min = curr.item;
		smaller = curr;
		
		for(Node ncurr = curr.next; ncurr.item != null; ncurr = ncurr.next) {
			if(ncurr.item.compareTo(min) > 0) {
				min = ncurr.item;
				smaller = ncurr;
			}
		}
		
		if(!(min.equals(curr.item))) {
			smaller.item = curr.item;
			curr.item = min;
			min = null;			
		}
	}
}

public void insertSortArray()
{
	Node insert1, cur, tmp1;
	Comparable temp;
	
	// int dataComapres, dataAssigns;
	// int loopCompares, loopAssigns;
	// int other;
	
	for(insert1 = this.head.next.next; insert1 != this.head; insert1 = insert1.next)
	{
		//++loopCompares; ++loopAssigns;
		for( cur = head.next; cur != insert1; cur = cur.next )
		{
			//++loopComares; ++loopAssigns;
			
			//++dataCompares
			if( insert1.item.compareTo(cur.item) < 0 )
			{
				temp = insert1.item;
				// ++dataAssign
				
				tmp1 = insert1;
				
				while( tmp1 != cur.prev )
				{
					//++ loopCompares
					tmp1.item = tmp1.prev.item;
					tmp1 = tmp1.prev;
					//dataAssign+=2;
				}
				//++loopCompares
				
				cur.item = temp;
				//++dataAssign;
				
				break;
			}
		}
		//++loopCompares; ++loopAssigns;
	}
	//++loopCompares; ++loopAssigns;
}

	public void zeroStats()
	{
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
import java.util.*; //Required for list interface.

public class LinkedQueue
{
	protected static class Node <Character>
	{
		Job data;
		Node next;
		
		public Node( Job data, Node next )
		{
			this.data = data;
			this.next = next;
		}
		
		public Node(Job data)
		{
			this(data, null);
		}
		
		public Node()
		{
			this(null, null);
		}
	}
	
	private Node tail;
	
	public LinkedQueue()
	{
		this.tail = null;
	}
	
	// Checks to see if the Linked List is empty.
	public boolean isEmpty()
	{
		return(this.tail == null);
	}
	
	//The dummy Node tail is used as a double pointer with:
	//tail 		-> last
	//tail.next -> first
	
	//Adds to the queue
	public void enqueue(Job data)
	{
		Node newNode = new Node(data, null);
		
		if(isEmpty()) // Case of 0
		{
			this.tail = newNode;
			this.tail.next = null; //this.tail;
		}
		else // Case of 1 or more.
		{
			newNode.next = this.tail.next;
			this.tail.next = newNode;
			this.tail = newNode;
		}
	}
	
	// Peek returns the value at the front of the queue without removing it.
	public Job peek()
	{
		if(isEmpty())
		{
			throw new IllegalStateException(" peek called on empty state " );
		}
		else
		{
			return(tail.next.data);
		}
	}
	
	// dequeue: removes a node from the queue
	public Job dequeue()
	{
		if(isEmpty())// Case of 0
		{
			throw new IllegalStateException(" dequeue called on an empty state ");
		}
		else
		{
			if(this.tail == this.tail.next) // Case of 1.
			{
				Job data = this.tail.data; // store data from last which is the only Node in the list.
				this.tail = null; // Mark for garbage collection.
				return(data);
			}
			else // Case of many
			{
				Job data = this.tail.next.data; // store data from first
				this.tail.next = this.tail.next.next; //assign new first by jumping over old beginning
				return(data);
			}
		}
	}
	
	
}
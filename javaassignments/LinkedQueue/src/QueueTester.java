import java.io.*;
import java.util.LinkedList;
import java.util.Queue;
public class QueueTester
{
	public static void main(String[] args) throws InterruptedException
	{
		LinkedQueue waitQ = new LinkedQueue();
		LinkedQueue runQ = new LinkedQueue();
		
		String fileName = "No fileName";
		int slice = 0, timeSW = 0;
		//************************************
		if(args.length !=3)
		{
			throw new IllegalStateException("No command line argument. . . \n");
		}
		
		fileName = args[0];
		slice = Integer.parseInt(args[1]);
		timeSW = Integer.parseInt(args[2]);
		
		System.out.println("\nTime Slice: " + slice);
		System.out.println("Latency: " + timeSW + "\n");
		
		Job job = null; // creates a job reference variable and sets it to null
		
		//--------------------------------------------------------------------------
		try
		{
			FileInputStream fstream = new FileInputStream(fileName);
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			String strLine;
			
			System.out.println("Adding Jobs to Queue");
			System.out.println("--------------------");
			System.out.println("Name\tTime");
			
			boolean first = true; //First data line ?
			while (( strLine = br.readLine()) != null)
			{
				if( job == null )//start with a fresh job
				{
					job = new Job("", 0);
				}
				
				strLine = strLine.trim();//trim any leading/trailing white space.
				
				if(strLine.startsWith("#"))//Skips lines with #
				{
					continue;
				}
				
				if(strLine.length() == 0)//Skips empty lines
				{
					continue;
				}
				
				if(first)//first data element
				{
					job.jobName = strLine;
					first = false;
				}
				else
				{
					job.runTime = Integer.parseInt(strLine);
					job.remainTime = job.runTime;
					//job.startTime = 0;
					
					System.out.println(job.jobName + "\t" + job.runTime + " milli-seconds to excute.");
					
					//waitQ.add(job);//enqueue
					waitQ.enqueue(job);
					
					job = null; // null job, ready for collection
					first = true;
				}
			}
			
			in.close();//Close the input stream
			
		}
		catch(Exception e)
		{
			System.err.println("Error: " + e.getMessage());
		}
		//--------------------------------------------------------------------------------
		
		Job waitJob;
		
		System.out.println("\n Begin Job Processing");
		System.out.println("-----------------------");
		
		boolean done = false;
		int elapsed = 0; // elasped time counter;
		
		while(!done)
		{
			System.out.println("Status:\t\tName:\t\tInfo:");

			while(!waitQ.isEmpty())
			{
				waitJob = waitQ.dequeue();//dequeue
				runQ.enqueue(waitJob);//enqueue
			}
			
			Job runJob;
			
			while(!runQ.isEmpty())
			{
				runJob = runQ.dequeue();	
				System.out.println("Wakeup\t\t[" + runJob.jobName + "]\t" + runJob.remainTime + " milliseconds remaining.");
				
				if( slice > runJob.remainTime)//job will not complete
				{
					System.out.printf("Done\t\t[%s]\tcompleted at %d ms\n", runJob.jobName, elapsed);
					elapsed += runJob.remainTime;
					Thread.sleep(runJob.remainTime);
				} else {
					runJob.remainTime -= slice;
					elapsed += slice;
					System.out.printf("Executing\t[%s]\texectued for %d ms\n", runJob.jobName, slice);
					waitQ.enqueue(runJob);
					Thread.sleep(slice);
				}
			}

			if(waitQ.isEmpty()) {
				done = true;
				break;
			}

			elapsed += timeSW;
			System.out.println("---");
			Thread.sleep(timeSW);
		}

		System.out.printf("\nTotal time:\t%d milliseconds\n\n", elapsed);
	}
}	

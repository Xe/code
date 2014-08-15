import java.util.Random;

public class LinkListTester
{
	public static void main(String[]args)
	{
		System.out.println("This is running");
		BigIntegerLinkedList list1 = new BigIntegerLinkedList();
		
		long startTime;
		long elapsedTime;
		
		String t500 = "";
		String t1000 = "";
		String t5000 = "";
		String t10000 = "";
		
		String data500 = ""; // data assignments String variable
		String data1000 = "";
		String data5000 = "";
		String data10000 = "";
		
		String tot500 = ""; // total actions string variables
		String tot1000 = "";
		String tot5000 = "";
		String tot10000 = "";
		
		list1.zeroStats(); //Set all stats to 0
		
		String header,data = null;
		String outName = "noNameYet";
		
		int[] a10000 = new int[10000]; // ascending
		int[] d10000 = new int[10000]; // decending
		int[] r10000 = new int[10000]; // random
		
		int c,c1; // temp counter
		
		for(c = 0; c < 10000; c++) // ascending order
		{
			a10000[c] = c;
		}
		
		for(c = 0; c < 10000; c++)
		{
			d10000[c] = 10000 - c;
		}
		
		Random generator = new Random();
		
		for(c = 0; c < 10000; c++)
		{
			r10000[c] = generator.nextInt(10000); //0-9999
		}
		
		int d_type; //data type 0 = ascending, 1 = descending, 2 = random
		
		for(d_type = 0; d_type < 9; d_type++)
		{
			for(c1 = 0; c1 < 4; c1++) // cycle all 4 sorts
			{
				for(c = 0; c < 500; c++)
				{
					if(d_type == 0)
					{
						list1.add(a10000[c]);
					}
					if(d_type == 1)
					{
						list1.add(d10000[c]);
					}
					if(d_type == 2)
					{
						list1.add(r10000[c]);
					}
				}	
					startTime = System.nanoTime();
					
					if(c1 == 0)
					{
						list1.bubbleSort();
					}
					if(c1 == 1)
					{
						list1.selectionSort();
					}
					if(c1 == 2)
					{
						list1.insertSortNode();
					}
					if(c1 == 3)
					{
						list1.insertSortArray();
					}
					//Missing Line
					elapsedTime = System.nanoTime() - startTime;
					t500 = String.format("%.9f",(double)(elapsedTime/1000000000.0));
					data500 = list1.getTotal();// get total actions for this sort.
					list1.zeroStats();
					list1.removeAll();
					
					//--------------------------------------------------------------------
					
					for(c = 0; c < 1000; c++)
					{
					
						if(d_type == 0)
						{
							list1.add(a10000[c]);
						}
						if(d_type == 1)
						{
							list1.add(d10000[c]);
						}
						if(d_type == 2)
						{
							list1.add(r10000[c]);
						}
					}	
					
						startTime = System.nanoTime();
					
						if(c1 == 0)
						{
							System.out.println("BUbble");
							list1.bubbleSort();
						}
						if(c1 == 1)
						{
							System.out.println("selection");
							list1.selectionSort();
						}
						if(c1 == 2)
						{
							System.out.println("insertnode");
							list1.insertSortNode();
						}
						if(c1 == 3)
						{
							System.out.println("insertarray");
							list1.insertSortArray();
						}
						//Missing Line
						elapsedTime = System.nanoTime() - startTime;
						t1000 = String.format("%.9f",(double)(elapsedTime/1000000000.0));
						data1000 = list1.getDataAssign(); // get data assignments as a string
						tot1000 = list1.getTotal(); // get total actions as a string
						list1.zeroStats();
						
						list1.removeAll();
					
						//-------------------------------------------------------------------
				for(c = 0; c < 5000; c++)
				{
					if(d_type == 0)
					{
						list1.add(a10000[c]);
					}
					if(d_type == 1)
					{
						list1.add(d10000[c]);
					}
					if(d_type == 2)
					{
						list1.add(r10000[c]);
					}
				}	
					startTime = System.nanoTime();
					
					if(c1 == 0)
					{
						System.out.println("BUbble");
						list1.bubbleSort();
					}
					if(c1 == 1)
					{
						System.out.println("selection");
						list1.selectionSort();
					}
					if(c1 == 2)
					{
						System.out.println("InsertNode");
						list1.insertSortNode();
					}
					if(c1 == 3)
					{
						System.out.println("InsertArray");
						list1.insertSortArray();
					}
					//Missing Line
					elapsedTime = System.nanoTime() - startTime;
					t5000 = String.format("%.9f",(double)(elapsedTime/1000000000.0));
					
					data5000 = list1.getDataAssign(); // get data assignments as a string
					tot5000 = list1.getTotal(); // get total actions as a string
					list1.zeroStats();
					
					list1.removeAll();
					
					//--------------------------------------------------------------------
				for(c = 0; c < 10000; c++)
				{
					if(d_type == 0)
					{
						list1.add(a10000[c]);
					}
					if(d_type == 1)
					{
						list1.add(d10000[c]);
					}
					if(d_type == 2)
					{
						list1.add(r10000[c]);
					}
				}	
					startTime = System.nanoTime();
					
					if(c1 == 0)
					{
						list1.bubbleSort();
					}
					if(c1 == 1)
					{
						list1.selectionSort();
					}
					if(c1 == 2)
					{
						list1.insertSortNode();
					}
					if(c1 == 3)
					{
						list1.insertSortArray();
					}
					//Missing Line
					list1.removeAll();
					elapsedTime = System.nanoTime() - startTime;
					t10000 = String.format("%.9f",(double)(elapsedTime/1000000000.0));
					data1000 = list1.getDataAssign(); // get data assignments as a string
					tot1000 = list1.getTotal(); // get total actions as a string
					
					list1.zeroStats();					
					//--------------------------------------------------------------------
					
					if(d_type == 0)
					{
						outName = "sort1Ascending.txt"; // ascending order
					}
					if(d_type == 1)
					{
						outName = "sort1Decending.txt"; // decending order
					}
					if(d_type == 2)
					{
						outName = "sort1Random.txt"; // Random order
					}
					
					if(d_type == 3)
					{
						outName = "dataAscending.txt"; // Random order
					}
					if(d_type == 4)
					{
						outName = "dataDecending.txt"; // Random order
					}
					if(d_type == 5)
					{
						outName = "dataRandom.txt"; // Random order
					}					

					if(d_type == 6)
					{
						outName = "totalAscending.txt"; // Random order
					}
					
					if(d_type == 7)
					{
						outName = "totalDecending.txt"; // Random order
					}
					
					if(d_type == 8)
					{
						outName = "totalRandom.txt"; // Random order
					}
					header = "\n\nsort" + "500" + "," + "1000" + "," +
								"5000" + "," + "10000";
								
					if( c1 == 0 ) //list1.bubbleSort();
					{
						if(d_type == 0 || d_type == 1 || d_type == 2)
						{
							data = "Bubble" + "," + t500 + "," + t1000 + "," +
									 t5000 + "," + t10000;
						}
						if(d_type == 3 || d_type == 4 || d_type == 5)
						{
							data = "Bubble" + "," + data500 + "," + data1000 + "," +
									 data5000 + "," + data10000;
						}
						if(d_type == 6 || d_type == 7 || d_type == 8)
						{
							data = "Bubble" + "," + data500 + "," + data1000 + "," +
							data5000 + "," + data10000;
						}
						list1.disp6sortsFile(false,outName,header,data);
					}
					if( c1 == 1 ) //list1.selSort();
					{
						if(d_type == 0 || d_type == 1 || d_type == 2)
						{
							data = "Selection" + "," + t500 + "," + t1000 + "," +
									 t5000 + "," + t10000;
						}
						if(d_type == 3 || d_type == 4 || d_type == 5)
						{
							data = "Selection" + "," + data500 + "," + data1000 + "," +
									 data5000 + "," + data10000;
						}
						if(d_type == 6 || d_type == 7 || d_type == 8)
						{
							data = "Selection" + "," + data500 + "," + data1000 + "," +
							data5000 + "," + data10000;
						}
						list1.disp6sortsFile(false,outName,header,data);
					}
					if( c1 == 2 ) //list1.bubbleSort();
					{
						if(d_type == 0 || d_type == 1 || d_type == 2)
						{
							data = "InsertSortNode" + "," + t500 + "," + t1000 + "," +
									 t5000 + "," + t10000;
						}
						if(d_type == 3 || d_type == 4 || d_type == 5)
						{
							data = "InsertSortNode" + "," + data500 + "," + data1000 + "," +
									 data5000 + "," + data10000;
						}
						if(d_type == 6 || d_type == 7 || d_type == 8)
						{
							data = "InsertSortNode" + "," + data500 + "," + data1000 + "," +
							data5000 + "," + data10000;
						}
						list1.disp6sortsFile(true,outName,header,data);
					}
					if( c1 == 3 ) //list1.bubbleSort();
					{
						if(d_type == 0 || d_type == 1 || d_type == 2)
						{
							data = "InsertArray" + "," + t500 + "," + t1000 + "," +
									 t5000 + "," + t10000;
						}
						if(d_type == 3 || d_type == 4 || d_type == 5)
						{
							data = "InsertArray" + "," + data500 + "," + data1000 + "," +
									 data5000 + "," + data10000;
						}
						if(d_type == 6 || d_type == 7 || d_type == 8)
						{
							data = "InsertArray" + "," + data500 + "," + data1000 + "," +
							data5000 + "," + data10000;
						}
						list1.disp6sortsFile(true,outName,header,data);
					}
			}
		}
	}
}

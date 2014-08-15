import java.io.*;

public class CSVWriter {
	String[] fields = {"elements", "smart bubble", "selection", "shift insertion", "insertion", "quicksort"};
	private File file;
	private FileWriter fstream;
	private PrintWriter fout;
	
	/**
	 * Automagically handles CSV file writing
	 * @param fname
	 * @throws IOException
	 */
	public CSVWriter(String fname) throws IOException {
		try {
			file = new File(fname);
			fstream = new FileWriter(file);
			fout = new PrintWriter(fstream);
		} catch (IOException e) {
			System.err.printf("File '%s' nor found or not able to read\n", fname);
			throw new IOException();
		} catch (NullPointerException e) {
			System.err.println("Need a valid file name");
			throw new IOException();
		}
		
		fout.printf("%s,%s,%s,%s,%s,%s\n", fields[0], fields[1], fields[2], fields[3], fields[4], fields[5]);
	}
	
	/**
	 * Add data to the file
	 */
	public void addRow (int elements, long sbtime, long sstime, long sitime, long itime, long qtime) {
		if(fout != null) {
			fout.printf("%d,%l,%l,%l,%l,%l\n", elements, sbtime, sstime, sitime, itime, qtime);
		}
	}
	
	/**
	 * Flush and close the file
	 */
	public void write() {
		fout.flush();
		try {
			fstream.close();
			fout = null;
			fstream = null;
			file = null;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}

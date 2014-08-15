public class Job
{
	String jobName;
	int runTime;
	int remainTime;
	int startTime;
	boolean done;

	// Construter
	public Job()
	{
		jobName = "";
		runTime =0;
		remainTime = 0;
		startTime = 0;
		done = false;
	}
	
	public Job(String _jobName, int _runTime)
	{
		jobName = _jobName;
		runTime = _runTime;
		remainTime = _runTime;
		startTime = 0;
	}

	/*public int run(int slice) {
		if(remainTime - slice > 0) {
			remainTime -= slice;
			return slice;
		} else {
			done = true;
			return remainTime;
		}
	}*/
}

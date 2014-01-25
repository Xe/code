package baavgai.textgame.framework;

public abstract class ProcessorImpl implements IActionProcessor {
	protected String [] verbList;
	protected final Object owner;
	public ProcessorImpl(Object owner) {
		this.owner = owner;
	}
	public ProcessorImpl(Object self, String [] verbList) {
		this(self); 
		this.verbList = verbList;
	}
	public boolean canProcess(UserAction action) {
		return action.isVerbMatch(verbList);
	}
	public abstract boolean process(UserAction action, Player player);
}

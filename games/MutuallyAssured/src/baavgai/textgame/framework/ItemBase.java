package baavgai.textgame.framework;

import java.util.*;

// everything the user interacts with extends this class
public abstract class ItemBase implements IActionProcessor {
	// the identical action can be described by several verbs
	// we keep the common ones here
	protected static final String[] V_USE = new String[] {"use", "activate", "apply"};
	protected static final String[] V_TAKE = new String[] {"take", "get", "pickup", "steal", "pocket"};
	protected static final String[] V_GO = new String[] {"go", "walk", "head", "crawl"};
	protected static final String[] V_LOOK = new String[] {"look", "examine", "inspect"};
	
	protected List<IActionProcessor> actions = new ArrayList<IActionProcessor>();
	protected final String name;
	protected String lookText;

	public ItemBase(String name, String lookText) { 
		this.name = name;
		this.lookText = lookText;
		initDefaulActions();
	}
	
	public ItemBase(String name) {
		this(name, "Yep, looks like a " + name + ".");
	}
	
	protected void initDefaulActions() {
		actions.add(new ProcessorImpl(this, V_LOOK) {
			public boolean process(UserAction action, Player player) {
				processLook(player);
				return true;
			}
		});
	}
	//protected ItemBase() { }
  
	public String getName() { return name; }
	protected String  getLook(Player player) { return lookText; }
	protected void processLook(Player player) { player.addMessage(getLook(player)); }
	public boolean isVisible(Player player) { return true; }
	public boolean canProcess(UserAction action) { return action.isNounMatch(this.name); }

	protected <T> IActionProcessor getProcessor(UserAction action, Collection<T> list) {
		for(T item : list) {
			IActionProcessor processor = (IActionProcessor)item;
			if (processor.canProcess(action)) { return processor; }
		}
		return null;
	}
	
	public boolean process(UserAction action, Player player) {
		IActionProcessor processor = getProcessor(action, actions);
		return processor!=null && processor.process(action, player);
	}
   
}

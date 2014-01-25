package baavgai.textgame.framework;

import java.util.*;

public abstract class Area extends ItemBase {
	protected int visits = 0;
	protected List<ItemBase> items = new ArrayList<ItemBase>();
	protected Area() { super("Area"); }
	
	
	public List<ItemBase> getItems() { return this.items; }


	// while these are essentially global
	// extending classes may still need to override
	protected void initDefaulActions() {
		actions.add(new ProcessorImpl(this, new String[] {"inv", "inventory", "stuff"}) {
			public boolean process(UserAction action, Player player) {
				player.addMessage("Inventory: ");
				if (player.items.size()==0) {
					player.addMessage("Nothing");
				} else {
					for (ItemBase item : player.items) {
						player.addMessage("\n" + item.getName());
					}
				}
				player.addMessage("\n");
				return true;
			}
		});

		actions.add(new ProcessorImpl(this, new String[] {"exit", "die", "seppuku"}) {
			public boolean process(UserAction action, Player player) {
				player.endGame();
				player.addMessage("Can't take it anymore?  We respect that.");
				player.addMessage("\nYour ultimate desmise is too disturbing to describe.");
				return true;
			}
		});

		actions.add(new ProcessorImpl(this, V_LOOK) {
			public boolean process(UserAction action, Player player) {
				if (action.noun==null) {
					processLook(player);
				} else {
					player.addMessage("You don't see one of those.");
				}
				return true;
			}
		});
		
	}
	
	public void processAction(UserAction action, Player player) {
		IActionProcessor processor = getProcessor(action, player.items);
		if (processor==null) { processor = getProcessor(action, items); }
		if (processor==null) { processor = getProcessor(action, actions); }
		if (processor==null || !processor.process(action, player)) {
			player.addMessage("I can't do that.");
		}
	}
	
	public void processEntry(Player player) {
		this.visits++;
		processLook(player);
	}
	
	
	
	protected String getLookDetails(Player player) {
		String strExits = "";
		String strItems = "";
		
		for(ItemBase item : items) {
			if (item instanceof Exit) {
				if (strExits.length()>0) { strExits += ", "; }
				strExits += item.getName();
			} else {
				if (strItems.length()>0) { strItems += ", "; }
				strItems += item.getName();
			}
		}
		
		String msg = "\n";
		if (strItems.length()>0) { msg += "You see:" + strItems + "\n"; }
		if (strExits.length()==0) {
			msg += "No apparent way out!";
		} else {
			msg += "Exits:" + strExits;
		}
		return msg + "\n";
	}
	
	
	public void processLook(Player player) {
		player.addMessage(getLook(player) + getLookDetails(player));
	}
}


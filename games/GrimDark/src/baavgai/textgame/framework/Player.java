package baavgai.textgame.framework;

import java.util.*;

public class Player implements IGameEngine {
	//private final String name;
	protected boolean gameOver = false;
	public List<ItemBase> items = new ArrayList<ItemBase>();
	protected Area currentArea;
	protected String pendingMessage = "";
	protected int turns = 0;

	public Player() {  }
	//public Player(String name) { this.name = name; }
	//public String getName() { return name; }
	public void moveTo(Area area) {
		this.turns++;
		this.currentArea = area;
		this.currentArea.processEntry(this);
	}
	public boolean hasItem(ItemBase item) { return items.contains(item); }

	protected ItemBase getItem(String itemName) {
		if (itemName!=null) {
			for(ItemBase item : items) {
				if (itemName.equalsIgnoreCase(item.getName())) { return item; }
			}
		}
		return null;
	}
	
	public boolean hasItem(String itemName) {
		return getItem(itemName)!=null;
	}

	public boolean isItemInArea(String itemName) {
		if (itemName!=null) {
			for(ItemBase item : this.currentArea.items) {
				if (itemName.equalsIgnoreCase(item.getName())) { return true; }
			}
		}
		return false;
	}
	
	public void processAction(String cmd) {
		UserAction action = UserAction.getUserAction(cmd);
		if (action!=null) {
			this.currentArea.processAction(action, this);
		}
	}
	
	public void moveItemAreaToSelf(InventoryItem item) {
		this.items.add(item);
		this.currentArea.items.remove(item);
		addMessage("You now have a " + item.getName() + ".");
	}
	
	public void moveItemSelfToArea(InventoryItem item) {
		this.items.remove(item);
		this.currentArea.items.add(item);
		addMessage("You no longer have a " + item.getName() + ".");
	}

	public void addNewItemToArea(ItemBase item) {
		this.currentArea.items.add(item);
	}
	
	public void destroyItemInArea(ItemBase item) {
		this.currentArea.items.remove(item);
	}

	public void destroyItemOnSelf(ItemBase item) { this.items.remove(item); }
	
	public void destroyItemOnSelf(String itemName) {
		destroyItemOnSelf(getItem(itemName));
	}

	
	public int getTurn() { return turns; }
	public boolean isGameOver() { return gameOver; }
	public void endGame() { gameOver = true; }
	
	public void addMessage(String msg) { pendingMessage += msg;	}
	public String getPendingMessage() {
		String msg = pendingMessage; 
		pendingMessage = "";
		return msg;
	}
}

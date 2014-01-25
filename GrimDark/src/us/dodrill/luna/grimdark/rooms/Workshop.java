package us.dodrill.luna.grimdark.rooms;

import us.dodrill.luna.grimdark.items.Bindings;
import us.dodrill.luna.grimdark.items.Knife;
import baavgai.textgame.framework.*;

public class Workshop extends Area {
	boolean firstLook = true;
	private Bindings bindings = new Bindings();
	
	public Workshop() {
		this.items.add(bindings);
		this.items.add(0, pinkie);
	}
	
	private DisplayItem pinkie = new DisplayItem("Pinkie-Pie") {
		protected String getLook(Player player) {
			player.addMessage("Pinkie starts laughing and charges at you.\n\n");
			player.addMessage("You are mutilated and die from blood loss. Your organs are displayed on the wall along with many others. You are dead.");
			player.endGame();
			
			return lookText;
		}
		
		//TODO: add a kill command that will let you kill pinkie and go down another route.
		protected void initDefaulActions() {
			super.initDefaulActions();
			
			actions.add(new ProcessorImpl(actions, new String[] {"kill"}) {

				public boolean process(UserAction action, Player player) {
					player.addMessage("You leap out towards Pinkie Pie, charging with the full force of your wings. You crash into her and she flies back. She does not respond to being poked.  She is dead.\n\n");
					//player.destroyItemInArea("Pinkie-Pie");
					
					player.addNewItemToArea(new Knife());
					player.addMessage("You notice that she dropped her knife, you should grab it.");
					return true;
				}
				
			});
		}
	};
	
	protected String getLook(Player player) {
		if(firstLook) {
			player.addMessage("\"Oh hi there Rainbow Dash!\" you hear Pinkie say.  \"How are you?\"\n");
			player.addMessage("\"Pinkie, what are you doing?! Why can't I move?\"\n\"Duh, you're tied down! Why do I need to tell you that?\"\n");
			player.addMessage("\"But why? I thought we were making cupcakes!\"\n\"You are helping, I just need the secret ingredient.\"\n\n");
			player.addMessage("You cannot bear to look again as Pinkie is preparing a hacksaw.\n\n");
			
			firstLook = false;
		}
		
		if(player.isItemInArea("knife")) {
			player.destroyItemInArea(pinkie);
		}
		
		if(player.isItemInArea("gore")) {
			player.destroyItemInArea(bindings);
		}
		
		player.addMessage("There are many body parts of dead fellow ponies displayed.  Intenstines are kept ");
		player.addMessage("aloft by helium, as if they were baloons. The scene is gruesome and somehow you feel as ");
		player.addMessage("though the grim scene is...happy.  You fear that calling out to Pinkie would be bad. \n\n");
		
		return "";
	}
}

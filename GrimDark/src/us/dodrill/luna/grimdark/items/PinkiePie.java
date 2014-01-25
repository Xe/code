package us.dodrill.luna.grimdark.items;

import us.dodrill.luna.grimdark.rooms.Workshop;
import baavgai.textgame.framework.*;

public class PinkiePie extends DisplayItem {
	int talkedToNum = 0;
	boolean gone = false;
	
	public PinkiePie() {
		super("Pinkie-Pie");
	}
	
	protected String getLook(Player player) {
		String msg = "";
		
		msg = msg + "Pinkie Pie, one of your best friends. She is bouncing ";
		msg = msg + "while waiting for you. She seems to have unlimited amounts of energy. ";
		
		talkedToNum++;
		
		return msg;
	}
	
	protected void processTake(Player player) {
		player.addMessage("No, you cannot take your friend. ");
	}
	
	protected void initDefaulActions() {
		super.initDefaulActions();
		actions.add(new ProcessorImpl(this, new String[] {"talk", "reply"}) {

			@Override
			public boolean process(UserAction action, Player player) {
				if(talkedToNum == 0) {					
					player.addMessage("\"Hey, Raindow Dash!\" Pinkie yells. ");
				} else if(talkedToNum == 1) {
					player.addMessage("\"Come down with me to the basement! I forgot the secret ingredient!\"\n");
					player.addMessage("You feel a chill go up and down your spine, but you shake it off. ");
				} else if(talkedToNum == 2) {
					player.addMessage("\"Oh, here, have this to tide you over for a minute.\"\nShe gives you a cupcake, you eat it.\n");
				} else {
					player.addMessage("\"What next, Pinkie?\" you ask.\n\"Now, you take a nap.\"\n\n");
					player.addMessage("The world fades to black.\n\n");
					player.moveTo(new Workshop());
				}
				
				talkedToNum++;
				talkedToNum++;
				return true;
			}
			
		});
	}
}

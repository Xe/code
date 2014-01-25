package baavgai.textgame.example;
import baavgai.textgame.framework.*;

class DemoWorld extends Area {
	public DemoWorld() {
		this.items.add(new InventoryItem("bucket", "It's got a hole in it."));
		
		this.items.add(new InventoryItem("rope", 
				"It's good, strong, hemp rope.  Pity it's not tied at the top of the well"
				+ "\nIt's surprisingly dry.  Don't smoke it."
				) {
			protected void initDefaulActions() {
				super.initDefaulActions();
				actions.add(new ProcessorImpl(this, new String[] {"smoke"}) {
					public boolean process(UserAction action, Player player) {
						player.addMessage("I told you not to do that!\n");
						return true;
					}
				});
			}
		});
		
		this.items.add(new DisplayItem("remains") {
			protected void processLook(Player player) { 
				player.addMessage(
						"They are human remains."
						+ " As you examine them they fall apart into a pile of bones." 
						+ " The skull rolls across the floor."
						+ " You hear a clinking noise."
						);
				player.addNewItemToArea(new DisplayItem("bones", "A pile of human bones.  Ick."));
				player.addNewItemToArea(new InventoryItem("skull"));
				player.addNewItemToArea(new InventoryItem("flint") {
					private boolean firstLook = true;
					protected String getLook(Player player) {
						String msg = "A stone with a fractured beveled edge.  ";
						if (firstLook) {
							firstLook = false;
							msg += "For some reason you can't recall, you recognize this as flint."
							+"  You could even make a fire with it, if you had some steel...";
						} else {
							msg += "If you had some steel, you might be able to make a fire.";
						}
						return msg;
					}
					
					protected void processUse(Player player) {
						if (!player.hasItem("knife")) {
							player.addMessage("You have nothing to strike against to make a spark.");
							return;
						}
						if (!player.hasItem("rope")) {
							player.addMessage("You make some nice sparks.  If only you had something dry and flammable.");
							return;
						}
						player.addMessage("Using skills acquired from watching too much reality TV, you manage to get the end of the rope lit.");
						player.addMessage("  The rope is stiff and makes a serviceable torch.");
						player.items.add(new InventoryItem("torch",
								"It's actually a flaming rope, but it works.  Now you can see more of where you are."));
						
						player.destroyItemOnSelf("rope");
						
					}

				});
				player.destroyItemInArea(this);
			}
		});
		
		Area area1, area2;

		area1 = new AreaImpl("You are in underground caverns.  A luminous fungus coats the walls here."
				+"  You hear the sound of water moving, but don't see any."
				+"  The cave continues east and west."
				);

		//area1.getItems().add(new InventoryItem("torch"));
		
		this.items.add(new Exit(Exit.DIR_N, area1));
		area1.getItems().add(new Exit(Exit.DIR_S, this));
		
		area2 = new AreaImpl("More glowing fungus.  The walls and floor are covered with tasty looking mushrooms.");
		area2.getItems().add(new Shroom());

		area1.getItems().add(new Exit(Exit.DIR_E, area2));
		area2.getItems().add(new Exit(Exit.DIR_W, area1)); 

		area2 = new AreaImpl("There is moving water.  Something glints in the water.");
		area2.getItems().add(new DisplayItem("stream") {
			private boolean firstLook = true;
			protected String getLook(Player player) {
				String msg = "A stream of black water eriely reflects the fungal glow.";
				if (firstLook) {
					firstLook = false;
					msg += "  Strange blind fish dart back and forth.";
					msg += "  You see something reflecting below the surface, looks like a knife.";
					
					player.addNewItemToArea(new Fish());
					player.addNewItemToArea(new InventoryItem("knife") {
						protected void processTake(Player player) {
							if (player.isItemInArea("fish")) {
								player.addMessage("You reach for the knife...\n");
								player.addMessage("Just as your hand closes on the handle, the fish swarm upon you.");
								player.addMessage("  Before you can react, in an explosion of blood and gore, you have no arm.");
								player.addMessage("  After that it gets ugly.  You die.");
								player.endGame();
							} else {
								player.moveItemAreaToSelf(this);
							}
						}
					});
				}
				return msg;
			}
		});

		area1.getItems().add(new Exit(Exit.DIR_W, area2));
		area2.getItems().add(new Exit(Exit.DIR_E, area1)); 
		
	}

	
	class Fish extends DisplayItem {
		public Fish() { 
			super("fish", 
					"Ugly pale blind subterranean fish.  All milky white except for nasty looking black teeth."
					+ "  They look like they'll make a meal of you."
					);
		}
		
		protected void processTake(Player player) {
			player.addMessage("Leave the fish alone.  They look hungry.");
		}

	   
		protected void initDefaulActions() {
			super.initDefaulActions();
			actions.add(new ProcessorImpl(this, new String[] {"feed", "give"}) {
				public boolean process(UserAction action, Player player) {
					boolean hasFood = player.hasItem("mushroom");
					if (!hasFood) {
						player.addMessage("You don't have anything to feed them with.");
					} else {
						player.addNewItemToArea(new DisplayItem("deadfish", 
								"Corpses of the murderous fish you killed are slowly rotting."));
						player.addMessage("You feed the shrooms to the fish...");
						player.addMessage("\nAll the fish descend on the bounty in a feeding frenzy!");
						player.addMessage("  Wait, some of them aren't moving anymore.");
						player.addMessage("  Looks like you killed them all.  I hope you're happy.");
						player.destroyItemOnSelf("mushroom");
						player.destroyItemInArea((ItemBase)owner);
					}
					return true;
				}
			});
		}
	}
	
	class Shroom extends InventoryItem {
		public Shroom() { super("mushroom"); }
	   
		protected void initDefaulActions() {
			super.initDefaulActions();
			String [] vEat = new String[] {"eat"};
			actions.add(new ProcessorImpl(this, vEat) {
				public boolean process(UserAction action, Player player) {
					player.addMessage("Are you an idiot?  You don't eat strange shrooms!");
					return true;
				}
			});
		}
	}
	

	// come back to the escape too often and you die
	protected String getLook(Player player) {
		String msg = "High above you is a well opening.";
		if (this.visits==1) {
			msg += "\nYou are at the bottom of a well ( yes, you're an idiot. )"
			+ "\nLucky for you, it's low tide and receding water has revealed a hole"
			+ "\nyou think you can squeeze into.";
		} else if (this.visits<3) {
			msg += "\nThe water apears to be rising"; 
		} else if (this.visits<8) {
			msg += "\nThe water is rising more quickly now."; 
		} else if (this.visits==9) {
			msg += "\nSuddenly the water comes rushing in."
			+ "\nTonight, you sleep with the fishes.";
			player.endGame();
		}
		if (player.hasItem("torch") && !player.isItemInArea("ladder")) {
			msg += "\nIn the light of your new found torch, you see an ancient ladder fused to the wall!";
			player.addNewItemToArea(new DisplayItem("ladder", 
			"A decaying arrangment of rope and wood can now be seen just above eye level under mossy cover.") {
				protected void initDefaulActions() {
					super.initDefaulActions();
					actions.add(new ProcessorImpl(this, V_USE) {
						public boolean process(UserAction action, Player player) {
							player.addMessage("You climb the ladder to freedom.  Congratulations!");
							player.endGame();
							return true;
						}
					});
				}
				
			});
			
		}
		return msg; 
	}
}


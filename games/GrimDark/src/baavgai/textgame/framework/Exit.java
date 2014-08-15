package baavgai.textgame.framework;

public class Exit extends ItemBase {
	public static final String[] DIR_N = new String[] {"North", "N"};
	public static final String[] DIR_S = new String[] {"South", "S"};
	public static final String[] DIR_E = new String[] {"East", "E"};
	public static final String[] DIR_W = new String[] {"West", "W"};
   
	protected Area exitLocation;
	protected String[] names;
	public Exit(String[] names, Area exitLocation) {
		super(names[0]);
		this.names = names;
		this.exitLocation = exitLocation;
	}

	public Area getExitLocation() { return this.exitLocation; }
	public void processLook(Player player) { 
		player.addMessage(this.names[0] + " looks like leads somewhere else.");
	}
   
	public boolean canProcess(UserAction action) {
		return action.isNounMatch(this.names);
	}

	protected void initDefaulActions() {
		super.initDefaulActions();
		actions.add(new ProcessorImpl(this, V_GO) {
			public boolean process(UserAction action, Player player) {
				player.addMessage("You go " + ((Exit)owner).getName() + ".\n");
				player.moveTo(((Exit)owner).exitLocation);
				return true;
			}
		});
	}
}

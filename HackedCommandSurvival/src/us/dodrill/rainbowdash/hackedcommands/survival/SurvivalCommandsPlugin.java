package us.dodrill.rainbowdash.hackedcommands.survival;

import org.bukkit.event.Event;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.plugin.PluginManager;

public class SurvivalCommandsPlugin extends JavaPlugin {
	HSPlayerListener playerListener = new HSPlayerListener(this);
	HSBlockListener blockListener = new HSBlockListener(this);

	@Override
	public void onDisable() {
		System.out.println("SurvivalCommands unloaded");
	}

	@Override
	public void onEnable() {
		PluginManager pm = getServer().getPluginManager();
		
		pm.registerEvent(Event.Type.PLAYER_JOIN, this.playerListener, Event.Priority.Highest, this);
		pm.registerEvent(Event.Type.PLAYER_COMMAND_PREPROCESS, this.playerListener, Event.Priority.Normal, this);
		pm.registerEvent(Event.Type.BLOCK_BREAK, this.blockListener, Event.Priority.Highest, this);
		pm.registerEvent(Event.Type.BLOCK_PLACE, this.blockListener, Event.Priority.Highest, this);
		
		System.out.println("SurvivalCommands loaded");
	}

}

package us.dodrill.rainbowdash.hackedcommands.survival;

import org.bukkit.GameMode;
import org.bukkit.entity.Player;
import org.bukkit.event.block.*;

public class HSBlockListener extends BlockListener {
	public static SurvivalCommandsPlugin plugin;
	
	public HSBlockListener(SurvivalCommandsPlugin instance) {
		plugin = instance;
	}
	
	public void onBlockBreak(BlockBreakEvent event) {
		Player player = event.getPlayer();
		
		if(player.getName().equals("Player")) {
			player.sendMessage("Permission denied");
			event.setCancelled(true);
		}
	}
	
	public void onBlockPlace (BlockPlaceEvent event) {
		Player player = event.getPlayer();
		
		if(player.getName().equals("Player")) {
			player.sendMessage("Permission denied");
			event.setCancelled(true);
		}
	}
}

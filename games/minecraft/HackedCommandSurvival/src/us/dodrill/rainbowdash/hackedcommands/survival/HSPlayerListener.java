package us.dodrill.rainbowdash.hackedcommands.survival;

import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerCommandPreprocessEvent;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerListener;

public class HSPlayerListener extends PlayerListener {
	
	public static SurvivalCommandsPlugin plugin;
	
	public HSPlayerListener(SurvivalCommandsPlugin instance) {
		plugin = instance;
	}
	
	public void onPlayerJoin(PlayerJoinEvent event) {
		Player player = event.getPlayer();
		
		if(!player.hasPlayedBefore()) {
			player.sendMessage("Welcome to Genesect!, " + player.getName() + "!");
			event.getPlayer().getServer().broadcastMessage("New Player " + player.getName() + " joined");
			player.sendMessage("This is a survival server.");
		} else {
			player.sendMessage("Welcome back to Genesect, " + player.getName() + "!");
		}
		
		if (player.getName().equals("Player")) {
			player.sendMessage("Sorry, but you need to connect using your");
			player.sendMessage("real username. This is a policy.");
			player.chat("Hey! I don't wanna give out my username, don't trust me!");
			event.getPlayer().getServer().broadcastMessage("ANONYMOUS USER PLAYER JOINED PLEASE KEEP AN EYE ON HIM");
		} else if (player.getName().equals("Honeydew")) {
			player.sendMessage("Hey, the admin wants to speak with ya");
			player.sendMessage("email him at shadowh511@derpymail.org plz");
		}
		
	}
	
	public void onPlayerCommandPreprocess(PlayerCommandPreprocessEvent event) {
		String[] split = event.getMessage().split(" ");
		
		if (split[0].equalsIgnoreCase("/spawn")) {
			Player sender = event.getPlayer();
			Location spawn = plugin.getServer().getWorld(sender.getWorld().getName()).getSpawnLocation();
			
			sender.teleport(spawn);
			sender.sendMessage("Returned to spawn.");
			
			event.setCancelled(true);
		} else if (split[0].equalsIgnoreCase("/help")) {
			Player sender = event.getPlayer();
			sender.sendMessage("You have these commands on Genesect:");
			sender.sendMessage("/cprivate - locks a chest");
			sender.sendMessage("/spawn - returns you to the spawn point");
			sender.sendMessage("/home - returns you to your bed");
			sender.sendMessage("/ob - toggles block protection");
			
			event.setCancelled(true);
		} else if (split[0].equalsIgnoreCase("/setspawn")) {
			Player sender = event.getPlayer();
			World world = sender.getWorld();
			
			if(sender.isOp()) {
				world.setSpawnLocation(
						sender.getLocation().getBlockX(), 
						sender.getLocation().getBlockY(), 
						sender.getLocation().getBlockZ()
				);
				sender.sendMessage("Spawn set to current location");
			} else {
				sender.sendMessage("Permission denied.");
			}
			event.setCancelled(true);
		} else if (split[0].equalsIgnoreCase("/home")) {
            Player sender = event.getPlayer();
                        
            if(sender.getBedSpawnLocation() != null){
                sender.teleport(sender.getBedSpawnLocation());
                sender.sendMessage("Sent home");
            } else {
                sender.sendMessage("You have no home to go to :(");
            }
                        
            event.setCancelled(true);
        } else if (split[0].equalsIgnoreCase("/sethome")) {
        	Player player = event.getPlayer();
        	
        	player.setBedSpawnLocation(player.getLocation());
        	
        	player.sendMessage("Bed location set to your current location");
        	
        	event.setCancelled(true);
        } else if (split[0].equalsIgnoreCase("/kill")) {
        	Player player = event.getPlayer();
        	
        	player.setHealth(0);
        	
        	event.setCancelled(true);
        }
	}
}

package baavgai.textgame.example;

import baavgai.textgame.framework.*;

public class GameTest {
  
	public static void main(String[] args) {
		Player player = new Player();
		player.moveTo(new DemoWorld());
		//GameTerminal game = new GameTerminal(player);
		GameWindow game = new GameWindow(player);
		game.play();
	}

}

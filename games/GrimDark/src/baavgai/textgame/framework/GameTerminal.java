package baavgai.textgame.framework;

import java.util.Scanner;

public class GameTerminal {
	protected Scanner input;
	protected IGameEngine engine = null;
	
	public GameTerminal(IGameEngine engine) {
		input = new Scanner(System.in);
		this.engine = engine;
	}
  
	public void play() {
		for(;;) {
			System.out.print(engine.getPendingMessage());
			if (engine.isGameOver()) { break; }
			System.out.print("\n> ");
			engine.processAction(input.nextLine());
		}
	}

}

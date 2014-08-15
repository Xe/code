package baavgai.textgame.framework;
// this is all you need for a text game
public interface IGameEngine {
	void processAction(String cmd);
	String getPendingMessage();
	boolean isGameOver();
}

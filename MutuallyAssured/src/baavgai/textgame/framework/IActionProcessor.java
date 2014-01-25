package baavgai.textgame.framework;

// all objects that can be interacted with are reponsible for this
public interface IActionProcessor {
	// if an object says it can process it, it gets too
	boolean canProcess(UserAction action);

	// this method will be called if above it true
	boolean process(UserAction action, Player player);
}

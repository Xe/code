package baavgai.textgame.framework;

// our natural language processor is dumb as dirt
// we just care about verb - noun pairs
public final class UserAction {
	public final String verb, noun;
	public UserAction(String verb, String noun) {
		this.verb = verb;
		this.noun = noun;
	}
   
	private boolean isMatch(String s, String other) {
		return (s==null || other==null) ? false : s.equalsIgnoreCase(other);
	}
	private boolean isMatch(String s, String [] others) {
		if (s==null || others==null) { return false; }
		for(String other : others ) { if (isMatch(s, other)) { return true; } }
		return false;
	}
   
	public boolean isVerbMatch(String [] others) { return isMatch(verb, others); }
	public boolean isVerbMatch(String other) { return isMatch(verb, other); }
	public boolean isNounMatch(String [] others) { return isMatch(noun, others); }
	public boolean isNounMatch(String other) { return isMatch(noun, other); }

	// parser logic provided here
	public static UserAction getUserAction(String userEntry) {
		if (userEntry==null) { return null; }
		String [] list = userEntry.split(" ");
		String verb = list[0].trim().toUpperCase();
		if (verb.length()==0) { return null; }
		String noun = (list.length>1) ? list[1].trim().toUpperCase() : null;
		return new UserAction(verb, noun);
	}
}

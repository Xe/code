package us.dodrill.luna.messaging.loadbalancing;

public class Tester extends Object {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Pool pool = new Pool();
		
		for(int i = 0; i < 18; i++) {
			pool.addDummy();
		}
		
		for(int i = 0; i < 5051; i++) {
			pool.sendSMS("Batman", "Test");
		}
		
		for(Messenger mess : pool.accounts) {
			System.out.println(mess.toString());
		}
	}

}

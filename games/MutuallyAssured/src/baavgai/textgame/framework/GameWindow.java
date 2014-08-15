package baavgai.textgame.framework;
import java.awt.*;
import javax.swing.*;

import java.awt.event.*;

public class GameWindow extends JFrame implements ActionListener {
	private static final long serialVersionUID = 6543979844852603770L;

	protected IGameEngine engine = null;
	protected JTextArea textWindow;
	protected JScrollPane scroller;
	protected JTextField txtEntry;
	protected int charWidth = 80;

	public GameWindow(IGameEngine engine) {
		this.engine = engine;

		Font font = new Font("Monospaced", Font.BOLD, 16);
		//SansSerif

		textWindow = new JTextArea(25, charWidth);
		textWindow.setEditable(false);
		textWindow.setFocusable(false);
		textWindow.setFont(font);

		scroller = new JScrollPane(textWindow, 
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		txtEntry = new JTextField();
		txtEntry.setFont(font);
		txtEntry.addActionListener(this);

		JPanel content = new JPanel();
		content.setLayout(new BorderLayout());
		content.add(scroller, BorderLayout.CENTER);
		content.add(txtEntry, BorderLayout.SOUTH);
		this.setContentPane(content);

		this.setTitle("Text Games are Fun");
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.pack();
	}

	protected String lineWrap(String line) {
		String msg = "";
		while (line.length() > charWidth) {
			int index = line.lastIndexOf(' ', charWidth);
			if (index==-1) { index = charWidth; }
			msg += line.substring(0, index) + "\n";
			line = line.substring(index+1);
		}
		msg += line + "\n";
		return msg;
	}
	
	
	protected String messageWrap(String s) {
		String msg = "";
		int nPos = s.indexOf('\n');
		while (nPos!=-1) {
			msg += lineWrap(s.substring(0, nPos)); 
			s = s.substring(nPos+1);
			nPos = s.indexOf('\n');
		}
		msg += lineWrap(s);
		return msg;
	}
	
	protected void write(String s) {
		textWindow.append(messageWrap(s));
	}
	
	/*
	protected void write(String s) {
		ArrayList<String> list = new ArrayList<String>();
		int nPos = s.indexOf('\n');
		while (nPos!=-1) {
			list.add(s.substring(0, nPos));
			s = line.substring(nPos+1);
			nPos = s.indexOf('\n');
		}
		list.add(s);
		
		String msg = "X:"+ list.size() + ":" + s + "\n";
		while (line.length()> charWidth) {
				msg += String.valueOf(i);
				int index = line.lastIndexOf(' ', charWidth);
				if (index==-1) { index = charWidth; }
				msg += line.substring(0, index) + "\n";
				line = line.substring(index+1);
			}
			msg += String.valueOf(i);
			msg += line;
			msg += "\n";
		}
		textWindow.append(msg);
		
	}
	protected void write(String s) {
		//String [] lines = s.replace('\n', '~').split("~");
		String msg = "X:" + s + "\n";
		int nPos = s.indexOf('\n');
		for(int i=0; i<lines.length; i++) {
			String line = lines[i];
			while (line.length()> charWidth) {
				msg += String.valueOf(i);
				int index = line.lastIndexOf(' ', charWidth);
				if (index==-1) { index = charWidth; }
				msg += line.substring(0, index) + "\n";
				line = line.substring(index+1);
			}
			msg += String.valueOf(i);
			msg += line;
			msg += "\n";
		}
		textWindow.append(msg);
		
	}
	protected void write(String s) {
		ArrayList<String> list = ArrayList<String>();
		String msg = "X:" + s + "\n";
		int nPos = s.indexOf('\n');
		while (nPos!=-1) {
			list.add(s);
		}
		list.add(s);
		while (line.length()> charWidth) {
				msg += String.valueOf(i);
				int index = line.lastIndexOf(' ', charWidth);
				if (index==-1) { index = charWidth; }
				msg += line.substring(0, index) + "\n";
				line = line.substring(index+1);
			}
			msg += String.valueOf(i);
			msg += line;
			msg += "\n";
		}
		textWindow.append(msg);
		
	}
	*/

	
	protected void processPending() {
		write(engine.getPendingMessage());
		if (engine.isGameOver()) {
			write("\nGame Over\nHit enter to close");
		}
		scrollToBottom();
	}

	public void play() {
		setVisible(true);
		processPending();
	}

	public void actionPerformed(ActionEvent evt) {
		if (engine.isGameOver()) {
			setVisible(false);
	        dispose();
	        return;
		}
		String txt = txtEntry.getText();
		write("> " + txt + "\n\n");
		txtEntry.setText("");
		engine.processAction(txt);
		processPending();
		if (!txtEntry.hasFocus()) {
			txtEntry.requestFocus();
		}
	}

	private void scrollToBottom() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				scroller.getVerticalScrollBar().setValue(
				scroller.getVerticalScrollBar().getMaximum());
			}
		});
	}

}

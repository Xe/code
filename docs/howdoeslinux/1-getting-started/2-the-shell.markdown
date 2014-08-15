# Basic Shell Use

In a linux environment, you are going to be using the command line a lot 
whether it is directly (via a shell) or indirectly (via a gui).  This can seem 
scary at first, but it is really quite simple to use.

In the shell, you enter commands one at a time for the computer to execute and 
bring back the results to you. 

In most cases, you will see a prompt when the computer is ready for your 
instructions. Here is a sample one:

```
xena@rarity:~$
```

There is a fair bit going on here, so it will be broken down for you bit by bit.

```
xena
```

This part is known as your username. It is what you used to log into the 
computer initially and one of the ways that the system identifies you.

```
@rarity
```

This is the hostname of the system you are using. In this case the hostname is 
`rarity`.

```
~
```

In this example the current working directory is xena's home directory, or `~`. 
As you move around the system, it will change to the directory you are currently 
in.

## Moving Around The Filesystem

The filesystem is shown as a tree, a lot like how Windows presents itself.  
The main difference is that the Linux filesystem does not have separate trees
for each individual drive. Everything is shown on one contiguous tree.

An example of the directory structure:

```
/			- root
/bin		- essential system programs, such as the shell
/boot		- boot files
/dev		- devices the system knows about
/etc		- system configuration files
/home		- user home directories
/lib		- system libraries
/media		- typical mountpoint location for extra drives
/mnt		- historic typical mountpoint location for extra drives
/opt		- optional and/or commercial software sometimes installs here
/proc		- information about processes on the system by PID
/root		- the superuser's home directory
/sbin		- essential system programs that require elevated permission
/srv		- service files
/usr		- user system resource files
/var		- volatile storage
```

To change what directory you are in, you use the `cd` command. If you wanted
to change to `/usr/bin`, you would type in:

```
xena@rarity:~$ cd /usr/bin
xena@rarity:/usr/bin$
```

If you give the `cd` command no arguments, it will take you back to your home
directory. For me it is `/home/xena`. 

You can also see what directory you are in using the `pwd` (print working 
directory) command.

```
xena@rarity:/usr/bin$ pwd
/usr/bin
xena@rarity:/usr/bin$ cd
xena@rarity:~$ pwd
/home/xena
xena@rarity:~$ 
```

## Files and You

The filesystem is made to store files. You can list the files and folders
in your current working directory with `ls`.

```
xena@rarity:~$ ls
Backups  Desktop    Downloads  Music     Public  Templates
Code     Documents  irclogs    Pictures  Temp    Videos
xena@rarity:~$ 
```

You can also specify a folder to list the contents of.

```
xena@rarity:~$ ls Pictures/
wallpaper-1165107.jpg  wallpaper-2094886.png
wallpaper-1457997.jpg  wallpaper-84692.jpg
xena@rarity:~$ 
```

You can copy one of those files out using the `cp` command. 
`wallpaper-84692.jpg` is a picture of Samus Aran, so if you wanted to
copy it, you would use a command similar to:

```
xena@rarity:~$ cp Pictures/wallpaper-84692.jpg samus.jpg
xena@rarity:~$ ls
Backups  Desktop    Downloads  Music     Public     Temp       Videos
Code     Documents  irclogs    Pictures  samus.jpg  Templates
xena@rarity:~$ 
```

With a lot of command line tools, no news is good news. If you don't
have permission to do something, it will tell you with a useful error
message.  

```
xena@rarity:~$ cp samus.jpg /samus.jpg
cp: cannot create regular file `/samus.jpg': Permission denied
```

Given that I am a regular user, I do not have permission to modify things
in `/`. Also given that the `cp` command creates a new file, it is telling
you to check your privilige.

You can also have a file show up as if it were two files, but still be linked 
such that changes to one of them affects both of the files.

The preferred way is to make a symbolic, or soft link between the two locations. 
To do this, use the `ln` command.

```
xena@rarity:~/Temp$ ls
OTPDecrypt.java  d30207a485d02566bbd2  niichan.github.io  purlox  ssl  tools  xena.pem
xena@rarity:~/Temp$ ln -s ~/ssl.pem xena.pem 
ln: failed to create symbolic link 'xena.pem': File exists
xena@rarity:~/Temp$ ln -s /home/xena/Temp/xena.pem /home/xena/ssl.pem
xena@rarity:~/Temp$ cd 
xena@rarity:~$ ls -l ssl.pem 
lrwxrwxrwx 1 xena xena 23 Aug 13 12:12 ssl.pem -> /home/xena/Temp/xena.pem
xena@rarity:~$ 
```

Here I linked my SSL certificate `xena.pem` to a place in my home directory, 
`~xena/ssl.pem`. This is a trivial example, but the link can have different 
permissions than the real file, making it easier to work with some finnicky 
programs that rely on their configuration being in one place. I personally use 
them with one of my [github repositories](https://github.com/Xe/dotfiles).

# Directories For Fun and Profit

In your home directory, you will usually start out with one or a couple other 
directories to put all your files in. If you want to make a new directory, you 
use the `mkdir` command. These directory names cannot have spaces in them.

```
xena@rarity:~/Temp$ mkdir secret_plan
xena@rarity:~/Temp$ ls -l
total 32
-rw-r--r-- 1 xena xena 1250 Aug 12 22:21 OTPDecrypt.java
drwxr-sr-x 3 xena xena 4096 Aug  1 19:28 d30207a485d02566bbd2
drwxr-sr-x 7 xena xena 4096 Jul 24 11:02 niichan.github.io
drwxr-sr-x 2 xena xena 4096 Jul 30 14:29 purlox
drwxr-sr-x 2 xena xena 4096 Aug 13 12:20 secret_plan
drwxr-sr-x 2 xena xena 4096 Aug  9 11:33 ssl
drwxr-sr-x 4 xena xena 4096 Aug  8 16:57 tools
-rw-r--r-- 1 xena xena 2957 Aug  9 11:33 xena.pem
xena@rarity:~/Temp$ 
```

Using the long listing of the `ls` command, we can see that our new directory 
`secret_plan` exists and is marked as a directory.

```
drwxr-sr-x 2 xena xena 4096 Aug 13 12:20 secret_plan
1    2     3  4    5    6        7            8
```

This shows:

1. the type of file it is (d for directory and - for nothing special)
2. the permissions of the file
3. number of links to the file
   (for directories, this is the number of files inside it)
4. the user that owns the file
5. the primary group the file is in
6. the file size (directories take up a full block)
7. the last modified date of the file
8. the name of the file

Now, say for some reason your covert job spying on the NSA to leak things ala 
Edward Snowden gets called off. You would need to get rid of `secret_plan` very 
quickly. You can use the `rmdir` command to delete directories if and only if 
they are empty.

```
xena@rarity:~/Temp$ rmdir secret_plan/
rmdir: failed to remove 'secret_plan/': Directory not empty
```

Oh no! Your escape is foiled! You need to go into `secret_plan` and get rid of 
all the files post haste!

```
xena@rarity:~/Temp$ cd secret_plan/
xena@rarity:~/Temp/secret_plan$ ls
evil_steps
xena@rarity:~/Temp/secret_plan$ 
```

Well, that is anticlimactic.  You need to use the `rm` command to delete the 
`evil_steps` file.

```
xena@rarity:~/Temp/secret_plan$ rm evil_steps 
xena@rarity:~/Temp/secret_plan$ ls
xena@rarity:~/Temp/secret_plan$ 
```

Good! Now you can delete the folder!

```
xena@rarity:~/Temp/secret_plan$ cd ..
xena@rarity:~/Temp$ rmdir secret_plan/
xena@rarity:~/Temp$ ls
OTPDecrypt.java  d30207a485d02566bbd2  niichan.github.io  purlox  ssl  tools  xena.pem
xena@rarity:~/Temp$ 
```

In case you are wondering, `..` is a filesystem alias for the directory 
immediatly above the directory you are in. `.` is another alias, but it is for 
the current directory.

`rm` can also delete directories when called such as:

```
rm -rf /home/xena/cute_kittens/
```

The `-rf` means recurse through directories and force deletion. You need to be 
the owner of a file to delete it.

## Making Empty Files

We've covered all this information on clobbering away and throwing files 
around, but so far you have not made your own plain file. The `touch` command 
is great at making files.

```
xena@rarity:~/Temp$ touch sweet_touch
xena@rarity:~/Temp$ ls -l sweet_touch
-rw-r--r-- 1 xena xena 0 Aug 13 13:21 sweet_touch
xena@rarity:~/Temp$ 
```

Congradulations, you have made your first file.

Additional help on commands can be gotten by referencing the manual pages. To 
look up information on `ls` for instance, use:

```
xena@rarity:~/Temp$ man ls
```

Use up and down arrow keys to scroll through the page and then when you are 
done, press the `q` key to quit.


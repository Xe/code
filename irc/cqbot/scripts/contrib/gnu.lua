-- Copyright (C) 2014  Christine Dodrill <xena@yolo-swag.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

math.randomseed(os.time())

linuxshit = {
  "I'd just like to interject for a moment. What you're referring to as Linux, is in fact, GNU/Linux, or as I've recently taken to calling it, GNU plus Linux.",
  "Linux is not an operating system unto itself, but rather another free component of a fully functioning GNU system made useful by the GNU corelibs, shell utilities and vital system components comprising a full OS as defined by POSIX.",
  "Many computer users run a modified version of the GNU system every day, without realizing it.",
  "Through a peculiar turn of events, the version of GNU which is widely used today is often called \"Linux\", and many of its users are not aware that it is basically the GNU system, developed by the GNU Project.",
  "There really is a Linux, and these people are using it, but it is just a part of the system they use.",
  "Linux is the kernel: the program in the system that allocates the machine's resources to the other programs that you run.",
  "The kernel is an essential part of an operating system, but useless by itself; it can only function in the context of a complete operating system.",
  "Linux is normally used in combination with the GNU operating system: the whole system is basically GNU with Linux added, or GNU/Linux.",
  "All the so-called \"Linux\" distributions are really distributions of GNU/Linux.",
}

function stallman(line)
  if math.random(100) > 6 then return end

  local selected = linuxshit[math.random(#linuxshit)]

  if line.Args[2]:match("[lL]inux") then
    if line.Args[2]:match("GNU%/") then return end
    bot.Say(line.Args[1], selected)
  end
  if line.Args[2]:match("[Ee]macs") then
    selected = selected:gsub("[Ll]inux", "Emacs")
    bot.Say(line.Args[1], selected)
  end
end

script.AddLuaProtohook(bot, "PRIVMSG", "stallman")


#!/usr/bin/luajit

--[[
Copyright Sam Dodrill 2014

proxyd is a program for proxying easier
--]]

require "irc"
require "notify"
require "socket"

local config = require "config"
local sleep = require "socket".sleep

function show_notification(title, contents)
  local noti = notify.new(title, contents, "")
  notify.set_urgency(noti, 2)
  notify.show(noti)
end

function chat_notification(source, target, message)
  if target == nickname then
    show_notification(source.nick .. ": Private message", message)
  else
    show_notification(target .. ": " .. source.nick, message)
  end
end

local s = irc.new{nick = nickname}

s:hook("OnChat", function(user, target, message)
  if string.find(message, nickname, 1) then
    chat_notification(user, target, message)
  end
end)

s:hook("PreRegister", function(connection)
  s:send("PASS " .. password)
end)

s:connect(hostname, port)

while true do
  s:think()
  sleep(0.1)
end


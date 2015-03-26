var app = require('app');  // Module to control application life.
var BrowserWindow = require('browser-window');  // Module to create native browser window.

require("coffee-script/register");

var mainWindow = null;

// Quit when all windows are closed.
app.on('window-all-closed', function() {
  app.quit();
  process.exit(0);
});

Menu = require('menu');

var template = [
{
  label: 'File',
  submenu: [
  {
    label: 'About Duta',
    selector: 'orderFrontStandardAboutPanel:'
  },
  {
    type: 'separator'
  },
  {
    label: 'Services',
    submenu: []
  },
  {
    label: 'Quit',
    accelerator: 'CmdOrCtrl+Q',
    click: function() { app.quit(); process.exit(0); }
  },
  ]
},
{
  label: 'Edit',
  submenu: [
  {
    label: 'Undo',
    accelerator: 'CmdOrCtrl+Z',
    selector: 'undo:'
  },
  {
    label: 'Redo',
    accelerator: 'Shift+CmdOrCtrl+Z',
    selector: 'redo:'
  },
  {
    type: 'separator'
  },
  {
    label: 'Cut',
    accelerator: 'CmdOrCtrl+X',
    selector: 'cut:'
  },
  {
    label: 'Copy',
    accelerator: 'CmdOrCtrl+C',
    selector: 'copy:'
  },
  {
    label: 'Paste',
    accelerator: 'CmdOrCtrl+V',
    selector: 'paste:'
  },
  {
    label: 'Select All',
    accelerator: 'CmdOrCtrl+A',
    selector: 'selectAll:'
  },
  ]
},
{
  label: 'View',
  submenu: [
  {
    label: 'Reload',
    accelerator: 'CmdOrCtrl+R',
    click: function() { BrowserWindow.getFocusedWindow().reloadIgnoringCache(); }
  },
  {
    label: 'Toggle DevTools',
    accelerator: 'Alt+CmdOrCtrl+I',
    click: function() { BrowserWindow.getFocusedWindow().toggleDevTools(); }
  },
  ]
},
];

menu = Menu.buildFromTemplate(template);
Menu.setApplicationMenu(menu);

app.on('ready', function() {
  mainWindow = new BrowserWindow({
    width:                1024,
    height:               768,
    "auto-hide-menu-bar": true,
  });

  mainWindow.loadUrl('file://' + __dirname + '/index.html');

  mainWindow.on('closed', function() {
    mainWindow = null;
  });
});

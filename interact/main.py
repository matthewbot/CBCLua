#!/usr/bin/python

import wx
import gui
import net
from functools import wraps

def verify_connected(msg=None):
	def decorator(f):
		@wraps(f)
		def wrapper(self, *args, **kwds):
			if self.cbcconn is None:
				if msg is not None:
					self.shellframe.write_line(msg, "systemerror")
				return
		
			f(self, *args, **kwds)
		return wrapper
	return decorator

class InteractApp(wx.App):
	def OnInit(self):
		self.cbcfinder = net.CBCFinder(self)
		self.cbcconn = None
		self.lastdownload = None
		
		self.shellframe = gui.ShellFrame(None, self)
		self.connectdialog = gui.ConnectDialog(self.shellframe, self)
		self.consoleframe = gui.ConsoleFrame(self.shellframe, self)
		
		self.shellframe.Show()
		self.show_connect_dialog()
		return True
		
	def show_connect_dialog(self):
		self.update_connect_dialog()
		self.connectdialog.ShowModal()
		
	def update_connect_dialog(self):
		self.connectdialog.clear_cbcs()
		self.cbcfinder.send_request()
		
	def disconnect(self):
		if self.cbcconn is not None:
			self.cbcconn.close()
			self.cbcconn = None
			
			def callback():
				self.shellframe.disable_send()
				self.shellframe.disable_stop()
			wx.CallAfter(callback)
		
	def on_shell_send(self, expr):
		self.shellframe.disable_send()
		self.shellframe.write_line()
		self.shellframe.write_line(expr)
		self.cbcconn.send_expr(expr)
		
	@verify_connected()
	def on_shell_stop(self):
		self.cbcconn.send_stop()
		
	def on_shell_connect(self):
		self.show_connect_dialog()
	
	def on_shell_disconnect(self):
		self.disconnect()
		self.shellframe.write_line("Disconnected", "system")
		
	@verify_connected("Must be connected to download")
	def on_shell_checkdownload(self):
		return True
		
	def on_shell_download(self, path):
		self.cbcconn.send_download(path)
		self.shellframe.write_line("Download complete!", "system")
		self.lastdownload = path
		
	@verify_connected("Must be connected to reload")
	def on_shell_reload(self):
		if self.lastdownload is None:
			self.shellframe.write_line("Must download a program before reloading it!", "systemerror")
		else:
			self.on_shell_download(self.lastdownload)
		
	def on_shell_window_console(self):
		self.consoleframe.Show()
		self.consoleframe.Raise()
		
	@verify_connected()
	def on_console_buttondown(self, buttonname):
		self.cbcconn.send_button_down(buttonname)
		
	@verify_connected()
	def on_console_buttonup(self, buttonname):
		self.cbcconn.send_button_up(buttonname)
		
	@verify_connected("Must be connected to run main")
	def on_console_runmain(self):
		self.cbcconn.send_runmain()
		
	@verify_connected("Must be connected to stop")
	def on_console_stop(self):
		self.cbcconn.send_stop()
		
	def on_console_close(self):
		self.consoleframe.Hide()
		
	def on_cbclist_connect(self, ip):
		self.disconnect()
		self.cbcconn = net.CBCConnection(self, ip)
		self.connectdialog.Hide()
		
	def on_cbclist_refresh(self):
		self.update_connect_dialog()
		
	def on_cbclist_cancel(self):
		self.connectdialog.Hide()
		
	def on_cbclist_close(self):
		self.connectdialog.Hide()
		
	def on_net_cbcresponse(self, name, ip, conns):
		wx.CallAfter(self.connectdialog.add_cbc, name, ip, conns)
		
	def on_net_version(self, name, version):
		def callback():
			self.shellframe.write_line("Connected to " + name + " (" + version + ")", "system")
			self.shellframe.enable_send()
			self.shellframe.enable_stop()
		wx.CallAfter(callback)
		
	def on_net_result(self, result):
		self.write_shellframe_and_enable_send(result, "result")
		
	def on_net_error(self, error):
		self.write_shellframe_and_enable_send(error, "error")
		
	def on_net_print(self, msg):
		wx.CallAfter(self.consoleframe.write, msg)
		
	def write_shellframe_and_enable_send(self, msg, style):
		def callback():
			if msg:
				self.shellframe.write_line(msg, style)
			self.shellframe.enable_send()
		wx.CallAfter(callback)
		
	def on_net_connerror(self):
		self.cbcconn = None
		wx.CallAfter(self.shellframe.write_line, "Connection Error", "systemerror")
		
def main():
	app = InteractApp()
	app.MainLoop()
	
if __name__ == '__main__':
	main()


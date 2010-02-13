#!/usr/bin/python
import wx
import gui
import net
import netstub
from functools import wraps

def verify_connected(msg=None):
	def decorator(f):
		@wraps(f)
		def wrapper(self, *args, **kwds):
			if self.cbcconn is None:
				if msg is not None:
					self.shellframe.write_line(msg, "systemerror")
				return
		
			return f(self, *args, **kwds)
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
		self.tasklistframe = gui.TaskListFrame(self.shellframe, self)
		
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
	
	def on_shell_send(self, expr):
		self.shellframe.write_line()
		self.shellframe.write_line(expr)
		if self.cbcconn:
			self.cbcconn.send_expr(expr)
		else:
			self.shellframe.write_line("Not connected", "systemerror")
		
	@verify_connected("Must be connected to stop")
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
		
	def on_shell_window_tasklist(self):
		self.tasklistframe.Show()
		self.tasklistframe.Raise()
		
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
		if ip == "stub":
			self.cbcconn = netstub.StubConnection(self)
		else:
			self.cbcconn = net.CBCConnection(self, ip)
		self.connectdialog.Hide()
		
	def on_cbclist_refresh(self):
		self.update_connect_dialog()
		
	def on_cbclist_cancel(self):
		self.connectdialog.Hide()
		
	def on_cbclist_close(self):
		self.connectdialog.Hide()
	
	@verify_connected("Must be connected to stop a task")
	def on_tasklist_stop(self, taskid):
		self.cbcconn.send_stop_task(taskid)
	
	def on_tasklist_close(self):
		self.tasklistframe.Hide()
		
	def on_net_cbcresponse(self, name, ip, conns):
		wx.CallAfter(self.connectdialog.add_cbc, name, ip, conns)
		
	def on_net_version(self, name, version):
		wx.CallAfter(self.shellframe.write_line, "Connected to " + name + " (" + version + ")", "system")
		
	def on_net_result(self, result):
		wx.CallAfter(self.shellframe.write_line, result, "result")
		
	def on_net_error(self, error):
		wx.CallAfter(self.shellframe.write_line, error, "error")
		
	def on_net_print(self, msg):
		wx.CallAfter(self.consoleframe.write, msg)
		
	def on_net_display_clear(self):
		wx.CallAfter(self.consoleframe.display_clear)
		
	def on_net_beep(self):
		wx.CallAfter(self.consoleframe.beep)
		
	def on_net_tasklist(self, tasks):
		wx.CallAfter(self.tasklistframe.update_tasks, tasks)
		
	def on_net_connerror(self):
		self.cbcconn = None
		wx.CallAfter(self.shellframe.write_line, "Connection Error", "systemerror")
		
def main():
	app = InteractApp()
	app.MainLoop()
	
if __name__ == '__main__':
	main()


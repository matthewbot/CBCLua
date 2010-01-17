#!/usr/bin/python

import wx
import gui
import net

class InteractApp(wx.App):
	def OnInit(self):
		self.cbcfinder = net.CBCFinder(self)
		self.cbcconn = None
		self.shellframe = gui.ShellFrame(None, self)
		self.shellframe.Show()
		self.connectdialog = gui.ConnectDialog(self.shellframe, self)
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
		
	def on_shell_stop(self):
		self.cbcconn.send_stop()
		
	def on_shell_connect(self):
		self.show_connect_dialog()
		
	def on_shell_disconnect(self):
		self.disconnect()
		self.shellframe.write_line("Disconnected", "system")
		
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


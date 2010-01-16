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
		
	def on_shell_send(self, expr):
		self.shellframe.disable_send()
		self.shellframe.write_line()
		self.shellframe.write_line(expr)
		self.cbcconn.send_expr(expr)
		
	def on_cbclist_connect(self, ip):
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
		wx.CallAfter(self.shellframe.write_line, "Connected to " + name + " (" + version + ")", style="system")
		
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
		wx.CallAfter(self.shellframe.write_line, "Connection Error", style="systemerror")
		
def main():
	app = InteractApp()
	app.MainLoop()
	
if __name__ == '__main__':
	main()


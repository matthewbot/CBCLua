import wx
from wx.lib.mixins.listctrl import ListCtrlAutoWidthMixin

class ShellFrame(wx.Frame):
	def __init__(self, parent, callbacks):
		wx.Frame.__init__(self, parent, title='Interact', size=wx.Size(500, 400))
		self.callbacks = callbacks
		self.sendenabled = False
		
		splitter = wx.SplitterWindow(self)
		bottom_panel = wx.Panel(splitter, style=wx.SP_3D)
		
		self.sendbutton = wx.Button(bottom_panel, label="Send")
		self.sendbutton.SetDefault()
		self.sendbutton.Disable()
		self.output = wx.TextCtrl(splitter, style=wx.TE_MULTILINE|wx.TE_RICH)
		self.input = wx.TextCtrl(bottom_panel, style=wx.TE_MULTILINE|wx.TE_RICH)
		self.input.SetFont(wx.Font(12, wx.FONTFAMILY_TELETYPE, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL))
		
		splitter.SplitHorizontally(self.output, bottom_panel, -35)
		splitter.SetSashGravity(1.0)
		
		bottom_panel_box = wx.BoxSizer(wx.HORIZONTAL)
		bottom_panel.SetSizer(bottom_panel_box)
		bottom_panel_box.Add(self.input, 1, wx.EXPAND)
		bottom_panel_box.Add(self.sendbutton, 0, wx.ALIGN_CENTER)
		
		self.input.Bind(wx.EVT_CHAR, self.evt_char)
		self.sendbutton.Bind(wx.EVT_BUTTON, self.evt_sendbutton)
		
		cbc_menu = wx.Menu()
		cbc_menu_connect = cbc_menu.Append(wx.ID_ANY, "&Connect")
		cbc_menu_disconnect = cbc_menu.Append(wx.ID_ANY, "&Disconnect")
		
		menubar = wx.MenuBar()
		menubar.Append(cbc_menu, "&CBC")
		self.SetMenuBar(menubar)
		
		self.Bind(wx.EVT_MENU, self.evt_menu_connect, cbc_menu_connect)
		self.Bind(wx.EVT_MENU, self.evt_menu_disconnect, cbc_menu_disconnect)
		
		self.stylemap = dict(
			user=wx.TextAttr(
				font=wx.Font(12, wx.FONTFAMILY_TELETYPE, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL),
				colText="BLACK"
			),
			result=wx.TextAttr(
				font=wx.Font(12, wx.FONTFAMILY_TELETYPE, wx.FONTSTYLE_ITALIC, wx.FONTWEIGHT_NORMAL),
				colText="BLACK"
			),
			error=wx.TextAttr(
				font=wx.Font(12, wx.FONTFAMILY_TELETYPE, wx.FONTSTYLE_ITALIC, wx.FONTWEIGHT_NORMAL),
				colText="RED"
			),
			system=wx.TextAttr(
				font=wx.Font(12, wx.FONTFAMILY_TELETYPE, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_BOLD),
				colText="BLUE"
			),
			systemerror=wx.TextAttr(
				font=wx.Font(12, wx.FONTFAMILY_TELETYPE, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_BOLD),
				colText="RED"
			)
		)
		
	def enable_send(self):
		self.sendenabled = True
		self.sendbutton.Enable()
		
	def disable_send(self):
		self.sendenabled = False
		self.sendbutton.Disable()
		
	def do_send(self):
		text = self.input.GetValue()
		if len(text) > 0:
			self.callbacks.on_shell_send(text)
			self.input.ChangeValue("")
		
	def write_line(self, text="", style="user"):
		self.output.SetDefaultStyle(self.stylemap[style])
		self.output.AppendText(text + "\n")
		
	def evt_char(self, keyevent):
		if keyevent.GetKeyCode() == wx.WXK_RETURN and not keyevent.ControlDown():
			if self.sendenabled:
				self.do_send()
		else:
			keyevent.Skip()
			
	def evt_sendbutton(self, buttonevent):
		self.do_send()
		
	def evt_menu_connect(self, menuevent):
		self.callbacks.on_shell_connect()
		
	def evt_menu_disconnect(self, menuevent):
		self.callbacks.on_shell_disconnect()

class ConnectDialog(wx.Dialog):
	def __init__(self, parent, callbacks):
		wx.Dialog.__init__(self, parent, title='Connect to CBC', size=wx.Size(300, 200))
		self.callbacks = callbacks
		self.cbcs = { }
		self.next_cbc_id = 1
		
		panel = wx.Panel(self)
		button_panel = wx.Panel(panel)
		self.cbclist = CBCList(panel)
		cancel = wx.Button(button_panel, label="Cancel")
		refresh = wx.Button(button_panel, label="&Refresh")
		self.connect = wx.Button(button_panel, label="&Connect")
		self.connect.SetDefault()
		self.connect.Disable()
		
		panel_box = wx.BoxSizer(wx.VERTICAL)
		panel.SetSizer(panel_box)
		panel_box.Add(self.cbclist, 1, wx.EXPAND)
		panel_box.Add(button_panel, 0, wx.EXPAND)
		
		button_panel_box = wx.BoxSizer(wx.HORIZONTAL)
		button_panel.SetSizer(button_panel_box)
		button_panel_box.AddStretchSpacer()
		button_panel_box.Add(cancel)
		button_panel_box.Add(refresh)
		button_panel_box.Add(self.connect)
		
		self.cbclist.Bind(wx.EVT_LIST_ITEM_SELECTED, self.evt_item_selected)
		self.cbclist.Bind(wx.EVT_LIST_ITEM_DESELECTED, self.evt_item_deselected)
		self.cbclist.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.evt_item_activated)
		
		cancel.Bind(wx.EVT_BUTTON, self.evt_cancelbutton)
		refresh.Bind(wx.EVT_BUTTON, self.evt_refreshbutton)
		self.connect.Bind(wx.EVT_BUTTON, self.evt_connectbutton)
		
		self.Bind(wx.EVT_CLOSE, self.evt_close)
		
	def add_cbc(self, name, ip, conns):
		cbcid = self.next_cbc_id
		self.next_cbc_id += 1
		
		cbc = dict(name=name, ip=ip, conns=conns)
		self.cbcs[cbcid] = cbc
		
		index = self.cbclist.InsertStringItem(cbcid, name)
		self.cbclist.SetStringItem(index, 1, ip)
		self.cbclist.SetStringItem(index, 2, str(conns))
		self.cbclist.SetItemData(index, cbcid)
		
	def clear_cbcs(self):
		self.cbcs = { }
		self.cbclist.DeleteAllItems()
		
	def evt_item_selected(self, listevent):
		self.connect.Enable()
		
	def evt_item_deselected(self, listevent):
		self.connect.Disable()
		
	def evt_item_activated(self, listevent):
		cbc = self.cbcs[listevent.GetData()]
		self.callbacks.on_cbclist_connect(cbc['ip'])
		
	def evt_cancelbutton(self, buttonevent):
		self.callbacks.on_cbclist_cancel()	
	
	def evt_refreshbutton(self, buttonevent):
		self.callbacks.on_cbclist_refresh()
	
	def evt_connectbutton(self, buttonevent):
		selected = self.cbclist.GetFirstSelected()
		if selected == -1:
			return
			
		cbc = self.cbcs[self.cbclist.GetItemData(selected)]
		self.callbacks.on_cbclist_connect(cbc['ip'])
		
	def evt_close(self, closeevent):
		self.callbacks.on_cbclist_close()
			
class CBCList(wx.ListCtrl, ListCtrlAutoWidthMixin):
	def __init__(self, parent):
		wx.ListCtrl.__init__(self, parent, style=wx.LC_REPORT | wx.LC_SINGLE_SEL)
		ListCtrlAutoWidthMixin.__init__(self)
		
		self.InsertColumn(0, "Name")
		self.InsertColumn(1, "IP", width=120)
		self.InsertColumn(2, "Conns", width=50)
		
		self.setResizeColumn(0)
	

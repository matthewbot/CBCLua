import wx
from wx.lib.mixins.listctrl import ListCtrlAutoWidthMixin

class ShellFrame(wx.Frame):
	def __init__(self, parent, callbacks):
		wx.Frame.__init__(self, parent, title='Interact', size=wx.Size(600, 400))
		self.callbacks = callbacks
		self.history = [ ]
		self.history_pos = 0
		
		splitter = wx.SplitterWindow(self)
		bottom_panel = wx.Panel(splitter, style=wx.SP_3D)
		
		self.sendbutton = wx.Button(bottom_panel, label="Send")
		self.stopbutton = wx.Button(bottom_panel, label="Stop")
		self.output = wx.TextCtrl(splitter, style=wx.TE_MULTILINE|wx.TE_RICH2|wx.TE_READONLY)
		self.input = wx.TextCtrl(bottom_panel, style=wx.TE_MULTILINE|wx.TE_RICH2|wx.TE_PROCESS_TAB)
		self.input.SetFont(wx.Font(12, wx.FONTFAMILY_TELETYPE, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL))
		
		splitter.SplitHorizontally(self.output, bottom_panel, -35)
		splitter.SetSashGravity(1.0)
		
		bottom_panel_box = wx.BoxSizer(wx.HORIZONTAL)
		bottom_panel.SetSizer(bottom_panel_box)
		bottom_panel_box.Add(self.input, 1, wx.EXPAND)
		bottom_panel_box.Add(self.sendbutton, 0, wx.ALIGN_CENTER)
		bottom_panel_box.Add(self.stopbutton, 0, wx.ALIGN_CENTER)
		
		self.input.Bind(wx.EVT_KEY_DOWN, self.evt_key_down)
		self.sendbutton.Bind(wx.EVT_BUTTON, self.evt_sendbutton)
		self.stopbutton.Bind(wx.EVT_BUTTON, self.evt_stopbutton)
		
		cbc_menu = wx.Menu()
		cbc_menu_connect = cbc_menu.Append(wx.ID_ANY, "&Connect")
		cbc_menu_disconnect = cbc_menu.Append(wx.ID_ANY, "&Disconnect")
		cbc_menu.AppendSeparator()
		cbc_menu_reset = cbc_menu.Append(wx.ID_ANY, "&Reset")
		cbc_menu_clearhistory = cbc_menu.Append(wx.ID_ANY, "Clear &History")
		cbc_menu_cleardisplay = cbc_menu.Append(wx.ID_ANY, "Clear &Display")
		
		program_menu = wx.Menu()
		program_menu_download = program_menu.Append(wx.ID_ANY, "&Download")
		program_menu_reload = program_menu.Append(wx.ID_ANY, "&Reload")
		
		window_menu = wx.Menu()
		window_menu_console = window_menu.Append(wx.ID_ANY, "&Console")
		window_menu_tasklist = window_menu.Append(wx.ID_ANY, "&Task list")
		
		menubar = wx.MenuBar()
		menubar.Append(cbc_menu, "&CBC")
		menubar.Append(program_menu, "&Program")
		menubar.Append(window_menu, "&Windows")
		self.SetMenuBar(menubar)
		
		self.Bind(wx.EVT_MENU, self.evt_menu_connect, cbc_menu_connect)
		self.Bind(wx.EVT_MENU, self.evt_menu_disconnect, cbc_menu_disconnect)
		self.Bind(wx.EVT_MENU, self.evt_menu_reset, cbc_menu_reset)
		self.Bind(wx.EVT_MENU, self.evt_menu_clearhistory, cbc_menu_clearhistory)
		self.Bind(wx.EVT_MENU, self.evt_menu_cleardisplay, cbc_menu_cleardisplay)
		self.Bind(wx.EVT_MENU, self.evt_menu_download, program_menu_download)
		self.Bind(wx.EVT_MENU, self.evt_menu_reload, program_menu_reload)
		self.Bind(wx.EVT_MENU, self.evt_menu_console, window_menu_console)
		self.Bind(wx.EVT_MENU, self.evt_menu_tasklist, window_menu_tasklist)
		
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
		
	def do_send(self):
		text = self.input.GetValue()
		if text == "":
			return
			
		self.callbacks.on_shell_send(text)
		self.input.ChangeValue("")
		
		if len(self.history) == 0 or self.history[-1] != text:
			self.history.append(text)
			
		self.history_pos = len(self.history)
		
	def write_line(self, text="", style="user"):
		self.output.SetDefaultStyle(self.stylemap[style])
		self.output.AppendText(text + "\n")
		self.output.ScrollLines(-1)
		
	def evt_key_down(self, keyevent):
		keycode = keyevent.GetKeyCode()
	
		if keycode == wx.WXK_RETURN and not keyevent.ShiftDown():
			self.do_send()
			return
		elif keycode == wx.WXK_UP:
			if self.history_pos == 0:
				return
			self.history_pos -= 1
			self.input.ChangeValue(self.history[self.history_pos])
			self.input.SetInsertionPointEnd()
			return		
		elif keycode == wx.WXK_DOWN:
			if self.history_pos == len(self.history):
				return
			self.history_pos += 1
			if self.history_pos == len(self.history):
				self.input.ChangeValue("")
			else:
				self.input.ChangeValue(self.history[self.history_pos])
			self.input.SetInsertionPointEnd()
			return
		elif keycode == ord('R') and keyevent.ControlDown():
			self.callbacks.on_shell_reload()
			return
		elif keycode == ord('S') and keyevent.ControlDown():
			self.callbacks.on_shell_stop()
			return
				
		keyevent.Skip()
			
	def evt_sendbutton(self, buttonevent):
		self.do_send()
		
	def evt_stopbutton(self, buttonevent):
		self.callbacks.on_shell_stop()
		
	def evt_menu_connect(self, menuevent):
		self.callbacks.on_shell_connect()
		
	def evt_menu_disconnect(self, menuevent):
		self.callbacks.on_shell_disconnect()
		
	def evt_menu_reset(self, menuevent):
		self.callbacks.on_shell_reset()
		
	def evt_menu_clearhistory(self, menuevent):
		self.history = [ ]
		self.history_pos = 0
		self.output.ChangeValue("")
		
	def evt_menu_cleardisplay(self, menuevent):
		self.callbacks.on_shell_clear_display()
		
	def evt_menu_download(self, menuevent):
		if not self.callbacks.on_shell_checkdownload():
			return
			
		dirdialog = wx.DirDialog(self, "Download folder", style=wx.DD_DIR_MUST_EXIST)
		if dirdialog.ShowModal() == wx.ID_OK:
			self.callbacks.on_shell_download(dirdialog.GetPath())
			
	def evt_menu_reload(self, menuevent):
		self.callbacks.on_shell_reload()
			
	def evt_menu_console(self, menuevent):
		self.callbacks.on_shell_window_console()
		
	def evt_menu_tasklist(self, menuevent):
		self.callbacks.on_shell_window_tasklist()

class ConnectDialog(wx.Dialog):
	def __init__(self, parent, callbacks):
		wx.Dialog.__init__(self, parent, title='Connect to CBC', size=wx.Size(350, 250))
		self.callbacks = callbacks
		self.cbcs = { }
		self.next_cbc_id = 1
		
		button_panel = wx.Panel(self)
		self.cbclist = CBCList(self)
		cancel = wx.Button(button_panel, label="Cancel")
		manual = wx.Button(button_panel, label="Manual...")
		refresh = wx.Button(button_panel, label="&Refresh")
		self.connect = wx.Button(button_panel, label="&Connect")
		self.connect.SetDefault()
		self.connect.Disable()
		
		self_box = wx.BoxSizer(wx.VERTICAL)
		self.SetSizer(self_box)
		self_box.Add(self.cbclist, 1, wx.EXPAND)
		self_box.Add(button_panel, 0, wx.EXPAND)
		
		button_panel_box = wx.BoxSizer(wx.HORIZONTAL)
		button_panel.SetSizer(button_panel_box)
		button_panel_box.AddStretchSpacer()
		button_panel_box.Add(cancel)
		button_panel_box.Add(manual)
		button_panel_box.Add(refresh)
		button_panel_box.Add(self.connect)
		
		self.cbclist.Bind(wx.EVT_LIST_ITEM_SELECTED, self.evt_item_selected)
		self.cbclist.Bind(wx.EVT_LIST_ITEM_DESELECTED, self.evt_item_deselected)
		self.cbclist.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.evt_item_activated)
		
		cancel.Bind(wx.EVT_BUTTON, self.evt_cancelbutton)
		refresh.Bind(wx.EVT_BUTTON, self.evt_refreshbutton)
		manual.Bind(wx.EVT_BUTTON, self.evt_manualbutton)
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
	
	def evt_manualbutton(self, buttonevent):
		textdialog = wx.TextEntryDialog(self, "IP Address", "Manual Connection")
		if textdialog.ShowModal() == wx.ID_OK:
			self.callbacks.on_cbclist_connect(textdialog.GetValue())	
	
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
		
class ConsoleFrame(wx.Frame):
	def __init__(self, parent, callbacks):
		wx.Frame.__init__(self, parent, title='Console', size=wx.Size(320, 240))
		self.callbacks = callbacks
		
		panel = wx.Panel(self)
		toppanel = wx.Panel(panel)
		buttonpanel = wx.Panel(panel)
		abpanel = wx.Panel(buttonpanel)
		dpadpanel = wx.Panel(buttonpanel)
	
		self.screen = wx.TextCtrl(panel, style=wx.TE_MULTILINE|wx.TE_RICH2)
		self.screen.SetEditable(False)
		self.bellofftimer = wx.Timer()
		self.bellofftimer.Bind(wx.EVT_TIMER, self.evt_bell_off)
		
		buttonconfigs = [
			("A", "a", abpanel),
			("B", "b", abpanel),
			("U", "up", dpadpanel),
			("D", "down", dpadpanel),
			("L", "left", dpadpanel),
			("R", "right", dpadpanel)]
			
		buttons = { }
			
		for (label, name, parent) in buttonconfigs:
			button = wx.Button(parent, label=label, size=(40, 30), name=name)
			button.Bind(wx.EVT_LEFT_DOWN, self.evt_left_down)
			button.Bind(wx.EVT_LEFT_UP, self.evt_left_up)
			buttons[label] = button
			
		runmain = wx.Button(toppanel, label="Run Main")
		runmain.Bind(wx.EVT_BUTTON, self.evt_runmain)
		stop = wx.Button(toppanel, label="Stop")
		stop.Bind(wx.EVT_BUTTON, self.evt_stop)
		
		panel_sizer = wx.BoxSizer(wx.VERTICAL)
		panel.SetSizer(panel_sizer)
		panel_sizer.Add(toppanel, 0, wx.EXPAND|wx.TOP|wx.BOTTOM, 2)
		panel_sizer.Add(self.screen, 1, wx.EXPAND)
		panel_sizer.Add(buttonpanel, 0, wx.EXPAND|wx.TOP|wx.BOTTOM, 2)
		
		toppanel_sizer = wx.GridSizer(1, 2)
		toppanel.SetSizer(toppanel_sizer)
		toppanel_sizer.Add(runmain, flag=wx.ALIGN_CENTER)
		toppanel_sizer.Add(stop, flag=wx.ALIGN_CENTER)
		
		buttonpanel_sizer = wx.BoxSizer(wx.HORIZONTAL)
		buttonpanel.SetSizer(buttonpanel_sizer)
		buttonpanel_sizer.Add(dpadpanel, flag=wx.LEFT, border=10)
		buttonpanel_sizer.AddStretchSpacer()
		buttonpanel_sizer.Add(abpanel, flag=wx.RIGHT|wx.EXPAND, border=30)
		
		dpadpanel_sizer = wx.GridBagSizer()
		dpadpanel.SetSizer(dpadpanel_sizer)
		dpadpanel_sizer.Add(buttons["L"], (0, 0), (2, 1), flag=wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL)
		dpadpanel_sizer.Add(buttons["R"], (0, 2), (2, 1), flag=wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL)
		dpadpanel_sizer.Add(buttons["U"], (0, 1), flag=wx.ALIGN_BOTTOM)
		dpadpanel_sizer.Add(buttons["D"], (1, 1), flag=wx.ALIGN_TOP)
		
		abpanel_sizer = wx.GridSizer(1, 2, hgap=30)
		abpanel.SetSizer(abpanel_sizer)
		abpanel_sizer.Add(buttons["A"], flag=wx.ALIGN_CENTER)
		abpanel_sizer.Add(buttons["B"], flag=wx.ALIGN_CENTER)
		
		self.Bind(wx.EVT_CLOSE, self.evt_close)
		
	def write(self, msg):
		self.screen.AppendText(msg)
		self.screen.ScrollLines(-1)
		
	def display_clear(self):
		self.screen.ChangeValue("")
		
	def beep(self):
		self.screen.SetStyle(0, self.screen.GetLastPosition(), wx.TextAttr("white", "black"))
		self.screen.SetBackgroundColour("black")
		self.bellofftimer.Start(200, True)
		
	def evt_bell_off(self, timerevent):
		self.screen.SetBackgroundColour("white")
		self.screen.SetStyle(0, self.screen.GetLastPosition(), wx.TextAttr("black", "white"))

	def evt_left_down(self, buttonevent):
		buttonevent.Skip()
		buttonname = buttonevent.GetEventObject().GetName().lower()
		self.callbacks.on_console_buttondown(buttonname)
		
	def evt_left_up(self, buttonevent):
		buttonevent.Skip()
		buttonname = buttonevent.GetEventObject().GetName().lower()
		self.callbacks.on_console_buttonup(buttonname)
		
	def evt_runmain(self, buttonevent):
		self.callbacks.on_console_runmain()
		
	def evt_stop(self, buttonevent):
		self.callbacks.on_console_stop()
		
	def evt_close(self, closeevent):
		self.callbacks.on_console_close()
		
class TaskListFrame(wx.Frame):
	def __init__(self, parent, callbacks):
		wx.Frame.__init__(self, parent, title='Task list', size=wx.Size(400, 240))
		self.callbacks = callbacks
		self.tasks = None
		self.selected_taskid = None
		
		panel = wx.Panel(self)
		checkpanel = wx.Panel(panel)
		
		self.tasklist = TaskList(panel)
		self.tasklist.Bind(wx.EVT_LIST_ITEM_SELECTED, self.evt_taskselected)
		self.showsystem = wx.CheckBox(checkpanel, wx.ID_ANY, "System tasks")
		self.showsystem.Bind(wx.EVT_CHECKBOX, self.evt_showsystem)
		self.showstopped = wx.CheckBox(checkpanel, wx.ID_ANY, "Stopped tasks")
		self.showstopped.Bind(wx.EVT_CHECKBOX, self.evt_showstopped)
		stopbutton = wx.Button(checkpanel, wx.ID_ANY, "Stop Task")
		stopbutton.Bind(wx.EVT_BUTTON, self.evt_stopbutton)
		
		panel_sizer = wx.BoxSizer(wx.VERTICAL)
		panel.SetSizer(panel_sizer)
		panel_sizer.Add(self.tasklist, 1, wx.EXPAND)
		panel_sizer.Add(checkpanel, 0, wx.EXPAND)
		
		checkpanel_sizer = wx.BoxSizer(wx.HORIZONTAL)
		checkpanel.SetSizer(checkpanel_sizer)
		checkpanel_sizer.Add(self.showsystem)
		checkpanel_sizer.Add(self.showstopped)
		checkpanel_sizer.AddStretchSpacer()
		checkpanel_sizer.Add(stopbutton)
		
		self.Bind(wx.EVT_CLOSE, self.evt_close)
		
	def update_tasks(self, tasks):
		self.tasks = tasks
		
		self.refresh()
		
	def refresh(self):			
		self.tasklist.DeleteAllItems()
		
		for taskid, taskname, taskstate, tasktype in self.tasks:
			if not self.showsystem.GetValue() and tasktype == "system":
				continue
				
			if not self.showstopped.GetValue() and taskstate == "stopped":
				continue
		
			index = self.tasklist.InsertStringItem(taskid, taskname)
			self.tasklist.SetStringItem(index, 1, taskstate)
			self.tasklist.SetStringItem(index, 2, tasktype)
			self.tasklist.SetItemData(index, taskid)
			
			if taskid == self.selected_taskid:
				self.tasklist.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
			
	def evt_showsystem(self, checkevent):
		self.refresh()
		
	def evt_showstopped(self, checkevent):
		self.refresh()
			
	def evt_taskselected(self, listevent):
		self.selected_taskid = listevent.GetData()
			
	def evt_stopbutton(self, buttonevent):
		if self.selected_taskid is not None:
			self.callbacks.on_tasklist_stop(self.selected_taskid)
	
	def evt_close(self, closeevent):
		self.callbacks.on_tasklist_close()
		
class TaskList(wx.ListCtrl, ListCtrlAutoWidthMixin):
	def __init__(self, parent):
		wx.ListCtrl.__init__(self, parent, style=wx.LC_REPORT | wx.LC_SINGLE_SEL)
		ListCtrlAutoWidthMixin.__init__(self)
		
		self.InsertColumn(0, "Name")
		self.InsertColumn(1, "State", width=100)
		self.InsertColumn(2, "Type", width=100)
		
		self.setResizeColumn(0)
		

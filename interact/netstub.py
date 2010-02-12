class StubConnection():
	def __init__(self, callbacks):
		self.callbacks = callbacks
	
	def close(self):
		pass
		
	def send_expr(self, expr):
		self.callbacks.on_net_result("Done")
		pass
		
	def send_runmain(self):
		pass
		
	def send_stop(self):
		pass
		
	def send_button_down(self, button):
		pass
		
	def send_button_up(self, button):
		pass
		
	def send_clear_code(self):
		pass
		
	def send_reset_env(self):
		pass
		
	def send_download(self, rootpath):
		pass
	
	def make_code_dir(self, codedir):		
		pass
		
	def put_file(self, cbcfilepath, localfilepath):
		pass
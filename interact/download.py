import os.path
import os
import re

class DownloadProcessorError(Exception):
	def __init__(self, msg):
		self.msg = msg
	def __str__(self):
		return repr(self.msg)
	def get_msg(self):
		return self.msg

class DownloadProcessor:
	def __init__(self):
		self.dirs = set()
		self.files = {}
		self.codepaths = set()
			
	def get_dirs(self):
		return self.dirs
		
	def get_files(self):
		return self.files.iteritems()
		
	def add_codefolder(self, codepath):
		codepath = os.path.abspath(codepath)
		if codepath in self.codepaths: # don't download a codefolder more than once
			return
		self.codepaths.add(codepath)
	
		configfilepath = codepath + os.sep + "codefolder.txt" # get a config file
		if not os.path.exists(configfilepath):
			self.add_directory(codepath) # no config file, just download the whole thing
			return
			
		configfile = open(configfilepath)
		lines = configfile.readlines()
		configfile.close()
		
		for line in lines:
			commentindex = line.find("#")
			if commentindex != -1:
				line = line[:commentindex]
			
			line = line.strip()
			if line == "":
				continue
			parts = re.split('\s+', line)
			command = parts[0].lower()
			
			if command == "codefolder":
				newcodepath = parts[1]
				if not os.path.isabs(newcodepath):
					newcodepath = codepath + os.sep + newcodepath
					
				self.add_codefolder(newcodepath)
			elif command == "file" or command == "dir":
				localpath = parts[1]
				if not os.path.isabs(localpath):
					localpath = codepath + os.sep + localpath
				
				if len(parts) >= 3:
					remotepath = "/" + parts[2]
				else:
					remotepath = ""
				
				if command == "file":
					self.add_file(localpath, remotepath + "/" + os.path.basename(localpath))
				else:
					self.add_directory(localpath, remotepath)
			else:
				raise DownloadProcessorError("Bad command '" + command + "' in codefolder '" + configfilepath + "'")
		
	def add_directory(self, localdir, cbcdir=""):
		for name in os.listdir(localdir):
			if name[0] == ".":
				continue
			if name == "codefolder.txt":
				continue
				
			localpath = localdir + os.sep + name
			cbcpath = cbcdir + "/" + name
			if os.path.isfile(localpath):
				self.add_file(localpath, cbcpath)
			elif os.path.isdir(localpath):
				self.add_directory(localpath, cbcpath)
		
	def add_file(self, localfile, cbcfile):
		cbcdir = cbcfile[:cbcfile.rindex("/")]
		self.make_dirs(cbcdir)
		
		if cbcfile not in self.files:
			self.files[cbcfile] = localfile
		else:
			raise DownloadProcessorError("Conflict on cbc file '" + cbcfile + "', between '" + localfile + "' and '" + self.files[cbcfile] + "'")
		
	def make_dirs(self, remotedir):
		dirs = remotedir.split("/")
		pathaccum = ""
		for d in dirs:
			if d == "":
				continue
				
			pathaccum += "/" + d
			self.dirs.add(pathaccum)
	
	def dump(self):
		print "Dirs:"
		for dirpath in self.dirs:
			print "\t" + dirpath
		print "Files:"
		for cbcfile, localfile in self.files.iteritems():
			print "\t" + localfile + " -> " + cbcfile


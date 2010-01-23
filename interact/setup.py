from distutils.core import setup
import py2exe
import sys

sys.argv.append('py2exe')

setup(
	name = "Interact",
	author = "Matthew Thompson",
	
	options = {
		'py2exe': {
			'bundle_files': 1,
			'compressed': 1,
			'optimize': 2,
			'excludes': [
				'_ssl', 'pyreadline', 'difflib', 'doctest', 'optparse', 'pickle', 'calender'
			]
		}
	},
	zipfile = None,
	windows = [{
		'script': "interact.py",
		'other_resources' : [(24, 1, open("manifest").read())]
	}],
)
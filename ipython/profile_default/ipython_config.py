import sys

c = get_config()

c.InteractiveShellApp.exec_lines = ['import os, sys']

c.TerminalInteractiveShell.banner1 = "\npython " + sys.version.replace("\n", "") + "\n"
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.autocall = 1

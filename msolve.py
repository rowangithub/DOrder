#!/usr/bin/python
# Copyright (c) 2008 The Regents of the University of California. All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
# IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
# OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
# TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

import sys, os, os.path, common

d_pats= "default_patterns"
solve = "./liquid.opt -dframes".split()
flags = []
tname = "/tmp/dsolve.scratch"
null  = open("/dev/null", "w")

def cat_files(files,outfile):
  os.system("rm -f %s" % outfile)
  for f in files: os.system("cat %s 1>> %s 2> /dev/null" % (f,outfile))

def gen_quals(src,bare,lq, col):
  bname = src[:-3]
  (fname,qname,hname) = (bname+".ml", bname+".quals", bname+".hquals")
  os.system("rm -f %s" % qname)
  if bare:
    os.system("cp -f %s %s" % (hname, tname))
  else:
    cat_files([hname,d_pats],tname)
  if lq:
    lq = "-lqualifs"
  else:
    lq = ""
  gen   = ("./liquid.opt %s -no-anormal -collect %d -dqualifs" % (lq, col)).split()
  qfile = open(qname, "w")
  succ = common.logged_sys_call(gen + [tname, fname])
  qfile.close()
  return succ

def solve_quals(file,bare,quiet,flags):
  bname = file[:-3]
  os.system("rm -f %s.annot" % bname)
  if quiet: out = null
  else: out = None
  return common.logged_sys_call(solve + flags + [("%s.ml" % bname)], out)

def main():
  argc = len(sys.argv)
  if argc == 1:
    print ("Usage: %s [flags] [sourcefile]" % sys.argv[0])
    sys.exit(0)
  if sys.argv[1] == "-help":
    os.system("./liquid.opt -help")
    sys.exit(0)
  bare = (sys.argv[1] == "-bare")
  if bare:
    flags = sys.argv[2:-1]
  else:
    flags = sys.argv[1:-1]
  fn = sys.argv[len(sys.argv) - 1]
  gen_succ = gen_quals(fn, bare, False, 4)
  if (gen_succ != 0):
    print "Qualifier generation failed"
    sys.exit(gen_succ)
  sys.exit(solve_quals(fn,bare,False,flags))

if __name__ == "__main__":
  main()

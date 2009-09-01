#!/usr/bin/python

# Pomegranate: A SPIN-compatible compiler for the SPIN-compatible Erigone model checker. 
# Copyright (C) 2008 Trishank Karthik.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
# 
# TESTER.PY: Writes automata files and compares them. 
# TODO: May expand to a test suite in the future.

import filecmp, glob, optparse, os, subprocess, sys

def write_automata( compiler, source_directory, target_directory ):
  """ WARNING: Do NOT pass in dangerous stuff as the 'compiler'. """
  assert os.path.isfile( compiler )         is True,  compiler          + ' is not a file!'
  source_directory  = os.path.abspath( source_directory )
  assert os.path.isdir( source_directory )  is True,  source_directory  + ' is not a directory!'
  source_pattern    = os.path.join( source_directory, '*.pml' )
  sources           = glob.glob( source_pattern )
  target_directory  = os.path.join( source_directory, target_directory )
  assert os.path.isdir( target_directory )  is False, target_directory  + ' already exists as a directory!'
  os.mkdir( target_directory )
  for source in sources:
    try:
      # TODO: remove compiler logs, if any
      assert os.path.isfile( source )       is True,  source      + ' is not a file!'
      source_filename = os.path.basename( source ).split( '.' )[ 0 ] 
      target_file = os.path.join( target_directory, source_filename + '.aut' )
      subprocess.check_call( [ compiler, source, target_file ] )
      assert os.path.isfile( target_file )  is True,  target_file + ' does not exist as a file!'
      print 'Compiled ' + source + ' into :' + target_file
    except Exception, error:
      print 'Failed to write automaton for ' + source + ' due to: ' + str( error )

def compare_automata( directory_1, directory_2 ):
  directory_1 = os.path.abspath( directory_1 )
  directory_2 = os.path.abspath( directory_2 )
  assert os.path.isdir( directory_1 ) is True, directory_1 + ' is not a directory!'
  assert os.path.isdir( directory_2 ) is True, directory_2 + ' is not a directory!'
  files_1 = [ object for object in os.listdir( directory_1 ) if os.path.isfile( os.path.join( directory_1, object ) ) is True ]
  files_2 = [ object for object in os.listdir( directory_2 ) if os.path.isfile( os.path.join( directory_2, object ) ) is True ]
  common_filenames = set( files_1 ) & set( files_2 )
  matches, mismatches, errors = filecmp.cmpfiles( directory_1, directory_2, common_filenames )
  print 'Files that match in both directories:'
  for match     in matches:
    print '  ' + match
  print 'Files that mismatch in both directories:'
  for mismatch  in mismatches:
    print '  ' + mismatch
  print 'Files that could not be compared:'
  for error     in errors:
    print '  ' + error

if __name__ == '__main__':
  """
    Usage:
      python tester.py --help
    Examples:
      python tester.py --write    --compiler  ./compiler1   --source  examples/   --target 1
      python tester.py --write    --compiler  ./compiler2   --source  examples/   --target 2
      python tester.py --compare  --dir1      examples/1    --dir2    examples/2
  """

  parser = optparse.OptionParser()
  parser.add_option( '--write',     action = 'store_true', dest = 'write_automata',   default = False,  help = 'Switch to write automata'   )
  parser.add_option( '--compiler',  action = 'store',      dest = 'compiler',         default = None,   help = 'Path to compiler'           )
  parser.add_option( '--source',    action = 'store',      dest = 'source_directory', default = None,   help = 'Source directory'           )
  parser.add_option( '--target',    action = 'store',      dest = 'target_directory', default = None,   help = 'Target directory'           )
 
  parser.add_option( '--compare',   action = 'store_true', dest = 'compare_automata', default = False,  help = 'Switch to compare automata' )
  parser.add_option( '--dir1',      action = 'store',      dest = 'directory_1',      default = None,   help = 'First directory'            )
  parser.add_option( '--dir2',      action = 'store',      dest = 'directory_2',      default = None,   help = 'Second directory'           )
  options, arguments = parser.parse_args()
  parser.destroy()

  if options.write_automata is True:
    assert options.compiler         is not None,  'compiler is not specified!'
    assert options.source_directory is not None,  'source is not specified!'
    assert options.target_directory is not None,  'target is not specified!'
    write_automata( options.compiler, options.source_directory, options.target_directory )
  
  if options.compare_automata is True:
    assert options.directory_1      is not None,  'dir1 is not specified!'
    assert options.directory_2      is not None,  'dir2 is not specified!'
    compare_automata( options.directory_1, options.directory_2 )

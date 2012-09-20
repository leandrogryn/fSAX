#! /bin/sh
#The next line executes wish - wherever it is \
exec tclsh "$0" "$@"

###############################################################################
#fSAX: Fortran implementation of the Simple API for XML (SAX)
#Copyright (C) 2011 Leandro Gryngarten
#
#This file is part of fSAX.
#
#fSAX is free software: you can redistribute it and/or modify
#it under the terms of the GNU Lesser General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#fSAX is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Lesser General Public License for more details.
#
#You should have received a copy of the GNU Lesser General Public License
#along with fSAX.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################

proc showHelp {} {
  global argv0
  puts "fSAX: Fortran implementation of the Simple API for XML (SAX)"
  puts ""
  puts "Use this installation script for a basic installation, which will generate the mod files and libfsax.a to compile and link with your application."
  puts "One may also want to generate the documentation and testing. In such case a manual installation is required."
  puts ""
  puts "Usage:"
  puts "$argv0 help"
  puts " will show the current help."
  puts "Or"
  puts "$argv0 \[options ...\]"
  puts " Where the options can be:"
  puts "- FC            = name of the fortran compiler to use."
  puts "                  Default: FC=gfortran"
  puts "- TARGET_PREFIX = target directory where the include and lib directories"
  puts "                  will be located."
  puts "                  Default: TARGET_PREFIX=./fsax"
  puts "Example:"
  puts "$argv0 FC=gfortran TARGET_PREFIX=/usr"
  puts ""
}

proc build {} {
  global fc target_prefix
  puts "Configuring..."
  file mkdir $target_prefix
  set savedDir [pwd]
  cd $target_prefix
  set cmd "cmake $savedDir/src -DCMAKE_Fortran_COMPILER=$fc"
  catch {eval "exec $cmd"} output
  puts $output
  set cmd "cmake $savedDir/src -DCMAKE_INSTALL_PREFIX=."
  catch {eval "exec $cmd"} output
  puts $output
  puts "\nCompiling..."
  catch {exec make install} output
  puts $output
  puts "\nDone."
  cd $savedDir
}

# default options:
set fc "gfortran"
set target_prefix "./fsax"

for {set i 0} {$i < $argc} {incr i} {
  set option [lindex $argv $i]
  switch [string range $option 0 [expr { [string first "=" $option] - 1}]] {
    FC {
      set fc [string range $option [expr { [string first "=" $option] + 1}] end]
    }
    TARGET_PREFIX {
      set target_prefix [string range $option [expr { [string first "=" $option] + 1}] end]
    }
  }
}

if {$argc == 1} {
  if {[lindex $argv 0] == "help"} {
    showHelp
  } else {
    build
  }
} elseif {$argc == 0} {
  showHelp
  build
} else {
  build
}

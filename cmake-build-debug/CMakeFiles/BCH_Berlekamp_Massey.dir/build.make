# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.15

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /cygdrive/c/Users/Gamer/.CLion2019.3/system/cygwin_cmake/bin/cmake.exe

# The command to remove a file.
RM = /cygdrive/c/Users/Gamer/.CLion2019.3/system/cygwin_cmake/bin/cmake.exe -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/BCH_Berlekamp_Massey.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/BCH_Berlekamp_Massey.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/BCH_Berlekamp_Massey.dir/flags.make

CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.o: CMakeFiles/BCH_Berlekamp_Massey.dir/flags.make
CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.o: ../main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.o"
	/usr/bin/c++.exe  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.o -c /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/main.cpp

CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.i"
	/usr/bin/c++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/main.cpp > CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.i

CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.s"
	/usr/bin/c++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/main.cpp -o CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.s

# Object files for target BCH_Berlekamp_Massey
BCH_Berlekamp_Massey_OBJECTS = \
"CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.o"

# External object files for target BCH_Berlekamp_Massey
BCH_Berlekamp_Massey_EXTERNAL_OBJECTS =

BCH_Berlekamp_Massey.exe: CMakeFiles/BCH_Berlekamp_Massey.dir/main.cpp.o
BCH_Berlekamp_Massey.exe: CMakeFiles/BCH_Berlekamp_Massey.dir/build.make
BCH_Berlekamp_Massey.exe: CMakeFiles/BCH_Berlekamp_Massey.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable BCH_Berlekamp_Massey.exe"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/BCH_Berlekamp_Massey.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/BCH_Berlekamp_Massey.dir/build: BCH_Berlekamp_Massey.exe

.PHONY : CMakeFiles/BCH_Berlekamp_Massey.dir/build

CMakeFiles/BCH_Berlekamp_Massey.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/BCH_Berlekamp_Massey.dir/cmake_clean.cmake
.PHONY : CMakeFiles/BCH_Berlekamp_Massey.dir/clean

CMakeFiles/BCH_Berlekamp_Massey.dir/depend:
	cd /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/cmake-build-debug && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/cmake-build-debug /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/cmake-build-debug /cygdrive/c/Users/Gamer/CLionProjects/BCH+Berlekamp_Massey/cmake-build-debug/CMakeFiles/BCH_Berlekamp_Massey.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/BCH_Berlekamp_Massey.dir/depend

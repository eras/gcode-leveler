# Gcode Leveler #

gcode-leveler attempts to overcome non-leveled and non-flat printing
surfaces by applying a formula to all Z heights in a gcode file. It
also interpolates long moves into smaller parts, so that the Z
adjustment can be targeted against those as well.

# Building and Installing #

You need Objective Caml to build gcode-leveler. On Debian and Ubuntu the following should do: 

% sudo apt-get install libbatteries-ocaml-dev libc6-dev libfindlib-ocaml ocaml-nox
% ./build.sh
% sudo install _build/leveler.native /usr/local/bin/gcode-leveler

# Usage #

First determine the Z offsets in your printing bed by moving around
the print head over the bed and moving the Z head so that it barely
touches the surface. You can choose for example 4 point (corners) or 9
points (corners and a cross) depending if you need just leveling or
also flattening. Once you have those, you pass them to leveler with -p
(you can also adjust the global offset with -ofs). Input goes to
standard input, output goes to standard output.

	% gcode-leveler -ofs 0.1 \
	-p 0,150,-0.05 -p 80,150,0.2 -p 160,150,0.15 \
	-p 0,80,0.0 -p 80,80,0.2 -p 180,80,0.15 \
	-p 0,0,0 -p 80,0,0.1 -p 160,0,0.2 < teapot.gcode > teapot.leveled.gcode

And now you're ready to print! Note that this is development quality
software and I'm not responsible if the codes command your printer to
sink deeply inside the printed object and break your printer in the
process. You get to keep all the parts, though.

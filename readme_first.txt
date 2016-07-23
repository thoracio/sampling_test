To use Samplend use the command "open Internals", to be used with
ocamlfind, findlibname:samplend

The principal methods are the preset_sampling_2D and preset_sampling_3D
functions, their defaults values are to work in standard conditions

To more specific sampling the sampling_2D and sampling_3D from Sampling
admits more specific settings.

The Geometry_tools library implements basics methods for the treatment
of 2D and 3D points and lines.

The methods to_tkiz_2D and to_tikz_3D from Tikz_tras creates a .tex file
to visualize the results from the smapling.

The method to_file from Utilities was tought to create a .txt file
with a list wich contains the data from the sampling to be used with
geogebra.

The methods to_list_2D and to_list_tuple_2D allow to transform the results
of sampling to a list of floats or a list of tuple of floats, the same
methos exits for the 3D case.

This  directory contains strings edit tests. To build them with GNAT Ada
compiler use the following command line: 

gnatmake -I../ test_strings_edit.adb
gnatmake -I../ test_string_streams.adb

The  directory  also  contains  the  utility  program used to modify the
library to reflect changes in the UnicodeData.txt file. It is built as: 

gnatmake -I ../ strings_edit-utf8-categorization_generator.adb

and then used as:

strings_edit-utf8-categorization_generator ../strings_edit-utf8-categorization.adb UnicodeData.txt 


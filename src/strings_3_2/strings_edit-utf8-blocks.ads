--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Blocks                    Luebeck            --
--  Interface                                      Spring, 2008       --
--                                                                    --
--                                Last revision :  21:03 21 Apr 2009  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  This package defines the ranges of Unicide code points as defined in
--  the  standard.  The  current  version  corresponds   to   the   file
--  Blocks-5.0.0.txt Date: 2006-02-15, 15:40:00 [KW]
--
with Ada.Finalization;

package Strings_Edit.UTF8.Blocks is
   Aegean_Numbers :
                 constant Code_Points_Range := (16#10100#,  16#1013F#);
   Alphabetic_Presentation_Forms :
                 constant Code_Points_Range := (16#FB00#,   16#FB4F#);
   Ancient_Greek_Musical_Notation :
                 constant Code_Points_Range := (16#1D200#,  16#1D24F#);
   Ancient_Greek_Numbers :
                 constant Code_Points_Range := (16#10140#,  16#1018F#);
   Arabic :      constant Code_Points_Range := (16#0600#,   16#06FF#);
   Arabic_Presentation_Forms_A :
                 constant Code_Points_Range := (16#FB50#,   16#FDFF#);
   Arabic_Presentation_Forms_B :
                 constant Code_Points_Range := (16#FE70#,   16#FEFF#);
   Arabic_Supplement :
                 constant Code_Points_Range := (16#0750#,   16#077F#);
   Armenian :    constant Code_Points_Range := (16#0530#,   16#058F#);
   Arrows :      constant Code_Points_Range := (16#2190#,   16#21FF#);
   Balinese :    constant Code_Points_Range := (16#1B00#,   16#1B7F#);
   Basic_Latin : constant Code_Points_Range := (16#0000#,   16#007F#);
   Bengali :     constant Code_Points_Range := (16#0980#,   16#09FF#);
   Block_Elements :
                 constant Code_Points_Range := (16#2580#,   16#259F#);
   Bopomofo :    constant Code_Points_Range := (16#3100#,   16#312F#);
   Bopomofo_Extended :
                 constant Code_Points_Range := (16#31A0#,   16#31BF#);
   Box_Drawing : constant Code_Points_Range := (16#2500#,   16#257F#);
   Braille_Patterns :
                 constant Code_Points_Range := (16#2800#,   16#28FF#);
   Buginese :    constant Code_Points_Range := (16#1A00#,   16#1A1F#);
   Buhid :       constant Code_Points_Range := (16#1740#,   16#175F#);
   Byzantine_Musical_Symbols :
                 constant Code_Points_Range := (16#1D000#,  16#1D0FF#);
   CJK_Compatibility :
                 constant Code_Points_Range := (16#3300#,   16#33FF#);
   CJK_Compatibility_Forms :
                 constant Code_Points_Range := (16#FE30#,   16#FE4F#);
   CJK_Compatibility_Ideographs :
                 constant Code_Points_Range := (16#F900#,   16#FAFF#);
   CJK_Compatibility_Ideographs_Supplement :
                 constant Code_Points_Range := (16#2F800#,  16#2FA1F#);
   CJK_Radicals_Supplement :
                 constant Code_Points_Range := (16#2E80#,   16#2EFF#);
   CJK_Strokes : constant Code_Points_Range := (16#31C0#,   16#31EF#);
   CJK_Symbols_and_Punctuation :
                 constant Code_Points_Range := (16#3000#,   16#303F#);
   CJK_Unified_Ideographs :
                 constant Code_Points_Range := (16#4E00#,   16#9FFF#);
   CJK_Unified_Ideographs_Extension_A :
                 constant Code_Points_Range := (16#3400#,   16#4DBF#);
   CJK_Unified_Ideographs_Extension_B :
                 constant Code_Points_Range := (16#20000#,  16#2A6DF#);
   Cherokee :    constant Code_Points_Range := (16#13A0#,   16#13FF#);
   Combining_Diacritical_Marks :
                 constant Code_Points_Range := (16#0300#,   16#036F#);
   Combining_Diacritical_Marks_Supplement :
                 constant Code_Points_Range := (16#1DC0#,   16#1DFF#);
   Combining_Diacritical_Marks_for_Symbols :
                 constant Code_Points_Range := (16#20D0#,   16#20FF#);
   Combining_Half_Marks :
                 constant Code_Points_Range := (16#FE20#,   16#FE2F#);
   Control_Pictures :
                 constant Code_Points_Range := (16#2400#,   16#243F#);
   Coptic :      constant Code_Points_Range := (16#2C80#,   16#2CFF#);
   Counting_Rod_Numerals :
                 constant Code_Points_Range := (16#1D360#,  16#1D37F#);
   Cuneiform :   constant Code_Points_Range := (16#12000#,  16#123FF#);
   Cuneiform_Numbers_and_Punctuation :
                 constant Code_Points_Range := (16#12400#,  16#1247F#);
   Currency_Symbols :
                 constant Code_Points_Range := (16#20A0#,   16#20CF#);
   Cypriot_Syllabary :
                 constant Code_Points_Range := (16#10800#,  16#1083F#);
   Cyrillic :    constant Code_Points_Range := (16#0400#,   16#04FF#);
   Cyrillic_Supplement :
                 constant Code_Points_Range := (16#0500#,   16#052F#);
   Deseret :     constant Code_Points_Range := (16#10400#,  16#1044F#);
   Devanagari :  constant Code_Points_Range := (16#0900#,   16#097F#);
   Dingbats :    constant Code_Points_Range := (16#2700#,   16#27BF#);
   Enclosed_Alphanumerics :
                 constant Code_Points_Range := (16#2460#,   16#24FF#);
   Enclosed_CJK_Letters_and_Months :
                 constant Code_Points_Range := (16#3200#,   16#32FF#);
   Ethiopic :    constant Code_Points_Range := (16#1200#,   16#137F#);
   Ethiopic_Extended :
                 constant Code_Points_Range := (16#2D80#,   16#2DDF#);
   Ethiopic_Supplement :
                 constant Code_Points_Range := (16#1380#,   16#139F#);
   General_Punctuation :
                 constant Code_Points_Range := (16#2000#,   16#206F#);
   Geometric_Shapes :
                 constant Code_Points_Range := (16#25A0#,   16#25FF#);
   Georgian :    constant Code_Points_Range := (16#10A0#,   16#10FF#);
   Georgian_Supplement :
                 constant Code_Points_Range := (16#2D00#,   16#2D2F#);
   Glagolitic :  constant Code_Points_Range := (16#2C00#,   16#2C5F#);
   Gothic :      constant Code_Points_Range := (16#10330#,  16#1034F#);
   Greek_Extended :
                 constant Code_Points_Range := (16#1F00#,   16#1FFF#);
   Greek_and_Coptic :
                 constant Code_Points_Range := (16#0370#,   16#03FF#);
   Gujarati :    constant Code_Points_Range := (16#0A80#,   16#0AFF#);
   Gurmukhi :    constant Code_Points_Range := (16#0A00#,   16#0A7F#);
   Halfwidth_and_Fullwidth_Forms :
                 constant Code_Points_Range := (16#FF00#,   16#FFEF#);
   Hangul_Compatibility_Jamo :
                 constant Code_Points_Range := (16#3130#,   16#318F#);
   Hangul_Jamo : constant Code_Points_Range := (16#1100#,   16#11FF#);
   Hangul_Syllables :
                 constant Code_Points_Range := (16#AC00#,   16#D7AF#);
   Hanunoo :     constant Code_Points_Range := (16#1720#,   16#173F#);
   Hebrew :      constant Code_Points_Range := (16#0590#,   16#05FF#);
   High_Private_Use_Surrogates :
                 constant Code_Points_Range := (16#DB80#,   16#DBFF#);
   High_Surrogates :
                 constant Code_Points_Range := (16#D800#,   16#DB7F#);
   Hiragana :    constant Code_Points_Range := (16#3040#,   16#309F#);
   IPA_Extensions :
                 constant Code_Points_Range := (16#0250#,   16#02AF#);
   Ideographic_Description_Characters :
                 constant Code_Points_Range := (16#2FF0#,   16#2FFF#);
   Kanbun :      constant Code_Points_Range := (16#3190#,   16#319F#);
   Kangxi_Radicals :
                 constant Code_Points_Range := (16#2F00#,   16#2FDF#);
   Kannada :     constant Code_Points_Range := (16#0C80#,   16#0CFF#);
   Katakana :    constant Code_Points_Range := (16#30A0#,   16#30FF#);
   Katakana_Phonetic_Extensions :
                 constant Code_Points_Range := (16#31F0#,   16#31FF#);
   Kharoshthi :  constant Code_Points_Range := (16#10A00#,  16#10A5F#);
   Khmer :       constant Code_Points_Range := (16#1780#,   16#17FF#);
   Khmer_Symbols :
                 constant Code_Points_Range := (16#19E0#,   16#19FF#);
   Lao :         constant Code_Points_Range := (16#0E80#,   16#0EFF#);
   Latin_1_Supplement :
                 constant Code_Points_Range := (16#0080#,   16#00FF#);
   Latin_Extended_A :
                 constant Code_Points_Range := (16#0100#,   16#017F#);
   Latin_Extended_Additional :
                 constant Code_Points_Range := (16#1E00#,   16#1EFF#);
   Latin_Extended_B :
                 constant Code_Points_Range := (16#0180#,   16#024F#);
   Latin_Extended_C :
                 constant Code_Points_Range := (16#2C60#,   16#2C7F#);
   Latin_Extended_D :
                 constant Code_Points_Range := (16#A720#,   16#A7FF#);
   Letterlike_Symbols :
                 constant Code_Points_Range := (16#2100#,   16#214F#);
   Limbu :       constant Code_Points_Range := (16#1900#,   16#194F#);
   Linear_B_Ideograms :
                 constant Code_Points_Range := (16#10080#,  16#100FF#);
   Linear_B_Syllabary :
                 constant Code_Points_Range := (16#10000#,  16#1007F#);
   Low_Surrogates :
                 constant Code_Points_Range := (16#DC00#,   16#DFFF#);
   Malayalam :   constant Code_Points_Range := (16#0D00#,   16#0D7F#);
   Mathematical_Alphanumeric_Symbols :
                 constant Code_Points_Range := (16#1D400#,  16#1D7FF#);
   Mathematical_perators :
                 constant Code_Points_Range := (16#2200#,   16#22FF#);
   Miscellaneous_Mathematical_Symbols_A :
                 constant Code_Points_Range := (16#27C0#,   16#27EF#);
   Miscellaneous_Mathematical_Symbols_B :
                 constant Code_Points_Range := (16#2980#,   16#29FF#);
   Miscellaneous_Symbols :
                 constant Code_Points_Range := (16#2600#,   16#26FF#);
   Miscellaneous_Symbols_and_Arrows :
                 constant Code_Points_Range := (16#2B00#,   16#2BFF#);
   Miscellaneous_Technical :
                 constant Code_Points_Range := (16#2300#,   16#23FF#);
   Modifier_Tone_Letters :
                 constant Code_Points_Range := (16#A700#,   16#A71F#);
   Mongolian :   constant Code_Points_Range := (16#1800#,   16#18AF#);
   Musical_Symbols :
                 constant Code_Points_Range := (16#1D100#,  16#1D1FF#);
   Myanmar :     constant Code_Points_Range := (16#1000#,   16#109F#);
   NKo :         constant Code_Points_Range := (16#07C0#,   16#07FF#);
   New_Tai_Lue : constant Code_Points_Range := (16#1980#,   16#19DF#);
   Number_Forms :
                 constant Code_Points_Range := (16#2150#,   16#218F#);
   Ogham :       constant Code_Points_Range := (16#1680#,   16#169F#);
   Old_Italic :  constant Code_Points_Range := (16#10300#,  16#1032F#);
   Old_Persian : constant Code_Points_Range := (16#103A0#,  16#103DF#);
   Optical_Character_Recognition :
                 constant Code_Points_Range := (16#2440#,   16#245F#);
   Oriya :       constant Code_Points_Range := (16#0B00#,   16#0B7F#);
   Osmanya :     constant Code_Points_Range := (16#10480#,  16#104AF#);
   Phags_pa :    constant Code_Points_Range := (16#A840#,   16#A87F#);
   Phoenician :  constant Code_Points_Range := (16#10900#,  16#1091F#);
   Phonetic_Extensions :
                 constant Code_Points_Range := (16#1D00#,   16#1D7F#);
   Phonetic_Extensions_Supplement :
                 constant Code_Points_Range := (16#1D80#,   16#1DBF#);
   Private_Use_Area :
                 constant Code_Points_Range := (16#E000#,   16#F8FF#);
   Runic :       constant Code_Points_Range := (16#16A0#,   16#16FF#);
   Shavian :     constant Code_Points_Range := (16#10450#,  16#1047F#);
   Sinhala :     constant Code_Points_Range := (16#0D80#,   16#0DFF#);
   Small_Form_Variants :
                 constant Code_Points_Range := (16#FE50#,   16#FE6F#);
   Spacing_Modifier_Letters :
                 constant Code_Points_Range := (16#02B0#,   16#02FF#);
   Specials :    constant Code_Points_Range := (16#FFF0#,   16#FFFF#);
   Superscripts_and_Subscripts :
                 constant Code_Points_Range := (16#2070#,   16#209F#);
   Supplemental_Arrows_A :
                 constant Code_Points_Range := (16#27F0#,   16#27FF#);
   Supplemental_Arrows_B :
                 constant Code_Points_Range := (16#2900#,   16#297F#);
   Supplemental_Mathematical_Operators :
                 constant Code_Points_Range := (16#2A00#,   16#2AFF#);
   Supplemental_Punctuation :
                 constant Code_Points_Range := (16#2E00#,   16#2E7F#);
   Supplementary_Private_Use_Area_A :
                 constant Code_Points_Range := (16#F0000#,  16#FFFFF#);
   Supplementary_Private_Use_Area_B :
                 constant Code_Points_Range := (16#100000#, 16#10FFFF#);
   Syloti_Nagri :
                 constant Code_Points_Range := (16#A800#,   16#A82F#);
   Syriac :      constant Code_Points_Range := (16#0700#,   16#074F#);
   Tagalog :     constant Code_Points_Range := (16#1700#,   16#171F#);
   Tagbanwa :    constant Code_Points_Range := (16#1760#,   16#177F#);
   Tags :        constant Code_Points_Range := (16#E0000#,  16#E007F#);
   Tai_Le :      constant Code_Points_Range := (16#1950#,   16#197F#);
   Tai_Xuan_Jing_Symbols :
                 constant Code_Points_Range := (16#1D300#,  16#1D35F#);
   Tamil :       constant Code_Points_Range := (16#0B80#,   16#0BFF#);
   Telugu :      constant Code_Points_Range := (16#0C00#,   16#0C7F#);
   Thaana :      constant Code_Points_Range := (16#0780#,   16#07BF#);
   Thai :        constant Code_Points_Range := (16#0E00#,   16#0E7F#);
   Tibetan :     constant Code_Points_Range := (16#0F00#,   16#0FFF#);
   Tifinagh :    constant Code_Points_Range := (16#2D30#,   16#2D7F#);
   Ugaritic :    constant Code_Points_Range := (16#10380#,  16#1039F#);
   Unified_Canadian_Aboriginal_Syllabics :
                 constant Code_Points_Range := (16#1400#,   16#167F#);
   Variation_Selectors :
                 constant Code_Points_Range := (16#FE00#,   16#FE0F#);
   Variation_Selectors_Supplement :
                 constant Code_Points_Range := (16#E0100#,  16#E01EF#);
   Vertical_Forms :
                 constant Code_Points_Range := (16#FE10#,   16#FE1F#);
   Yi_Radicals : constant Code_Points_Range := (16#A490#,   16#A4CF#);
   Yi_Syllables :
                 constant Code_Points_Range := (16#A000#,   16#A48F#);
   Yijing_Hexagram_Symbols :
                 constant Code_Points_Range := (16#4DC0#,   16#4DFF#);

end Strings_Edit.UTF8.Blocks;

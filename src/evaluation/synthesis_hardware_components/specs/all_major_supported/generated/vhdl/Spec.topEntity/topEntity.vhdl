-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Spec_topEntity_types.all;

entity topEntity is
  port(-- clock
       clk        : in Spec_topEntity_types.clk_TestDomain;
       -- reset
       rst        : in Spec_topEntity_types.rst_TestDomain;
       -- enable
       en         : in Spec_topEntity_types.en_TestDomain;
       inputs_0   : in signed(63 downto 0);
       inputs_1   : in boolean;
       result_0_0 : out signed(63 downto 0);
       result_0_1 : out boolean;
       result_1_0 : out signed(63 downto 0);
       result_1_1 : out boolean;
       result_2_0 : out signed(63 downto 0);
       result_2_1 : out boolean;
       result_3_0 : out boolean;
       result_3_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_4                        : signed(63 downto 0);
  signal x                               : signed(63 downto 0);
  signal result_5                        : signed(63 downto 0) := (to_signed(0,64));
  signal result_6                        : signed(63 downto 0);
  signal x_0                             : signed(63 downto 0);
  signal result_7                        : signed(63 downto 0) := (to_signed(0,64));
  signal result_8                        : Spec_topEntity_types.Tuple2;
  -- spec.hs:(250,1)-(252,31)
  signal hasInput0                       : boolean;
  -- spec.hs:(250,1)-(252,31)
  signal pIn0                            : boolean;
  -- spec.hs:(250,1)-(252,31)
  signal timer0Over                      : boolean;
  -- spec.hs:(250,1)-(252,31)
  signal timer1Over                      : boolean;
  signal result_9                        : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                     : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_10                       : boolean;
  -- spec.hs:(616,1)-(621,29)
  signal \c$t_app_arg\                   : signed(63 downto 0);
  signal result_11                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(616,1)-(621,29)
  signal result_12                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(103,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(616,1)-(621,29)
  signal b                               : boolean;
  -- spec.hs:(616,1)-(621,29)
  signal t                               : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(669,1)-(674,38)
  signal x_1                             : signed(63 downto 0);
  -- spec.hs:(669,1)-(674,38)
  signal win                             : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(669,1)-(674,38)
  signal window                          : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(103,8)
, Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_13                       : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:(669,1)-(674,38)
  signal t_0                             : Spec_topEntity_types.Tuple2_2;
  signal result_14                       : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(669,1)-(674,38)
  signal \c$t_case_alt\                  : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(669,1)-(674,38)
  signal \c$t_case_alt_0\                : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(669,1)-(674,38)
  signal lastBucketUpdated               : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(669,1)-(674,38)
  signal b_0                             : boolean;
  signal \c$app_arg_0\                   : Spec_topEntity_types.array_of_Tags(0 to 4);
  signal result_15                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_1\                   : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_16                       : boolean;
  signal \c$app_arg_2\                   : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_17                       : boolean;
  signal \c$app_arg_3\                   : Spec_topEntity_types.array_of_Tags(0 to 5);
  signal result_18                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_4\                   : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_19                       : boolean;
  signal \c$app_arg_5\                   : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_20                       : boolean;
  -- spec.hs:312:1-63
  signal \c$ws_app_arg\                  : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:312:1-63
  signal \c$ws_app_arg_0\                : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut\                  : Spec_topEntity_types.Tuple2_4;
  signal \c$case_scrut_0\                : Spec_topEntity_types.Maybe;
  signal result_21                       : signed(63 downto 0);
  -- spec.hs:312:1-63
  signal i                               : Spec_topEntity_types.index_2;
  -- spec.hs:312:1-63
  signal ws                              : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_6\                   : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_22                       : boolean;
  -- spec.hs:312:1-63
  signal \c$ws_app_arg_1\                : Spec_topEntity_types.array_of_Tuple2_5(0 to 2);
  -- spec.hs:312:1-63
  signal \c$ws_app_arg_2\                : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$case_scrut_1\                : Spec_topEntity_types.Tuple2_4;
  signal \c$case_scrut_2\                : Spec_topEntity_types.Maybe_0;
  signal result_23                       : signed(63 downto 0);
  -- spec.hs:312:1-63
  signal i_0                             : Spec_topEntity_types.index_3;
  -- spec.hs:312:1-63
  signal ws_0                            : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$app_arg_7\                   : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_24                       : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal tag                             : unsigned(7 downto 0);
  -- spec.hs:(356,1)-(358,33)
  signal \c$out2_case_scrut\             : Spec_topEntity_types.Tuple2_4;
  signal result_25                       : Spec_topEntity_types.array_of_Tuple2_4(0 to 1);
  -- spec.hs:103:34-42
  signal x_2                             : boolean;
  -- spec.hs:(584,1)-(590,36)
  signal result_26                       : Spec_topEntity_types.array_of_Tuple2_4(0 to 1) := Spec_topEntity_types.array_of_Tuple2_4'( ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_4_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_4_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(584,1)-(590,36)
  signal t_1                             : Spec_topEntity_types.array_of_Tuple2_4(0 to 1);
  signal x_3                             : signed(63 downto 0);
  signal y                               : signed(63 downto 0);
  signal x_4                             : signed(63 downto 0);
  signal y_0                             : signed(63 downto 0);
  -- spec.hs:304:1-65
  signal eta3                            : signed(63 downto 0);
  -- spec.hs:304:1-65
  signal \c$ws_app_arg_3\                : Spec_topEntity_types.array_of_Tuple2_5(0 to 2);
  -- spec.hs:304:1-65
  signal \c$ws_app_arg_4\                : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$case_scrut_3\                : Spec_topEntity_types.Tuple2_4;
  signal \c$case_scrut_4\                : Spec_topEntity_types.Maybe_0;
  signal result_27                       : signed(63 downto 0);
  signal result_28                       : unsigned(7 downto 0);
  signal \c$app_arg_8\                   : unsigned(7 downto 0);
  -- spec.hs:304:1-65
  signal i_1                             : Spec_topEntity_types.index_3;
  -- spec.hs:304:1-65
  signal ws_1                            : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal y_1                             : signed(63 downto 0);
  signal x_5                             : signed(63 downto 0);
  -- spec.hs:(356,1)-(358,33)
  signal \c$out1_case_alt\               : signed(63 downto 0);
  -- spec.hs:334:1-46
  signal dta                             : signed(63 downto 0);
  -- spec.hs:334:1-46
  signal tag_0                           : unsigned(7 downto 0);
  signal result_29                       : Spec_topEntity_types.Tuple2_4;
  -- spec.hs:(600,1)-(605,28)
  signal result_30                       : Spec_topEntity_types.Tuple2_4 := ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
, Tuple2_4_sel1_signed => to_signed(0,64) );
  -- spec.hs:(600,1)-(605,28)
  signal b_1                             : boolean;
  -- spec.hs:(600,1)-(605,28)
  signal t_2                             : Spec_topEntity_types.Tuple2_4;
  signal ds1                             : signed(63 downto 0);
  signal x_6                             : signed(63 downto 0);
  signal y_2                             : signed(63 downto 0);
  -- spec.hs:(640,1)-(645,38)
  signal y_3                             : signed(63 downto 0);
  -- spec.hs:(640,1)-(645,38)
  signal x_7                             : signed(63 downto 0);
  -- spec.hs:(640,1)-(645,38)
  signal win_0                           : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(640,1)-(645,38)
  signal window_0                        : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(103,8)
, Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_31                       : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:(640,1)-(645,38)
  signal t_3                             : Spec_topEntity_types.Tuple2_2;
  signal result_32                       : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(640,1)-(645,38)
  signal \c$t_case_alt_1\                : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(640,1)-(645,38)
  signal \c$t_case_alt_2\                : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(640,1)-(645,38)
  signal lastBucketUpdated_0             : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(640,1)-(645,38)
  signal b_2                             : boolean;
  -- spec.hs:103:34-42
  signal x_8                             : boolean;
  -- spec.hs:312:1-63
  signal \c$ws_app_arg_5\                : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:312:1-63
  signal \c$ws_app_arg_6\                : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_5\                : Spec_topEntity_types.Tuple2_4;
  signal \c$case_scrut_6\                : Spec_topEntity_types.Maybe;
  signal result_33                       : signed(63 downto 0);
  -- spec.hs:312:1-63
  signal i_2                             : Spec_topEntity_types.index_2;
  -- spec.hs:312:1-63
  signal ws_2                            : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_9\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_34                       : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal slides                          : Spec_topEntity_types.Slides_1;
  signal \c$app_arg_10\                  : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_35                       : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal \c$out2_case_alt\               : signed(63 downto 0);
  -- spec.hs:(356,1)-(358,33)
  signal dta_0                           : signed(63 downto 0);
  -- spec.hs:312:1-63
  signal \c$ws_app_arg_7\                : Spec_topEntity_types.array_of_Tuple2_5(0 to 2);
  -- spec.hs:312:1-63
  signal \c$ws_app_arg_8\                : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$case_scrut_7\                : Spec_topEntity_types.Tuple2_4;
  signal \c$case_scrut_8\                : Spec_topEntity_types.Maybe_0;
  signal result_36                       : signed(63 downto 0);
  -- spec.hs:312:1-63
  signal i_3                             : Spec_topEntity_types.index_3;
  -- spec.hs:312:1-63
  signal ws_3                            : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal result_37                       : Spec_topEntity_types.array_of_Tuple2_4(0 to 2);
  -- spec.hs:102:34-42
  signal x_9                             : boolean;
  -- spec.hs:(569,1)-(574,36)
  signal result_38                       : Spec_topEntity_types.array_of_Tuple2_4(0 to 2) := Spec_topEntity_types.array_of_Tuple2_4'( ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_4_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_4_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_4_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(569,1)-(574,36)
  signal t_4                             : Spec_topEntity_types.array_of_Tuple2_4(0 to 2);
  signal x_10                            : signed(63 downto 0);
  signal y_4                             : signed(63 downto 0);
  -- spec.hs:304:1-65
  signal \c$ws_app_arg_9\                : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:304:1-65
  signal \c$ws_app_arg_10\               : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_9\                : Spec_topEntity_types.Tuple2_4;
  signal \c$case_scrut_10\               : Spec_topEntity_types.Maybe;
  signal result_39                       : signed(63 downto 0);
  signal result_40                       : unsigned(7 downto 0);
  signal \c$app_arg_11\                  : unsigned(7 downto 0);
  -- spec.hs:304:1-65
  signal i_4                             : Spec_topEntity_types.index_2;
  -- spec.hs:304:1-65
  signal ws_4                            : Spec_topEntity_types.array_of_Maybe(0 to 2);
  -- spec.hs:(356,1)-(358,33)
  signal \c$out0_case_alt\               : signed(63 downto 0);
  -- spec.hs:319:1-55
  signal winData                         : signed(63 downto 0);
  -- spec.hs:319:1-55
  signal winTag                          : unsigned(7 downto 0);
  signal result_41                       : unsigned(7 downto 0);
  signal \c$app_arg_12\                  : unsigned(7 downto 0);
  signal \c$app_arg_13\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_42                       : boolean;
  signal \c$app_arg_14\                  : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_43                       : boolean;
  signal \c$app_arg_15\                  : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_44                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_16\                  : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_45                       : boolean;
  signal result_46                       : Spec_topEntity_types.Tuple2_4;
  -- spec.hs:(558,1)-(562,28)
  signal result_47                       : Spec_topEntity_types.Tuple2_4 := ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
, Tuple2_4_sel1_signed => to_signed(0,64) );
  -- spec.hs:(558,1)-(562,28)
  signal b_3                             : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal \c$input0Win_app_arg\           : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_17\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_48                       : boolean;
  signal \c$app_arg_18\                  : Spec_topEntity_types.array_of_Tags(0 to 6);
  signal result_49                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_19\                  : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_50                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_20\                  : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_51                       : Spec_topEntity_types.Tags;
  -- spec.hs:(537,9)-(539,29)
  signal t_5                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_52                       : unsigned(7 downto 0);
  -- spec.hs:(537,9)-(539,29)
  signal b_4                             : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal f1                              : unsigned(7 downto 0);
  signal result_53                       : unsigned(7 downto 0);
  -- spec.hs:(356,1)-(358,33)
  signal pOut3                           : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal t_6                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_54                       : unsigned(7 downto 0);
  -- spec.hs:(537,9)-(539,29)
  signal b_5                             : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal f1_0                            : unsigned(7 downto 0);
  signal result_55                       : unsigned(7 downto 0);
  -- spec.hs:(356,1)-(358,33)
  signal pOut2                           : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal t_7                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_56                       : unsigned(7 downto 0);
  -- spec.hs:(537,9)-(539,29)
  signal b_6                             : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal f1_1                            : unsigned(7 downto 0);
  signal result_57                       : unsigned(7 downto 0);
  -- spec.hs:103:34-42
  signal x_11                            : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal pOut1                           : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal t_8                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_58                       : unsigned(7 downto 0);
  -- spec.hs:(537,9)-(539,29)
  signal b_7                             : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal f1_2                            : unsigned(7 downto 0);
  signal result_59                       : unsigned(7 downto 0);
  -- spec.hs:102:34-42
  signal x_12                            : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal pOut0                           : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal t_9                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_60                       : unsigned(7 downto 0);
  -- spec.hs:(537,9)-(539,29)
  signal b_8                             : boolean;
  -- spec.hs:(537,9)-(539,29)
  signal f1_3                            : unsigned(7 downto 0);
  signal result_61                       : unsigned(7 downto 0);
  -- spec.hs:(356,1)-(358,33)
  signal pIn0_0                          : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal pacings                         : Spec_topEntity_types.Pacings_1;
  -- spec.hs:(546,1)-(548,22)
  signal toWait                          : signed(63 downto 0) := (to_signed(0,64));
  signal result_62                       : boolean;
  signal result_63                       : signed(63 downto 0);
  -- spec.hs:(546,1)-(548,22)
  signal b_9                             : boolean;
  signal result_64                       : signed(63 downto 0);
  signal x_13                            : signed(63 downto 0);
  signal result_65                       : boolean;
  -- spec.hs:(356,1)-(358,33)
  signal b_10                            : boolean;
  signal result_66                       : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_21\                  : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
, Slides_1_sel1_slide1 => false )
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingOut0 => false
, Pacings_1_sel2_pacingOut1 => false
, Pacings_1_sel3_pacingOut2 => false
, Pacings_1_sel4_pacingOut3 => false ) );
  signal \c$case_alt\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_22\                  : boolean := false;
  signal \c$case_alt_5\                  : boolean;
  signal \c$case_alt_6\                  : boolean;
  signal \c$case_alt_7\                  : boolean;
  signal \c$case_alt_8\                  : boolean;
  signal \c$case_alt_9\                  : boolean;
  signal \c$app_arg_23\                  : boolean := false;
  signal \c$case_alt_10\                 : boolean;
  signal \c$case_alt_11\                 : boolean;
  signal \c$case_alt_12\                 : boolean;
  signal \c$app_arg_24\                  : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal \buffer\                        : Spec_topEntity_types.array_of_Tuple3_0(0 to 3) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) )
                                       , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) )
                                       , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) )
                                       , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) ) );
  -- spec.hs:(167,1)-(169,25)
  signal \c$buffer_case_alt\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 3);
  -- spec.hs:(167,1)-(169,25)
  signal \c$buffer_case_alt_0\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 3);
  -- spec.hs:(167,1)-(169,25)
  signal \c$buffer_case_alt_1\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 3);
  -- spec.hs:(167,1)-(169,25)
  signal qData                           : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:(167,1)-(169,25)
  signal x_14                            : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal cursor                          : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_case_alt\             : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal push                            : boolean;
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_case_alt_0\           : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_case_alt_1\           : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_case_scrut\           : boolean;
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_app_arg\              : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_case_alt_2\           : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal pop                             : boolean;
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_case_alt_3\           : signed(63 downto 0);
  -- spec.hs:(167,1)-(169,25)
  signal \c$cursor_case_alt_4\           : signed(63 downto 0);
  signal inputs                          : Spec_topEntity_types.ValidInt;
  signal \c$vec\                         : Spec_topEntity_types.array_of_signed_64(0 to 100);
  signal x_projection_100                : signed(63 downto 0);
  signal \c$t_case_alt_selection_1\      : boolean;
  signal \c$vec_0\                       : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_0\        : Spec_topEntity_types.Tuple2_6;
  signal \c$vec_1\                       : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_2\        : Spec_topEntity_types.Tuple2_6;
  signal \c$t_case_alt_selection_4\      : boolean;
  signal \c$vec2\                        : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$vec2_0\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal result_selection_8              : boolean;
  signal \c$vec_2\                       : Spec_topEntity_types.array_of_Tuple2_4(0 to 2);
  signal t_projection_1                  : Spec_topEntity_types.Tuple2_7;
  signal x_projection_105                : signed(63 downto 0);
  signal \c$vec2_1\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal result_selection_res            : boolean;
  signal \c$out1_case_alt_selection_res\ : boolean;
  signal \c$vec_3\                       : Spec_topEntity_types.array_of_signed_64(0 to 100);
  signal x_projection_208                : signed(63 downto 0);
  signal \c$t_case_alt_selection_7\      : boolean;
  signal \c$vec_4\                       : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_6\        : Spec_topEntity_types.Tuple2_6;
  signal \c$vec_5\                       : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_8\        : Spec_topEntity_types.Tuple2_6;
  signal \c$t_case_alt_selection_10\     : boolean;
  signal \c$vec2_2\                      : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$out2_case_alt_selection_res\ : boolean;
  signal \c$vec2_3\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal result_selection_18             : boolean;
  signal \c$vec_6\                       : Spec_topEntity_types.array_of_Tuple2_4(0 to 3);
  signal t_projection_5                  : Spec_topEntity_types.Tuple2_8;
  signal \c$vec2_4\                      : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal result_selection_res_2          : boolean;
  signal \c$out0_case_alt_selection_res\ : boolean;
  signal result_selection_res_3          : boolean;
  signal result_selection_25             : boolean;
  signal result_selection_29             : boolean;
  signal result_selection_33             : boolean;
  signal result_selection_37             : boolean;
  signal result_selection_41             : boolean;
  signal \c$vec_7\                       : Spec_topEntity_types.array_of_Tuple3_0(0 to 4);
  signal \c$buffer_case_alt_sel_alt_3\   : Spec_topEntity_types.Tuple2_9;
  signal \c$vec_8\                       : Spec_topEntity_types.array_of_Tuple3_0(0 to 4);
  signal \c$buffer_case_alt_sel_alt_5\   : Spec_topEntity_types.Tuple2_9;
  signal result                          : Spec_topEntity_types.Outputs;
  signal result_0                        : Spec_topEntity_types.ValidInt;
  signal result_1                        : Spec_topEntity_types.ValidInt;
  signal result_2                        : Spec_topEntity_types.ValidInt;
  signal result_3                        : Spec_topEntity_types.ValidBool;

begin
  inputs <= ( ValidInt_sel0_value => inputs_0
            , ValidInt_sel1_valid => inputs_1 );

  result_4 <= to_signed(2000,64) when timer1Over else
              (x + to_signed(2000,64));

  x <= result_5;

  -- register begin
  result_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_5 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_5 <= result_4;
      end if;
    end if;
  end process;
  -- register end

  result_6 <= to_signed(2000,64) when timer0Over else
              (x_0 + to_signed(2000,64));

  x_0 <= result_7;

  -- register begin
  result_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_7 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_7 <= result_6;
      end if;
    end if;
  end process;
  -- register end

  result_8 <= ( Tuple2_sel0_boolean => hasInput0 or (timer0Over or timer1Over)
              , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_ValidInt => inputs
              , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => timer1Over
              , Slides_1_sel1_slide1 => timer0Over )
              , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
              , Pacings_1_sel1_pacingOut0 => pIn0
              , Pacings_1_sel2_pacingOut1 => pIn0
              , Pacings_1_sel3_pacingOut2 => timer1Over
              , Pacings_1_sel4_pacingOut3 => timer0Over ) ) );

  hasInput0 <= inputs.ValidInt_sel1_valid;

  pIn0 <= hasInput0;

  timer0Over <= result_7 >= (to_signed(500000,64));

  timer1Over <= result_5 >= (to_signed(1000000,64));

  result_9 <= ( Tuple2_0_sel0_boolean => result_62 and (not result_65)
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => result_23
              , ValidInt_sel1_valid => result_24 )
              , Outputs_sel1_output1 => ( ValidInt_sel0_value => result_21
              , ValidInt_sel1_valid => result_22 )
              , Outputs_sel2_output2 => ( ValidInt_sel0_value => dta
              , ValidInt_sel1_valid => result_20 )
              , Outputs_sel3_output3 => ( ValidBool_sel0_value_0 => result_12.Tuple2_1_sel1_boolean
              , ValidBool_sel1_valid_0 => result_10 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
    signal iterateI_ho1_5_res  : boolean;
    signal iterateI_ho1_6_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut3;

    r_block_78 : block
        signal \c$bb_res_res\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res\ <= iterateI_ho1_0_arg0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res <= \c$bb_res_res\;


      end block;

    r_block_79 : block
        signal \c$bb_res_res_0\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_0_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_0\ <= iterateI_ho1_0_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res <= \c$bb_res_res_0\;


      end block;

    r_block_80 : block
        signal \c$bb_res_res_1\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_1_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_1\ <= iterateI_ho1_1_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res <= \c$bb_res_res_1\;


      end block;

    r_block_81 : block
        signal \c$bb_res_res_2\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_2_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_2\ <= iterateI_ho1_2_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res <= \c$bb_res_res_2\;


      end block;

    r_block_82 : block
        signal \c$bb_res_res_3\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_3_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_3\ <= iterateI_ho1_3_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res <= \c$bb_res_res_3\;


      end block;

    r_block_83 : block
        signal \c$bb_res_res_4\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= iterateI_ho1_4_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res <= \c$bb_res_res_4\;


      end block;

    r_block_84 : block
        signal \c$bb_res_res_5\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_5_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_5\ <= iterateI_ho1_5_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res <= \c$bb_res_res_5\;


      end block;

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res
                                                         , iterateI_ho1_4_res
                                                         , iterateI_ho1_5_res
                                                         , iterateI_ho1_6_res );


  end block;

  result_10 <=  \c$app_arg\(\c$app_arg\'high) ;

  \c$vec\ <= window.Tuple2_2_sel1_array_of_signed_64;

  fold : block
    signal vec      : Spec_topEntity_types.array_of_signed_64(0 to 99);
    signal acc_7_0  : signed(63 downto 0);
    signal acc_1    : signed(63 downto 0);
    signal acc_2    : signed(63 downto 0);
    signal acc_3    : signed(63 downto 0);
    signal acc_4    : signed(63 downto 0);
    signal acc_5    : signed(63 downto 0);
    signal acc_6    : signed(63 downto 0);
    signal acc_7    : signed(63 downto 0);
    signal acc_8    : signed(63 downto 0);
    signal acc_9    : signed(63 downto 0);
    signal acc_10   : signed(63 downto 0);
    signal acc_11   : signed(63 downto 0);
    signal acc_12   : signed(63 downto 0);
    signal acc_13   : signed(63 downto 0);
    signal acc_14   : signed(63 downto 0);
    signal acc_15   : signed(63 downto 0);
    signal acc_16   : signed(63 downto 0);
    signal acc_17   : signed(63 downto 0);
    signal acc_18   : signed(63 downto 0);
    signal acc_19   : signed(63 downto 0);
    signal acc_20   : signed(63 downto 0);
    signal acc_21   : signed(63 downto 0);
    signal acc_22   : signed(63 downto 0);
    signal acc_23   : signed(63 downto 0);
    signal acc_24   : signed(63 downto 0);
    signal acc_25   : signed(63 downto 0);
    signal acc_26   : signed(63 downto 0);
    signal acc_27   : signed(63 downto 0);
    signal acc_28   : signed(63 downto 0);
    signal acc_29   : signed(63 downto 0);
    signal acc_30   : signed(63 downto 0);
    signal acc_31   : signed(63 downto 0);
    signal acc_32   : signed(63 downto 0);
    signal acc_33   : signed(63 downto 0);
    signal acc_34   : signed(63 downto 0);
    signal acc_35   : signed(63 downto 0);
    signal acc_36   : signed(63 downto 0);
    signal acc_37   : signed(63 downto 0);
    signal acc_38   : signed(63 downto 0);
    signal acc_39   : signed(63 downto 0);
    signal acc_40   : signed(63 downto 0);
    signal acc_41   : signed(63 downto 0);
    signal acc_42   : signed(63 downto 0);
    signal acc_43   : signed(63 downto 0);
    signal acc_44   : signed(63 downto 0);
    signal acc_45   : signed(63 downto 0);
    signal acc_46   : signed(63 downto 0);
    signal acc_47   : signed(63 downto 0);
    signal acc_48   : signed(63 downto 0);
    signal acc_49   : signed(63 downto 0);
    signal acc_50   : signed(63 downto 0);
    signal acc_51   : signed(63 downto 0);
    signal acc_52   : signed(63 downto 0);
    signal acc_53   : signed(63 downto 0);
    signal acc_54   : signed(63 downto 0);
    signal acc_55   : signed(63 downto 0);
    signal acc_56   : signed(63 downto 0);
    signal acc_57   : signed(63 downto 0);
    signal acc_58   : signed(63 downto 0);
    signal acc_59   : signed(63 downto 0);
    signal acc_60   : signed(63 downto 0);
    signal acc_61   : signed(63 downto 0);
    signal acc_62   : signed(63 downto 0);
    signal acc_63   : signed(63 downto 0);
    signal acc_64   : signed(63 downto 0);
    signal acc_65   : signed(63 downto 0);
    signal acc_66   : signed(63 downto 0);
    signal acc_67   : signed(63 downto 0);
    signal acc_68   : signed(63 downto 0);
    signal acc_69   : signed(63 downto 0);
    signal acc_70   : signed(63 downto 0);
    signal acc_71   : signed(63 downto 0);
    signal acc_72   : signed(63 downto 0);
    signal acc_73   : signed(63 downto 0);
    signal acc_74   : signed(63 downto 0);
    signal acc_75   : signed(63 downto 0);
    signal acc_76   : signed(63 downto 0);
    signal acc_77   : signed(63 downto 0);
    signal acc_78   : signed(63 downto 0);
    signal acc_79   : signed(63 downto 0);
    signal acc_80   : signed(63 downto 0);
    signal acc_81   : signed(63 downto 0);
    signal acc_82   : signed(63 downto 0);
    signal acc_83   : signed(63 downto 0);
    signal acc_84   : signed(63 downto 0);
    signal acc_85   : signed(63 downto 0);
    signal acc_86   : signed(63 downto 0);
    signal acc_87   : signed(63 downto 0);
    signal acc_88   : signed(63 downto 0);
    signal acc_89   : signed(63 downto 0);
    signal acc_90   : signed(63 downto 0);
    signal acc_91   : signed(63 downto 0);
    signal acc_92   : signed(63 downto 0);
    signal acc_93   : signed(63 downto 0);
    signal acc_94   : signed(63 downto 0);
    signal acc_95   : signed(63 downto 0);
    signal acc_96   : signed(63 downto 0);
    signal acc_97   : signed(63 downto 0);
    signal acc_98   : signed(63 downto 0);
    signal acc_99   : signed(63 downto 0);
    signal acc_100  : signed(63 downto 0);
    signal acc_1_0  : signed(63 downto 0);
    signal acc_1_1  : signed(63 downto 0);
    signal acc_1_2  : signed(63 downto 0);
    signal acc_1_3  : signed(63 downto 0);
    signal acc_1_4  : signed(63 downto 0);
    signal acc_1_5  : signed(63 downto 0);
    signal acc_1_6  : signed(63 downto 0);
    signal acc_1_7  : signed(63 downto 0);
    signal acc_1_8  : signed(63 downto 0);
    signal acc_1_9  : signed(63 downto 0);
    signal acc_1_10 : signed(63 downto 0);
    signal acc_1_11 : signed(63 downto 0);
    signal acc_1_12 : signed(63 downto 0);
    signal acc_1_13 : signed(63 downto 0);
    signal acc_1_14 : signed(63 downto 0);
    signal acc_1_15 : signed(63 downto 0);
    signal acc_1_16 : signed(63 downto 0);
    signal acc_1_17 : signed(63 downto 0);
    signal acc_1_18 : signed(63 downto 0);
    signal acc_1_19 : signed(63 downto 0);
    signal acc_1_20 : signed(63 downto 0);
    signal acc_1_21 : signed(63 downto 0);
    signal acc_1_22 : signed(63 downto 0);
    signal acc_1_23 : signed(63 downto 0);
    signal acc_1_24 : signed(63 downto 0);
    signal acc_1_25 : signed(63 downto 0);
    signal acc_1_26 : signed(63 downto 0);
    signal acc_1_27 : signed(63 downto 0);
    signal acc_1_28 : signed(63 downto 0);
    signal acc_1_29 : signed(63 downto 0);
    signal acc_1_30 : signed(63 downto 0);
    signal acc_1_31 : signed(63 downto 0);
    signal acc_1_32 : signed(63 downto 0);
    signal acc_1_33 : signed(63 downto 0);
    signal acc_1_34 : signed(63 downto 0);
    signal acc_1_35 : signed(63 downto 0);
    signal acc_1_36 : signed(63 downto 0);
    signal acc_1_37 : signed(63 downto 0);
    signal acc_1_38 : signed(63 downto 0);
    signal acc_1_39 : signed(63 downto 0);
    signal acc_1_40 : signed(63 downto 0);
    signal acc_1_41 : signed(63 downto 0);
    signal acc_1_42 : signed(63 downto 0);
    signal acc_1_43 : signed(63 downto 0);
    signal acc_1_44 : signed(63 downto 0);
    signal acc_1_45 : signed(63 downto 0);
    signal acc_1_46 : signed(63 downto 0);
    signal acc_1_47 : signed(63 downto 0);
    signal acc_1_48 : signed(63 downto 0);
    signal acc_1_49 : signed(63 downto 0);
    signal acc_2_0  : signed(63 downto 0);
    signal acc_2_1  : signed(63 downto 0);
    signal acc_2_2  : signed(63 downto 0);
    signal acc_2_3  : signed(63 downto 0);
    signal acc_2_4  : signed(63 downto 0);
    signal acc_2_5  : signed(63 downto 0);
    signal acc_2_6  : signed(63 downto 0);
    signal acc_2_7  : signed(63 downto 0);
    signal acc_2_8  : signed(63 downto 0);
    signal acc_2_9  : signed(63 downto 0);
    signal acc_2_10 : signed(63 downto 0);
    signal acc_2_11 : signed(63 downto 0);
    signal acc_2_12 : signed(63 downto 0);
    signal acc_2_13 : signed(63 downto 0);
    signal acc_2_14 : signed(63 downto 0);
    signal acc_2_15 : signed(63 downto 0);
    signal acc_2_16 : signed(63 downto 0);
    signal acc_2_17 : signed(63 downto 0);
    signal acc_2_18 : signed(63 downto 0);
    signal acc_2_19 : signed(63 downto 0);
    signal acc_2_20 : signed(63 downto 0);
    signal acc_2_21 : signed(63 downto 0);
    signal acc_2_22 : signed(63 downto 0);
    signal acc_2_23 : signed(63 downto 0);
    signal acc_3_0  : signed(63 downto 0);
    signal acc_3_1  : signed(63 downto 0);
    signal acc_3_2  : signed(63 downto 0);
    signal acc_3_3  : signed(63 downto 0);
    signal acc_3_4  : signed(63 downto 0);
    signal acc_3_5  : signed(63 downto 0);
    signal acc_3_6  : signed(63 downto 0);
    signal acc_3_7  : signed(63 downto 0);
    signal acc_3_8  : signed(63 downto 0);
    signal acc_3_9  : signed(63 downto 0);
    signal acc_3_10 : signed(63 downto 0);
    signal acc_3_11 : signed(63 downto 0);
    signal acc_4_0  : signed(63 downto 0);
    signal acc_4_1  : signed(63 downto 0);
    signal acc_4_2  : signed(63 downto 0);
    signal acc_4_3  : signed(63 downto 0);
    signal acc_4_4  : signed(63 downto 0);
    signal acc_4_5  : signed(63 downto 0);
    signal acc_5_0  : signed(63 downto 0);
    signal acc_5_1  : signed(63 downto 0);
    signal acc_5_2  : signed(63 downto 0);
    signal acc_2_24 : signed(63 downto 0);
    signal acc_6_0  : signed(63 downto 0);
    signal acc_6_1  : signed(63 downto 0);
  begin
    \c$t_app_arg\ <= acc_7_0;

    vec <= (\c$vec\(1 to \c$vec\'high));

    acc_1 <= vec(0);

    acc_2 <= vec(1);

    acc_3 <= vec(2);

    acc_4 <= vec(3);

    acc_5 <= vec(4);

    acc_6 <= vec(5);

    acc_7 <= vec(6);

    acc_8 <= vec(7);

    acc_9 <= vec(8);

    acc_10 <= vec(9);

    acc_11 <= vec(10);

    acc_12 <= vec(11);

    acc_13 <= vec(12);

    acc_14 <= vec(13);

    acc_15 <= vec(14);

    acc_16 <= vec(15);

    acc_17 <= vec(16);

    acc_18 <= vec(17);

    acc_19 <= vec(18);

    acc_20 <= vec(19);

    acc_21 <= vec(20);

    acc_22 <= vec(21);

    acc_23 <= vec(22);

    acc_24 <= vec(23);

    acc_25 <= vec(24);

    acc_26 <= vec(25);

    acc_27 <= vec(26);

    acc_28 <= vec(27);

    acc_29 <= vec(28);

    acc_30 <= vec(29);

    acc_31 <= vec(30);

    acc_32 <= vec(31);

    acc_33 <= vec(32);

    acc_34 <= vec(33);

    acc_35 <= vec(34);

    acc_36 <= vec(35);

    acc_37 <= vec(36);

    acc_38 <= vec(37);

    acc_39 <= vec(38);

    acc_40 <= vec(39);

    acc_41 <= vec(40);

    acc_42 <= vec(41);

    acc_43 <= vec(42);

    acc_44 <= vec(43);

    acc_45 <= vec(44);

    acc_46 <= vec(45);

    acc_47 <= vec(46);

    acc_48 <= vec(47);

    acc_49 <= vec(48);

    acc_50 <= vec(49);

    acc_51 <= vec(50);

    acc_52 <= vec(51);

    acc_53 <= vec(52);

    acc_54 <= vec(53);

    acc_55 <= vec(54);

    acc_56 <= vec(55);

    acc_57 <= vec(56);

    acc_58 <= vec(57);

    acc_59 <= vec(58);

    acc_60 <= vec(59);

    acc_61 <= vec(60);

    acc_62 <= vec(61);

    acc_63 <= vec(62);

    acc_64 <= vec(63);

    acc_65 <= vec(64);

    acc_66 <= vec(65);

    acc_67 <= vec(66);

    acc_68 <= vec(67);

    acc_69 <= vec(68);

    acc_70 <= vec(69);

    acc_71 <= vec(70);

    acc_72 <= vec(71);

    acc_73 <= vec(72);

    acc_74 <= vec(73);

    acc_75 <= vec(74);

    acc_76 <= vec(75);

    acc_77 <= vec(76);

    acc_78 <= vec(77);

    acc_79 <= vec(78);

    acc_80 <= vec(79);

    acc_81 <= vec(80);

    acc_82 <= vec(81);

    acc_83 <= vec(82);

    acc_84 <= vec(83);

    acc_85 <= vec(84);

    acc_86 <= vec(85);

    acc_87 <= vec(86);

    acc_88 <= vec(87);

    acc_89 <= vec(88);

    acc_90 <= vec(89);

    acc_91 <= vec(90);

    acc_92 <= vec(91);

    acc_93 <= vec(92);

    acc_94 <= vec(93);

    acc_95 <= vec(94);

    acc_96 <= vec(95);

    acc_97 <= vec(96);

    acc_98 <= vec(97);

    acc_99 <= vec(98);

    acc_100 <= vec(99);

    fun_203 : block
        signal x_16            : signed(63 downto 0);
        signal y_5             : signed(63 downto 0);
        signal \c$case_alt_13\ : signed(63 downto 0);
      begin
        acc_1_0 <= \c$case_alt_13\;

        x_16 <= acc_1;

        y_5 <= acc_2;

        \c$case_alt_13\ <= (x_16 + y_5);


      end block;

    fun_204 : block
        signal x_17            : signed(63 downto 0);
        signal y_6             : signed(63 downto 0);
        signal \c$case_alt_14\ : signed(63 downto 0);
      begin
        acc_1_1 <= \c$case_alt_14\;

        x_17 <= acc_3;

        y_6 <= acc_4;

        \c$case_alt_14\ <= (x_17 + y_6);


      end block;

    fun_205 : block
        signal x_18            : signed(63 downto 0);
        signal y_7             : signed(63 downto 0);
        signal \c$case_alt_15\ : signed(63 downto 0);
      begin
        acc_1_2 <= \c$case_alt_15\;

        x_18 <= acc_5;

        y_7 <= acc_6;

        \c$case_alt_15\ <= (x_18 + y_7);


      end block;

    fun_206 : block
        signal x_19            : signed(63 downto 0);
        signal y_8             : signed(63 downto 0);
        signal \c$case_alt_16\ : signed(63 downto 0);
      begin
        acc_1_3 <= \c$case_alt_16\;

        x_19 <= acc_7;

        y_8 <= acc_8;

        \c$case_alt_16\ <= (x_19 + y_8);


      end block;

    fun_207 : block
        signal x_20            : signed(63 downto 0);
        signal y_9             : signed(63 downto 0);
        signal \c$case_alt_17\ : signed(63 downto 0);
      begin
        acc_1_4 <= \c$case_alt_17\;

        x_20 <= acc_9;

        y_9 <= acc_10;

        \c$case_alt_17\ <= (x_20 + y_9);


      end block;

    fun_208 : block
        signal x_21            : signed(63 downto 0);
        signal y_10            : signed(63 downto 0);
        signal \c$case_alt_18\ : signed(63 downto 0);
      begin
        acc_1_5 <= \c$case_alt_18\;

        x_21 <= acc_11;

        y_10 <= acc_12;

        \c$case_alt_18\ <= (x_21 + y_10);


      end block;

    fun_209 : block
        signal x_22            : signed(63 downto 0);
        signal y_11            : signed(63 downto 0);
        signal \c$case_alt_19\ : signed(63 downto 0);
      begin
        acc_1_6 <= \c$case_alt_19\;

        x_22 <= acc_13;

        y_11 <= acc_14;

        \c$case_alt_19\ <= (x_22 + y_11);


      end block;

    fun_210 : block
        signal x_23            : signed(63 downto 0);
        signal y_12            : signed(63 downto 0);
        signal \c$case_alt_20\ : signed(63 downto 0);
      begin
        acc_1_7 <= \c$case_alt_20\;

        x_23 <= acc_15;

        y_12 <= acc_16;

        \c$case_alt_20\ <= (x_23 + y_12);


      end block;

    fun_211 : block
        signal x_24            : signed(63 downto 0);
        signal y_13            : signed(63 downto 0);
        signal \c$case_alt_21\ : signed(63 downto 0);
      begin
        acc_1_8 <= \c$case_alt_21\;

        x_24 <= acc_17;

        y_13 <= acc_18;

        \c$case_alt_21\ <= (x_24 + y_13);


      end block;

    fun_212 : block
        signal x_25            : signed(63 downto 0);
        signal y_14            : signed(63 downto 0);
        signal \c$case_alt_22\ : signed(63 downto 0);
      begin
        acc_1_9 <= \c$case_alt_22\;

        x_25 <= acc_19;

        y_14 <= acc_20;

        \c$case_alt_22\ <= (x_25 + y_14);


      end block;

    fun_213 : block
        signal x_26            : signed(63 downto 0);
        signal y_15            : signed(63 downto 0);
        signal \c$case_alt_23\ : signed(63 downto 0);
      begin
        acc_1_10 <= \c$case_alt_23\;

        x_26 <= acc_21;

        y_15 <= acc_22;

        \c$case_alt_23\ <= (x_26 + y_15);


      end block;

    fun_214 : block
        signal x_27            : signed(63 downto 0);
        signal y_16            : signed(63 downto 0);
        signal \c$case_alt_24\ : signed(63 downto 0);
      begin
        acc_1_11 <= \c$case_alt_24\;

        x_27 <= acc_23;

        y_16 <= acc_24;

        \c$case_alt_24\ <= (x_27 + y_16);


      end block;

    fun_215 : block
        signal x_28            : signed(63 downto 0);
        signal y_17            : signed(63 downto 0);
        signal \c$case_alt_25\ : signed(63 downto 0);
      begin
        acc_1_12 <= \c$case_alt_25\;

        x_28 <= acc_25;

        y_17 <= acc_26;

        \c$case_alt_25\ <= (x_28 + y_17);


      end block;

    fun_216 : block
        signal x_29            : signed(63 downto 0);
        signal y_18            : signed(63 downto 0);
        signal \c$case_alt_26\ : signed(63 downto 0);
      begin
        acc_1_13 <= \c$case_alt_26\;

        x_29 <= acc_27;

        y_18 <= acc_28;

        \c$case_alt_26\ <= (x_29 + y_18);


      end block;

    fun_217 : block
        signal x_30            : signed(63 downto 0);
        signal y_19            : signed(63 downto 0);
        signal \c$case_alt_27\ : signed(63 downto 0);
      begin
        acc_1_14 <= \c$case_alt_27\;

        x_30 <= acc_29;

        y_19 <= acc_30;

        \c$case_alt_27\ <= (x_30 + y_19);


      end block;

    fun_218 : block
        signal x_31            : signed(63 downto 0);
        signal y_20            : signed(63 downto 0);
        signal \c$case_alt_28\ : signed(63 downto 0);
      begin
        acc_1_15 <= \c$case_alt_28\;

        x_31 <= acc_31;

        y_20 <= acc_32;

        \c$case_alt_28\ <= (x_31 + y_20);


      end block;

    fun_219 : block
        signal x_32            : signed(63 downto 0);
        signal y_21            : signed(63 downto 0);
        signal \c$case_alt_29\ : signed(63 downto 0);
      begin
        acc_1_16 <= \c$case_alt_29\;

        x_32 <= acc_33;

        y_21 <= acc_34;

        \c$case_alt_29\ <= (x_32 + y_21);


      end block;

    fun_220 : block
        signal x_33            : signed(63 downto 0);
        signal y_22            : signed(63 downto 0);
        signal \c$case_alt_30\ : signed(63 downto 0);
      begin
        acc_1_17 <= \c$case_alt_30\;

        x_33 <= acc_35;

        y_22 <= acc_36;

        \c$case_alt_30\ <= (x_33 + y_22);


      end block;

    fun_221 : block
        signal x_34            : signed(63 downto 0);
        signal y_23            : signed(63 downto 0);
        signal \c$case_alt_31\ : signed(63 downto 0);
      begin
        acc_1_18 <= \c$case_alt_31\;

        x_34 <= acc_37;

        y_23 <= acc_38;

        \c$case_alt_31\ <= (x_34 + y_23);


      end block;

    fun_222 : block
        signal x_35            : signed(63 downto 0);
        signal y_24            : signed(63 downto 0);
        signal \c$case_alt_32\ : signed(63 downto 0);
      begin
        acc_1_19 <= \c$case_alt_32\;

        x_35 <= acc_39;

        y_24 <= acc_40;

        \c$case_alt_32\ <= (x_35 + y_24);


      end block;

    fun_223 : block
        signal x_36            : signed(63 downto 0);
        signal y_25            : signed(63 downto 0);
        signal \c$case_alt_33\ : signed(63 downto 0);
      begin
        acc_1_20 <= \c$case_alt_33\;

        x_36 <= acc_41;

        y_25 <= acc_42;

        \c$case_alt_33\ <= (x_36 + y_25);


      end block;

    fun_224 : block
        signal x_37            : signed(63 downto 0);
        signal y_26            : signed(63 downto 0);
        signal \c$case_alt_34\ : signed(63 downto 0);
      begin
        acc_1_21 <= \c$case_alt_34\;

        x_37 <= acc_43;

        y_26 <= acc_44;

        \c$case_alt_34\ <= (x_37 + y_26);


      end block;

    fun_225 : block
        signal x_38            : signed(63 downto 0);
        signal y_27            : signed(63 downto 0);
        signal \c$case_alt_35\ : signed(63 downto 0);
      begin
        acc_1_22 <= \c$case_alt_35\;

        x_38 <= acc_45;

        y_27 <= acc_46;

        \c$case_alt_35\ <= (x_38 + y_27);


      end block;

    fun_226 : block
        signal x_39            : signed(63 downto 0);
        signal y_28            : signed(63 downto 0);
        signal \c$case_alt_36\ : signed(63 downto 0);
      begin
        acc_1_23 <= \c$case_alt_36\;

        x_39 <= acc_47;

        y_28 <= acc_48;

        \c$case_alt_36\ <= (x_39 + y_28);


      end block;

    fun_227 : block
        signal x_40            : signed(63 downto 0);
        signal y_29            : signed(63 downto 0);
        signal \c$case_alt_37\ : signed(63 downto 0);
      begin
        acc_1_24 <= \c$case_alt_37\;

        x_40 <= acc_49;

        y_29 <= acc_50;

        \c$case_alt_37\ <= (x_40 + y_29);


      end block;

    fun_228 : block
        signal x_41            : signed(63 downto 0);
        signal y_30            : signed(63 downto 0);
        signal \c$case_alt_38\ : signed(63 downto 0);
      begin
        acc_1_25 <= \c$case_alt_38\;

        x_41 <= acc_51;

        y_30 <= acc_52;

        \c$case_alt_38\ <= (x_41 + y_30);


      end block;

    fun_229 : block
        signal x_42            : signed(63 downto 0);
        signal y_31            : signed(63 downto 0);
        signal \c$case_alt_39\ : signed(63 downto 0);
      begin
        acc_1_26 <= \c$case_alt_39\;

        x_42 <= acc_53;

        y_31 <= acc_54;

        \c$case_alt_39\ <= (x_42 + y_31);


      end block;

    fun_230 : block
        signal x_43            : signed(63 downto 0);
        signal y_32            : signed(63 downto 0);
        signal \c$case_alt_40\ : signed(63 downto 0);
      begin
        acc_1_27 <= \c$case_alt_40\;

        x_43 <= acc_55;

        y_32 <= acc_56;

        \c$case_alt_40\ <= (x_43 + y_32);


      end block;

    fun_231 : block
        signal x_44            : signed(63 downto 0);
        signal y_33            : signed(63 downto 0);
        signal \c$case_alt_41\ : signed(63 downto 0);
      begin
        acc_1_28 <= \c$case_alt_41\;

        x_44 <= acc_57;

        y_33 <= acc_58;

        \c$case_alt_41\ <= (x_44 + y_33);


      end block;

    fun_232 : block
        signal x_45            : signed(63 downto 0);
        signal y_34            : signed(63 downto 0);
        signal \c$case_alt_42\ : signed(63 downto 0);
      begin
        acc_1_29 <= \c$case_alt_42\;

        x_45 <= acc_59;

        y_34 <= acc_60;

        \c$case_alt_42\ <= (x_45 + y_34);


      end block;

    fun_233 : block
        signal x_46            : signed(63 downto 0);
        signal y_35            : signed(63 downto 0);
        signal \c$case_alt_43\ : signed(63 downto 0);
      begin
        acc_1_30 <= \c$case_alt_43\;

        x_46 <= acc_61;

        y_35 <= acc_62;

        \c$case_alt_43\ <= (x_46 + y_35);


      end block;

    fun_234 : block
        signal x_47            : signed(63 downto 0);
        signal y_36            : signed(63 downto 0);
        signal \c$case_alt_44\ : signed(63 downto 0);
      begin
        acc_1_31 <= \c$case_alt_44\;

        x_47 <= acc_63;

        y_36 <= acc_64;

        \c$case_alt_44\ <= (x_47 + y_36);


      end block;

    fun_235 : block
        signal x_48            : signed(63 downto 0);
        signal y_37            : signed(63 downto 0);
        signal \c$case_alt_45\ : signed(63 downto 0);
      begin
        acc_1_32 <= \c$case_alt_45\;

        x_48 <= acc_65;

        y_37 <= acc_66;

        \c$case_alt_45\ <= (x_48 + y_37);


      end block;

    fun_236 : block
        signal x_49            : signed(63 downto 0);
        signal y_38            : signed(63 downto 0);
        signal \c$case_alt_46\ : signed(63 downto 0);
      begin
        acc_1_33 <= \c$case_alt_46\;

        x_49 <= acc_67;

        y_38 <= acc_68;

        \c$case_alt_46\ <= (x_49 + y_38);


      end block;

    fun_237 : block
        signal x_50            : signed(63 downto 0);
        signal y_39            : signed(63 downto 0);
        signal \c$case_alt_47\ : signed(63 downto 0);
      begin
        acc_1_34 <= \c$case_alt_47\;

        x_50 <= acc_69;

        y_39 <= acc_70;

        \c$case_alt_47\ <= (x_50 + y_39);


      end block;

    fun_238 : block
        signal x_51            : signed(63 downto 0);
        signal y_40            : signed(63 downto 0);
        signal \c$case_alt_48\ : signed(63 downto 0);
      begin
        acc_1_35 <= \c$case_alt_48\;

        x_51 <= acc_71;

        y_40 <= acc_72;

        \c$case_alt_48\ <= (x_51 + y_40);


      end block;

    fun_239 : block
        signal x_52            : signed(63 downto 0);
        signal y_41            : signed(63 downto 0);
        signal \c$case_alt_49\ : signed(63 downto 0);
      begin
        acc_1_36 <= \c$case_alt_49\;

        x_52 <= acc_73;

        y_41 <= acc_74;

        \c$case_alt_49\ <= (x_52 + y_41);


      end block;

    fun_240 : block
        signal x_53            : signed(63 downto 0);
        signal y_42            : signed(63 downto 0);
        signal \c$case_alt_50\ : signed(63 downto 0);
      begin
        acc_1_37 <= \c$case_alt_50\;

        x_53 <= acc_75;

        y_42 <= acc_76;

        \c$case_alt_50\ <= (x_53 + y_42);


      end block;

    fun_241 : block
        signal x_54            : signed(63 downto 0);
        signal y_43            : signed(63 downto 0);
        signal \c$case_alt_51\ : signed(63 downto 0);
      begin
        acc_1_38 <= \c$case_alt_51\;

        x_54 <= acc_77;

        y_43 <= acc_78;

        \c$case_alt_51\ <= (x_54 + y_43);


      end block;

    fun_242 : block
        signal x_55            : signed(63 downto 0);
        signal y_44            : signed(63 downto 0);
        signal \c$case_alt_52\ : signed(63 downto 0);
      begin
        acc_1_39 <= \c$case_alt_52\;

        x_55 <= acc_79;

        y_44 <= acc_80;

        \c$case_alt_52\ <= (x_55 + y_44);


      end block;

    fun_243 : block
        signal x_56            : signed(63 downto 0);
        signal y_45            : signed(63 downto 0);
        signal \c$case_alt_53\ : signed(63 downto 0);
      begin
        acc_1_40 <= \c$case_alt_53\;

        x_56 <= acc_81;

        y_45 <= acc_82;

        \c$case_alt_53\ <= (x_56 + y_45);


      end block;

    fun_244 : block
        signal x_57            : signed(63 downto 0);
        signal y_46            : signed(63 downto 0);
        signal \c$case_alt_54\ : signed(63 downto 0);
      begin
        acc_1_41 <= \c$case_alt_54\;

        x_57 <= acc_83;

        y_46 <= acc_84;

        \c$case_alt_54\ <= (x_57 + y_46);


      end block;

    fun_245 : block
        signal x_58            : signed(63 downto 0);
        signal y_47            : signed(63 downto 0);
        signal \c$case_alt_55\ : signed(63 downto 0);
      begin
        acc_1_42 <= \c$case_alt_55\;

        x_58 <= acc_85;

        y_47 <= acc_86;

        \c$case_alt_55\ <= (x_58 + y_47);


      end block;

    fun_246 : block
        signal x_59            : signed(63 downto 0);
        signal y_48            : signed(63 downto 0);
        signal \c$case_alt_56\ : signed(63 downto 0);
      begin
        acc_1_43 <= \c$case_alt_56\;

        x_59 <= acc_87;

        y_48 <= acc_88;

        \c$case_alt_56\ <= (x_59 + y_48);


      end block;

    fun_247 : block
        signal x_60            : signed(63 downto 0);
        signal y_49            : signed(63 downto 0);
        signal \c$case_alt_57\ : signed(63 downto 0);
      begin
        acc_1_44 <= \c$case_alt_57\;

        x_60 <= acc_89;

        y_49 <= acc_90;

        \c$case_alt_57\ <= (x_60 + y_49);


      end block;

    fun_248 : block
        signal x_61            : signed(63 downto 0);
        signal y_50            : signed(63 downto 0);
        signal \c$case_alt_58\ : signed(63 downto 0);
      begin
        acc_1_45 <= \c$case_alt_58\;

        x_61 <= acc_91;

        y_50 <= acc_92;

        \c$case_alt_58\ <= (x_61 + y_50);


      end block;

    fun_249 : block
        signal x_62            : signed(63 downto 0);
        signal y_51            : signed(63 downto 0);
        signal \c$case_alt_59\ : signed(63 downto 0);
      begin
        acc_1_46 <= \c$case_alt_59\;

        x_62 <= acc_93;

        y_51 <= acc_94;

        \c$case_alt_59\ <= (x_62 + y_51);


      end block;

    fun_250 : block
        signal x_63            : signed(63 downto 0);
        signal y_52            : signed(63 downto 0);
        signal \c$case_alt_60\ : signed(63 downto 0);
      begin
        acc_1_47 <= \c$case_alt_60\;

        x_63 <= acc_95;

        y_52 <= acc_96;

        \c$case_alt_60\ <= (x_63 + y_52);


      end block;

    fun_251 : block
        signal x_64            : signed(63 downto 0);
        signal y_53            : signed(63 downto 0);
        signal \c$case_alt_61\ : signed(63 downto 0);
      begin
        acc_1_48 <= \c$case_alt_61\;

        x_64 <= acc_97;

        y_53 <= acc_98;

        \c$case_alt_61\ <= (x_64 + y_53);


      end block;

    fun_252 : block
        signal x_65            : signed(63 downto 0);
        signal y_54            : signed(63 downto 0);
        signal \c$case_alt_62\ : signed(63 downto 0);
      begin
        acc_1_49 <= \c$case_alt_62\;

        x_65 <= acc_99;

        y_54 <= acc_100;

        \c$case_alt_62\ <= (x_65 + y_54);


      end block;

    fun_253 : block
        signal x_66            : signed(63 downto 0);
        signal y_55            : signed(63 downto 0);
        signal \c$case_alt_63\ : signed(63 downto 0);
      begin
        acc_2_0 <= \c$case_alt_63\;

        x_66 <= acc_1_0;

        y_55 <= acc_1_1;

        \c$case_alt_63\ <= (x_66 + y_55);


      end block;

    fun_254 : block
        signal x_67            : signed(63 downto 0);
        signal y_56            : signed(63 downto 0);
        signal \c$case_alt_64\ : signed(63 downto 0);
      begin
        acc_2_1 <= \c$case_alt_64\;

        x_67 <= acc_1_2;

        y_56 <= acc_1_3;

        \c$case_alt_64\ <= (x_67 + y_56);


      end block;

    fun_255 : block
        signal x_68            : signed(63 downto 0);
        signal y_57            : signed(63 downto 0);
        signal \c$case_alt_65\ : signed(63 downto 0);
      begin
        acc_2_2 <= \c$case_alt_65\;

        x_68 <= acc_1_4;

        y_57 <= acc_1_5;

        \c$case_alt_65\ <= (x_68 + y_57);


      end block;

    fun_256 : block
        signal x_69            : signed(63 downto 0);
        signal y_58            : signed(63 downto 0);
        signal \c$case_alt_66\ : signed(63 downto 0);
      begin
        acc_2_3 <= \c$case_alt_66\;

        x_69 <= acc_1_6;

        y_58 <= acc_1_7;

        \c$case_alt_66\ <= (x_69 + y_58);


      end block;

    fun_257 : block
        signal x_70            : signed(63 downto 0);
        signal y_59            : signed(63 downto 0);
        signal \c$case_alt_67\ : signed(63 downto 0);
      begin
        acc_2_4 <= \c$case_alt_67\;

        x_70 <= acc_1_8;

        y_59 <= acc_1_9;

        \c$case_alt_67\ <= (x_70 + y_59);


      end block;

    fun_258 : block
        signal x_71            : signed(63 downto 0);
        signal y_60            : signed(63 downto 0);
        signal \c$case_alt_68\ : signed(63 downto 0);
      begin
        acc_2_5 <= \c$case_alt_68\;

        x_71 <= acc_1_10;

        y_60 <= acc_1_11;

        \c$case_alt_68\ <= (x_71 + y_60);


      end block;

    fun_259 : block
        signal x_72            : signed(63 downto 0);
        signal y_61            : signed(63 downto 0);
        signal \c$case_alt_69\ : signed(63 downto 0);
      begin
        acc_2_6 <= \c$case_alt_69\;

        x_72 <= acc_1_12;

        y_61 <= acc_1_13;

        \c$case_alt_69\ <= (x_72 + y_61);


      end block;

    fun_260 : block
        signal x_73            : signed(63 downto 0);
        signal y_62            : signed(63 downto 0);
        signal \c$case_alt_70\ : signed(63 downto 0);
      begin
        acc_2_7 <= \c$case_alt_70\;

        x_73 <= acc_1_14;

        y_62 <= acc_1_15;

        \c$case_alt_70\ <= (x_73 + y_62);


      end block;

    fun_261 : block
        signal x_74            : signed(63 downto 0);
        signal y_63            : signed(63 downto 0);
        signal \c$case_alt_71\ : signed(63 downto 0);
      begin
        acc_2_8 <= \c$case_alt_71\;

        x_74 <= acc_1_16;

        y_63 <= acc_1_17;

        \c$case_alt_71\ <= (x_74 + y_63);


      end block;

    fun_262 : block
        signal x_75            : signed(63 downto 0);
        signal y_64            : signed(63 downto 0);
        signal \c$case_alt_72\ : signed(63 downto 0);
      begin
        acc_2_9 <= \c$case_alt_72\;

        x_75 <= acc_1_18;

        y_64 <= acc_1_19;

        \c$case_alt_72\ <= (x_75 + y_64);


      end block;

    fun_263 : block
        signal x_76            : signed(63 downto 0);
        signal y_65            : signed(63 downto 0);
        signal \c$case_alt_73\ : signed(63 downto 0);
      begin
        acc_2_10 <= \c$case_alt_73\;

        x_76 <= acc_1_20;

        y_65 <= acc_1_21;

        \c$case_alt_73\ <= (x_76 + y_65);


      end block;

    fun_264 : block
        signal x_77            : signed(63 downto 0);
        signal y_66            : signed(63 downto 0);
        signal \c$case_alt_74\ : signed(63 downto 0);
      begin
        acc_2_11 <= \c$case_alt_74\;

        x_77 <= acc_1_22;

        y_66 <= acc_1_23;

        \c$case_alt_74\ <= (x_77 + y_66);


      end block;

    fun_265 : block
        signal x_78            : signed(63 downto 0);
        signal y_67            : signed(63 downto 0);
        signal \c$case_alt_75\ : signed(63 downto 0);
      begin
        acc_2_12 <= \c$case_alt_75\;

        x_78 <= acc_1_24;

        y_67 <= acc_1_25;

        \c$case_alt_75\ <= (x_78 + y_67);


      end block;

    fun_266 : block
        signal x_79            : signed(63 downto 0);
        signal y_68            : signed(63 downto 0);
        signal \c$case_alt_76\ : signed(63 downto 0);
      begin
        acc_2_13 <= \c$case_alt_76\;

        x_79 <= acc_1_26;

        y_68 <= acc_1_27;

        \c$case_alt_76\ <= (x_79 + y_68);


      end block;

    fun_267 : block
        signal x_80            : signed(63 downto 0);
        signal y_69            : signed(63 downto 0);
        signal \c$case_alt_77\ : signed(63 downto 0);
      begin
        acc_2_14 <= \c$case_alt_77\;

        x_80 <= acc_1_28;

        y_69 <= acc_1_29;

        \c$case_alt_77\ <= (x_80 + y_69);


      end block;

    fun_268 : block
        signal x_81            : signed(63 downto 0);
        signal y_70            : signed(63 downto 0);
        signal \c$case_alt_78\ : signed(63 downto 0);
      begin
        acc_2_15 <= \c$case_alt_78\;

        x_81 <= acc_1_30;

        y_70 <= acc_1_31;

        \c$case_alt_78\ <= (x_81 + y_70);


      end block;

    fun_269 : block
        signal x_82            : signed(63 downto 0);
        signal y_71            : signed(63 downto 0);
        signal \c$case_alt_79\ : signed(63 downto 0);
      begin
        acc_2_16 <= \c$case_alt_79\;

        x_82 <= acc_1_32;

        y_71 <= acc_1_33;

        \c$case_alt_79\ <= (x_82 + y_71);


      end block;

    fun_270 : block
        signal x_83            : signed(63 downto 0);
        signal y_72            : signed(63 downto 0);
        signal \c$case_alt_80\ : signed(63 downto 0);
      begin
        acc_2_17 <= \c$case_alt_80\;

        x_83 <= acc_1_34;

        y_72 <= acc_1_35;

        \c$case_alt_80\ <= (x_83 + y_72);


      end block;

    fun_271 : block
        signal x_84            : signed(63 downto 0);
        signal y_73            : signed(63 downto 0);
        signal \c$case_alt_81\ : signed(63 downto 0);
      begin
        acc_2_18 <= \c$case_alt_81\;

        x_84 <= acc_1_36;

        y_73 <= acc_1_37;

        \c$case_alt_81\ <= (x_84 + y_73);


      end block;

    fun_272 : block
        signal x_85            : signed(63 downto 0);
        signal y_74            : signed(63 downto 0);
        signal \c$case_alt_82\ : signed(63 downto 0);
      begin
        acc_2_19 <= \c$case_alt_82\;

        x_85 <= acc_1_38;

        y_74 <= acc_1_39;

        \c$case_alt_82\ <= (x_85 + y_74);


      end block;

    fun_273 : block
        signal x_86            : signed(63 downto 0);
        signal y_75            : signed(63 downto 0);
        signal \c$case_alt_83\ : signed(63 downto 0);
      begin
        acc_2_20 <= \c$case_alt_83\;

        x_86 <= acc_1_40;

        y_75 <= acc_1_41;

        \c$case_alt_83\ <= (x_86 + y_75);


      end block;

    fun_274 : block
        signal x_87            : signed(63 downto 0);
        signal y_76            : signed(63 downto 0);
        signal \c$case_alt_84\ : signed(63 downto 0);
      begin
        acc_2_21 <= \c$case_alt_84\;

        x_87 <= acc_1_42;

        y_76 <= acc_1_43;

        \c$case_alt_84\ <= (x_87 + y_76);


      end block;

    fun_275 : block
        signal x_88            : signed(63 downto 0);
        signal y_77            : signed(63 downto 0);
        signal \c$case_alt_85\ : signed(63 downto 0);
      begin
        acc_2_22 <= \c$case_alt_85\;

        x_88 <= acc_1_44;

        y_77 <= acc_1_45;

        \c$case_alt_85\ <= (x_88 + y_77);


      end block;

    fun_276 : block
        signal x_89            : signed(63 downto 0);
        signal y_78            : signed(63 downto 0);
        signal \c$case_alt_86\ : signed(63 downto 0);
      begin
        acc_2_23 <= \c$case_alt_86\;

        x_89 <= acc_1_46;

        y_78 <= acc_1_47;

        \c$case_alt_86\ <= (x_89 + y_78);


      end block;

    fun_277 : block
        signal x_90            : signed(63 downto 0);
        signal y_79            : signed(63 downto 0);
        signal \c$case_alt_87\ : signed(63 downto 0);
      begin
        acc_2_24 <= \c$case_alt_87\;

        x_90 <= acc_1_48;

        y_79 <= acc_1_49;

        \c$case_alt_87\ <= (x_90 + y_79);


      end block;

    fun_278 : block
        signal x_91            : signed(63 downto 0);
        signal y_80            : signed(63 downto 0);
        signal \c$case_alt_88\ : signed(63 downto 0);
      begin
        acc_3_0 <= \c$case_alt_88\;

        x_91 <= acc_2_0;

        y_80 <= acc_2_1;

        \c$case_alt_88\ <= (x_91 + y_80);


      end block;

    fun_279 : block
        signal x_92            : signed(63 downto 0);
        signal y_81            : signed(63 downto 0);
        signal \c$case_alt_89\ : signed(63 downto 0);
      begin
        acc_3_1 <= \c$case_alt_89\;

        x_92 <= acc_2_2;

        y_81 <= acc_2_3;

        \c$case_alt_89\ <= (x_92 + y_81);


      end block;

    fun_280 : block
        signal x_93            : signed(63 downto 0);
        signal y_82            : signed(63 downto 0);
        signal \c$case_alt_90\ : signed(63 downto 0);
      begin
        acc_3_2 <= \c$case_alt_90\;

        x_93 <= acc_2_4;

        y_82 <= acc_2_5;

        \c$case_alt_90\ <= (x_93 + y_82);


      end block;

    fun_281 : block
        signal x_94            : signed(63 downto 0);
        signal y_83            : signed(63 downto 0);
        signal \c$case_alt_91\ : signed(63 downto 0);
      begin
        acc_3_3 <= \c$case_alt_91\;

        x_94 <= acc_2_6;

        y_83 <= acc_2_7;

        \c$case_alt_91\ <= (x_94 + y_83);


      end block;

    fun_282 : block
        signal x_95            : signed(63 downto 0);
        signal y_84            : signed(63 downto 0);
        signal \c$case_alt_92\ : signed(63 downto 0);
      begin
        acc_3_4 <= \c$case_alt_92\;

        x_95 <= acc_2_8;

        y_84 <= acc_2_9;

        \c$case_alt_92\ <= (x_95 + y_84);


      end block;

    fun_283 : block
        signal x_96            : signed(63 downto 0);
        signal y_85            : signed(63 downto 0);
        signal \c$case_alt_93\ : signed(63 downto 0);
      begin
        acc_3_5 <= \c$case_alt_93\;

        x_96 <= acc_2_10;

        y_85 <= acc_2_11;

        \c$case_alt_93\ <= (x_96 + y_85);


      end block;

    fun_284 : block
        signal x_97            : signed(63 downto 0);
        signal y_86            : signed(63 downto 0);
        signal \c$case_alt_94\ : signed(63 downto 0);
      begin
        acc_3_6 <= \c$case_alt_94\;

        x_97 <= acc_2_12;

        y_86 <= acc_2_13;

        \c$case_alt_94\ <= (x_97 + y_86);


      end block;

    fun_285 : block
        signal x_98            : signed(63 downto 0);
        signal y_87            : signed(63 downto 0);
        signal \c$case_alt_95\ : signed(63 downto 0);
      begin
        acc_3_7 <= \c$case_alt_95\;

        x_98 <= acc_2_14;

        y_87 <= acc_2_15;

        \c$case_alt_95\ <= (x_98 + y_87);


      end block;

    fun_286 : block
        signal x_99            : signed(63 downto 0);
        signal y_88            : signed(63 downto 0);
        signal \c$case_alt_96\ : signed(63 downto 0);
      begin
        acc_3_8 <= \c$case_alt_96\;

        x_99 <= acc_2_16;

        y_88 <= acc_2_17;

        \c$case_alt_96\ <= (x_99 + y_88);


      end block;

    fun_287 : block
        signal x_100           : signed(63 downto 0);
        signal y_89            : signed(63 downto 0);
        signal \c$case_alt_97\ : signed(63 downto 0);
      begin
        acc_3_9 <= \c$case_alt_97\;

        x_100 <= acc_2_18;

        y_89 <= acc_2_19;

        \c$case_alt_97\ <= (x_100 + y_89);


      end block;

    fun_288 : block
        signal x_101           : signed(63 downto 0);
        signal y_90            : signed(63 downto 0);
        signal \c$case_alt_98\ : signed(63 downto 0);
      begin
        acc_3_10 <= \c$case_alt_98\;

        x_101 <= acc_2_20;

        y_90 <= acc_2_21;

        \c$case_alt_98\ <= (x_101 + y_90);


      end block;

    fun_289 : block
        signal x_102           : signed(63 downto 0);
        signal y_91            : signed(63 downto 0);
        signal \c$case_alt_99\ : signed(63 downto 0);
      begin
        acc_3_11 <= \c$case_alt_99\;

        x_102 <= acc_2_22;

        y_91 <= acc_2_23;

        \c$case_alt_99\ <= (x_102 + y_91);


      end block;

    fun_290 : block
        signal x_103            : signed(63 downto 0);
        signal y_92             : signed(63 downto 0);
        signal \c$case_alt_100\ : signed(63 downto 0);
      begin
        acc_4_0 <= \c$case_alt_100\;

        x_103 <= acc_3_0;

        y_92 <= acc_3_1;

        \c$case_alt_100\ <= (x_103 + y_92);


      end block;

    fun_291 : block
        signal x_104            : signed(63 downto 0);
        signal y_93             : signed(63 downto 0);
        signal \c$case_alt_101\ : signed(63 downto 0);
      begin
        acc_4_1 <= \c$case_alt_101\;

        x_104 <= acc_3_2;

        y_93 <= acc_3_3;

        \c$case_alt_101\ <= (x_104 + y_93);


      end block;

    fun_292 : block
        signal x_105            : signed(63 downto 0);
        signal y_94             : signed(63 downto 0);
        signal \c$case_alt_102\ : signed(63 downto 0);
      begin
        acc_4_2 <= \c$case_alt_102\;

        x_105 <= acc_3_4;

        y_94 <= acc_3_5;

        \c$case_alt_102\ <= (x_105 + y_94);


      end block;

    fun_293 : block
        signal x_106            : signed(63 downto 0);
        signal y_95             : signed(63 downto 0);
        signal \c$case_alt_103\ : signed(63 downto 0);
      begin
        acc_4_3 <= \c$case_alt_103\;

        x_106 <= acc_3_6;

        y_95 <= acc_3_7;

        \c$case_alt_103\ <= (x_106 + y_95);


      end block;

    fun_294 : block
        signal x_107            : signed(63 downto 0);
        signal y_96             : signed(63 downto 0);
        signal \c$case_alt_104\ : signed(63 downto 0);
      begin
        acc_4_4 <= \c$case_alt_104\;

        x_107 <= acc_3_8;

        y_96 <= acc_3_9;

        \c$case_alt_104\ <= (x_107 + y_96);


      end block;

    fun_295 : block
        signal x_108            : signed(63 downto 0);
        signal y_97             : signed(63 downto 0);
        signal \c$case_alt_105\ : signed(63 downto 0);
      begin
        acc_4_5 <= \c$case_alt_105\;

        x_108 <= acc_3_10;

        y_97 <= acc_3_11;

        \c$case_alt_105\ <= (x_108 + y_97);


      end block;

    fun_296 : block
        signal x_109            : signed(63 downto 0);
        signal y_98             : signed(63 downto 0);
        signal \c$case_alt_106\ : signed(63 downto 0);
      begin
        acc_5_0 <= \c$case_alt_106\;

        x_109 <= acc_4_0;

        y_98 <= acc_4_1;

        \c$case_alt_106\ <= (x_109 + y_98);


      end block;

    fun_297 : block
        signal x_110            : signed(63 downto 0);
        signal y_99             : signed(63 downto 0);
        signal \c$case_alt_107\ : signed(63 downto 0);
      begin
        acc_5_1 <= \c$case_alt_107\;

        x_110 <= acc_4_2;

        y_99 <= acc_4_3;

        \c$case_alt_107\ <= (x_110 + y_99);


      end block;

    fun_298 : block
        signal x_111            : signed(63 downto 0);
        signal y_100            : signed(63 downto 0);
        signal \c$case_alt_108\ : signed(63 downto 0);
      begin
        acc_5_2 <= \c$case_alt_108\;

        x_111 <= acc_4_4;

        y_100 <= acc_4_5;

        \c$case_alt_108\ <= (x_111 + y_100);


      end block;

    fun_299 : block
        signal x_112            : signed(63 downto 0);
        signal y_101            : signed(63 downto 0);
        signal \c$case_alt_109\ : signed(63 downto 0);
      begin
        acc_6_0 <= \c$case_alt_109\;

        x_112 <= acc_5_0;

        y_101 <= acc_5_1;

        \c$case_alt_109\ <= (x_112 + y_101);


      end block;

    fun_300 : block
        signal x_113            : signed(63 downto 0);
        signal y_102            : signed(63 downto 0);
        signal \c$case_alt_110\ : signed(63 downto 0);
      begin
        acc_6_1 <= \c$case_alt_110\;

        x_113 <= acc_5_2;

        y_102 <= acc_2_24;

        \c$case_alt_110\ <= (x_113 + y_102);


      end block;

    fun_301 : block
        signal x_114            : signed(63 downto 0);
        signal y_103            : signed(63 downto 0);
        signal \c$case_alt_111\ : signed(63 downto 0);
      begin
        acc_7_0 <= \c$case_alt_111\;

        x_114 <= acc_6_0;

        y_103 <= acc_6_1;

        \c$case_alt_111\ <= (x_114 + y_103);


      end block;


  end block;

  result_11 <= t when b else
               result_12;

  -- register begin
  result_12_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_12 <= (Tuple2_1_sel0_unsigned => to_unsigned(103,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_12 <= result_11;
      end if;
    end if;
  end process;
  -- register end

  b <= result_19;

  t <= ( Tuple2_1_sel0_unsigned => result_18.Tags_sel4_output3_0
       , Tuple2_1_sel1_boolean => (\c$t_app_arg\ < (to_signed(10,64))) and (\c$out1_case_alt\ > (to_signed(15,64))) );

  x_projection_100 <=  win(0) ;

  x_1 <= x_projection_100;

  win <= window.Tuple2_2_sel1_array_of_signed_64;

  -- register begin
  window_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window <= ( Tuple2_2_sel0_unsigned => to_unsigned(103,8)
  , Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window <= result_13;
      end if;
    end if;
  end process;
  -- register end

  result_13 <= t_0 when b_0 else
               window;

  t_0 <= ( Tuple2_2_sel0_unsigned => result_15.Tags_sel6_slide1_0
         , Tuple2_2_sel1_array_of_signed_64 => result_14 );

  result_14 <= \c$t_case_alt\ when result_16 else
               \c$t_case_alt_0\;

  \c$t_case_alt_selection_1\ <= result_17;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec_0\(0 to 101-1),\c$vec_0\(101 to \c$vec_0\'high));

  \c$vec_1\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_1\(0 to 101-1),\c$vec_1\(101 to \c$vec_1\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_6_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_1\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_6_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_4\ <= result_17;

  \c$t_case_alt_0\ <= lastBucketUpdated when \c$t_case_alt_selection_4\ else
                      win;

  -- replace begin
  replaceVec : block
    signal vec_index : integer range 0 to 101-1;
  begin
    vec_index <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 101
    -- pragma translate_on
                 ;

    process(vec_index,win,x_1)
      variable ivec : Spec_topEntity_types.array_of_signed_64(0 to 100);
    begin
      ivec := win;
      ivec(vec_index) := ((x_1 + to_signed(1,64)));
      lastBucketUpdated <= ivec;
    end process;
  end block;
  -- replace end

  b_0 <= result_17 or result_16;

  iterateI_0 : block
    signal iterateI_ho1_0_arg0_0 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_0  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_0  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_0  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_0  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_0 <= ( Tags_sel0_input0 => t_9
                             , Tags_sel1_output0_0 => t_8
                             , Tags_sel2_output1_0 => t_7
                             , Tags_sel3_output2_0 => t_6
                             , Tags_sel4_output3_0 => t_5
                             , Tags_sel5_slide0_0 => t_7
                             , Tags_sel6_slide1_0 => t_6 );

    r_block_85 : block
        signal \c$bb_res_res_6\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_6\;


      end block;

    r_block_86 : block
        signal \c$bb_res_res_7\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= iterateI_ho1_0_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_7\;


      end block;

    r_block_87 : block
        signal \c$bb_res_res_8\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_1_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_8\;


      end block;

    r_block_88 : block
        signal \c$bb_res_res_9\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_2_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_9\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_9
                                                        , Tags_sel1_output0_0 => t_8
                                                        , Tags_sel2_output1_0 => t_7
                                                        , Tags_sel3_output2_0 => t_6
                                                        , Tags_sel4_output3_0 => t_5
                                                        , Tags_sel5_slide0_0 => t_7
                                                        , Tags_sel6_slide1_0 => t_6 )
                                                        , iterateI_ho1_0_res_0
                                                        , iterateI_ho1_1_res_0
                                                        , iterateI_ho1_2_res_0
                                                        , iterateI_ho1_3_res_0 );


  end block;

  result_15 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_1  : boolean;
    signal iterateI_ho1_1_res_1  : boolean;
    signal iterateI_ho1_2_res_1  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
    signal iterateI_ho1_4_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= slides.Slides_1_sel1_slide1;

    r_block_89 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_10\;


      end block;

    r_block_90 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_11\;


      end block;

    r_block_91 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_12\;


      end block;

    r_block_92 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_2_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_13\;


      end block;

    r_block_93 : block
        signal \c$bb_res_res_14\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_3_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_0 <= \c$bb_res_res_14\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel1_slide1
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_1
                                                           , iterateI_ho1_3_res_1
                                                           , iterateI_ho1_4_res_0 );


  end block;

  result_16 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_res_2 : boolean;
    signal iterateI_ho1_1_res_2 : boolean;
    signal iterateI_ho1_2_res_2 : boolean;
    signal iterateI_ho1_3_res_2 : boolean;
    signal iterateI_ho1_4_res_1 : boolean;
  begin
    r_block_94 : block
        signal \c$bb_res_res_15\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_15\;


      end block;

    r_block_95 : block
        signal \c$bb_res_res_16\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_16\;


      end block;

    r_block_96 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_2 <= \c$bb_res_res_17\;


      end block;

    r_block_97 : block
        signal \c$bb_res_res_18\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= iterateI_ho1_2_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_18\;


      end block;

    r_block_98 : block
        signal \c$bb_res_res_19\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_3_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_1 <= \c$bb_res_res_19\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_2
                                                           , iterateI_ho1_3_res_2
                                                           , iterateI_ho1_4_res_1 );


  end block;

  result_17 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  iterateI_3 : block
    signal iterateI_ho1_0_arg0_2 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_3  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_3  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_3  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_3  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_2  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_2 <= ( Tags_sel0_input0 => t_9
                             , Tags_sel1_output0_0 => t_8
                             , Tags_sel2_output1_0 => t_7
                             , Tags_sel3_output2_0 => t_6
                             , Tags_sel4_output3_0 => t_5
                             , Tags_sel5_slide0_0 => t_7
                             , Tags_sel6_slide1_0 => t_6 );

    r_block_99 : block
        signal \c$bb_res_res_20\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_20\;


      end block;

    r_block_100 : block
        signal \c$bb_res_res_21\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_21\;


      end block;

    r_block_101 : block
        signal \c$bb_res_res_22\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_22\;


      end block;

    r_block_102 : block
        signal \c$bb_res_res_23\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_3 <= \c$bb_res_res_23\;


      end block;

    r_block_103 : block
        signal \c$bb_res_res_24\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= iterateI_ho1_3_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_2 <= \c$bb_res_res_24\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_9
                                                        , Tags_sel1_output0_0 => t_8
                                                        , Tags_sel2_output1_0 => t_7
                                                        , Tags_sel3_output2_0 => t_6
                                                        , Tags_sel4_output3_0 => t_5
                                                        , Tags_sel5_slide0_0 => t_7
                                                        , Tags_sel6_slide1_0 => t_6 )
                                                        , iterateI_ho1_0_res_3
                                                        , iterateI_ho1_1_res_3
                                                        , iterateI_ho1_2_res_3
                                                        , iterateI_ho1_3_res_3
                                                        , iterateI_ho1_4_res_2 );


  end block;

  result_18 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  iterateI_4 : block
    signal iterateI_ho1_0_res_4 : boolean;
    signal iterateI_ho1_1_res_4 : boolean;
    signal iterateI_ho1_2_res_4 : boolean;
    signal iterateI_ho1_3_res_4 : boolean;
    signal iterateI_ho1_4_res_3 : boolean;
    signal iterateI_ho1_5_res_0 : boolean;
  begin
    r_block_104 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_25\;


      end block;

    r_block_105 : block
        signal \c$bb_res_res_26\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_26\;


      end block;

    r_block_106 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_27\;


      end block;

    r_block_107 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_2_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_4 <= \c$bb_res_res_28\;


      end block;

    r_block_108 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= iterateI_ho1_3_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_3 <= \c$bb_res_res_29\;


      end block;

    r_block_109 : block
        signal \c$bb_res_res_30\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_4_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_0 <= \c$bb_res_res_30\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_4
                                                           , iterateI_ho1_3_res_4
                                                           , iterateI_ho1_4_res_3
                                                           , iterateI_ho1_5_res_0 );


  end block;

  result_19 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_arg0_3 : boolean;
    signal iterateI_ho1_0_res_5  : boolean;
    signal iterateI_ho1_1_res_5  : boolean;
    signal iterateI_ho1_2_res_5  : boolean;
    signal iterateI_ho1_3_res_5  : boolean;
    signal iterateI_ho1_4_res_4  : boolean;
    signal iterateI_ho1_5_res_1  : boolean;
    signal iterateI_ho1_6_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_3 <= pOut2;

    r_block_110 : block
        signal \c$bb_res_res_31\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_31\;


      end block;

    r_block_111 : block
        signal \c$bb_res_res_32\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_32\;


      end block;

    r_block_112 : block
        signal \c$bb_res_res_33\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_33_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_33\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_33\;


      end block;

    r_block_113 : block
        signal \c$bb_res_res_34\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_34_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_34\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_5 <= \c$bb_res_res_34\;


      end block;

    r_block_114 : block
        signal \c$bb_res_res_35\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_35_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_35\ <= iterateI_ho1_3_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_4 <= \c$bb_res_res_35\;


      end block;

    r_block_115 : block
        signal \c$bb_res_res_36\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_36_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_36\ <= iterateI_ho1_4_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_1 <= \c$bb_res_res_36\;


      end block;

    r_block_116 : block
        signal \c$bb_res_res_37\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_37_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_37\ <= iterateI_ho1_5_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_0 <= \c$bb_res_res_37\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_5
                                                           , iterateI_ho1_4_res_4
                                                           , iterateI_ho1_5_res_1
                                                           , iterateI_ho1_6_res_0 );


  end block;

  result_20 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_5 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_5) <= ( Tuple2_3_sel0_Tuple2_4 => result_26(i_5)
               , Tuple2_3_sel1_Maybe => \c$vec2\(i_5) );


  end generate;
  -- zipWith end

  -- imap begin
  imap : block
    function max (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_0 : for i_6 in \c$ws_app_arg_0\'range generate
    begin
      fun_302 : block
      signal \c$case_alt_112\           : Spec_topEntity_types.Maybe;
      -- spec.hs:312:1-63
      signal t_11                       : unsigned(7 downto 0);
      -- spec.hs:312:1-63
      signal x_115                      : Spec_topEntity_types.Tuple2_4;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_6) <= \c$case_alt_112\;

      \c$case_alt_selection_res\ <= t_11 = result_49.Tags_sel2_output1_0;

      \c$case_alt_112\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_6,max(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res\ else
                          \c$ws_app_arg\(i_6).Tuple2_3_sel1_Maybe;

      t_11 <= x_115.Tuple2_4_sel0_unsigned;

      x_115 <= \c$ws_app_arg\(i_6).Tuple2_3_sel0_Tuple2_4;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec : block
    signal vec_index_0 : integer range 0 to 2-1;
  begin
    vec_index_0 <= to_integer((signed(std_logic_vector(resize(i,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut\ <= result_26(vec_index_0);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(1 downto 1)) select
    result_21 <= to_signed(0,64) when "0",
                 \c$case_scrut\.Tuple2_4_sel1_signed when others;

  i <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_0\(0 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_6 : block
    signal iterateI_ho1_0_arg0_4 : boolean;
    signal iterateI_ho1_0_res_6  : boolean;
    signal iterateI_ho1_1_res_6  : boolean;
    signal iterateI_ho1_2_res_6  : boolean;
    signal iterateI_ho1_3_res_6  : boolean;
    signal iterateI_ho1_4_res_5  : boolean;
    signal iterateI_ho1_5_res_2  : boolean;
    signal iterateI_ho1_6_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_4 <= x_11;

    r_block_117 : block
        signal \c$bb_res_res_38\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_38_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_38\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_38\;


      end block;

    r_block_118 : block
        signal \c$bb_res_res_39\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_39_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_39\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_39\;


      end block;

    r_block_119 : block
        signal \c$bb_res_res_40\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_40_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_40\ <= iterateI_ho1_1_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_6 <= \c$bb_res_res_40\;


      end block;

    r_block_120 : block
        signal \c$bb_res_res_41\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_41_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_41\ <= iterateI_ho1_2_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_6 <= \c$bb_res_res_41\;


      end block;

    r_block_121 : block
        signal \c$bb_res_res_42\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_42_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_42\ <= iterateI_ho1_3_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_5 <= \c$bb_res_res_42\;


      end block;

    r_block_122 : block
        signal \c$bb_res_res_43\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_43_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_43\ <= iterateI_ho1_4_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_2 <= \c$bb_res_res_43\;


      end block;

    r_block_123 : block
        signal \c$bb_res_res_44\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_44_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_44\ <= iterateI_ho1_5_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_1 <= \c$bb_res_res_44\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( x_11
                                                           , iterateI_ho1_0_res_6
                                                           , iterateI_ho1_1_res_6
                                                           , iterateI_ho1_2_res_6
                                                           , iterateI_ho1_3_res_6
                                                           , iterateI_ho1_4_res_5
                                                           , iterateI_ho1_5_res_2
                                                           , iterateI_ho1_6_res_1 );


  end block;

  result_22 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_8 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_8) <= ( Tuple2_5_sel0_Tuple2_4 => result_38(i_8)
               , Tuple2_5_sel1_Maybe_0 => \c$vec2_0\(i_8) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_1 : block
    function max_0 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_2 : for i_9 in \c$ws_app_arg_2\'range generate
    begin
      fun_303 : block
      signal \c$case_alt_113\             : Spec_topEntity_types.Maybe_0;
      -- spec.hs:312:1-63
      signal t_12                         : unsigned(7 downto 0);
      -- spec.hs:312:1-63
      signal x_116                        : Spec_topEntity_types.Tuple2_4;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_9) <= \c$case_alt_113\;

      \c$case_alt_selection_res_2\ <= t_12 = result_49.Tags_sel1_output0_0;

      \c$case_alt_113\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_9,max_0(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_2\ else
                          \c$ws_app_arg_1\(i_9).Tuple2_5_sel1_Maybe_0;

      t_12 <= x_116.Tuple2_4_sel0_unsigned;

      x_116 <= \c$ws_app_arg_1\(i_9).Tuple2_5_sel0_Tuple2_4;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_0 : block
    signal vec_index_1 : integer range 0 to 3-1;
  begin
    vec_index_1 <= to_integer((signed(std_logic_vector(resize(i_0,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_1\ <= result_38(vec_index_1);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(2 downto 2)) select
    result_23 <= to_signed(0,64) when "0",
                 \c$case_scrut_1\.Tuple2_4_sel1_signed when others;

  i_0 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_2\(1 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  iterateI_7 : block
    signal iterateI_ho1_0_arg0_5 : boolean;
    signal iterateI_ho1_0_res_7  : boolean;
    signal iterateI_ho1_1_res_7  : boolean;
    signal iterateI_ho1_2_res_7  : boolean;
    signal iterateI_ho1_3_res_7  : boolean;
    signal iterateI_ho1_4_res_6  : boolean;
    signal iterateI_ho1_5_res_3  : boolean;
    signal iterateI_ho1_6_res_2  : boolean;
  begin
    iterateI_ho1_0_arg0_5 <= x_12;

    r_block_124 : block
        signal \c$bb_res_res_45\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_45_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_45\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_45\;


      end block;

    r_block_125 : block
        signal \c$bb_res_res_46\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_46_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_46\ <= iterateI_ho1_0_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_46\;


      end block;

    r_block_126 : block
        signal \c$bb_res_res_47\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_47_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_47\ <= iterateI_ho1_1_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_7 <= \c$bb_res_res_47\;


      end block;

    r_block_127 : block
        signal \c$bb_res_res_48\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_48_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_48\ <= iterateI_ho1_2_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_7 <= \c$bb_res_res_48\;


      end block;

    r_block_128 : block
        signal \c$bb_res_res_49\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_49_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_49\ <= iterateI_ho1_3_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_6 <= \c$bb_res_res_49\;


      end block;

    r_block_129 : block
        signal \c$bb_res_res_50\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_50_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_50\ <= iterateI_ho1_4_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_3 <= \c$bb_res_res_50\;


      end block;

    r_block_130 : block
        signal \c$bb_res_res_51\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_51_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_51\ <= iterateI_ho1_5_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_2 <= \c$bb_res_res_51\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_boolean'( x_12
                                                           , iterateI_ho1_0_res_7
                                                           , iterateI_ho1_1_res_7
                                                           , iterateI_ho1_2_res_7
                                                           , iterateI_ho1_3_res_7
                                                           , iterateI_ho1_4_res_6
                                                           , iterateI_ho1_5_res_3
                                                           , iterateI_ho1_6_res_2 );


  end block;

  result_24 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  tag <= \c$out2_case_scrut\.Tuple2_4_sel0_unsigned;

  \c$out2_case_scrut\ <=  result_26(result_26'high) ;

  result_selection_8 <= x_2;

  result_25 <= t_1 when result_selection_8 else
               result_26;

  x_2 <= result_43;

  -- register begin
  result_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_26 <= Spec_topEntity_types.array_of_Tuple2_4'( ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_4_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_4_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_26 <= result_25;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_2\ <= (Spec_topEntity_types.array_of_Tuple2_4'(Spec_topEntity_types.array_of_Tuple2_4'(result_26) & Spec_topEntity_types.array_of_Tuple2_4'(Spec_topEntity_types.array_of_Tuple2_4'(0 => ( Tuple2_4_sel0_unsigned => result_51.Tags_sel2_output1_0
             , Tuple2_4_sel1_signed => (x_4 + y_0) )))));

  t_projection_1 <= (\c$vec_2\(0 to 1-1),\c$vec_2\(1 to \c$vec_2\'high));

  t_1 <= t_projection_1.Tuple2_7_sel1_array_of_Tuple2_4_1;

  x_3 <= result_36;

  y <= \c$out1_case_alt\;

  x_projection_105 <= (x_3 + y);

  x_4 <= x_projection_105;

  y_0 <= result_27;

  eta3 <= (x_5 + y_1);

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_10 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_10) <= ( Tuple2_5_sel0_Tuple2_4 => result_38(i_10)
               , Tuple2_5_sel1_Maybe_0 => \c$vec2_1\(i_10) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_3 : block
    function max_1 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_4 : for i_11 in \c$ws_app_arg_4\'range generate
    begin
      fun_304 : block
      signal \c$case_alt_114\             : Spec_topEntity_types.Maybe_0;
      -- spec.hs:312:1-63
      signal t_13                         : unsigned(7 downto 0);
      -- spec.hs:312:1-63
      signal x_117                        : Spec_topEntity_types.Tuple2_4;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_11) <= \c$case_alt_114\;

      \c$case_alt_selection_res_3\ <= t_13 = result_28;

      \c$case_alt_114\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_11,max_1(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_3\ else
                          \c$ws_app_arg_3\(i_11).Tuple2_5_sel1_Maybe_0;

      t_13 <= x_117.Tuple2_4_sel0_unsigned;

      x_117 <= \c$ws_app_arg_3\(i_11).Tuple2_5_sel0_Tuple2_4;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_1 : block
    signal vec_index_2 : integer range 0 to 3-1;
  begin
    vec_index_2 <= to_integer((signed(std_logic_vector(resize(i_1,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_3\ <= result_38(vec_index_2);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(2 downto 2)) select
    result_27 <= eta3 when "0",
                 \c$case_scrut_3\.Tuple2_4_sel1_signed when others;

  result_selection_res <= result_51.Tags_sel1_output0_0 > to_unsigned(2,8);

  result_28 <= \c$app_arg_8\ when result_selection_res else
               \c$app_arg_8\ + to_unsigned(102,8);

  \c$app_arg_8\ <= result_51.Tags_sel1_output0_0 - to_unsigned(2,8);

  i_1 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_4\(1 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  y_1 <= winData;

  x_5 <= result_36;

  \c$out1_case_alt_selection_res\ <= tag_0 = to_unsigned(103,8);

  \c$out1_case_alt\ <= to_signed(0,64) when \c$out1_case_alt_selection_res\ else
                       dta;

  dta <= result_30.Tuple2_4_sel1_signed;

  tag_0 <= result_30.Tuple2_4_sel0_unsigned;

  result_29 <= t_2 when b_1 else
               result_30;

  -- register begin
  result_30_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_30 <= ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
  , Tuple2_4_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_30 <= result_29;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_45;

  t_2 <= ( Tuple2_4_sel0_unsigned => result_44.Tags_sel3_output2_0
         , Tuple2_4_sel1_signed => (x_6 + y_2) );

  \c$vec_3\ <= window_0.Tuple2_2_sel1_array_of_signed_64;

  fold_0 : block
    signal vec_0       : Spec_topEntity_types.array_of_signed_64(0 to 99);
    signal acc_7_0_98  : signed(63 downto 0);
    signal acc_0_51    : signed(63 downto 0);
    signal acc_0_52    : signed(63 downto 0);
    signal acc_0_53    : signed(63 downto 0);
    signal acc_0_54    : signed(63 downto 0);
    signal acc_0_55    : signed(63 downto 0);
    signal acc_0_56    : signed(63 downto 0);
    signal acc_0_57    : signed(63 downto 0);
    signal acc_0_58    : signed(63 downto 0);
    signal acc_0_59    : signed(63 downto 0);
    signal acc_0_60    : signed(63 downto 0);
    signal acc_0_61    : signed(63 downto 0);
    signal acc_0_62    : signed(63 downto 0);
    signal acc_0_63    : signed(63 downto 0);
    signal acc_0_64    : signed(63 downto 0);
    signal acc_0_65    : signed(63 downto 0);
    signal acc_0_66    : signed(63 downto 0);
    signal acc_0_67    : signed(63 downto 0);
    signal acc_0_68    : signed(63 downto 0);
    signal acc_0_69    : signed(63 downto 0);
    signal acc_0_70    : signed(63 downto 0);
    signal acc_0_71    : signed(63 downto 0);
    signal acc_0_72    : signed(63 downto 0);
    signal acc_0_73    : signed(63 downto 0);
    signal acc_0_74    : signed(63 downto 0);
    signal acc_0_75    : signed(63 downto 0);
    signal acc_0_76    : signed(63 downto 0);
    signal acc_0_77    : signed(63 downto 0);
    signal acc_0_78    : signed(63 downto 0);
    signal acc_0_79    : signed(63 downto 0);
    signal acc_0_80    : signed(63 downto 0);
    signal acc_0_81    : signed(63 downto 0);
    signal acc_0_82    : signed(63 downto 0);
    signal acc_0_83    : signed(63 downto 0);
    signal acc_0_84    : signed(63 downto 0);
    signal acc_0_85    : signed(63 downto 0);
    signal acc_0_86    : signed(63 downto 0);
    signal acc_0_87    : signed(63 downto 0);
    signal acc_0_88    : signed(63 downto 0);
    signal acc_0_89    : signed(63 downto 0);
    signal acc_0_90    : signed(63 downto 0);
    signal acc_0_91    : signed(63 downto 0);
    signal acc_0_92    : signed(63 downto 0);
    signal acc_0_93    : signed(63 downto 0);
    signal acc_0_94    : signed(63 downto 0);
    signal acc_0_95    : signed(63 downto 0);
    signal acc_0_96    : signed(63 downto 0);
    signal acc_0_97    : signed(63 downto 0);
    signal acc_0_98    : signed(63 downto 0);
    signal acc_0_99    : signed(63 downto 0);
    signal acc_0_100   : signed(63 downto 0);
    signal acc_0_101   : signed(63 downto 0);
    signal acc_0_102   : signed(63 downto 0);
    signal acc_0_103   : signed(63 downto 0);
    signal acc_0_104   : signed(63 downto 0);
    signal acc_0_105   : signed(63 downto 0);
    signal acc_0_106   : signed(63 downto 0);
    signal acc_0_107   : signed(63 downto 0);
    signal acc_0_108   : signed(63 downto 0);
    signal acc_0_109   : signed(63 downto 0);
    signal acc_0_110   : signed(63 downto 0);
    signal acc_0_111   : signed(63 downto 0);
    signal acc_0_112   : signed(63 downto 0);
    signal acc_0_113   : signed(63 downto 0);
    signal acc_0_114   : signed(63 downto 0);
    signal acc_0_115   : signed(63 downto 0);
    signal acc_0_116   : signed(63 downto 0);
    signal acc_0_117   : signed(63 downto 0);
    signal acc_0_118   : signed(63 downto 0);
    signal acc_0_119   : signed(63 downto 0);
    signal acc_0_120   : signed(63 downto 0);
    signal acc_0_121   : signed(63 downto 0);
    signal acc_0_122   : signed(63 downto 0);
    signal acc_0_123   : signed(63 downto 0);
    signal acc_0_124   : signed(63 downto 0);
    signal acc_0_125   : signed(63 downto 0);
    signal acc_0_126   : signed(63 downto 0);
    signal acc_0_127   : signed(63 downto 0);
    signal acc_0_128   : signed(63 downto 0);
    signal acc_0_129   : signed(63 downto 0);
    signal acc_0_130   : signed(63 downto 0);
    signal acc_0_131   : signed(63 downto 0);
    signal acc_0_132   : signed(63 downto 0);
    signal acc_0_133   : signed(63 downto 0);
    signal acc_0_134   : signed(63 downto 0);
    signal acc_0_135   : signed(63 downto 0);
    signal acc_0_136   : signed(63 downto 0);
    signal acc_0_137   : signed(63 downto 0);
    signal acc_0_138   : signed(63 downto 0);
    signal acc_0_139   : signed(63 downto 0);
    signal acc_0_140   : signed(63 downto 0);
    signal acc_0_141   : signed(63 downto 0);
    signal acc_0_142   : signed(63 downto 0);
    signal acc_0_143   : signed(63 downto 0);
    signal acc_0_144   : signed(63 downto 0);
    signal acc_0_145   : signed(63 downto 0);
    signal acc_0_146   : signed(63 downto 0);
    signal acc_0_147   : signed(63 downto 0);
    signal acc_0_148   : signed(63 downto 0);
    signal acc_0_149   : signed(63 downto 0);
    signal acc_0_150   : signed(63 downto 0);
    signal acc_1_0_0   : signed(63 downto 0);
    signal acc_1_1_1   : signed(63 downto 0);
    signal acc_1_2_2   : signed(63 downto 0);
    signal acc_1_3_3   : signed(63 downto 0);
    signal acc_1_4_4   : signed(63 downto 0);
    signal acc_1_5_5   : signed(63 downto 0);
    signal acc_1_6_6   : signed(63 downto 0);
    signal acc_1_7_7   : signed(63 downto 0);
    signal acc_1_8_8   : signed(63 downto 0);
    signal acc_1_9_9   : signed(63 downto 0);
    signal acc_1_10_10 : signed(63 downto 0);
    signal acc_1_11_11 : signed(63 downto 0);
    signal acc_1_12_12 : signed(63 downto 0);
    signal acc_1_13_13 : signed(63 downto 0);
    signal acc_1_14_14 : signed(63 downto 0);
    signal acc_1_15_15 : signed(63 downto 0);
    signal acc_1_16_16 : signed(63 downto 0);
    signal acc_1_17_17 : signed(63 downto 0);
    signal acc_1_18_18 : signed(63 downto 0);
    signal acc_1_19_19 : signed(63 downto 0);
    signal acc_1_20_20 : signed(63 downto 0);
    signal acc_1_21_21 : signed(63 downto 0);
    signal acc_1_22_22 : signed(63 downto 0);
    signal acc_1_23_23 : signed(63 downto 0);
    signal acc_1_24_24 : signed(63 downto 0);
    signal acc_1_25_25 : signed(63 downto 0);
    signal acc_1_26_26 : signed(63 downto 0);
    signal acc_1_27_27 : signed(63 downto 0);
    signal acc_1_28_28 : signed(63 downto 0);
    signal acc_1_29_29 : signed(63 downto 0);
    signal acc_1_30_30 : signed(63 downto 0);
    signal acc_1_31_31 : signed(63 downto 0);
    signal acc_1_32_32 : signed(63 downto 0);
    signal acc_1_33_33 : signed(63 downto 0);
    signal acc_1_34_34 : signed(63 downto 0);
    signal acc_1_35_35 : signed(63 downto 0);
    signal acc_1_36_36 : signed(63 downto 0);
    signal acc_1_37_37 : signed(63 downto 0);
    signal acc_1_38_38 : signed(63 downto 0);
    signal acc_1_39_39 : signed(63 downto 0);
    signal acc_1_40_40 : signed(63 downto 0);
    signal acc_1_41_41 : signed(63 downto 0);
    signal acc_1_42_42 : signed(63 downto 0);
    signal acc_1_43_43 : signed(63 downto 0);
    signal acc_1_44_44 : signed(63 downto 0);
    signal acc_1_45_45 : signed(63 downto 0);
    signal acc_1_46_46 : signed(63 downto 0);
    signal acc_1_47_47 : signed(63 downto 0);
    signal acc_1_48_48 : signed(63 downto 0);
    signal acc_1_49_49 : signed(63 downto 0);
    signal acc_2_0_50  : signed(63 downto 0);
    signal acc_2_1_51  : signed(63 downto 0);
    signal acc_2_2_52  : signed(63 downto 0);
    signal acc_2_3_53  : signed(63 downto 0);
    signal acc_2_4_54  : signed(63 downto 0);
    signal acc_2_5_55  : signed(63 downto 0);
    signal acc_2_6_56  : signed(63 downto 0);
    signal acc_2_7_57  : signed(63 downto 0);
    signal acc_2_8_58  : signed(63 downto 0);
    signal acc_2_9_59  : signed(63 downto 0);
    signal acc_2_10_60 : signed(63 downto 0);
    signal acc_2_11_61 : signed(63 downto 0);
    signal acc_2_12_62 : signed(63 downto 0);
    signal acc_2_13_63 : signed(63 downto 0);
    signal acc_2_14_64 : signed(63 downto 0);
    signal acc_2_15_65 : signed(63 downto 0);
    signal acc_2_16_66 : signed(63 downto 0);
    signal acc_2_17_67 : signed(63 downto 0);
    signal acc_2_18_68 : signed(63 downto 0);
    signal acc_2_19_69 : signed(63 downto 0);
    signal acc_2_20_70 : signed(63 downto 0);
    signal acc_2_21_71 : signed(63 downto 0);
    signal acc_2_22_72 : signed(63 downto 0);
    signal acc_2_23_73 : signed(63 downto 0);
    signal acc_3_0_75  : signed(63 downto 0);
    signal acc_3_1_76  : signed(63 downto 0);
    signal acc_3_2_77  : signed(63 downto 0);
    signal acc_3_3_78  : signed(63 downto 0);
    signal acc_3_4_79  : signed(63 downto 0);
    signal acc_3_5_80  : signed(63 downto 0);
    signal acc_3_6_81  : signed(63 downto 0);
    signal acc_3_7_82  : signed(63 downto 0);
    signal acc_3_8_83  : signed(63 downto 0);
    signal acc_3_9_84  : signed(63 downto 0);
    signal acc_3_10_85 : signed(63 downto 0);
    signal acc_3_11_86 : signed(63 downto 0);
    signal acc_4_0_87  : signed(63 downto 0);
    signal acc_4_1_88  : signed(63 downto 0);
    signal acc_4_2_89  : signed(63 downto 0);
    signal acc_4_3_90  : signed(63 downto 0);
    signal acc_4_4_91  : signed(63 downto 0);
    signal acc_4_5_92  : signed(63 downto 0);
    signal acc_5_0_93  : signed(63 downto 0);
    signal acc_5_1_94  : signed(63 downto 0);
    signal acc_5_2_95  : signed(63 downto 0);
    signal acc_2_24_74 : signed(63 downto 0);
    signal acc_6_0_96  : signed(63 downto 0);
    signal acc_6_1_97  : signed(63 downto 0);
  begin
    ds1 <= acc_7_0_98;

    vec_0 <= (\c$vec_3\(1 to \c$vec_3\'high));

    acc_0_51 <= vec_0(0);

    acc_0_52 <= vec_0(1);

    acc_0_53 <= vec_0(2);

    acc_0_54 <= vec_0(3);

    acc_0_55 <= vec_0(4);

    acc_0_56 <= vec_0(5);

    acc_0_57 <= vec_0(6);

    acc_0_58 <= vec_0(7);

    acc_0_59 <= vec_0(8);

    acc_0_60 <= vec_0(9);

    acc_0_61 <= vec_0(10);

    acc_0_62 <= vec_0(11);

    acc_0_63 <= vec_0(12);

    acc_0_64 <= vec_0(13);

    acc_0_65 <= vec_0(14);

    acc_0_66 <= vec_0(15);

    acc_0_67 <= vec_0(16);

    acc_0_68 <= vec_0(17);

    acc_0_69 <= vec_0(18);

    acc_0_70 <= vec_0(19);

    acc_0_71 <= vec_0(20);

    acc_0_72 <= vec_0(21);

    acc_0_73 <= vec_0(22);

    acc_0_74 <= vec_0(23);

    acc_0_75 <= vec_0(24);

    acc_0_76 <= vec_0(25);

    acc_0_77 <= vec_0(26);

    acc_0_78 <= vec_0(27);

    acc_0_79 <= vec_0(28);

    acc_0_80 <= vec_0(29);

    acc_0_81 <= vec_0(30);

    acc_0_82 <= vec_0(31);

    acc_0_83 <= vec_0(32);

    acc_0_84 <= vec_0(33);

    acc_0_85 <= vec_0(34);

    acc_0_86 <= vec_0(35);

    acc_0_87 <= vec_0(36);

    acc_0_88 <= vec_0(37);

    acc_0_89 <= vec_0(38);

    acc_0_90 <= vec_0(39);

    acc_0_91 <= vec_0(40);

    acc_0_92 <= vec_0(41);

    acc_0_93 <= vec_0(42);

    acc_0_94 <= vec_0(43);

    acc_0_95 <= vec_0(44);

    acc_0_96 <= vec_0(45);

    acc_0_97 <= vec_0(46);

    acc_0_98 <= vec_0(47);

    acc_0_99 <= vec_0(48);

    acc_0_100 <= vec_0(49);

    acc_0_101 <= vec_0(50);

    acc_0_102 <= vec_0(51);

    acc_0_103 <= vec_0(52);

    acc_0_104 <= vec_0(53);

    acc_0_105 <= vec_0(54);

    acc_0_106 <= vec_0(55);

    acc_0_107 <= vec_0(56);

    acc_0_108 <= vec_0(57);

    acc_0_109 <= vec_0(58);

    acc_0_110 <= vec_0(59);

    acc_0_111 <= vec_0(60);

    acc_0_112 <= vec_0(61);

    acc_0_113 <= vec_0(62);

    acc_0_114 <= vec_0(63);

    acc_0_115 <= vec_0(64);

    acc_0_116 <= vec_0(65);

    acc_0_117 <= vec_0(66);

    acc_0_118 <= vec_0(67);

    acc_0_119 <= vec_0(68);

    acc_0_120 <= vec_0(69);

    acc_0_121 <= vec_0(70);

    acc_0_122 <= vec_0(71);

    acc_0_123 <= vec_0(72);

    acc_0_124 <= vec_0(73);

    acc_0_125 <= vec_0(74);

    acc_0_126 <= vec_0(75);

    acc_0_127 <= vec_0(76);

    acc_0_128 <= vec_0(77);

    acc_0_129 <= vec_0(78);

    acc_0_130 <= vec_0(79);

    acc_0_131 <= vec_0(80);

    acc_0_132 <= vec_0(81);

    acc_0_133 <= vec_0(82);

    acc_0_134 <= vec_0(83);

    acc_0_135 <= vec_0(84);

    acc_0_136 <= vec_0(85);

    acc_0_137 <= vec_0(86);

    acc_0_138 <= vec_0(87);

    acc_0_139 <= vec_0(88);

    acc_0_140 <= vec_0(89);

    acc_0_141 <= vec_0(90);

    acc_0_142 <= vec_0(91);

    acc_0_143 <= vec_0(92);

    acc_0_144 <= vec_0(93);

    acc_0_145 <= vec_0(94);

    acc_0_146 <= vec_0(95);

    acc_0_147 <= vec_0(96);

    acc_0_148 <= vec_0(97);

    acc_0_149 <= vec_0(98);

    acc_0_150 <= vec_0(99);

    fun_305 : block
        signal x_118            : signed(63 downto 0);
        signal y_105            : signed(63 downto 0);
        signal \c$case_alt_115\ : signed(63 downto 0);
      begin
        acc_1_0_0 <= \c$case_alt_115\;

        x_118 <= acc_0_51;

        y_105 <= acc_0_52;

        \c$case_alt_115\ <= (x_118 + y_105);


      end block;

    fun_306 : block
        signal x_119            : signed(63 downto 0);
        signal y_106            : signed(63 downto 0);
        signal \c$case_alt_116\ : signed(63 downto 0);
      begin
        acc_1_1_1 <= \c$case_alt_116\;

        x_119 <= acc_0_53;

        y_106 <= acc_0_54;

        \c$case_alt_116\ <= (x_119 + y_106);


      end block;

    fun_307 : block
        signal x_120            : signed(63 downto 0);
        signal y_107            : signed(63 downto 0);
        signal \c$case_alt_117\ : signed(63 downto 0);
      begin
        acc_1_2_2 <= \c$case_alt_117\;

        x_120 <= acc_0_55;

        y_107 <= acc_0_56;

        \c$case_alt_117\ <= (x_120 + y_107);


      end block;

    fun_308 : block
        signal x_121            : signed(63 downto 0);
        signal y_108            : signed(63 downto 0);
        signal \c$case_alt_118\ : signed(63 downto 0);
      begin
        acc_1_3_3 <= \c$case_alt_118\;

        x_121 <= acc_0_57;

        y_108 <= acc_0_58;

        \c$case_alt_118\ <= (x_121 + y_108);


      end block;

    fun_309 : block
        signal x_122            : signed(63 downto 0);
        signal y_109            : signed(63 downto 0);
        signal \c$case_alt_119\ : signed(63 downto 0);
      begin
        acc_1_4_4 <= \c$case_alt_119\;

        x_122 <= acc_0_59;

        y_109 <= acc_0_60;

        \c$case_alt_119\ <= (x_122 + y_109);


      end block;

    fun_310 : block
        signal x_123            : signed(63 downto 0);
        signal y_110            : signed(63 downto 0);
        signal \c$case_alt_120\ : signed(63 downto 0);
      begin
        acc_1_5_5 <= \c$case_alt_120\;

        x_123 <= acc_0_61;

        y_110 <= acc_0_62;

        \c$case_alt_120\ <= (x_123 + y_110);


      end block;

    fun_311 : block
        signal x_124            : signed(63 downto 0);
        signal y_111            : signed(63 downto 0);
        signal \c$case_alt_121\ : signed(63 downto 0);
      begin
        acc_1_6_6 <= \c$case_alt_121\;

        x_124 <= acc_0_63;

        y_111 <= acc_0_64;

        \c$case_alt_121\ <= (x_124 + y_111);


      end block;

    fun_312 : block
        signal x_125            : signed(63 downto 0);
        signal y_112            : signed(63 downto 0);
        signal \c$case_alt_122\ : signed(63 downto 0);
      begin
        acc_1_7_7 <= \c$case_alt_122\;

        x_125 <= acc_0_65;

        y_112 <= acc_0_66;

        \c$case_alt_122\ <= (x_125 + y_112);


      end block;

    fun_313 : block
        signal x_126            : signed(63 downto 0);
        signal y_113            : signed(63 downto 0);
        signal \c$case_alt_123\ : signed(63 downto 0);
      begin
        acc_1_8_8 <= \c$case_alt_123\;

        x_126 <= acc_0_67;

        y_113 <= acc_0_68;

        \c$case_alt_123\ <= (x_126 + y_113);


      end block;

    fun_314 : block
        signal x_127            : signed(63 downto 0);
        signal y_114            : signed(63 downto 0);
        signal \c$case_alt_124\ : signed(63 downto 0);
      begin
        acc_1_9_9 <= \c$case_alt_124\;

        x_127 <= acc_0_69;

        y_114 <= acc_0_70;

        \c$case_alt_124\ <= (x_127 + y_114);


      end block;

    fun_315 : block
        signal x_128            : signed(63 downto 0);
        signal y_115            : signed(63 downto 0);
        signal \c$case_alt_125\ : signed(63 downto 0);
      begin
        acc_1_10_10 <= \c$case_alt_125\;

        x_128 <= acc_0_71;

        y_115 <= acc_0_72;

        \c$case_alt_125\ <= (x_128 + y_115);


      end block;

    fun_316 : block
        signal x_129            : signed(63 downto 0);
        signal y_116            : signed(63 downto 0);
        signal \c$case_alt_126\ : signed(63 downto 0);
      begin
        acc_1_11_11 <= \c$case_alt_126\;

        x_129 <= acc_0_73;

        y_116 <= acc_0_74;

        \c$case_alt_126\ <= (x_129 + y_116);


      end block;

    fun_317 : block
        signal x_130            : signed(63 downto 0);
        signal y_117            : signed(63 downto 0);
        signal \c$case_alt_127\ : signed(63 downto 0);
      begin
        acc_1_12_12 <= \c$case_alt_127\;

        x_130 <= acc_0_75;

        y_117 <= acc_0_76;

        \c$case_alt_127\ <= (x_130 + y_117);


      end block;

    fun_318 : block
        signal x_131            : signed(63 downto 0);
        signal y_118            : signed(63 downto 0);
        signal \c$case_alt_128\ : signed(63 downto 0);
      begin
        acc_1_13_13 <= \c$case_alt_128\;

        x_131 <= acc_0_77;

        y_118 <= acc_0_78;

        \c$case_alt_128\ <= (x_131 + y_118);


      end block;

    fun_319 : block
        signal x_132            : signed(63 downto 0);
        signal y_119            : signed(63 downto 0);
        signal \c$case_alt_129\ : signed(63 downto 0);
      begin
        acc_1_14_14 <= \c$case_alt_129\;

        x_132 <= acc_0_79;

        y_119 <= acc_0_80;

        \c$case_alt_129\ <= (x_132 + y_119);


      end block;

    fun_320 : block
        signal x_133            : signed(63 downto 0);
        signal y_120            : signed(63 downto 0);
        signal \c$case_alt_130\ : signed(63 downto 0);
      begin
        acc_1_15_15 <= \c$case_alt_130\;

        x_133 <= acc_0_81;

        y_120 <= acc_0_82;

        \c$case_alt_130\ <= (x_133 + y_120);


      end block;

    fun_321 : block
        signal x_134            : signed(63 downto 0);
        signal y_121            : signed(63 downto 0);
        signal \c$case_alt_131\ : signed(63 downto 0);
      begin
        acc_1_16_16 <= \c$case_alt_131\;

        x_134 <= acc_0_83;

        y_121 <= acc_0_84;

        \c$case_alt_131\ <= (x_134 + y_121);


      end block;

    fun_322 : block
        signal x_135            : signed(63 downto 0);
        signal y_122            : signed(63 downto 0);
        signal \c$case_alt_132\ : signed(63 downto 0);
      begin
        acc_1_17_17 <= \c$case_alt_132\;

        x_135 <= acc_0_85;

        y_122 <= acc_0_86;

        \c$case_alt_132\ <= (x_135 + y_122);


      end block;

    fun_323 : block
        signal x_136            : signed(63 downto 0);
        signal y_123            : signed(63 downto 0);
        signal \c$case_alt_133\ : signed(63 downto 0);
      begin
        acc_1_18_18 <= \c$case_alt_133\;

        x_136 <= acc_0_87;

        y_123 <= acc_0_88;

        \c$case_alt_133\ <= (x_136 + y_123);


      end block;

    fun_324 : block
        signal x_137            : signed(63 downto 0);
        signal y_124            : signed(63 downto 0);
        signal \c$case_alt_134\ : signed(63 downto 0);
      begin
        acc_1_19_19 <= \c$case_alt_134\;

        x_137 <= acc_0_89;

        y_124 <= acc_0_90;

        \c$case_alt_134\ <= (x_137 + y_124);


      end block;

    fun_325 : block
        signal x_138            : signed(63 downto 0);
        signal y_125            : signed(63 downto 0);
        signal \c$case_alt_135\ : signed(63 downto 0);
      begin
        acc_1_20_20 <= \c$case_alt_135\;

        x_138 <= acc_0_91;

        y_125 <= acc_0_92;

        \c$case_alt_135\ <= (x_138 + y_125);


      end block;

    fun_326 : block
        signal x_139            : signed(63 downto 0);
        signal y_126            : signed(63 downto 0);
        signal \c$case_alt_136\ : signed(63 downto 0);
      begin
        acc_1_21_21 <= \c$case_alt_136\;

        x_139 <= acc_0_93;

        y_126 <= acc_0_94;

        \c$case_alt_136\ <= (x_139 + y_126);


      end block;

    fun_327 : block
        signal x_140            : signed(63 downto 0);
        signal y_127            : signed(63 downto 0);
        signal \c$case_alt_137\ : signed(63 downto 0);
      begin
        acc_1_22_22 <= \c$case_alt_137\;

        x_140 <= acc_0_95;

        y_127 <= acc_0_96;

        \c$case_alt_137\ <= (x_140 + y_127);


      end block;

    fun_328 : block
        signal x_141            : signed(63 downto 0);
        signal y_128            : signed(63 downto 0);
        signal \c$case_alt_138\ : signed(63 downto 0);
      begin
        acc_1_23_23 <= \c$case_alt_138\;

        x_141 <= acc_0_97;

        y_128 <= acc_0_98;

        \c$case_alt_138\ <= (x_141 + y_128);


      end block;

    fun_329 : block
        signal x_142            : signed(63 downto 0);
        signal y_129            : signed(63 downto 0);
        signal \c$case_alt_139\ : signed(63 downto 0);
      begin
        acc_1_24_24 <= \c$case_alt_139\;

        x_142 <= acc_0_99;

        y_129 <= acc_0_100;

        \c$case_alt_139\ <= (x_142 + y_129);


      end block;

    fun_330 : block
        signal x_143            : signed(63 downto 0);
        signal y_130            : signed(63 downto 0);
        signal \c$case_alt_140\ : signed(63 downto 0);
      begin
        acc_1_25_25 <= \c$case_alt_140\;

        x_143 <= acc_0_101;

        y_130 <= acc_0_102;

        \c$case_alt_140\ <= (x_143 + y_130);


      end block;

    fun_331 : block
        signal x_144            : signed(63 downto 0);
        signal y_131            : signed(63 downto 0);
        signal \c$case_alt_141\ : signed(63 downto 0);
      begin
        acc_1_26_26 <= \c$case_alt_141\;

        x_144 <= acc_0_103;

        y_131 <= acc_0_104;

        \c$case_alt_141\ <= (x_144 + y_131);


      end block;

    fun_332 : block
        signal x_145            : signed(63 downto 0);
        signal y_132            : signed(63 downto 0);
        signal \c$case_alt_142\ : signed(63 downto 0);
      begin
        acc_1_27_27 <= \c$case_alt_142\;

        x_145 <= acc_0_105;

        y_132 <= acc_0_106;

        \c$case_alt_142\ <= (x_145 + y_132);


      end block;

    fun_333 : block
        signal x_146            : signed(63 downto 0);
        signal y_133            : signed(63 downto 0);
        signal \c$case_alt_143\ : signed(63 downto 0);
      begin
        acc_1_28_28 <= \c$case_alt_143\;

        x_146 <= acc_0_107;

        y_133 <= acc_0_108;

        \c$case_alt_143\ <= (x_146 + y_133);


      end block;

    fun_334 : block
        signal x_147            : signed(63 downto 0);
        signal y_134            : signed(63 downto 0);
        signal \c$case_alt_144\ : signed(63 downto 0);
      begin
        acc_1_29_29 <= \c$case_alt_144\;

        x_147 <= acc_0_109;

        y_134 <= acc_0_110;

        \c$case_alt_144\ <= (x_147 + y_134);


      end block;

    fun_335 : block
        signal x_148            : signed(63 downto 0);
        signal y_135            : signed(63 downto 0);
        signal \c$case_alt_145\ : signed(63 downto 0);
      begin
        acc_1_30_30 <= \c$case_alt_145\;

        x_148 <= acc_0_111;

        y_135 <= acc_0_112;

        \c$case_alt_145\ <= (x_148 + y_135);


      end block;

    fun_336 : block
        signal x_149            : signed(63 downto 0);
        signal y_136            : signed(63 downto 0);
        signal \c$case_alt_146\ : signed(63 downto 0);
      begin
        acc_1_31_31 <= \c$case_alt_146\;

        x_149 <= acc_0_113;

        y_136 <= acc_0_114;

        \c$case_alt_146\ <= (x_149 + y_136);


      end block;

    fun_337 : block
        signal x_150            : signed(63 downto 0);
        signal y_137            : signed(63 downto 0);
        signal \c$case_alt_147\ : signed(63 downto 0);
      begin
        acc_1_32_32 <= \c$case_alt_147\;

        x_150 <= acc_0_115;

        y_137 <= acc_0_116;

        \c$case_alt_147\ <= (x_150 + y_137);


      end block;

    fun_338 : block
        signal x_151            : signed(63 downto 0);
        signal y_138            : signed(63 downto 0);
        signal \c$case_alt_148\ : signed(63 downto 0);
      begin
        acc_1_33_33 <= \c$case_alt_148\;

        x_151 <= acc_0_117;

        y_138 <= acc_0_118;

        \c$case_alt_148\ <= (x_151 + y_138);


      end block;

    fun_339 : block
        signal x_152            : signed(63 downto 0);
        signal y_139            : signed(63 downto 0);
        signal \c$case_alt_149\ : signed(63 downto 0);
      begin
        acc_1_34_34 <= \c$case_alt_149\;

        x_152 <= acc_0_119;

        y_139 <= acc_0_120;

        \c$case_alt_149\ <= (x_152 + y_139);


      end block;

    fun_340 : block
        signal x_153            : signed(63 downto 0);
        signal y_140            : signed(63 downto 0);
        signal \c$case_alt_150\ : signed(63 downto 0);
      begin
        acc_1_35_35 <= \c$case_alt_150\;

        x_153 <= acc_0_121;

        y_140 <= acc_0_122;

        \c$case_alt_150\ <= (x_153 + y_140);


      end block;

    fun_341 : block
        signal x_154            : signed(63 downto 0);
        signal y_141            : signed(63 downto 0);
        signal \c$case_alt_151\ : signed(63 downto 0);
      begin
        acc_1_36_36 <= \c$case_alt_151\;

        x_154 <= acc_0_123;

        y_141 <= acc_0_124;

        \c$case_alt_151\ <= (x_154 + y_141);


      end block;

    fun_342 : block
        signal x_155            : signed(63 downto 0);
        signal y_142            : signed(63 downto 0);
        signal \c$case_alt_152\ : signed(63 downto 0);
      begin
        acc_1_37_37 <= \c$case_alt_152\;

        x_155 <= acc_0_125;

        y_142 <= acc_0_126;

        \c$case_alt_152\ <= (x_155 + y_142);


      end block;

    fun_343 : block
        signal x_156            : signed(63 downto 0);
        signal y_143            : signed(63 downto 0);
        signal \c$case_alt_153\ : signed(63 downto 0);
      begin
        acc_1_38_38 <= \c$case_alt_153\;

        x_156 <= acc_0_127;

        y_143 <= acc_0_128;

        \c$case_alt_153\ <= (x_156 + y_143);


      end block;

    fun_344 : block
        signal x_157            : signed(63 downto 0);
        signal y_144            : signed(63 downto 0);
        signal \c$case_alt_154\ : signed(63 downto 0);
      begin
        acc_1_39_39 <= \c$case_alt_154\;

        x_157 <= acc_0_129;

        y_144 <= acc_0_130;

        \c$case_alt_154\ <= (x_157 + y_144);


      end block;

    fun_345 : block
        signal x_158            : signed(63 downto 0);
        signal y_145            : signed(63 downto 0);
        signal \c$case_alt_155\ : signed(63 downto 0);
      begin
        acc_1_40_40 <= \c$case_alt_155\;

        x_158 <= acc_0_131;

        y_145 <= acc_0_132;

        \c$case_alt_155\ <= (x_158 + y_145);


      end block;

    fun_346 : block
        signal x_159            : signed(63 downto 0);
        signal y_146            : signed(63 downto 0);
        signal \c$case_alt_156\ : signed(63 downto 0);
      begin
        acc_1_41_41 <= \c$case_alt_156\;

        x_159 <= acc_0_133;

        y_146 <= acc_0_134;

        \c$case_alt_156\ <= (x_159 + y_146);


      end block;

    fun_347 : block
        signal x_160            : signed(63 downto 0);
        signal y_147            : signed(63 downto 0);
        signal \c$case_alt_157\ : signed(63 downto 0);
      begin
        acc_1_42_42 <= \c$case_alt_157\;

        x_160 <= acc_0_135;

        y_147 <= acc_0_136;

        \c$case_alt_157\ <= (x_160 + y_147);


      end block;

    fun_348 : block
        signal x_161            : signed(63 downto 0);
        signal y_148            : signed(63 downto 0);
        signal \c$case_alt_158\ : signed(63 downto 0);
      begin
        acc_1_43_43 <= \c$case_alt_158\;

        x_161 <= acc_0_137;

        y_148 <= acc_0_138;

        \c$case_alt_158\ <= (x_161 + y_148);


      end block;

    fun_349 : block
        signal x_162            : signed(63 downto 0);
        signal y_149            : signed(63 downto 0);
        signal \c$case_alt_159\ : signed(63 downto 0);
      begin
        acc_1_44_44 <= \c$case_alt_159\;

        x_162 <= acc_0_139;

        y_149 <= acc_0_140;

        \c$case_alt_159\ <= (x_162 + y_149);


      end block;

    fun_350 : block
        signal x_163            : signed(63 downto 0);
        signal y_150            : signed(63 downto 0);
        signal \c$case_alt_160\ : signed(63 downto 0);
      begin
        acc_1_45_45 <= \c$case_alt_160\;

        x_163 <= acc_0_141;

        y_150 <= acc_0_142;

        \c$case_alt_160\ <= (x_163 + y_150);


      end block;

    fun_351 : block
        signal x_164            : signed(63 downto 0);
        signal y_151            : signed(63 downto 0);
        signal \c$case_alt_161\ : signed(63 downto 0);
      begin
        acc_1_46_46 <= \c$case_alt_161\;

        x_164 <= acc_0_143;

        y_151 <= acc_0_144;

        \c$case_alt_161\ <= (x_164 + y_151);


      end block;

    fun_352 : block
        signal x_165            : signed(63 downto 0);
        signal y_152            : signed(63 downto 0);
        signal \c$case_alt_162\ : signed(63 downto 0);
      begin
        acc_1_47_47 <= \c$case_alt_162\;

        x_165 <= acc_0_145;

        y_152 <= acc_0_146;

        \c$case_alt_162\ <= (x_165 + y_152);


      end block;

    fun_353 : block
        signal x_166            : signed(63 downto 0);
        signal y_153            : signed(63 downto 0);
        signal \c$case_alt_163\ : signed(63 downto 0);
      begin
        acc_1_48_48 <= \c$case_alt_163\;

        x_166 <= acc_0_147;

        y_153 <= acc_0_148;

        \c$case_alt_163\ <= (x_166 + y_153);


      end block;

    fun_354 : block
        signal x_167            : signed(63 downto 0);
        signal y_154            : signed(63 downto 0);
        signal \c$case_alt_164\ : signed(63 downto 0);
      begin
        acc_1_49_49 <= \c$case_alt_164\;

        x_167 <= acc_0_149;

        y_154 <= acc_0_150;

        \c$case_alt_164\ <= (x_167 + y_154);


      end block;

    fun_355 : block
        signal x_168            : signed(63 downto 0);
        signal y_155            : signed(63 downto 0);
        signal \c$case_alt_165\ : signed(63 downto 0);
      begin
        acc_2_0_50 <= \c$case_alt_165\;

        x_168 <= acc_1_0_0;

        y_155 <= acc_1_1_1;

        \c$case_alt_165\ <= (x_168 + y_155);


      end block;

    fun_356 : block
        signal x_169            : signed(63 downto 0);
        signal y_156            : signed(63 downto 0);
        signal \c$case_alt_166\ : signed(63 downto 0);
      begin
        acc_2_1_51 <= \c$case_alt_166\;

        x_169 <= acc_1_2_2;

        y_156 <= acc_1_3_3;

        \c$case_alt_166\ <= (x_169 + y_156);


      end block;

    fun_357 : block
        signal x_170            : signed(63 downto 0);
        signal y_157            : signed(63 downto 0);
        signal \c$case_alt_167\ : signed(63 downto 0);
      begin
        acc_2_2_52 <= \c$case_alt_167\;

        x_170 <= acc_1_4_4;

        y_157 <= acc_1_5_5;

        \c$case_alt_167\ <= (x_170 + y_157);


      end block;

    fun_358 : block
        signal x_171            : signed(63 downto 0);
        signal y_158            : signed(63 downto 0);
        signal \c$case_alt_168\ : signed(63 downto 0);
      begin
        acc_2_3_53 <= \c$case_alt_168\;

        x_171 <= acc_1_6_6;

        y_158 <= acc_1_7_7;

        \c$case_alt_168\ <= (x_171 + y_158);


      end block;

    fun_359 : block
        signal x_172            : signed(63 downto 0);
        signal y_159            : signed(63 downto 0);
        signal \c$case_alt_169\ : signed(63 downto 0);
      begin
        acc_2_4_54 <= \c$case_alt_169\;

        x_172 <= acc_1_8_8;

        y_159 <= acc_1_9_9;

        \c$case_alt_169\ <= (x_172 + y_159);


      end block;

    fun_360 : block
        signal x_173            : signed(63 downto 0);
        signal y_160            : signed(63 downto 0);
        signal \c$case_alt_170\ : signed(63 downto 0);
      begin
        acc_2_5_55 <= \c$case_alt_170\;

        x_173 <= acc_1_10_10;

        y_160 <= acc_1_11_11;

        \c$case_alt_170\ <= (x_173 + y_160);


      end block;

    fun_361 : block
        signal x_174            : signed(63 downto 0);
        signal y_161            : signed(63 downto 0);
        signal \c$case_alt_171\ : signed(63 downto 0);
      begin
        acc_2_6_56 <= \c$case_alt_171\;

        x_174 <= acc_1_12_12;

        y_161 <= acc_1_13_13;

        \c$case_alt_171\ <= (x_174 + y_161);


      end block;

    fun_362 : block
        signal x_175            : signed(63 downto 0);
        signal y_162            : signed(63 downto 0);
        signal \c$case_alt_172\ : signed(63 downto 0);
      begin
        acc_2_7_57 <= \c$case_alt_172\;

        x_175 <= acc_1_14_14;

        y_162 <= acc_1_15_15;

        \c$case_alt_172\ <= (x_175 + y_162);


      end block;

    fun_363 : block
        signal x_176            : signed(63 downto 0);
        signal y_163            : signed(63 downto 0);
        signal \c$case_alt_173\ : signed(63 downto 0);
      begin
        acc_2_8_58 <= \c$case_alt_173\;

        x_176 <= acc_1_16_16;

        y_163 <= acc_1_17_17;

        \c$case_alt_173\ <= (x_176 + y_163);


      end block;

    fun_364 : block
        signal x_177            : signed(63 downto 0);
        signal y_164            : signed(63 downto 0);
        signal \c$case_alt_174\ : signed(63 downto 0);
      begin
        acc_2_9_59 <= \c$case_alt_174\;

        x_177 <= acc_1_18_18;

        y_164 <= acc_1_19_19;

        \c$case_alt_174\ <= (x_177 + y_164);


      end block;

    fun_365 : block
        signal x_178            : signed(63 downto 0);
        signal y_165            : signed(63 downto 0);
        signal \c$case_alt_175\ : signed(63 downto 0);
      begin
        acc_2_10_60 <= \c$case_alt_175\;

        x_178 <= acc_1_20_20;

        y_165 <= acc_1_21_21;

        \c$case_alt_175\ <= (x_178 + y_165);


      end block;

    fun_366 : block
        signal x_179            : signed(63 downto 0);
        signal y_166            : signed(63 downto 0);
        signal \c$case_alt_176\ : signed(63 downto 0);
      begin
        acc_2_11_61 <= \c$case_alt_176\;

        x_179 <= acc_1_22_22;

        y_166 <= acc_1_23_23;

        \c$case_alt_176\ <= (x_179 + y_166);


      end block;

    fun_367 : block
        signal x_180            : signed(63 downto 0);
        signal y_167            : signed(63 downto 0);
        signal \c$case_alt_177\ : signed(63 downto 0);
      begin
        acc_2_12_62 <= \c$case_alt_177\;

        x_180 <= acc_1_24_24;

        y_167 <= acc_1_25_25;

        \c$case_alt_177\ <= (x_180 + y_167);


      end block;

    fun_368 : block
        signal x_181            : signed(63 downto 0);
        signal y_168            : signed(63 downto 0);
        signal \c$case_alt_178\ : signed(63 downto 0);
      begin
        acc_2_13_63 <= \c$case_alt_178\;

        x_181 <= acc_1_26_26;

        y_168 <= acc_1_27_27;

        \c$case_alt_178\ <= (x_181 + y_168);


      end block;

    fun_369 : block
        signal x_182            : signed(63 downto 0);
        signal y_169            : signed(63 downto 0);
        signal \c$case_alt_179\ : signed(63 downto 0);
      begin
        acc_2_14_64 <= \c$case_alt_179\;

        x_182 <= acc_1_28_28;

        y_169 <= acc_1_29_29;

        \c$case_alt_179\ <= (x_182 + y_169);


      end block;

    fun_370 : block
        signal x_183            : signed(63 downto 0);
        signal y_170            : signed(63 downto 0);
        signal \c$case_alt_180\ : signed(63 downto 0);
      begin
        acc_2_15_65 <= \c$case_alt_180\;

        x_183 <= acc_1_30_30;

        y_170 <= acc_1_31_31;

        \c$case_alt_180\ <= (x_183 + y_170);


      end block;

    fun_371 : block
        signal x_184            : signed(63 downto 0);
        signal y_171            : signed(63 downto 0);
        signal \c$case_alt_181\ : signed(63 downto 0);
      begin
        acc_2_16_66 <= \c$case_alt_181\;

        x_184 <= acc_1_32_32;

        y_171 <= acc_1_33_33;

        \c$case_alt_181\ <= (x_184 + y_171);


      end block;

    fun_372 : block
        signal x_185            : signed(63 downto 0);
        signal y_172            : signed(63 downto 0);
        signal \c$case_alt_182\ : signed(63 downto 0);
      begin
        acc_2_17_67 <= \c$case_alt_182\;

        x_185 <= acc_1_34_34;

        y_172 <= acc_1_35_35;

        \c$case_alt_182\ <= (x_185 + y_172);


      end block;

    fun_373 : block
        signal x_186            : signed(63 downto 0);
        signal y_173            : signed(63 downto 0);
        signal \c$case_alt_183\ : signed(63 downto 0);
      begin
        acc_2_18_68 <= \c$case_alt_183\;

        x_186 <= acc_1_36_36;

        y_173 <= acc_1_37_37;

        \c$case_alt_183\ <= (x_186 + y_173);


      end block;

    fun_374 : block
        signal x_187            : signed(63 downto 0);
        signal y_174            : signed(63 downto 0);
        signal \c$case_alt_184\ : signed(63 downto 0);
      begin
        acc_2_19_69 <= \c$case_alt_184\;

        x_187 <= acc_1_38_38;

        y_174 <= acc_1_39_39;

        \c$case_alt_184\ <= (x_187 + y_174);


      end block;

    fun_375 : block
        signal x_188            : signed(63 downto 0);
        signal y_175            : signed(63 downto 0);
        signal \c$case_alt_185\ : signed(63 downto 0);
      begin
        acc_2_20_70 <= \c$case_alt_185\;

        x_188 <= acc_1_40_40;

        y_175 <= acc_1_41_41;

        \c$case_alt_185\ <= (x_188 + y_175);


      end block;

    fun_376 : block
        signal x_189            : signed(63 downto 0);
        signal y_176            : signed(63 downto 0);
        signal \c$case_alt_186\ : signed(63 downto 0);
      begin
        acc_2_21_71 <= \c$case_alt_186\;

        x_189 <= acc_1_42_42;

        y_176 <= acc_1_43_43;

        \c$case_alt_186\ <= (x_189 + y_176);


      end block;

    fun_377 : block
        signal x_190            : signed(63 downto 0);
        signal y_177            : signed(63 downto 0);
        signal \c$case_alt_187\ : signed(63 downto 0);
      begin
        acc_2_22_72 <= \c$case_alt_187\;

        x_190 <= acc_1_44_44;

        y_177 <= acc_1_45_45;

        \c$case_alt_187\ <= (x_190 + y_177);


      end block;

    fun_378 : block
        signal x_191            : signed(63 downto 0);
        signal y_178            : signed(63 downto 0);
        signal \c$case_alt_188\ : signed(63 downto 0);
      begin
        acc_2_23_73 <= \c$case_alt_188\;

        x_191 <= acc_1_46_46;

        y_178 <= acc_1_47_47;

        \c$case_alt_188\ <= (x_191 + y_178);


      end block;

    fun_379 : block
        signal x_192            : signed(63 downto 0);
        signal y_179            : signed(63 downto 0);
        signal \c$case_alt_189\ : signed(63 downto 0);
      begin
        acc_2_24_74 <= \c$case_alt_189\;

        x_192 <= acc_1_48_48;

        y_179 <= acc_1_49_49;

        \c$case_alt_189\ <= (x_192 + y_179);


      end block;

    fun_380 : block
        signal x_193            : signed(63 downto 0);
        signal y_180            : signed(63 downto 0);
        signal \c$case_alt_190\ : signed(63 downto 0);
      begin
        acc_3_0_75 <= \c$case_alt_190\;

        x_193 <= acc_2_0_50;

        y_180 <= acc_2_1_51;

        \c$case_alt_190\ <= (x_193 + y_180);


      end block;

    fun_381 : block
        signal x_194            : signed(63 downto 0);
        signal y_181            : signed(63 downto 0);
        signal \c$case_alt_191\ : signed(63 downto 0);
      begin
        acc_3_1_76 <= \c$case_alt_191\;

        x_194 <= acc_2_2_52;

        y_181 <= acc_2_3_53;

        \c$case_alt_191\ <= (x_194 + y_181);


      end block;

    fun_382 : block
        signal x_195            : signed(63 downto 0);
        signal y_182            : signed(63 downto 0);
        signal \c$case_alt_192\ : signed(63 downto 0);
      begin
        acc_3_2_77 <= \c$case_alt_192\;

        x_195 <= acc_2_4_54;

        y_182 <= acc_2_5_55;

        \c$case_alt_192\ <= (x_195 + y_182);


      end block;

    fun_383 : block
        signal x_196            : signed(63 downto 0);
        signal y_183            : signed(63 downto 0);
        signal \c$case_alt_193\ : signed(63 downto 0);
      begin
        acc_3_3_78 <= \c$case_alt_193\;

        x_196 <= acc_2_6_56;

        y_183 <= acc_2_7_57;

        \c$case_alt_193\ <= (x_196 + y_183);


      end block;

    fun_384 : block
        signal x_197            : signed(63 downto 0);
        signal y_184            : signed(63 downto 0);
        signal \c$case_alt_194\ : signed(63 downto 0);
      begin
        acc_3_4_79 <= \c$case_alt_194\;

        x_197 <= acc_2_8_58;

        y_184 <= acc_2_9_59;

        \c$case_alt_194\ <= (x_197 + y_184);


      end block;

    fun_385 : block
        signal x_198            : signed(63 downto 0);
        signal y_185            : signed(63 downto 0);
        signal \c$case_alt_195\ : signed(63 downto 0);
      begin
        acc_3_5_80 <= \c$case_alt_195\;

        x_198 <= acc_2_10_60;

        y_185 <= acc_2_11_61;

        \c$case_alt_195\ <= (x_198 + y_185);


      end block;

    fun_386 : block
        signal x_199            : signed(63 downto 0);
        signal y_186            : signed(63 downto 0);
        signal \c$case_alt_196\ : signed(63 downto 0);
      begin
        acc_3_6_81 <= \c$case_alt_196\;

        x_199 <= acc_2_12_62;

        y_186 <= acc_2_13_63;

        \c$case_alt_196\ <= (x_199 + y_186);


      end block;

    fun_387 : block
        signal x_200            : signed(63 downto 0);
        signal y_187            : signed(63 downto 0);
        signal \c$case_alt_197\ : signed(63 downto 0);
      begin
        acc_3_7_82 <= \c$case_alt_197\;

        x_200 <= acc_2_14_64;

        y_187 <= acc_2_15_65;

        \c$case_alt_197\ <= (x_200 + y_187);


      end block;

    fun_388 : block
        signal x_201            : signed(63 downto 0);
        signal y_188            : signed(63 downto 0);
        signal \c$case_alt_198\ : signed(63 downto 0);
      begin
        acc_3_8_83 <= \c$case_alt_198\;

        x_201 <= acc_2_16_66;

        y_188 <= acc_2_17_67;

        \c$case_alt_198\ <= (x_201 + y_188);


      end block;

    fun_389 : block
        signal x_202            : signed(63 downto 0);
        signal y_189            : signed(63 downto 0);
        signal \c$case_alt_199\ : signed(63 downto 0);
      begin
        acc_3_9_84 <= \c$case_alt_199\;

        x_202 <= acc_2_18_68;

        y_189 <= acc_2_19_69;

        \c$case_alt_199\ <= (x_202 + y_189);


      end block;

    fun_390 : block
        signal x_203            : signed(63 downto 0);
        signal y_190            : signed(63 downto 0);
        signal \c$case_alt_200\ : signed(63 downto 0);
      begin
        acc_3_10_85 <= \c$case_alt_200\;

        x_203 <= acc_2_20_70;

        y_190 <= acc_2_21_71;

        \c$case_alt_200\ <= (x_203 + y_190);


      end block;

    fun_391 : block
        signal x_204            : signed(63 downto 0);
        signal y_191            : signed(63 downto 0);
        signal \c$case_alt_201\ : signed(63 downto 0);
      begin
        acc_3_11_86 <= \c$case_alt_201\;

        x_204 <= acc_2_22_72;

        y_191 <= acc_2_23_73;

        \c$case_alt_201\ <= (x_204 + y_191);


      end block;

    fun_392 : block
        signal x_205            : signed(63 downto 0);
        signal y_192            : signed(63 downto 0);
        signal \c$case_alt_202\ : signed(63 downto 0);
      begin
        acc_4_0_87 <= \c$case_alt_202\;

        x_205 <= acc_3_0_75;

        y_192 <= acc_3_1_76;

        \c$case_alt_202\ <= (x_205 + y_192);


      end block;

    fun_393 : block
        signal x_206            : signed(63 downto 0);
        signal y_193            : signed(63 downto 0);
        signal \c$case_alt_203\ : signed(63 downto 0);
      begin
        acc_4_1_88 <= \c$case_alt_203\;

        x_206 <= acc_3_2_77;

        y_193 <= acc_3_3_78;

        \c$case_alt_203\ <= (x_206 + y_193);


      end block;

    fun_394 : block
        signal x_207            : signed(63 downto 0);
        signal y_194            : signed(63 downto 0);
        signal \c$case_alt_204\ : signed(63 downto 0);
      begin
        acc_4_2_89 <= \c$case_alt_204\;

        x_207 <= acc_3_4_79;

        y_194 <= acc_3_5_80;

        \c$case_alt_204\ <= (x_207 + y_194);


      end block;

    fun_395 : block
        signal x_208            : signed(63 downto 0);
        signal y_195            : signed(63 downto 0);
        signal \c$case_alt_205\ : signed(63 downto 0);
      begin
        acc_4_3_90 <= \c$case_alt_205\;

        x_208 <= acc_3_6_81;

        y_195 <= acc_3_7_82;

        \c$case_alt_205\ <= (x_208 + y_195);


      end block;

    fun_396 : block
        signal x_209            : signed(63 downto 0);
        signal y_196            : signed(63 downto 0);
        signal \c$case_alt_206\ : signed(63 downto 0);
      begin
        acc_4_4_91 <= \c$case_alt_206\;

        x_209 <= acc_3_8_83;

        y_196 <= acc_3_9_84;

        \c$case_alt_206\ <= (x_209 + y_196);


      end block;

    fun_397 : block
        signal x_210            : signed(63 downto 0);
        signal y_197            : signed(63 downto 0);
        signal \c$case_alt_207\ : signed(63 downto 0);
      begin
        acc_4_5_92 <= \c$case_alt_207\;

        x_210 <= acc_3_10_85;

        y_197 <= acc_3_11_86;

        \c$case_alt_207\ <= (x_210 + y_197);


      end block;

    fun_398 : block
        signal x_211            : signed(63 downto 0);
        signal y_198            : signed(63 downto 0);
        signal \c$case_alt_208\ : signed(63 downto 0);
      begin
        acc_5_0_93 <= \c$case_alt_208\;

        x_211 <= acc_4_0_87;

        y_198 <= acc_4_1_88;

        \c$case_alt_208\ <= (x_211 + y_198);


      end block;

    fun_399 : block
        signal x_212            : signed(63 downto 0);
        signal y_199            : signed(63 downto 0);
        signal \c$case_alt_209\ : signed(63 downto 0);
      begin
        acc_5_1_94 <= \c$case_alt_209\;

        x_212 <= acc_4_2_89;

        y_199 <= acc_4_3_90;

        \c$case_alt_209\ <= (x_212 + y_199);


      end block;

    fun_400 : block
        signal x_213            : signed(63 downto 0);
        signal y_200            : signed(63 downto 0);
        signal \c$case_alt_210\ : signed(63 downto 0);
      begin
        acc_5_2_95 <= \c$case_alt_210\;

        x_213 <= acc_4_4_91;

        y_200 <= acc_4_5_92;

        \c$case_alt_210\ <= (x_213 + y_200);


      end block;

    fun_401 : block
        signal x_214            : signed(63 downto 0);
        signal y_201            : signed(63 downto 0);
        signal \c$case_alt_211\ : signed(63 downto 0);
      begin
        acc_6_0_96 <= \c$case_alt_211\;

        x_214 <= acc_5_0_93;

        y_201 <= acc_5_1_94;

        \c$case_alt_211\ <= (x_214 + y_201);


      end block;

    fun_402 : block
        signal x_215            : signed(63 downto 0);
        signal y_202            : signed(63 downto 0);
        signal \c$case_alt_212\ : signed(63 downto 0);
      begin
        acc_6_1_97 <= \c$case_alt_212\;

        x_215 <= acc_5_2_95;

        y_202 <= acc_2_24_74;

        \c$case_alt_212\ <= (x_215 + y_202);


      end block;

    fun_403 : block
        signal x_216            : signed(63 downto 0);
        signal y_203            : signed(63 downto 0);
        signal \c$case_alt_213\ : signed(63 downto 0);
      begin
        acc_7_0_98 <= \c$case_alt_213\;

        x_216 <= acc_6_0_96;

        y_203 <= acc_6_1_97;

        \c$case_alt_213\ <= (x_216 + y_203);


      end block;


  end block;

  x_6 <= \c$out2_case_alt\;

  y_2 <= ds1;

  y_3 <= result_33;

  x_projection_208 <=  win_0(0) ;

  x_7 <= x_projection_208;

  win_0 <= window_0.Tuple2_2_sel1_array_of_signed_64;

  -- register begin
  window_0_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_0 <= ( Tuple2_2_sel0_unsigned => to_unsigned(103,8)
  , Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window_0 <= result_31;
      end if;
    end if;
  end process;
  -- register end

  result_31 <= t_3 when b_2 else
               window_0;

  t_3 <= ( Tuple2_2_sel0_unsigned => result_50.Tags_sel5_slide0_0
         , Tuple2_2_sel1_array_of_signed_64 => result_32 );

  result_32 <= \c$t_case_alt_1\ when result_34 else
               \c$t_case_alt_2\;

  \c$t_case_alt_selection_7\ <= x_8;

  \c$vec_4\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_0)));

  \c$t_case_alt_sel_alt_6\ <= (\c$vec_4\(0 to 101-1),\c$vec_4\(101 to \c$vec_4\'high));

  \c$vec_5\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_0)));

  \c$t_case_alt_sel_alt_8\ <= (\c$vec_5\(0 to 101-1),\c$vec_5\(101 to \c$vec_5\'high));

  \c$t_case_alt_1\ <= \c$t_case_alt_sel_alt_8\.Tuple2_6_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_7\ else
                      \c$t_case_alt_sel_alt_6\.Tuple2_6_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_10\ <= x_8;

  \c$t_case_alt_2\ <= lastBucketUpdated_0 when \c$t_case_alt_selection_10\ else
                      win_0;

  -- replace begin
  replaceVec_0 : block
    signal vec_index_3 : integer range 0 to 101-1;
  begin
    vec_index_3 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 101
    -- pragma translate_on
                 ;

    process(vec_index_3,win_0,x_7,y_3)
      variable ivec_0 : Spec_topEntity_types.array_of_signed_64(0 to 100);
    begin
      ivec_0 := win_0;
      ivec_0(vec_index_3) := ((x_7 + y_3));
      lastBucketUpdated_0 <= ivec_0;
    end process;
  end block;
  -- replace end

  b_2 <= x_8 or result_34;

  x_8 <= result_35;

  \c$vec2_2\ <= (ws_2(1 to ws_2'high));

  -- zipWith begin
  zipWith_2 : for i_12 in \c$ws_app_arg_5\'range generate
  begin
    \c$ws_app_arg_5\(i_12) <= ( Tuple2_3_sel0_Tuple2_4 => result_26(i_12)
               , Tuple2_3_sel1_Maybe => \c$vec2_2\(i_12) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_5 : block
    function max_2 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_6 : for i_13 in \c$ws_app_arg_6\'range generate
    begin
      fun_404 : block
      signal \c$case_alt_214\             : Spec_topEntity_types.Maybe;
      -- spec.hs:312:1-63
      signal t_14                         : unsigned(7 downto 0);
      -- spec.hs:312:1-63
      signal x_217                        : Spec_topEntity_types.Tuple2_4;
      signal \c$case_alt_selection_res_4\ : boolean;
    begin
      \c$ws_app_arg_6\(i_13) <= \c$case_alt_214\;

      \c$case_alt_selection_res_4\ <= t_14 = result_50.Tags_sel2_output1_0;

      \c$case_alt_214\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_13,max_2(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_4\ else
                          \c$ws_app_arg_5\(i_13).Tuple2_3_sel1_Maybe;

      t_14 <= x_217.Tuple2_4_sel0_unsigned;

      x_217 <= \c$ws_app_arg_5\(i_13).Tuple2_3_sel0_Tuple2_4;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_2 : block
    signal vec_index_4 : integer range 0 to 2-1;
  begin
    vec_index_4 <= to_integer((signed(std_logic_vector(resize(i_2,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_5\ <= result_26(vec_index_4);
  end block;
  -- index end

  \c$case_scrut_6\ <=  ws_2(0) ;

  with (\c$case_scrut_6\(1 downto 1)) select
    result_33 <= to_signed(0,64) when "0",
                 \c$case_scrut_5\.Tuple2_4_sel1_signed when others;

  i_2 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_6\(0 downto 0)));

  ws_2 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_6\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_8 : block
    signal iterateI_ho1_0_arg0_6 : boolean;
    signal iterateI_ho1_0_res_8  : boolean;
    signal iterateI_ho1_1_res_8  : boolean;
    signal iterateI_ho1_2_res_8  : boolean;
  begin
    iterateI_ho1_0_arg0_6 <= slides.Slides_1_sel0_slide0;

    r_block_131 : block
        signal \c$bb_res_res_52\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_52_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_52\ <= iterateI_ho1_0_arg0_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_52\;


      end block;

    r_block_132 : block
        signal \c$bb_res_res_53\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_53_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_53\ <= iterateI_ho1_0_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_53\;


      end block;

    r_block_133 : block
        signal \c$bb_res_res_54\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_54_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_54\ <= iterateI_ho1_1_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_8 <= \c$bb_res_res_54\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel0_slide0
                                                           , iterateI_ho1_0_res_8
                                                           , iterateI_ho1_1_res_8
                                                           , iterateI_ho1_2_res_8 );


  end block;

  result_34 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  slides <= result_66.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_Slides_1;

  iterateI_9 : block
    signal iterateI_ho1_0_res_9 : boolean;
    signal iterateI_ho1_1_res_9 : boolean;
    signal iterateI_ho1_2_res_9 : boolean;
  begin
    r_block_134 : block
        signal \c$bb_res_res_55\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_55_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_55\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_55\;


      end block;

    r_block_135 : block
        signal \c$bb_res_res_56\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_56_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_56\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_9 <= \c$bb_res_res_56\;


      end block;

    r_block_136 : block
        signal \c$bb_res_res_57\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_57_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_57\ <= iterateI_ho1_1_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_9 <= \c$bb_res_res_57\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                            , iterateI_ho1_0_res_9
                                                            , iterateI_ho1_1_res_9
                                                            , iterateI_ho1_2_res_9 );


  end block;

  result_35 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  \c$out2_case_alt_selection_res\ <= tag = to_unsigned(103,8);

  \c$out2_case_alt\ <= to_signed(0,64) when \c$out2_case_alt_selection_res\ else
                       dta_0;

  dta_0 <= \c$out2_case_scrut\.Tuple2_4_sel1_signed;

  \c$vec2_3\ <= (ws_3(1 to ws_3'high));

  -- zipWith begin
  zipWith_3 : for i_14 in \c$ws_app_arg_7\'range generate
  begin
    \c$ws_app_arg_7\(i_14) <= ( Tuple2_5_sel0_Tuple2_4 => result_38(i_14)
               , Tuple2_5_sel1_Maybe_0 => \c$vec2_3\(i_14) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_7 : block
    function max_3 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_8 : for i_15 in \c$ws_app_arg_8\'range generate
    begin
      fun_405 : block
      signal \c$case_alt_215\             : Spec_topEntity_types.Maybe_0;
      -- spec.hs:312:1-63
      signal t_15                         : unsigned(7 downto 0);
      -- spec.hs:312:1-63
      signal x_218                        : Spec_topEntity_types.Tuple2_4;
      signal \c$case_alt_selection_res_5\ : boolean;
    begin
      \c$ws_app_arg_8\(i_15) <= \c$case_alt_215\;

      \c$case_alt_selection_res_5\ <= t_15 = result_51.Tags_sel1_output0_0;

      \c$case_alt_215\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_15,max_3(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_5\ else
                          \c$ws_app_arg_7\(i_15).Tuple2_5_sel1_Maybe_0;

      t_15 <= x_218.Tuple2_4_sel0_unsigned;

      x_218 <= \c$ws_app_arg_7\(i_15).Tuple2_5_sel0_Tuple2_4;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_3 : block
    signal vec_index_5 : integer range 0 to 3-1;
  begin
    vec_index_5 <= to_integer((signed(std_logic_vector(resize(i_3,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_7\ <= result_38(vec_index_5);
  end block;
  -- index end

  \c$case_scrut_8\ <=  ws_3(0) ;

  with (\c$case_scrut_8\(2 downto 2)) select
    result_36 <= to_signed(0,64) when "0",
                 \c$case_scrut_7\.Tuple2_4_sel1_signed when others;

  i_3 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_8\(1 downto 0)));

  ws_3 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_8\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  result_selection_18 <= x_9;

  result_37 <= t_4 when result_selection_18 else
               result_38;

  x_9 <= result_42;

  -- register begin
  result_38_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_38 <= Spec_topEntity_types.array_of_Tuple2_4'( ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_4_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_4_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_4_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_38 <= result_37;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_6\ <= (Spec_topEntity_types.array_of_Tuple2_4'(Spec_topEntity_types.array_of_Tuple2_4'(result_38) & Spec_topEntity_types.array_of_Tuple2_4'(Spec_topEntity_types.array_of_Tuple2_4'(0 => ( Tuple2_4_sel0_unsigned => t_8
             , Tuple2_4_sel1_signed => (x_10 + y_4) )))));

  t_projection_5 <= (\c$vec_6\(0 to 1-1),\c$vec_6\(1 to \c$vec_6\'high));

  t_4 <= t_projection_5.Tuple2_8_sel1_array_of_Tuple2_4_1;

  x_10 <= \c$out0_case_alt\;

  y_4 <= result_39;

  \c$vec2_4\ <= (ws_4(1 to ws_4'high));

  -- zipWith begin
  zipWith_4 : for i_16 in \c$ws_app_arg_9\'range generate
  begin
    \c$ws_app_arg_9\(i_16) <= ( Tuple2_3_sel0_Tuple2_4 => result_26(i_16)
               , Tuple2_3_sel1_Maybe => \c$vec2_4\(i_16) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_9 : block
    function max_4 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_10 : for i_17 in \c$ws_app_arg_10\'range generate
    begin
      fun_406 : block
      signal \c$case_alt_216\             : Spec_topEntity_types.Maybe;
      -- spec.hs:312:1-63
      signal t_16                         : unsigned(7 downto 0);
      -- spec.hs:312:1-63
      signal x_219                        : Spec_topEntity_types.Tuple2_4;
      signal \c$case_alt_selection_res_6\ : boolean;
    begin
      \c$ws_app_arg_10\(i_17) <= \c$case_alt_216\;

      \c$case_alt_selection_res_6\ <= t_16 = result_40;

      \c$case_alt_216\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_17,max_4(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_6\ else
                          \c$ws_app_arg_9\(i_17).Tuple2_3_sel1_Maybe;

      t_16 <= x_219.Tuple2_4_sel0_unsigned;

      x_219 <= \c$ws_app_arg_9\(i_17).Tuple2_3_sel0_Tuple2_4;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_4 : block
    signal vec_index_6 : integer range 0 to 2-1;
  begin
    vec_index_6 <= to_integer((signed(std_logic_vector(resize(i_4,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_9\ <= result_26(vec_index_6);
  end block;
  -- index end

  \c$case_scrut_10\ <=  ws_4(0) ;

  with (\c$case_scrut_10\(1 downto 1)) select
    result_39 <= to_signed(0,64) when "0",
                 \c$case_scrut_9\.Tuple2_4_sel1_signed when others;

  result_selection_res_2 <= t_7 > to_unsigned(1,8);

  result_40 <= \c$app_arg_11\ when result_selection_res_2 else
               \c$app_arg_11\ + to_unsigned(102,8);

  \c$app_arg_11\ <= t_7 - to_unsigned(1,8);

  i_4 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_10\(0 downto 0)));

  ws_4 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_10\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  \c$out0_case_alt_selection_res\ <= result_41 = winTag;

  \c$out0_case_alt\ <= winData when \c$out0_case_alt_selection_res\ else
                       to_signed(0,64);

  winData <= result_47.Tuple2_4_sel1_signed;

  winTag <= result_47.Tuple2_4_sel0_unsigned;

  result_selection_res_3 <= t_9 > to_unsigned(1,8);

  result_41 <= \c$app_arg_12\ when result_selection_res_3 else
               \c$app_arg_12\ + to_unsigned(102,8);

  \c$app_arg_12\ <= t_9 - to_unsigned(1,8);

  iterateI_10 : block
    signal iterateI_ho1_0_res_10 : boolean;
  begin
    r_block_137 : block
        signal \c$bb_res_res_58\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_58_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_58\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_58\;


      end block;

    \c$app_arg_13\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                            , iterateI_ho1_0_res_10 );


  end block;

  result_42 <=  \c$app_arg_13\(\c$app_arg_13\'high) ;

  iterateI_11 : block
    signal iterateI_ho1_0_res_11 : boolean;
    signal iterateI_ho1_1_res_10 : boolean;
  begin
    r_block_138 : block
        signal \c$bb_res_res_59\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_59_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_59\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_11 <= \c$bb_res_res_59\;


      end block;

    r_block_139 : block
        signal \c$bb_res_res_60\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_60_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_60\ <= iterateI_ho1_0_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_10 <= \c$bb_res_res_60\;


      end block;

    \c$app_arg_14\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                            , iterateI_ho1_0_res_11
                                                            , iterateI_ho1_1_res_10 );


  end block;

  result_43 <=  \c$app_arg_14\(\c$app_arg_14\'high) ;

  iterateI_12 : block
    signal iterateI_ho1_0_arg0_7 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_12 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_11 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_10 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_7 <= ( Tags_sel0_input0 => t_9
                             , Tags_sel1_output0_0 => t_8
                             , Tags_sel2_output1_0 => t_7
                             , Tags_sel3_output2_0 => t_6
                             , Tags_sel4_output3_0 => t_5
                             , Tags_sel5_slide0_0 => t_7
                             , Tags_sel6_slide1_0 => t_6 );

    r_block_140 : block
        signal \c$bb_res_res_61\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_61_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_61\ <= iterateI_ho1_0_arg0_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_12 <= \c$bb_res_res_61\;


      end block;

    r_block_141 : block
        signal \c$bb_res_res_62\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_62_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_62\ <= iterateI_ho1_0_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_11 <= \c$bb_res_res_62\;


      end block;

    r_block_142 : block
        signal \c$bb_res_res_63\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_63_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_63\ <= iterateI_ho1_1_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_10 <= \c$bb_res_res_63\;


      end block;

    \c$app_arg_15\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_9
                                                         , Tags_sel1_output0_0 => t_8
                                                         , Tags_sel2_output1_0 => t_7
                                                         , Tags_sel3_output2_0 => t_6
                                                         , Tags_sel4_output3_0 => t_5
                                                         , Tags_sel5_slide0_0 => t_7
                                                         , Tags_sel6_slide1_0 => t_6 )
                                                         , iterateI_ho1_0_res_12
                                                         , iterateI_ho1_1_res_11
                                                         , iterateI_ho1_2_res_10 );


  end block;

  result_44 <=  \c$app_arg_15\(\c$app_arg_15\'high) ;

  iterateI_13 : block
    signal iterateI_ho1_0_res_13 : boolean;
    signal iterateI_ho1_1_res_12 : boolean;
    signal iterateI_ho1_2_res_11 : boolean;
    signal iterateI_ho1_3_res_8  : boolean;
  begin
    r_block_143 : block
        signal \c$bb_res_res_64\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_64_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_64\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_13 <= \c$bb_res_res_64\;


      end block;

    r_block_144 : block
        signal \c$bb_res_res_65\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_65_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_65\ <= iterateI_ho1_0_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_12 <= \c$bb_res_res_65\;


      end block;

    r_block_145 : block
        signal \c$bb_res_res_66\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_66_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_66\ <= iterateI_ho1_1_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_11 <= \c$bb_res_res_66\;


      end block;

    r_block_146 : block
        signal \c$bb_res_res_67\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_67_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_67\ <= iterateI_ho1_2_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_8 <= \c$bb_res_res_67\;


      end block;

    \c$app_arg_16\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                            , iterateI_ho1_0_res_13
                                                            , iterateI_ho1_1_res_12
                                                            , iterateI_ho1_2_res_11
                                                            , iterateI_ho1_3_res_8 );


  end block;

  result_45 <=  \c$app_arg_16\(\c$app_arg_16\'high) ;

  result_46 <= ( Tuple2_4_sel0_unsigned => t_9
               , Tuple2_4_sel1_signed => \c$input0Win_app_arg\ ) when b_3 else
               result_47;

  -- register begin
  result_47_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_47 <= ( Tuple2_4_sel0_unsigned => to_unsigned(103,8)
  , Tuple2_4_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_47 <= result_46;
      end if;
    end if;
  end process;
  -- register end

  b_3 <= result_48;

  -- delay begin
  cinput0Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input0Win_app_arg\ <= result_66.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_ValidInt.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_14 : block
    signal iterateI_ho1_0_res_14 : boolean;
  begin
    r_block_147 : block
        signal \c$bb_res_res_68\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_68_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_68\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_14 <= \c$bb_res_res_68\;


      end block;

    \c$app_arg_17\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                            , iterateI_ho1_0_res_14 );


  end block;

  result_48 <=  \c$app_arg_17\(\c$app_arg_17\'high) ;

  iterateI_15 : block
    signal iterateI_ho1_0_arg0_8 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_15 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_13 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_12 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_9  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_7  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_5_res_4  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_8 <= ( Tags_sel0_input0 => t_9
                             , Tags_sel1_output0_0 => t_8
                             , Tags_sel2_output1_0 => t_7
                             , Tags_sel3_output2_0 => t_6
                             , Tags_sel4_output3_0 => t_5
                             , Tags_sel5_slide0_0 => t_7
                             , Tags_sel6_slide1_0 => t_6 );

    r_block_148 : block
        signal \c$bb_res_res_69\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_69_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_69\ <= iterateI_ho1_0_arg0_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_15 <= \c$bb_res_res_69\;


      end block;

    r_block_149 : block
        signal \c$bb_res_res_70\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_70_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_70\ <= iterateI_ho1_0_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_13 <= \c$bb_res_res_70\;


      end block;

    r_block_150 : block
        signal \c$bb_res_res_71\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_71_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_71\ <= iterateI_ho1_1_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_12 <= \c$bb_res_res_71\;


      end block;

    r_block_151 : block
        signal \c$bb_res_res_72\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_72_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_72\ <= iterateI_ho1_2_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_9 <= \c$bb_res_res_72\;


      end block;

    r_block_152 : block
        signal \c$bb_res_res_73\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_73_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_73\ <= iterateI_ho1_3_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_7 <= \c$bb_res_res_73\;


      end block;

    r_block_153 : block
        signal \c$bb_res_res_74\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_74_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_74\ <= iterateI_ho1_4_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_4 <= \c$bb_res_res_74\;


      end block;

    \c$app_arg_18\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_9
                                                         , Tags_sel1_output0_0 => t_8
                                                         , Tags_sel2_output1_0 => t_7
                                                         , Tags_sel3_output2_0 => t_6
                                                         , Tags_sel4_output3_0 => t_5
                                                         , Tags_sel5_slide0_0 => t_7
                                                         , Tags_sel6_slide1_0 => t_6 )
                                                         , iterateI_ho1_0_res_15
                                                         , iterateI_ho1_1_res_13
                                                         , iterateI_ho1_2_res_12
                                                         , iterateI_ho1_3_res_9
                                                         , iterateI_ho1_4_res_7
                                                         , iterateI_ho1_5_res_4 );


  end block;

  result_49 <=  \c$app_arg_18\(\c$app_arg_18\'high) ;

  iterateI_16 : block
    signal iterateI_ho1_0_arg0_9 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_16 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_14 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_9 <= ( Tags_sel0_input0 => t_9
                             , Tags_sel1_output0_0 => t_8
                             , Tags_sel2_output1_0 => t_7
                             , Tags_sel3_output2_0 => t_6
                             , Tags_sel4_output3_0 => t_5
                             , Tags_sel5_slide0_0 => t_7
                             , Tags_sel6_slide1_0 => t_6 );

    r_block_154 : block
        signal \c$bb_res_res_75\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_75_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_75\ <= iterateI_ho1_0_arg0_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_16 <= \c$bb_res_res_75\;


      end block;

    r_block_155 : block
        signal \c$bb_res_res_76\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_76_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_76\ <= iterateI_ho1_0_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_14 <= \c$bb_res_res_76\;


      end block;

    \c$app_arg_19\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_9
                                                         , Tags_sel1_output0_0 => t_8
                                                         , Tags_sel2_output1_0 => t_7
                                                         , Tags_sel3_output2_0 => t_6
                                                         , Tags_sel4_output3_0 => t_5
                                                         , Tags_sel5_slide0_0 => t_7
                                                         , Tags_sel6_slide1_0 => t_6 )
                                                         , iterateI_ho1_0_res_16
                                                         , iterateI_ho1_1_res_14 );


  end block;

  result_50 <=  \c$app_arg_19\(\c$app_arg_19\'high) ;

  iterateI_17 : block
    signal iterateI_ho1_0_arg0_10 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_17  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_10 <= ( Tags_sel0_input0 => t_9
                              , Tags_sel1_output0_0 => t_8
                              , Tags_sel2_output1_0 => t_7
                              , Tags_sel3_output2_0 => t_6
                              , Tags_sel4_output3_0 => t_5
                              , Tags_sel5_slide0_0 => t_7
                              , Tags_sel6_slide1_0 => t_6 );

    r_block_156 : block
        signal \c$bb_res_res_77\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_output3_0 => to_unsigned(103,8)
      , Tags_sel5_slide0_0 => to_unsigned(103,8)
      , Tags_sel6_slide1_0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_77_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_77\ <= iterateI_ho1_0_arg0_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_17 <= \c$bb_res_res_77\;


      end block;

    \c$app_arg_20\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_9
                                                         , Tags_sel1_output0_0 => t_8
                                                         , Tags_sel2_output1_0 => t_7
                                                         , Tags_sel3_output2_0 => t_6
                                                         , Tags_sel4_output3_0 => t_5
                                                         , Tags_sel5_slide0_0 => t_7
                                                         , Tags_sel6_slide1_0 => t_6 )
                                                         , iterateI_ho1_0_res_17 );


  end block;

  result_51 <=  \c$app_arg_20\(\c$app_arg_20\'high) ;

  -- register begin
  t_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_5 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_5 <= result_52;
      end if;
    end if;
  end process;
  -- register end

  result_selection_25 <= pOut3;

  result_52 <= result_53 when result_selection_25 else
               t_5;

  b_4 <= t_5 = to_unsigned(102,8);

  f1 <= t_5 + to_unsigned(1,8);

  result_53 <= to_unsigned(1,8) when b_4 else
               f1;

  pOut3 <= pacings.Pacings_1_sel4_pacingOut3;

  -- register begin
  t_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_6 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_6 <= result_54;
      end if;
    end if;
  end process;
  -- register end

  result_selection_29 <= pOut2;

  result_54 <= result_55 when result_selection_29 else
               t_6;

  b_5 <= t_6 = to_unsigned(102,8);

  f1_0 <= t_6 + to_unsigned(1,8);

  result_55 <= to_unsigned(1,8) when b_5 else
               f1_0;

  pOut2 <= pacings.Pacings_1_sel3_pacingOut2;

  -- register begin
  t_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_7 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_7 <= result_56;
      end if;
    end if;
  end process;
  -- register end

  result_selection_33 <= x_11;

  result_56 <= result_57 when result_selection_33 else
               t_7;

  b_6 <= t_7 = to_unsigned(102,8);

  f1_1 <= t_7 + to_unsigned(1,8);

  result_57 <= to_unsigned(1,8) when b_6 else
               f1_1;

  x_11 <= pOut1;

  pOut1 <= pacings.Pacings_1_sel2_pacingOut1;

  -- register begin
  t_8_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_8 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_8 <= result_58;
      end if;
    end if;
  end process;
  -- register end

  result_selection_37 <= x_12;

  result_58 <= result_59 when result_selection_37 else
               t_8;

  b_7 <= t_8 = to_unsigned(102,8);

  f1_2 <= t_8 + to_unsigned(1,8);

  result_59 <= to_unsigned(1,8) when b_7 else
               f1_2;

  x_12 <= pOut0;

  pOut0 <= pacings.Pacings_1_sel1_pacingOut0;

  -- register begin
  t_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_9 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_9 <= result_60;
      end if;
    end if;
  end process;
  -- register end

  result_selection_41 <= pIn0_0;

  result_60 <= result_61 when result_selection_41 else
               t_9;

  b_8 <= t_9 = to_unsigned(102,8);

  f1_3 <= t_9 + to_unsigned(1,8);

  result_61 <= to_unsigned(1,8) when b_8 else
               f1_3;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_66.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  -- register begin
  toWait_register : process(clk,rst)
  begin
    if rst =  '1'  then
      toWait <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        toWait <= result_63;
      end if;
    end if;
  end process;
  -- register end

  result_62 <= toWait = (to_signed(0,64));

  result_63 <= to_signed(2,64) when result_65 else
               result_64;

  b_9 <= toWait > (to_signed(0,64));

  result_64 <= (x_13 - to_signed(1,64)) when b_9 else
               toWait;

  x_13 <= toWait;

  result_65 <= true when b_10 else
               false;

  b_10 <= result_62 and result_66.Tuple3_sel1_boolean_1;

  result_66 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_23\
               , Tuple3_sel1_boolean_1 => \c$app_arg_22\
               , Tuple3_sel2_Tuple3_0 => \c$app_arg_21\ );

  -- register begin
  capp_arg_21_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_21\ <= ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
  , Slides_1_sel1_slide1 => false )
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingOut0 => false
  , Pacings_1_sel2_pacingOut1 => false
  , Pacings_1_sel3_pacingOut2 => false
  , Pacings_1_sel4_pacingOut3 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_21\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                    , Slides_1_sel1_slide1 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false
                    , Pacings_1_sel4_pacingOut3 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                    , Slides_1_sel1_slide1 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false
                    , Pacings_1_sel4_pacingOut3 => false ) );

  with (x_14) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_14) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                      , Slides_1_sel1_slide1 => false )
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingOut0 => false
                      , Pacings_1_sel2_pacingOut1 => false
                      , Pacings_1_sel3_pacingOut2 => false
                      , Pacings_1_sel4_pacingOut3 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_5 : block
    signal vec_index_7 : integer range 0 to 4-1;
  begin
    vec_index_7 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_7);
  end block;
  -- index end

  -- register begin
  capp_arg_22_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_22\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_22\ <= \c$case_alt_5\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_5\ <= \c$case_alt_6\ when push else
                    \c$case_alt_7\;

  \c$case_alt_6\ <= true when pop else
                    \c$case_alt_8\;

  \c$case_alt_7\ <= \c$case_alt_9\ when pop else
                    \c$case_alt_8\;

  \c$case_alt_8\ <= true when pop else
                    false;

  with (x_14) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_23_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_23\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_23\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_24\);

  \c$app_arg_24\ <= to_signed(1,64) when x_14 /= to_signed(4,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) )
                                         , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) )
                                         , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) )
                                         , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec_7\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_7\(0 to 4-1),\c$vec_7\(4 to \c$vec_7\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_9_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_8\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_8\(0 to 4-1),\c$vec_8\(4 to \c$vec_8\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_9_sel0_array_of_Tuple3_0_0;

  qData <= result_8.Tuple2_sel1_Tuple3_0;

  x_14 <= cursor;

  -- register begin
  cursor_register : process(clk,rst)
  begin
    if rst =  '1'  then
      cursor <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        cursor <= \c$cursor_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$cursor_case_alt\ <= \c$cursor_case_alt_0\ when push else
                         \c$cursor_case_alt_2\;

  push <= result_8.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_14 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_14 = to_signed(4,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_9.Tuple2_0_sel0_boolean;

  with (x_14) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_14 - to_signed(1,64));

  result <= result_9.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_3 <= result.Outputs_sel3_output3;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidInt_sel0_value;

  result_1_1 <= result_1.ValidInt_sel1_valid;

  result_2_0 <= result_2.ValidInt_sel0_value;

  result_2_1 <= result_2.ValidInt_sel1_valid;

  result_3_0 <= result_3.ValidBool_sel0_value_0;

  result_3_1 <= result_3.ValidBool_sel1_valid_0;


end;


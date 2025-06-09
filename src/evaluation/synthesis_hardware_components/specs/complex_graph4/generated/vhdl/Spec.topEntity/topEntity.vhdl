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
       clk         : in Spec_topEntity_types.clk_TestDomain;
       -- reset
       rst         : in Spec_topEntity_types.rst_TestDomain;
       -- enable
       en          : in Spec_topEntity_types.en_TestDomain;
       inputs_0_0  : in signed(63 downto 0);
       inputs_0_1  : in boolean;
       inputs_1_0  : in signed(63 downto 0);
       inputs_1_1  : in boolean;
       result_0_0  : out signed(63 downto 0);
       result_0_1  : out boolean;
       result_1_0  : out signed(63 downto 0);
       result_1_1  : out boolean;
       result_2_0  : out signed(63 downto 0);
       result_2_1  : out boolean;
       result_3_0  : out signed(63 downto 0);
       result_3_1  : out boolean;
       result_4_0  : out signed(63 downto 0);
       result_4_1  : out boolean;
       result_5_0  : out signed(63 downto 0);
       result_5_1  : out boolean;
       result_6_0  : out signed(63 downto 0);
       result_6_1  : out boolean;
       result_7_0  : out signed(63 downto 0);
       result_7_1  : out boolean;
       result_8_0  : out signed(63 downto 0);
       result_8_1  : out boolean;
       result_9_0  : out signed(63 downto 0);
       result_9_1  : out boolean;
       result_10_0 : out signed(63 downto 0);
       result_10_1 : out boolean;
       result_11_0 : out signed(63 downto 0);
       result_11_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_12                         : signed(63 downto 0);
  signal x                                 : signed(63 downto 0);
  signal result_13                         : signed(63 downto 0) := (to_signed(0,64));
  signal result_14                         : signed(63 downto 0);
  signal x_0                               : signed(63 downto 0);
  signal result_15                         : signed(63 downto 0) := (to_signed(0,64));
  signal result_16                         : Spec_topEntity_types.Tuple2;
  -- spec.hs:302:1-82
  signal pIn0                              : boolean;
  -- spec.hs:302:1-82
  signal pIn1                              : boolean;
  -- spec.hs:302:1-82
  signal timer0Over                        : boolean;
  -- spec.hs:302:1-82
  signal timer1Over                        : boolean;
  signal result_17                         : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                       : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_18                         : boolean;
  signal \c$app_arg_0\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_19                         : boolean;
  signal \c$app_arg_1\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_20                         : boolean;
  signal \c$app_arg_2\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_21                         : boolean;
  signal \c$app_arg_3\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_22                         : boolean;
  signal \c$app_arg_4\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_23                         : boolean;
  signal \c$app_arg_5\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_24                         : boolean;
  signal \c$app_arg_6\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_25                         : boolean;
  -- spec.hs:363:1-63
  signal \c$ws_app_arg\                    : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:363:1-63
  signal \c$ws_app_arg_0\                  : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut\                    : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_0\                  : Spec_topEntity_types.Maybe;
  signal result_26                         : signed(63 downto 0);
  -- spec.hs:363:1-63
  signal i                                 : Spec_topEntity_types.index_2;
  -- spec.hs:363:1-63
  signal ws                                : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_7\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_27                         : boolean;
  signal \c$app_arg_8\                     : Spec_topEntity_types.array_of_Tags(0 to 7);
  signal result_28                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_9\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_29                         : boolean;
  signal \c$app_arg_10\                    : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_30                         : boolean;
  signal \c$app_arg_11\                    : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_31                         : boolean;
  -- spec.hs:745:1-157
  signal y                                 : signed(63 downto 0);
  -- spec.hs:745:1-157
  signal x_1                               : signed(63 downto 0);
  -- spec.hs:745:1-157
  signal win                               : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:745:1-157
  signal window                            : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_unsigned => to_unsigned(5,8)
, Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_32                         : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:745:1-157
  signal t                                 : Spec_topEntity_types.Tuple2_3;
  signal result_33                         : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:745:1-157
  signal \c$t_case_alt\                    : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:745:1-157
  signal \c$t_case_alt_0\                  : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:745:1-157
  signal lastBucketUpdated                 : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:745:1-157
  signal b                                 : boolean;
  -- spec.hs:745:1-157
  signal \c$b_case_alt\                    : boolean;
  -- spec.hs:143:34-42
  signal x1                                : boolean;
  -- spec.hs:143:34-42
  signal x0                                : boolean;
  -- spec.hs:370:1-55
  signal winData                           : signed(63 downto 0);
  signal result_34                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:143:34-42
  signal x0_0                              : boolean;
  -- spec.hs:143:34-42
  signal x1_0                              : boolean;
  -- spec.hs:677:1-183
  signal result_35                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:677:1-183
  signal t_0                               : Spec_topEntity_types.Tuple2_2;
  signal x_2                               : signed(63 downto 0);
  signal y_0                               : signed(63 downto 0);
  signal x_3                               : signed(63 downto 0);
  signal x_4                               : signed(63 downto 0);
  signal y_1                               : signed(63 downto 0);
  signal y_2                               : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out5_case_alt\                 : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal dta                               : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tag                               : unsigned(7 downto 0);
  signal result_36                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:145:34-42
  signal x0_1                              : boolean;
  -- spec.hs:145:34-42
  signal x1_1                              : boolean;
  -- spec.hs:693:1-165
  signal result_37                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:693:1-165
  signal t_1                               : Spec_topEntity_types.Tuple2_2;
  signal x_5                               : signed(63 downto 0);
  signal y_3                               : signed(63 downto 0);
  signal x_6                               : signed(63 downto 0);
  signal y_4                               : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out7_case_alt\                 : signed(63 downto 0);
  -- spec.hs:384:1-46
  signal dta_0                             : signed(63 downto 0);
  -- spec.hs:384:1-46
  signal tag_0                             : unsigned(7 downto 0);
  signal result_38                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:725:1-165
  signal result_39                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:725:1-165
  signal b_0                               : boolean;
  -- spec.hs:725:1-165
  signal t_2                               : Spec_topEntity_types.Tuple2_2;
  signal x_7                               : signed(63 downto 0);
  signal y_5                               : signed(63 downto 0);
  -- spec.hs:763:1-157
  signal y_6                               : signed(63 downto 0);
  -- spec.hs:763:1-157
  signal x_8                               : signed(63 downto 0);
  -- spec.hs:763:1-157
  signal win_0                             : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:763:1-157
  signal window_0                          : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_unsigned => to_unsigned(5,8)
, Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_40                         : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:763:1-157
  signal t_3                               : Spec_topEntity_types.Tuple2_3;
  signal result_41                         : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:763:1-157
  signal \c$t_case_alt_1\                  : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:763:1-157
  signal \c$t_case_alt_2\                  : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:763:1-157
  signal lastBucketUpdated_0               : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:763:1-157
  signal b_1                               : boolean;
  -- spec.hs:139:34-42
  signal x_9                               : boolean;
  signal \c$app_arg_12\                    : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_42                         : boolean;
  signal \c$app_arg_13\                    : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_43                         : boolean;
  -- spec.hs:397:1-91
  signal \c$out7_case_alt_0\               : signed(63 downto 0);
  signal y_7                               : signed(63 downto 0);
  signal x_10                              : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out7_case_alt_1\               : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal dta_1                             : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tag_1                             : unsigned(7 downto 0);
  -- spec.hs:376:1-53
  signal tagToMatch                        : unsigned(7 downto 0);
  signal \c$app_arg_14\                    : Spec_topEntity_types.array_of_PacingOut7(0 to 4);
  signal result_44                         : Spec_topEntity_types.PacingOut7;
  -- spec.hs:376:1-53
  signal tagToMatch_0                      : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$out5_case_alt_0\               : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tagToMatch_1                      : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$out5_case_alt_1\               : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tagToMatch_2                      : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$out5_case_alt_2\               : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winTag                            : unsigned(7 downto 0);
  signal result_45                         : unsigned(7 downto 0);
  signal \c$app_arg_15\                    : unsigned(7 downto 0);
  signal \c$app_arg_16\                    : Spec_topEntity_types.array_of_PacingOut5(0 to 5);
  signal result_46                         : Spec_topEntity_types.PacingOut5;
  signal \c$app_arg_17\                    : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_47                         : boolean;
  -- spec.hs:397:1-91
  signal slides                            : Spec_topEntity_types.Slides_1;
  signal \c$app_arg_18\                    : Spec_topEntity_types.array_of_PacingOut5(0 to 6);
  signal result_48                         : Spec_topEntity_types.PacingOut5;
  signal \c$app_arg_19\                    : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_49                         : boolean;
  signal result_50                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:144:34-42
  signal x0_2                              : boolean;
  -- spec.hs:144:34-42
  signal x1_2                              : boolean;
  -- spec.hs:685:1-165
  signal result_51                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:685:1-165
  signal t_4                               : Spec_topEntity_types.Tuple2_2;
  signal x_11                              : signed(63 downto 0);
  signal y_8                               : signed(63 downto 0);
  signal x_12                              : signed(63 downto 0);
  signal y_9                               : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out6_case_alt\                 : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal dta_2                             : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tag_2                             : unsigned(7 downto 0);
  -- spec.hs:376:1-53
  signal tagToMatch_3                      : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$out6_case_alt_0\               : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tagToMatch_4                      : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$out6_case_alt_1\               : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tag_3                             : unsigned(7 downto 0);
  -- spec.hs:376:1-53
  signal tagToMatch_5                      : unsigned(7 downto 0);
  signal \c$app_arg_20\                    : Spec_topEntity_types.array_of_PacingOut6(0 to 2);
  signal result_52                         : Spec_topEntity_types.PacingOut6;
  signal result_53                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:146:34-42
  signal x_13                              : boolean;
  -- spec.hs:701:1-165
  signal result_54                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:701:1-165
  signal t_5                               : Spec_topEntity_types.Tuple2_2;
  signal x_14                              : signed(63 downto 0);
  signal y_10                              : signed(63 downto 0);
  signal x_15                              : signed(63 downto 0);
  signal y_11                              : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out8_case_alt\                 : signed(63 downto 0);
  -- spec.hs:384:1-46
  signal dta_3                             : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out8_case_scrut\               : boolean;
  -- spec.hs:384:1-46
  signal tag_4                             : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$out8_case_alt_0\               : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out8_case_alt_1\               : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winData_0                         : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winTag_0                          : unsigned(7 downto 0);
  signal result_55                         : unsigned(7 downto 0);
  signal \c$app_arg_21\                    : unsigned(7 downto 0);
  signal \c$app_arg_22\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_56                         : boolean;
  signal result_57                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:630:1-127
  signal result_58                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:630:1-127
  signal b_2                               : boolean;
  -- spec.hs:397:1-91
  signal \c$input1Win_app_arg\             : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_23\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_59                         : boolean;
  -- spec.hs:376:1-53
  signal tag_5                             : unsigned(7 downto 0);
  signal result_60                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:138:34-42
  signal x_16                              : boolean;
  -- spec.hs:636:1-147
  signal result_61                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:636:1-147
  signal t_6                               : Spec_topEntity_types.Tuple2_2;
  signal x_17                              : signed(63 downto 0);
  signal y_12                              : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out0_case_alt\                 : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winData_1                         : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winTag_1                          : unsigned(7 downto 0);
  signal result_62                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:140:34-42
  signal x_18                              : boolean;
  -- spec.hs:652:1-129
  signal result_63                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:652:1-129
  signal t_7                               : Spec_topEntity_types.Tuple2_2;
  signal x_19                              : signed(63 downto 0);
  signal result_64                         : unsigned(7 downto 0);
  signal \c$app_arg_24\                    : unsigned(7 downto 0);
  -- spec.hs:370:1-55
  signal tag_6                             : unsigned(7 downto 0);
  -- spec.hs:355:1-65
  signal \c$ws_app_arg_1\                  : Spec_topEntity_types.array_of_Tuple2_4(0 to 2);
  -- spec.hs:355:1-65
  signal \c$ws_app_arg_2\                  : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$case_scrut_1\                  : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_2\                  : Spec_topEntity_types.Maybe_0;
  signal result_65                         : signed(63 downto 0);
  signal result_66                         : unsigned(7 downto 0);
  signal \c$app_arg_25\                    : unsigned(7 downto 0);
  -- spec.hs:355:1-65
  signal i_0                               : Spec_topEntity_types.index_3;
  -- spec.hs:355:1-65
  signal ws_0                              : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal result_67                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:142:34-42
  signal x_20                              : boolean;
  -- spec.hs:669:1-147
  signal result_68                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:669:1-147
  signal t_8                               : Spec_topEntity_types.Tuple2_2;
  signal x_21                              : signed(63 downto 0);
  signal y_13                              : signed(63 downto 0);
  -- spec.hs:355:1-65
  signal \c$ws_app_arg_3\                  : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:355:1-65
  signal \c$ws_app_arg_4\                  : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_3\                  : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_4\                  : Spec_topEntity_types.Maybe;
  signal result_69                         : signed(63 downto 0);
  signal result_70                         : unsigned(7 downto 0);
  signal \c$app_arg_26\                    : unsigned(7 downto 0);
  -- spec.hs:355:1-65
  signal i_1                               : Spec_topEntity_types.index_2;
  -- spec.hs:355:1-65
  signal ws_1                              : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal result_71                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:141:34-42
  signal x_22                              : boolean;
  -- spec.hs:660:1-137
  signal result_72                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 1) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:660:1-137
  signal t_9                               : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  signal x_23                              : signed(63 downto 0);
  signal \c$app_arg_27\                    : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_73                         : boolean;
  -- spec.hs:397:1-91
  signal \c$out4_case_alt\                 : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winData_2                         : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winTag_2                          : unsigned(7 downto 0);
  signal result_74                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:139:34-42
  signal x_24                              : boolean;
  -- spec.hs:644:1-147
  signal result_75                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:644:1-147
  signal t_10                              : Spec_topEntity_types.Tuple2_2;
  signal x_25                              : signed(63 downto 0);
  signal y_14                              : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out1_case_alt\                 : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal dta_4                             : signed(63 downto 0);
  -- spec.hs:363:1-63
  signal \c$ws_app_arg_5\                  : Spec_topEntity_types.array_of_Tuple2_4(0 to 2);
  -- spec.hs:363:1-63
  signal \c$ws_app_arg_6\                  : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$case_scrut_5\                  : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_6\                  : Spec_topEntity_types.Maybe_0;
  signal result_76                         : signed(63 downto 0);
  -- spec.hs:363:1-63
  signal i_2                               : Spec_topEntity_types.index_3;
  -- spec.hs:363:1-63
  signal ws_2                              : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$app_arg_28\                    : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_77                         : boolean;
  signal result_78                         : unsigned(7 downto 0);
  signal \c$app_arg_29\                    : unsigned(7 downto 0);
  signal \c$app_arg_30\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_79                         : boolean;
  signal \c$app_arg_31\                    : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_80                         : boolean;
  -- spec.hs:376:1-53
  signal tagToMatch_6                      : unsigned(7 downto 0);
  signal \c$app_arg_32\                    : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_81                         : boolean;
  signal result_82                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 2);
  -- spec.hs:625:1-135
  signal result_83                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 2) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:625:1-135
  signal b_3                               : boolean;
  -- spec.hs:625:1-135
  signal t_11                              : Spec_topEntity_types.array_of_Tuple2_2(0 to 2);
  -- spec.hs:397:1-91
  signal \c$input0Win_app_arg\             : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:397:1-91
  signal inputs_2                          : Spec_topEntity_types.Inputs_3;
  signal \c$app_arg_33\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_84                         : boolean;
  signal result_85                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:717:1-131
  signal result_86                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:717:1-131
  signal b_4                               : boolean;
  -- spec.hs:717:1-131
  signal t_12                              : Spec_topEntity_types.Tuple2_2;
  signal x_26                              : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out10_case_alt\                : signed(63 downto 0);
  -- spec.hs:376:1-53
  signal tagToMatch_7                      : unsigned(7 downto 0);
  signal \c$app_arg_34\                    : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_87                         : boolean;
  signal \c$app_arg_35\                    : Spec_topEntity_types.array_of_Tags(0 to 6);
  signal result_88                         : Spec_topEntity_types.Tags;
  -- spec.hs:370:1-55
  signal winTag_3                          : unsigned(7 downto 0);
  signal result_89                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:709:1-129
  signal result_90                         : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:709:1-129
  signal b_5                               : boolean;
  -- spec.hs:709:1-129
  signal t_13                              : Spec_topEntity_types.Tuple2_2;
  signal x_27                              : signed(63 downto 0);
  -- spec.hs:397:1-91
  signal \c$out9_case_alt\                 : signed(63 downto 0);
  -- spec.hs:370:1-55
  signal winData_3                         : signed(63 downto 0);
  signal result_91                         : unsigned(7 downto 0);
  signal \c$app_arg_36\                    : unsigned(7 downto 0);
  signal \c$app_arg_37\                    : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_92                         : boolean;
  signal \c$app_arg_38\                    : Spec_topEntity_types.array_of_Tags(0 to 5);
  signal result_93                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_39\                    : Spec_topEntity_types.array_of_Tags(0 to 4);
  signal result_94                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_40\                    : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_95                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_41\                    : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_96                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_42\                    : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_97                         : Spec_topEntity_types.Tags;
  -- spec.hs:609:9-81
  signal t_14                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_98                         : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_6                               : boolean;
  -- spec.hs:609:9-81
  signal f1                                : unsigned(7 downto 0);
  signal result_99                         : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal pOut11                            : boolean;
  -- spec.hs:609:9-81
  signal t_15                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_100                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_7                               : boolean;
  -- spec.hs:609:9-81
  signal f1_0                              : unsigned(7 downto 0);
  signal result_101                        : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal pOut10                            : boolean;
  -- spec.hs:609:9-81
  signal t_16                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_102                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_8                               : boolean;
  -- spec.hs:609:9-81
  signal f1_1                              : unsigned(7 downto 0);
  signal result_103                        : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal pOut9                             : boolean;
  -- spec.hs:609:9-81
  signal t_17                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_104                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_9                               : boolean;
  -- spec.hs:609:9-81
  signal f1_2                              : unsigned(7 downto 0);
  signal result_105                        : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$ds11_case_alt\                 : boolean;
  -- spec.hs:145:34-42
  signal x1_3                              : boolean;
  -- spec.hs:145:34-42
  signal x0_3                              : boolean;
  -- spec.hs:397:1-91
  signal pOut7                             : Spec_topEntity_types.PacingOut7;
  -- spec.hs:609:9-81
  signal t_18                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_106                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_10                              : boolean;
  -- spec.hs:609:9-81
  signal f1_3                              : unsigned(7 downto 0);
  signal result_107                        : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$ds10_case_alt\                 : boolean;
  -- spec.hs:144:34-42
  signal x1_4                              : boolean;
  -- spec.hs:144:34-42
  signal x0_4                              : boolean;
  -- spec.hs:397:1-91
  signal pOut6                             : Spec_topEntity_types.PacingOut6;
  -- spec.hs:609:9-81
  signal t_19                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_108                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_11                              : boolean;
  -- spec.hs:609:9-81
  signal f1_4                              : unsigned(7 downto 0);
  signal result_109                        : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal \c$ds9_case_alt\                  : boolean;
  -- spec.hs:143:34-42
  signal x1_5                              : boolean;
  -- spec.hs:143:34-42
  signal x0_5                              : boolean;
  -- spec.hs:397:1-91
  signal pOut5                             : Spec_topEntity_types.PacingOut5;
  -- spec.hs:609:9-81
  signal t_20                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_110                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_12                              : boolean;
  -- spec.hs:609:9-81
  signal f1_5                              : unsigned(7 downto 0);
  signal result_111                        : unsigned(7 downto 0);
  -- spec.hs:140:34-42
  signal x_28                              : boolean;
  -- spec.hs:397:1-91
  signal pOut2                             : boolean;
  -- spec.hs:609:9-81
  signal t_21                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_112                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_13                              : boolean;
  -- spec.hs:609:9-81
  signal f1_6                              : unsigned(7 downto 0);
  signal result_113                        : unsigned(7 downto 0);
  -- spec.hs:138:34-42
  signal x_29                              : boolean;
  -- spec.hs:397:1-91
  signal pOut0                             : boolean;
  -- spec.hs:609:9-81
  signal t_22                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_114                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_14                              : boolean;
  -- spec.hs:609:9-81
  signal f1_7                              : unsigned(7 downto 0);
  signal result_115                        : unsigned(7 downto 0);
  -- spec.hs:146:34-42
  signal x_30                              : boolean;
  -- spec.hs:397:1-91
  signal pOut8                             : boolean;
  -- spec.hs:609:9-81
  signal t_23                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_116                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_15                              : boolean;
  -- spec.hs:609:9-81
  signal f1_8                              : unsigned(7 downto 0);
  signal result_117                        : unsigned(7 downto 0);
  -- spec.hs:142:34-42
  signal x_31                              : boolean;
  -- spec.hs:397:1-91
  signal pOut4                             : boolean;
  -- spec.hs:609:9-81
  signal t_24                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_118                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_16                              : boolean;
  -- spec.hs:609:9-81
  signal f1_9                              : unsigned(7 downto 0);
  signal result_119                        : unsigned(7 downto 0);
  -- spec.hs:141:34-42
  signal x_32                              : boolean;
  -- spec.hs:397:1-91
  signal pOut3                             : boolean;
  -- spec.hs:609:9-81
  signal t_25                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_120                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_17                              : boolean;
  -- spec.hs:609:9-81
  signal f1_10                             : unsigned(7 downto 0);
  signal result_121                        : unsigned(7 downto 0);
  -- spec.hs:139:34-42
  signal x_33                              : boolean;
  -- spec.hs:397:1-91
  signal pOut1                             : boolean;
  -- spec.hs:609:9-81
  signal t_26                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_122                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_18                              : boolean;
  -- spec.hs:609:9-81
  signal f1_11                             : unsigned(7 downto 0);
  signal result_123                        : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal pIn1_0                            : boolean;
  -- spec.hs:609:9-81
  signal t_27                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_124                        : unsigned(7 downto 0);
  -- spec.hs:609:9-81
  signal b_19                              : boolean;
  -- spec.hs:609:9-81
  signal f1_12                             : unsigned(7 downto 0);
  signal result_125                        : unsigned(7 downto 0);
  -- spec.hs:397:1-91
  signal pIn0_0                            : boolean;
  -- spec.hs:397:1-91
  signal pacings                           : Spec_topEntity_types.Pacings_1;
  -- spec.hs:616:1-81
  signal toWait                            : signed(63 downto 0) := (to_signed(0,64));
  signal result_126                        : boolean;
  signal result_127                        : signed(63 downto 0);
  -- spec.hs:616:1-81
  signal b_20                              : boolean;
  signal result_128                        : signed(63 downto 0);
  signal x_34                              : signed(63 downto 0);
  signal result_129                        : boolean;
  -- spec.hs:397:1-91
  signal b_21                              : boolean;
  signal result_130                        : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_43\                    : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false ) )
, Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
, Slides_1_sel1_slide1 => false )
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingIn1 => false
, Pacings_1_sel2_pacingOut0 => false
, Pacings_1_sel3_pacingOut1 => false
, Pacings_1_sel4_pacingOut2 => false
, Pacings_1_sel5_pacingOut3 => false
, Pacings_1_sel6_pacingOut4 => false
, Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
, PacingOut5_sel1_boolean_1 => false )
, Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
, PacingOut6_sel1_boolean_1 => false )
, Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
, PacingOut7_sel1_boolean_1 => false )
, Pacings_1_sel10_pacingOut8 => false
, Pacings_1_sel11_pacingOut9 => false
, Pacings_1_sel12_pacingOut10 => false
, Pacings_1_sel13_pacingOut11 => false ) );
  signal \c$case_alt\                      : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_44\                    : boolean := false;
  signal \c$case_alt_5\                    : boolean;
  signal \c$case_alt_6\                    : boolean;
  signal \c$case_alt_7\                    : boolean;
  signal \c$case_alt_8\                    : boolean;
  signal \c$case_alt_9\                    : boolean;
  signal \c$app_arg_45\                    : boolean := false;
  signal \c$case_alt_10\                   : boolean;
  signal \c$case_alt_11\                   : boolean;
  signal \c$case_alt_12\                   : boolean;
  signal \c$app_arg_46\                    : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal \buffer\                          : Spec_topEntity_types.array_of_Tuple3_0(0 to 7) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3 => false
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                       , PacingOut5_sel1_boolean_1 => false )
                                       , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                       , PacingOut6_sel1_boolean_1 => false )
                                       , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                       , PacingOut7_sel1_boolean_1 => false )
                                       , Pacings_1_sel10_pacingOut8 => false
                                       , Pacings_1_sel11_pacingOut9 => false
                                       , Pacings_1_sel12_pacingOut10 => false
                                       , Pacings_1_sel13_pacingOut11 => false ) ) );
  -- spec.hs:233:1-78
  signal \c$buffer_case_alt\               : Spec_topEntity_types.array_of_Tuple3_0(0 to 7);
  -- spec.hs:233:1-78
  signal \c$buffer_case_alt_0\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 7);
  -- spec.hs:233:1-78
  signal \c$buffer_case_alt_1\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 7);
  -- spec.hs:233:1-78
  signal qData                             : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:233:1-78
  signal x_35                              : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal cursor                            : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:233:1-78
  signal \c$cursor_case_alt\               : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal push                              : boolean;
  -- spec.hs:233:1-78
  signal \c$cursor_case_alt_0\             : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal \c$cursor_case_alt_1\             : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal \c$cursor_case_scrut\             : boolean;
  -- spec.hs:233:1-78
  signal \c$cursor_app_arg\                : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal \c$cursor_case_alt_2\             : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal pop                               : boolean;
  -- spec.hs:233:1-78
  signal \c$cursor_case_alt_3\             : signed(63 downto 0);
  -- spec.hs:233:1-78
  signal \c$cursor_case_alt_4\             : signed(63 downto 0);
  signal inputs                            : Spec_topEntity_types.Inputs_3;
  signal \c$vec2\                          : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal x_projection_2                    : signed(63 downto 0);
  signal \c$vec\                           : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_0\          : Spec_topEntity_types.Tuple2_5;
  signal \c$vec_0\                         : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_2\          : Spec_topEntity_types.Tuple2_5;
  signal result_selection_res              : boolean;
  signal x_projection_4                    : signed(63 downto 0);
  signal x_projection_5                    : signed(63 downto 0);
  signal \c$out5_case_alt_selection_res\   : boolean;
  signal result_selection_res_4            : boolean;
  signal x_projection_7                    : signed(63 downto 0);
  signal \c$out7_case_alt_selection_res\   : boolean;
  signal \c$vec_1\                         : Spec_topEntity_types.array_of_signed_64(0 to 1);
  signal x_projection_9                    : Spec_topEntity_types.array_of_signed_64(0 to 0);
  signal \c$vec_2\                         : Spec_topEntity_types.array_of_signed_64(0 to 1);
  signal y_projection_6                    : Spec_topEntity_types.array_of_signed_64(0 to 0);
  signal x_projection_10                   : signed(63 downto 0);
  signal \c$t_case_alt_selection_3\        : boolean;
  signal \c$vec_3\                         : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_6\          : Spec_topEntity_types.Tuple2_5;
  signal \c$vec_4\                         : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_8\          : Spec_topEntity_types.Tuple2_5;
  signal \c$t_case_alt_selection_6\        : boolean;
  signal \c$out7_case_alt_selection_res_0\ : boolean;
  signal \c$out5_case_alt_selection_res_0\ : boolean;
  signal \c$out5_case_alt_selection_res_1\ : boolean;
  signal \c$out5_case_alt_selection_res_2\ : boolean;
  signal result_selection_res_5            : boolean;
  signal result_selection_res_6            : boolean;
  signal x_projection_13                   : signed(63 downto 0);
  signal \c$out6_case_alt_selection_res\   : boolean;
  signal \c$out6_case_alt_selection_res_0\ : boolean;
  signal \c$out6_case_alt_selection_res_1\ : boolean;
  signal result_selection_13               : boolean;
  signal x_projection_17                   : signed(63 downto 0);
  signal \c$out8_case_alt_selection_res\   : boolean;
  signal \c$out8_case_alt_selection_res_0\ : boolean;
  signal result_selection_res_7            : boolean;
  signal result_selection_18               : boolean;
  signal \c$out0_case_alt_selection_res\   : boolean;
  signal result_selection_21               : boolean;
  signal result_selection_res_8            : boolean;
  signal \c$vec2_0\                        : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal result_selection_res_9            : boolean;
  signal result_selection_27               : boolean;
  signal \c$vec2_1\                        : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal result_selection_res_10           : boolean;
  signal result_selection_32               : boolean;
  signal \c$vec_5\                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 2);
  signal t_projection_2                    : Spec_topEntity_types.Tuple2_6;
  signal \c$out4_case_alt_selection_res\   : boolean;
  signal result_selection_35               : boolean;
  signal \c$out1_case_alt_selection_res\   : boolean;
  signal \c$vec2_2\                        : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal result_selection_res_11           : boolean;
  signal \c$vec_6\                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 3);
  signal t_projection_4                    : Spec_topEntity_types.Tuple2_7;
  signal \c$out10_case_alt_selection_res\  : boolean;
  signal \c$out9_case_alt_selection_res\   : boolean;
  signal result_selection_res_12           : boolean;
  signal result_selection_44               : boolean;
  signal result_selection_48               : boolean;
  signal result_selection_52               : boolean;
  signal result_selection_62               : boolean;
  signal result_selection_66               : boolean;
  signal result_selection_70               : boolean;
  signal result_selection_74               : boolean;
  signal result_selection_78               : boolean;
  signal result_selection_82               : boolean;
  signal result_selection_86               : boolean;
  signal result_selection_90               : boolean;
  signal \c$vec_7\                         : Spec_topEntity_types.array_of_Tuple3_0(0 to 8);
  signal \c$buffer_case_alt_sel_alt_3\     : Spec_topEntity_types.Tuple2_8;
  signal \c$vec_8\                         : Spec_topEntity_types.array_of_Tuple3_0(0 to 8);
  signal \c$buffer_case_alt_sel_alt_5\     : Spec_topEntity_types.Tuple2_8;
  signal result                            : Spec_topEntity_types.Outputs;
  signal result_0                          : Spec_topEntity_types.ValidInt;
  signal result_1                          : Spec_topEntity_types.ValidInt;
  signal result_2                          : Spec_topEntity_types.ValidInt;
  signal result_3                          : Spec_topEntity_types.ValidInt;
  signal result_4                          : Spec_topEntity_types.ValidInt;
  signal result_5                          : Spec_topEntity_types.ValidInt;
  signal result_6                          : Spec_topEntity_types.ValidInt;
  signal result_7                          : Spec_topEntity_types.ValidInt;
  signal result_8                          : Spec_topEntity_types.ValidInt;
  signal result_9                          : Spec_topEntity_types.ValidInt;
  signal result_10                         : Spec_topEntity_types.ValidInt;
  signal result_11                         : Spec_topEntity_types.ValidInt;

begin
  inputs <= ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => inputs_0_0
            , ValidInt_sel1_valid => inputs_0_1 )
            , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => inputs_1_0
            , ValidInt_sel1_valid => inputs_1_1 ) );

  result_12 <= to_signed(2000,64) when timer1Over else
               (x + to_signed(2000,64));

  x <= result_13;

  -- register begin
  result_13_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_13 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_13 <= result_12;
      end if;
    end if;
  end process;
  -- register end

  result_14 <= to_signed(2000,64) when timer0Over else
               (x_0 + to_signed(2000,64));

  x_0 <= result_15;

  -- register begin
  result_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_15 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_15 <= result_14;
      end if;
    end if;
  end process;
  -- register end

  result_16 <= ( Tuple2_sel0_boolean => inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid or (inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid or (timer0Over or timer1Over))
               , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_Inputs_3 => inputs
               , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => timer0Over
               , Slides_1_sel1_slide1 => timer1Over )
               , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
               , Pacings_1_sel1_pacingIn1 => pIn1
               , Pacings_1_sel2_pacingOut0 => pIn0
               , Pacings_1_sel3_pacingOut1 => pIn0
               , Pacings_1_sel4_pacingOut2 => pIn0
               , Pacings_1_sel5_pacingOut3 => pIn0
               , Pacings_1_sel6_pacingOut4 => pIn0
               , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => pIn0
               , PacingOut5_sel1_boolean_1 => pIn1 )
               , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => pIn0
               , PacingOut6_sel1_boolean_1 => pIn1 )
               , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => pIn0
               , PacingOut7_sel1_boolean_1 => pIn1 )
               , Pacings_1_sel10_pacingOut8 => pIn1
               , Pacings_1_sel11_pacingOut9 => timer0Over
               , Pacings_1_sel12_pacingOut10 => timer0Over
               , Pacings_1_sel13_pacingOut11 => timer1Over ) ) );

  pIn0 <= inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid;

  pIn1 <= inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid;

  timer0Over <= result_15 >= (to_signed(1000000,64));

  timer1Over <= result_13 >= (to_signed(2000000,64));

  result_17 <= ( Tuple2_0_sel0_boolean => result_126 and (not result_129)
               , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => dta_4
               , ValidInt_sel1_valid => result_31 )
               , Outputs_sel1_output1 => ( ValidInt_sel0_value => winData_2
               , ValidInt_sel1_valid => result_30 )
               , Outputs_sel2_output2 => ( ValidInt_sel0_value => winData_1
               , ValidInt_sel1_valid => result_29 )
               , Outputs_sel3_output3 => ( ValidInt_sel0_value => result_26
               , ValidInt_sel1_valid => result_27 )
               , Outputs_sel4_output4 => ( ValidInt_sel0_value => result_68.Tuple2_2_sel1_signed
               , ValidInt_sel1_valid => result_25 )
               , Outputs_sel5_output5 => ( ValidInt_sel0_value => winData
               , ValidInt_sel1_valid => result_24 )
               , Outputs_sel6_output6 => ( ValidInt_sel0_value => dta_1
               , ValidInt_sel1_valid => result_23 )
               , Outputs_sel7_output7 => ( ValidInt_sel0_value => dta
               , ValidInt_sel1_valid => result_22 )
               , Outputs_sel8_output8 => ( ValidInt_sel0_value => dta_2
               , ValidInt_sel1_valid => result_21 )
               , Outputs_sel9_output9 => ( ValidInt_sel0_value => winData_3
               , ValidInt_sel1_valid => result_20 )
               , Outputs_sel10_output10 => ( ValidInt_sel0_value => dta_3
               , ValidInt_sel1_valid => result_19 )
               , Outputs_sel11_output11 => ( ValidInt_sel0_value => dta_0
               , ValidInt_sel1_valid => result_18 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
    signal iterateI_ho1_5_res  : boolean;
    signal iterateI_ho1_6_res  : boolean;
    signal iterateI_ho1_7_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut11;

    r_block_193 : block
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

    r_block_194 : block
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

    r_block_195 : block
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

    r_block_196 : block
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

    r_block_197 : block
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

    r_block_198 : block
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

    r_block_199 : block
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

    r_block_200 : block
        signal \c$bb_res_res_6\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_6_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res <= \c$bb_res_res_6\;


      end block;

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut11
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res
                                                         , iterateI_ho1_4_res
                                                         , iterateI_ho1_5_res
                                                         , iterateI_ho1_6_res
                                                         , iterateI_ho1_7_res );


  end block;

  result_18 <=  \c$app_arg\(\c$app_arg\'high) ;

  iterateI_0 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_0  : boolean;
    signal iterateI_ho1_1_res_0  : boolean;
    signal iterateI_ho1_2_res_0  : boolean;
    signal iterateI_ho1_3_res_0  : boolean;
    signal iterateI_ho1_4_res_0  : boolean;
    signal iterateI_ho1_5_res_0  : boolean;
    signal iterateI_ho1_6_res_0  : boolean;
    signal iterateI_ho1_7_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= pOut10;

    r_block_201 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_7\;


      end block;

    r_block_202 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_0_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_8\;


      end block;

    r_block_203 : block
        signal \c$bb_res_res_9\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_1_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_9\;


      end block;

    r_block_204 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_2_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_10\;


      end block;

    r_block_205 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= iterateI_ho1_3_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_0 <= \c$bb_res_res_11\;


      end block;

    r_block_206 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_4_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_0 <= \c$bb_res_res_12\;


      end block;

    r_block_207 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_5_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_0 <= \c$bb_res_res_13\;


      end block;

    r_block_208 : block
        signal \c$bb_res_res_14\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_6_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_0 <= \c$bb_res_res_14\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( pOut10
                                                           , iterateI_ho1_0_res_0
                                                           , iterateI_ho1_1_res_0
                                                           , iterateI_ho1_2_res_0
                                                           , iterateI_ho1_3_res_0
                                                           , iterateI_ho1_4_res_0
                                                           , iterateI_ho1_5_res_0
                                                           , iterateI_ho1_6_res_0
                                                           , iterateI_ho1_7_res_0 );


  end block;

  result_19 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_1  : boolean;
    signal iterateI_ho1_1_res_1  : boolean;
    signal iterateI_ho1_2_res_1  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
    signal iterateI_ho1_4_res_1  : boolean;
    signal iterateI_ho1_5_res_1  : boolean;
    signal iterateI_ho1_6_res_1  : boolean;
    signal iterateI_ho1_7_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= pOut9;

    r_block_209 : block
        signal \c$bb_res_res_15\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_15\;


      end block;

    r_block_210 : block
        signal \c$bb_res_res_16\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_16\;


      end block;

    r_block_211 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_17\;


      end block;

    r_block_212 : block
        signal \c$bb_res_res_18\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= iterateI_ho1_2_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_18\;


      end block;

    r_block_213 : block
        signal \c$bb_res_res_19\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_3_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_1 <= \c$bb_res_res_19\;


      end block;

    r_block_214 : block
        signal \c$bb_res_res_20\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_4_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_1 <= \c$bb_res_res_20\;


      end block;

    r_block_215 : block
        signal \c$bb_res_res_21\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_5_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_1 <= \c$bb_res_res_21\;


      end block;

    r_block_216 : block
        signal \c$bb_res_res_22\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_6_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_1 <= \c$bb_res_res_22\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( pOut9
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_1
                                                           , iterateI_ho1_3_res_1
                                                           , iterateI_ho1_4_res_1
                                                           , iterateI_ho1_5_res_1
                                                           , iterateI_ho1_6_res_1
                                                           , iterateI_ho1_7_res_1 );


  end block;

  result_20 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_arg0_2 : boolean;
    signal iterateI_ho1_0_res_2  : boolean;
    signal iterateI_ho1_1_res_2  : boolean;
    signal iterateI_ho1_2_res_2  : boolean;
    signal iterateI_ho1_3_res_2  : boolean;
    signal iterateI_ho1_4_res_2  : boolean;
    signal iterateI_ho1_5_res_2  : boolean;
    signal iterateI_ho1_6_res_2  : boolean;
    signal iterateI_ho1_7_res_2  : boolean;
  begin
    iterateI_ho1_0_arg0_2 <= x_30;

    r_block_217 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_23\;


      end block;

    r_block_218 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_24\;


      end block;

    r_block_219 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_2 <= \c$bb_res_res_25\;


      end block;

    r_block_220 : block
        signal \c$bb_res_res_26\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_2_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_26\;


      end block;

    r_block_221 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_3_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_2 <= \c$bb_res_res_27\;


      end block;

    r_block_222 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_4_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_2 <= \c$bb_res_res_28\;


      end block;

    r_block_223 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= iterateI_ho1_5_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_2 <= \c$bb_res_res_29\;


      end block;

    r_block_224 : block
        signal \c$bb_res_res_30\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_6_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_2 <= \c$bb_res_res_30\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( x_30
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_2
                                                           , iterateI_ho1_3_res_2
                                                           , iterateI_ho1_4_res_2
                                                           , iterateI_ho1_5_res_2
                                                           , iterateI_ho1_6_res_2
                                                           , iterateI_ho1_7_res_2 );


  end block;

  result_21 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  iterateI_3 : block
    signal iterateI_ho1_0_res_3 : boolean;
    signal iterateI_ho1_1_res_3 : boolean;
    signal iterateI_ho1_2_res_3 : boolean;
    signal iterateI_ho1_3_res_3 : boolean;
    signal iterateI_ho1_4_res_3 : boolean;
    signal iterateI_ho1_5_res_3 : boolean;
    signal iterateI_ho1_6_res_3 : boolean;
    signal iterateI_ho1_7_res_3 : boolean;
  begin
    r_block_225 : block
        signal \c$bb_res_res_31\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= \c$ds11_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_31\;


      end block;

    r_block_226 : block
        signal \c$bb_res_res_32\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_32\;


      end block;

    r_block_227 : block
        signal \c$bb_res_res_33\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_33_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_33\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_33\;


      end block;

    r_block_228 : block
        signal \c$bb_res_res_34\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_34_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_34\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_3 <= \c$bb_res_res_34\;


      end block;

    r_block_229 : block
        signal \c$bb_res_res_35\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_35_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_35\ <= iterateI_ho1_3_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_3 <= \c$bb_res_res_35\;


      end block;

    r_block_230 : block
        signal \c$bb_res_res_36\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_36_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_36\ <= iterateI_ho1_4_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_3 <= \c$bb_res_res_36\;


      end block;

    r_block_231 : block
        signal \c$bb_res_res_37\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_37_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_37\ <= iterateI_ho1_5_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_3 <= \c$bb_res_res_37\;


      end block;

    r_block_232 : block
        signal \c$bb_res_res_38\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_38_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_38\ <= iterateI_ho1_6_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_3 <= \c$bb_res_res_38\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( \c$ds11_case_alt\
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_3
                                                           , iterateI_ho1_4_res_3
                                                           , iterateI_ho1_5_res_3
                                                           , iterateI_ho1_6_res_3
                                                           , iterateI_ho1_7_res_3 );


  end block;

  result_22 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  iterateI_4 : block
    signal iterateI_ho1_0_res_4 : boolean;
    signal iterateI_ho1_1_res_4 : boolean;
    signal iterateI_ho1_2_res_4 : boolean;
    signal iterateI_ho1_3_res_4 : boolean;
    signal iterateI_ho1_4_res_4 : boolean;
    signal iterateI_ho1_5_res_4 : boolean;
    signal iterateI_ho1_6_res_4 : boolean;
    signal iterateI_ho1_7_res_4 : boolean;
  begin
    r_block_233 : block
        signal \c$bb_res_res_39\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_39_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_39\ <= \c$ds10_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_39\;


      end block;

    r_block_234 : block
        signal \c$bb_res_res_40\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_40_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_40\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_40\;


      end block;

    r_block_235 : block
        signal \c$bb_res_res_41\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_41_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_41\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_41\;


      end block;

    r_block_236 : block
        signal \c$bb_res_res_42\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_42_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_42\ <= iterateI_ho1_2_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_4 <= \c$bb_res_res_42\;


      end block;

    r_block_237 : block
        signal \c$bb_res_res_43\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_43_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_43\ <= iterateI_ho1_3_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_4 <= \c$bb_res_res_43\;


      end block;

    r_block_238 : block
        signal \c$bb_res_res_44\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_44_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_44\ <= iterateI_ho1_4_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_4 <= \c$bb_res_res_44\;


      end block;

    r_block_239 : block
        signal \c$bb_res_res_45\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_45_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_45\ <= iterateI_ho1_5_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_4 <= \c$bb_res_res_45\;


      end block;

    r_block_240 : block
        signal \c$bb_res_res_46\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_46_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_46\ <= iterateI_ho1_6_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_4 <= \c$bb_res_res_46\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( \c$ds10_case_alt\
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_4
                                                           , iterateI_ho1_3_res_4
                                                           , iterateI_ho1_4_res_4
                                                           , iterateI_ho1_5_res_4
                                                           , iterateI_ho1_6_res_4
                                                           , iterateI_ho1_7_res_4 );


  end block;

  result_23 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_res_5 : boolean;
    signal iterateI_ho1_1_res_5 : boolean;
    signal iterateI_ho1_2_res_5 : boolean;
    signal iterateI_ho1_3_res_5 : boolean;
    signal iterateI_ho1_4_res_5 : boolean;
    signal iterateI_ho1_5_res_5 : boolean;
    signal iterateI_ho1_6_res_5 : boolean;
    signal iterateI_ho1_7_res_5 : boolean;
  begin
    r_block_241 : block
        signal \c$bb_res_res_47\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_47_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_47\ <= \c$ds9_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_47\;


      end block;

    r_block_242 : block
        signal \c$bb_res_res_48\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_48_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_48\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_48\;


      end block;

    r_block_243 : block
        signal \c$bb_res_res_49\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_49_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_49\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_49\;


      end block;

    r_block_244 : block
        signal \c$bb_res_res_50\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_50_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_50\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_5 <= \c$bb_res_res_50\;


      end block;

    r_block_245 : block
        signal \c$bb_res_res_51\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_51_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_51\ <= iterateI_ho1_3_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_5 <= \c$bb_res_res_51\;


      end block;

    r_block_246 : block
        signal \c$bb_res_res_52\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_52_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_52\ <= iterateI_ho1_4_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_5 <= \c$bb_res_res_52\;


      end block;

    r_block_247 : block
        signal \c$bb_res_res_53\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_53_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_53\ <= iterateI_ho1_5_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_5 <= \c$bb_res_res_53\;


      end block;

    r_block_248 : block
        signal \c$bb_res_res_54\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_54_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_54\ <= iterateI_ho1_6_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_5 <= \c$bb_res_res_54\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( \c$ds9_case_alt\
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_5
                                                           , iterateI_ho1_4_res_5
                                                           , iterateI_ho1_5_res_5
                                                           , iterateI_ho1_6_res_5
                                                           , iterateI_ho1_7_res_5 );


  end block;

  result_24 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  iterateI_6 : block
    signal iterateI_ho1_0_arg0_3 : boolean;
    signal iterateI_ho1_0_res_6  : boolean;
    signal iterateI_ho1_1_res_6  : boolean;
    signal iterateI_ho1_2_res_6  : boolean;
    signal iterateI_ho1_3_res_6  : boolean;
    signal iterateI_ho1_4_res_6  : boolean;
    signal iterateI_ho1_5_res_6  : boolean;
    signal iterateI_ho1_6_res_6  : boolean;
    signal iterateI_ho1_7_res_6  : boolean;
  begin
    iterateI_ho1_0_arg0_3 <= x_31;

    r_block_249 : block
        signal \c$bb_res_res_55\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_55_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_55\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_55\;


      end block;

    r_block_250 : block
        signal \c$bb_res_res_56\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_56_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_56\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_56\;


      end block;

    r_block_251 : block
        signal \c$bb_res_res_57\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_57_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_57\ <= iterateI_ho1_1_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_6 <= \c$bb_res_res_57\;


      end block;

    r_block_252 : block
        signal \c$bb_res_res_58\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_58_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_58\ <= iterateI_ho1_2_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_6 <= \c$bb_res_res_58\;


      end block;

    r_block_253 : block
        signal \c$bb_res_res_59\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_59_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_59\ <= iterateI_ho1_3_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_6 <= \c$bb_res_res_59\;


      end block;

    r_block_254 : block
        signal \c$bb_res_res_60\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_60_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_60\ <= iterateI_ho1_4_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_6 <= \c$bb_res_res_60\;


      end block;

    r_block_255 : block
        signal \c$bb_res_res_61\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_61_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_61\ <= iterateI_ho1_5_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_6 <= \c$bb_res_res_61\;


      end block;

    r_block_256 : block
        signal \c$bb_res_res_62\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_62_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_62\ <= iterateI_ho1_6_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_6 <= \c$bb_res_res_62\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( x_31
                                                           , iterateI_ho1_0_res_6
                                                           , iterateI_ho1_1_res_6
                                                           , iterateI_ho1_2_res_6
                                                           , iterateI_ho1_3_res_6
                                                           , iterateI_ho1_4_res_6
                                                           , iterateI_ho1_5_res_6
                                                           , iterateI_ho1_6_res_6
                                                           , iterateI_ho1_7_res_6 );


  end block;

  result_25 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_3 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_3) <= ( Tuple2_1_sel0_Tuple2_2 => result_72(i_3)
               , Tuple2_1_sel1_Maybe => \c$vec2\(i_3) );


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
    imap_0 : for i_4 in \c$ws_app_arg_0\'range generate
    begin
      fun_3 : block
      signal \c$case_alt_13\            : Spec_topEntity_types.Maybe;
      -- spec.hs:363:1-63
      signal t_28                       : unsigned(7 downto 0);
      -- spec.hs:363:1-63
      signal x_37                       : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_4) <= \c$case_alt_13\;

      \c$case_alt_selection_res\ <= t_28 = result_28.Tags_sel5_output3_0;

      \c$case_alt_13\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_4,max(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res\ else
                         \c$ws_app_arg\(i_4).Tuple2_1_sel1_Maybe;

      t_28 <= x_37.Tuple2_2_sel0_unsigned;

      x_37 <= \c$ws_app_arg\(i_4).Tuple2_1_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec : block
    signal vec_index : integer range 0 to 2-1;
  begin
    vec_index <= to_integer((signed(std_logic_vector(resize(i,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut\ <= result_72(vec_index);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(1 downto 1)) select
    result_26 <= to_signed(0,64) when "0",
                 \c$case_scrut\.Tuple2_2_sel1_signed when others;

  i <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_0\(0 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_7 : block
    signal iterateI_ho1_0_arg0_4 : boolean;
    signal iterateI_ho1_0_res_7  : boolean;
    signal iterateI_ho1_1_res_7  : boolean;
    signal iterateI_ho1_2_res_7  : boolean;
    signal iterateI_ho1_3_res_7  : boolean;
    signal iterateI_ho1_4_res_7  : boolean;
    signal iterateI_ho1_5_res_7  : boolean;
    signal iterateI_ho1_6_res_7  : boolean;
    signal iterateI_ho1_7_res_7  : boolean;
  begin
    iterateI_ho1_0_arg0_4 <= x_32;

    r_block_257 : block
        signal \c$bb_res_res_63\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_63_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_63\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_63\;


      end block;

    r_block_258 : block
        signal \c$bb_res_res_64\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_64_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_64\ <= iterateI_ho1_0_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_64\;


      end block;

    r_block_259 : block
        signal \c$bb_res_res_65\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_65_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_65\ <= iterateI_ho1_1_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_7 <= \c$bb_res_res_65\;


      end block;

    r_block_260 : block
        signal \c$bb_res_res_66\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_66_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_66\ <= iterateI_ho1_2_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_7 <= \c$bb_res_res_66\;


      end block;

    r_block_261 : block
        signal \c$bb_res_res_67\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_67_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_67\ <= iterateI_ho1_3_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_7 <= \c$bb_res_res_67\;


      end block;

    r_block_262 : block
        signal \c$bb_res_res_68\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_68_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_68\ <= iterateI_ho1_4_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_7 <= \c$bb_res_res_68\;


      end block;

    r_block_263 : block
        signal \c$bb_res_res_69\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_69_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_69\ <= iterateI_ho1_5_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_7 <= \c$bb_res_res_69\;


      end block;

    r_block_264 : block
        signal \c$bb_res_res_70\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_70_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_70\ <= iterateI_ho1_6_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_7 <= \c$bb_res_res_70\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_boolean'( x_32
                                                           , iterateI_ho1_0_res_7
                                                           , iterateI_ho1_1_res_7
                                                           , iterateI_ho1_2_res_7
                                                           , iterateI_ho1_3_res_7
                                                           , iterateI_ho1_4_res_7
                                                           , iterateI_ho1_5_res_7
                                                           , iterateI_ho1_6_res_7
                                                           , iterateI_ho1_7_res_7 );


  end block;

  result_27 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  iterateI_8 : block
    signal iterateI_ho1_0_arg0_5 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_5_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_6_res_8  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_5 <= ( Tags_sel0_input0_0 => t_27
                             , Tags_sel1_input1_0 => t_26
                             , Tags_sel2_output0_0 => t_21
                             , Tags_sel3_output1_0 => t_25
                             , Tags_sel4_output2_0 => t_20
                             , Tags_sel5_output3_0 => t_24
                             , Tags_sel6_output4_0 => t_23
                             , Tags_sel7_output5_0 => t_19
                             , Tags_sel8_output6_0 => t_18
                             , Tags_sel9_output7_0 => t_17
                             , Tags_sel10_output8_0 => t_22
                             , Tags_sel11_output9_0 => t_16
                             , Tags_sel12_output10_0 => t_15
                             , Tags_sel13_output11_0 => t_14
                             , Tags_sel14_slide0_0 => t_19
                             , Tags_sel15_slide1_0 => t_25 );

    r_block_265 : block
        signal \c$bb_res_res_71\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_71_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_71\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_71\;


      end block;

    r_block_266 : block
        signal \c$bb_res_res_72\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_72_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_72\ <= iterateI_ho1_0_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_72\;


      end block;

    r_block_267 : block
        signal \c$bb_res_res_73\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_73_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_73\ <= iterateI_ho1_1_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_8 <= \c$bb_res_res_73\;


      end block;

    r_block_268 : block
        signal \c$bb_res_res_74\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_74_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_74\ <= iterateI_ho1_2_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_8 <= \c$bb_res_res_74\;


      end block;

    r_block_269 : block
        signal \c$bb_res_res_75\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_75_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_75\ <= iterateI_ho1_3_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_8 <= \c$bb_res_res_75\;


      end block;

    r_block_270 : block
        signal \c$bb_res_res_76\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_76_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_76\ <= iterateI_ho1_4_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_8 <= \c$bb_res_res_76\;


      end block;

    r_block_271 : block
        signal \c$bb_res_res_77\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_77_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_77\ <= iterateI_ho1_5_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_8 <= \c$bb_res_res_77\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_27
                                                        , Tags_sel1_input1_0 => t_26
                                                        , Tags_sel2_output0_0 => t_21
                                                        , Tags_sel3_output1_0 => t_25
                                                        , Tags_sel4_output2_0 => t_20
                                                        , Tags_sel5_output3_0 => t_24
                                                        , Tags_sel6_output4_0 => t_23
                                                        , Tags_sel7_output5_0 => t_19
                                                        , Tags_sel8_output6_0 => t_18
                                                        , Tags_sel9_output7_0 => t_17
                                                        , Tags_sel10_output8_0 => t_22
                                                        , Tags_sel11_output9_0 => t_16
                                                        , Tags_sel12_output10_0 => t_15
                                                        , Tags_sel13_output11_0 => t_14
                                                        , Tags_sel14_slide0_0 => t_19
                                                        , Tags_sel15_slide1_0 => t_25 )
                                                        , iterateI_ho1_0_res_8
                                                        , iterateI_ho1_1_res_8
                                                        , iterateI_ho1_2_res_8
                                                        , iterateI_ho1_3_res_8
                                                        , iterateI_ho1_4_res_8
                                                        , iterateI_ho1_5_res_8
                                                        , iterateI_ho1_6_res_8 );


  end block;

  result_28 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_arg0_6 : boolean;
    signal iterateI_ho1_0_res_9  : boolean;
    signal iterateI_ho1_1_res_9  : boolean;
    signal iterateI_ho1_2_res_9  : boolean;
    signal iterateI_ho1_3_res_9  : boolean;
    signal iterateI_ho1_4_res_9  : boolean;
    signal iterateI_ho1_5_res_9  : boolean;
    signal iterateI_ho1_6_res_9  : boolean;
    signal iterateI_ho1_7_res_8  : boolean;
  begin
    iterateI_ho1_0_arg0_6 <= x_28;

    r_block_272 : block
        signal \c$bb_res_res_78\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_78_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_78\ <= iterateI_ho1_0_arg0_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_78\;


      end block;

    r_block_273 : block
        signal \c$bb_res_res_79\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_79_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_79\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_9 <= \c$bb_res_res_79\;


      end block;

    r_block_274 : block
        signal \c$bb_res_res_80\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_80_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_80\ <= iterateI_ho1_1_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_9 <= \c$bb_res_res_80\;


      end block;

    r_block_275 : block
        signal \c$bb_res_res_81\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_81_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_81\ <= iterateI_ho1_2_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_9 <= \c$bb_res_res_81\;


      end block;

    r_block_276 : block
        signal \c$bb_res_res_82\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_82_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_82\ <= iterateI_ho1_3_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_9 <= \c$bb_res_res_82\;


      end block;

    r_block_277 : block
        signal \c$bb_res_res_83\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_83_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_83\ <= iterateI_ho1_4_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_9 <= \c$bb_res_res_83\;


      end block;

    r_block_278 : block
        signal \c$bb_res_res_84\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_84_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_84\ <= iterateI_ho1_5_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_9 <= \c$bb_res_res_84\;


      end block;

    r_block_279 : block
        signal \c$bb_res_res_85\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_85_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_85\ <= iterateI_ho1_6_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_8 <= \c$bb_res_res_85\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_boolean'( x_28
                                                           , iterateI_ho1_0_res_9
                                                           , iterateI_ho1_1_res_9
                                                           , iterateI_ho1_2_res_9
                                                           , iterateI_ho1_3_res_9
                                                           , iterateI_ho1_4_res_9
                                                           , iterateI_ho1_5_res_9
                                                           , iterateI_ho1_6_res_9
                                                           , iterateI_ho1_7_res_8 );


  end block;

  result_29 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  iterateI_10 : block
    signal iterateI_ho1_0_arg0_7 : boolean;
    signal iterateI_ho1_0_res_10 : boolean;
    signal iterateI_ho1_1_res_10 : boolean;
    signal iterateI_ho1_2_res_10 : boolean;
    signal iterateI_ho1_3_res_10 : boolean;
    signal iterateI_ho1_4_res_10 : boolean;
    signal iterateI_ho1_5_res_10 : boolean;
    signal iterateI_ho1_6_res_10 : boolean;
    signal iterateI_ho1_7_res_9  : boolean;
  begin
    iterateI_ho1_0_arg0_7 <= x_33;

    r_block_280 : block
        signal \c$bb_res_res_86\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_86_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_86\ <= iterateI_ho1_0_arg0_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_86\;


      end block;

    r_block_281 : block
        signal \c$bb_res_res_87\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_87_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_87\ <= iterateI_ho1_0_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_10 <= \c$bb_res_res_87\;


      end block;

    r_block_282 : block
        signal \c$bb_res_res_88\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_88_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_88\ <= iterateI_ho1_1_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_10 <= \c$bb_res_res_88\;


      end block;

    r_block_283 : block
        signal \c$bb_res_res_89\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_89_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_89\ <= iterateI_ho1_2_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_10 <= \c$bb_res_res_89\;


      end block;

    r_block_284 : block
        signal \c$bb_res_res_90\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_90_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_90\ <= iterateI_ho1_3_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_10 <= \c$bb_res_res_90\;


      end block;

    r_block_285 : block
        signal \c$bb_res_res_91\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_91_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_91\ <= iterateI_ho1_4_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_10 <= \c$bb_res_res_91\;


      end block;

    r_block_286 : block
        signal \c$bb_res_res_92\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_92_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_92\ <= iterateI_ho1_5_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_10 <= \c$bb_res_res_92\;


      end block;

    r_block_287 : block
        signal \c$bb_res_res_93\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_93_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_93\ <= iterateI_ho1_6_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_9 <= \c$bb_res_res_93\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_boolean'( x_33
                                                            , iterateI_ho1_0_res_10
                                                            , iterateI_ho1_1_res_10
                                                            , iterateI_ho1_2_res_10
                                                            , iterateI_ho1_3_res_10
                                                            , iterateI_ho1_4_res_10
                                                            , iterateI_ho1_5_res_10
                                                            , iterateI_ho1_6_res_10
                                                            , iterateI_ho1_7_res_9 );


  end block;

  result_30 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  iterateI_11 : block
    signal iterateI_ho1_0_arg0_8 : boolean;
    signal iterateI_ho1_0_res_11 : boolean;
    signal iterateI_ho1_1_res_11 : boolean;
    signal iterateI_ho1_2_res_11 : boolean;
    signal iterateI_ho1_3_res_11 : boolean;
    signal iterateI_ho1_4_res_11 : boolean;
    signal iterateI_ho1_5_res_11 : boolean;
    signal iterateI_ho1_6_res_11 : boolean;
    signal iterateI_ho1_7_res_10 : boolean;
  begin
    iterateI_ho1_0_arg0_8 <= x_29;

    r_block_288 : block
        signal \c$bb_res_res_94\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_94_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_94\ <= iterateI_ho1_0_arg0_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_11 <= \c$bb_res_res_94\;


      end block;

    r_block_289 : block
        signal \c$bb_res_res_95\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_95_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_95\ <= iterateI_ho1_0_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_11 <= \c$bb_res_res_95\;


      end block;

    r_block_290 : block
        signal \c$bb_res_res_96\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_96_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_96\ <= iterateI_ho1_1_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_11 <= \c$bb_res_res_96\;


      end block;

    r_block_291 : block
        signal \c$bb_res_res_97\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_97_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_97\ <= iterateI_ho1_2_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_11 <= \c$bb_res_res_97\;


      end block;

    r_block_292 : block
        signal \c$bb_res_res_98\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_98_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_98\ <= iterateI_ho1_3_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_11 <= \c$bb_res_res_98\;


      end block;

    r_block_293 : block
        signal \c$bb_res_res_99\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_99_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_99\ <= iterateI_ho1_4_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_11 <= \c$bb_res_res_99\;


      end block;

    r_block_294 : block
        signal \c$bb_res_res_100\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_100_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_100\ <= iterateI_ho1_5_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_11 <= \c$bb_res_res_100\;


      end block;

    r_block_295 : block
        signal \c$bb_res_res_101\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_101_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_101\ <= iterateI_ho1_6_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_10 <= \c$bb_res_res_101\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_boolean'( x_29
                                                            , iterateI_ho1_0_res_11
                                                            , iterateI_ho1_1_res_11
                                                            , iterateI_ho1_2_res_11
                                                            , iterateI_ho1_3_res_11
                                                            , iterateI_ho1_4_res_11
                                                            , iterateI_ho1_5_res_11
                                                            , iterateI_ho1_6_res_11
                                                            , iterateI_ho1_7_res_10 );


  end block;

  result_31 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  y <= winData;

  x_projection_2 <=  win(0) ;

  x_1 <= x_projection_2;

  win <= window.Tuple2_3_sel1_array_of_signed_64;

  -- register begin
  window_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window <= ( Tuple2_3_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window <= result_32;
      end if;
    end if;
  end process;
  -- register end

  result_32 <= t when b else
               window;

  t <= ( Tuple2_3_sel0_unsigned => result_93.Tags_sel14_slide0_0
       , Tuple2_3_sel1_array_of_signed_64 => result_33 );

  result_33 <= \c$t_case_alt\ when result_47 else
               \c$t_case_alt_0\;

  \c$vec\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec\(0 to 2-1),\c$vec\(2 to \c$vec\'high));

  \c$vec_0\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_0\(0 to 2-1),\c$vec_0\(2 to \c$vec_0\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_5_sel0_array_of_signed_64_0 when \c$b_case_alt\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_5_sel0_array_of_signed_64_0;

  \c$t_case_alt_0\ <= lastBucketUpdated when \c$b_case_alt\ else
                      win;

  -- replace begin
  replaceVec : block
    signal vec_index_0 : integer range 0 to 2-1;
  begin
    vec_index_0 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    process(vec_index_0,win,x_1,y)
      variable ivec : Spec_topEntity_types.array_of_signed_64(0 to 1);
    begin
      ivec := win;
      ivec(vec_index_0) := ((x_1 + y));
      lastBucketUpdated <= ivec;
    end process;
  end block;
  -- replace end

  b <= \c$b_case_alt\ or result_47;

  \c$b_case_alt\ <= x0 and x1;

  x1 <= result_48.PacingOut5_sel1_boolean_1;

  x0 <= result_48.PacingOut5_sel0_boolean_0;

  winData <= result_35.Tuple2_2_sel1_signed;

  result_selection_res <= x0_0 and x1_0;

  result_34 <= t_0 when result_selection_res else
               result_35;

  x0_0 <= result_46.PacingOut5_sel0_boolean_0;

  x1_0 <= result_46.PacingOut5_sel1_boolean_1;

  -- register begin
  result_35_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_35 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_35 <= result_34;
      end if;
    end if;
  end process;
  -- register end

  t_0 <= ( Tuple2_2_sel0_unsigned => result_94.Tags_sel7_output5_0
         , Tuple2_2_sel1_signed => (x_4 + y_2) );

  x_2 <= \c$out5_case_alt_2\;

  y_0 <= \c$out5_case_alt_1\;

  x_projection_4 <= (x_2 + y_0);

  x_3 <= x_projection_4;

  x_projection_5 <= (x_3 + y_1);

  x_4 <= x_projection_5;

  y_1 <= \c$out5_case_alt_0\;

  y_2 <= \c$out5_case_alt\;

  \c$out5_case_alt_selection_res\ <= tag = tagToMatch_0;

  \c$out5_case_alt\ <= dta when \c$out5_case_alt_selection_res\ else
                       to_signed(0,64);

  dta <= result_37.Tuple2_2_sel1_signed;

  tag <= result_37.Tuple2_2_sel0_unsigned;

  result_selection_res_4 <= x0_1 and x1_1;

  result_36 <= t_1 when result_selection_res_4 else
               result_37;

  x0_1 <= result_44.PacingOut7_sel0_boolean_0;

  x1_1 <= result_44.PacingOut7_sel1_boolean_1;

  -- register begin
  result_37_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_37 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_37 <= result_36;
      end if;
    end if;
  end process;
  -- register end

  t_1 <= ( Tuple2_2_sel0_unsigned => result_95.Tags_sel9_output7_0
         , Tuple2_2_sel1_signed => (x_6 + y_4) );

  x_5 <= \c$out7_case_alt_1\;

  y_3 <= \c$out7_case_alt_0\;

  x_projection_7 <= (x_5 + y_3);

  x_6 <= x_projection_7;

  y_4 <= \c$out7_case_alt\;

  \c$out7_case_alt_selection_res\ <= tag_0 = to_unsigned(5,8);

  \c$out7_case_alt\ <= to_signed(0,64) when \c$out7_case_alt_selection_res\ else
                       dta_0;

  dta_0 <= result_39.Tuple2_2_sel1_signed;

  tag_0 <= result_39.Tuple2_2_sel0_unsigned;

  result_38 <= t_2 when b_0 else
               result_39;

  -- register begin
  result_39_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_39 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_39 <= result_38;
      end if;
    end if;
  end process;
  -- register end

  b_0 <= result_49;

  t_2 <= ( Tuple2_2_sel0_unsigned => result_88.Tags_sel13_output11_0
         , Tuple2_2_sel1_signed => (x_7 + y_5) );

  \c$vec_1\ <= window.Tuple2_3_sel1_array_of_signed_64;

  x_projection_9 <= \c$vec_1\(1 to \c$vec_1\'high);

  x_7 <= x_projection_9(0);

  \c$vec_2\ <= window_0.Tuple2_3_sel1_array_of_signed_64;

  y_projection_6 <= \c$vec_2\(1 to \c$vec_2\'high);

  y_5 <= y_projection_6(0);

  y_6 <= winData_2;

  x_projection_10 <=  win_0(0) ;

  x_8 <= x_projection_10;

  win_0 <= window_0.Tuple2_3_sel1_array_of_signed_64;

  -- register begin
  window_0_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_0 <= ( Tuple2_3_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window_0 <= result_40;
      end if;
    end if;
  end process;
  -- register end

  result_40 <= t_3 when b_1 else
               window_0;

  t_3 <= ( Tuple2_3_sel0_unsigned => result_93.Tags_sel15_slide1_0
         , Tuple2_3_sel1_array_of_signed_64 => result_41 );

  result_41 <= \c$t_case_alt_1\ when result_42 else
               \c$t_case_alt_2\;

  \c$t_case_alt_selection_3\ <= x_9;

  \c$vec_3\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_0)));

  \c$t_case_alt_sel_alt_6\ <= (\c$vec_3\(0 to 2-1),\c$vec_3\(2 to \c$vec_3\'high));

  \c$vec_4\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_0)));

  \c$t_case_alt_sel_alt_8\ <= (\c$vec_4\(0 to 2-1),\c$vec_4\(2 to \c$vec_4\'high));

  \c$t_case_alt_1\ <= \c$t_case_alt_sel_alt_8\.Tuple2_5_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_3\ else
                      \c$t_case_alt_sel_alt_6\.Tuple2_5_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_6\ <= x_9;

  \c$t_case_alt_2\ <= lastBucketUpdated_0 when \c$t_case_alt_selection_6\ else
                      win_0;

  -- replace begin
  replaceVec_0 : block
    signal vec_index_1 : integer range 0 to 2-1;
  begin
    vec_index_1 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    process(vec_index_1,win_0,x_8,y_6)
      variable ivec_0 : Spec_topEntity_types.array_of_signed_64(0 to 1);
    begin
      ivec_0 := win_0;
      ivec_0(vec_index_1) := ((x_8 + y_6));
      lastBucketUpdated_0 <= ivec_0;
    end process;
  end block;
  -- replace end

  b_1 <= x_9 or result_42;

  x_9 <= result_43;

  iterateI_12 : block
    signal iterateI_ho1_0_arg0_9 : boolean;
    signal iterateI_ho1_0_res_12 : boolean;
    signal iterateI_ho1_1_res_12 : boolean;
    signal iterateI_ho1_2_res_12 : boolean;
    signal iterateI_ho1_3_res_12 : boolean;
    signal iterateI_ho1_4_res_12 : boolean;
    signal iterateI_ho1_5_res_12 : boolean;
  begin
    iterateI_ho1_0_arg0_9 <= slides.Slides_1_sel1_slide1;

    r_block_296 : block
        signal \c$bb_res_res_102\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_102_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_102\ <= iterateI_ho1_0_arg0_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_12 <= \c$bb_res_res_102\;


      end block;

    r_block_297 : block
        signal \c$bb_res_res_103\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_103_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_103\ <= iterateI_ho1_0_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_12 <= \c$bb_res_res_103\;


      end block;

    r_block_298 : block
        signal \c$bb_res_res_104\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_104_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_104\ <= iterateI_ho1_1_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_12 <= \c$bb_res_res_104\;


      end block;

    r_block_299 : block
        signal \c$bb_res_res_105\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_105_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_105\ <= iterateI_ho1_2_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_12 <= \c$bb_res_res_105\;


      end block;

    r_block_300 : block
        signal \c$bb_res_res_106\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_106_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_106\ <= iterateI_ho1_3_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_12 <= \c$bb_res_res_106\;


      end block;

    r_block_301 : block
        signal \c$bb_res_res_107\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_107_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_107\ <= iterateI_ho1_4_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_12 <= \c$bb_res_res_107\;


      end block;

    \c$app_arg_12\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel1_slide1
                                                            , iterateI_ho1_0_res_12
                                                            , iterateI_ho1_1_res_12
                                                            , iterateI_ho1_2_res_12
                                                            , iterateI_ho1_3_res_12
                                                            , iterateI_ho1_4_res_12
                                                            , iterateI_ho1_5_res_12 );


  end block;

  result_42 <=  \c$app_arg_12\(\c$app_arg_12\'high) ;

  iterateI_13 : block
    signal iterateI_ho1_0_res_13 : boolean;
    signal iterateI_ho1_1_res_13 : boolean;
    signal iterateI_ho1_2_res_13 : boolean;
    signal iterateI_ho1_3_res_13 : boolean;
    signal iterateI_ho1_4_res_13 : boolean;
    signal iterateI_ho1_5_res_13 : boolean;
  begin
    r_block_302 : block
        signal \c$bb_res_res_108\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_108_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_108\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_13 <= \c$bb_res_res_108\;


      end block;

    r_block_303 : block
        signal \c$bb_res_res_109\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_109_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_109\ <= iterateI_ho1_0_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_13 <= \c$bb_res_res_109\;


      end block;

    r_block_304 : block
        signal \c$bb_res_res_110\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_110_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_110\ <= iterateI_ho1_1_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_13 <= \c$bb_res_res_110\;


      end block;

    r_block_305 : block
        signal \c$bb_res_res_111\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_111_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_111\ <= iterateI_ho1_2_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_13 <= \c$bb_res_res_111\;


      end block;

    r_block_306 : block
        signal \c$bb_res_res_112\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_112_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_112\ <= iterateI_ho1_3_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_13 <= \c$bb_res_res_112\;


      end block;

    r_block_307 : block
        signal \c$bb_res_res_113\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_113_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_113\ <= iterateI_ho1_4_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_13 <= \c$bb_res_res_113\;


      end block;

    \c$app_arg_13\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                            , iterateI_ho1_0_res_13
                                                            , iterateI_ho1_1_res_13
                                                            , iterateI_ho1_2_res_13
                                                            , iterateI_ho1_3_res_13
                                                            , iterateI_ho1_4_res_13
                                                            , iterateI_ho1_5_res_13 );


  end block;

  result_43 <=  \c$app_arg_13\(\c$app_arg_13\'high) ;

  \c$out7_case_alt_0\ <= (x_10 + y_7) when \c$out8_case_scrut\ else
                         dta_3;

  y_7 <= winData_2;

  x_10 <= dta_4;

  \c$out7_case_alt_selection_res_0\ <= tag_1 = tagToMatch;

  \c$out7_case_alt_1\ <= dta_1 when \c$out7_case_alt_selection_res_0\ else
                         to_signed(0,64);

  dta_1 <= result_51.Tuple2_2_sel1_signed;

  tag_1 <= result_51.Tuple2_2_sel0_unsigned;

  tagToMatch <= result_95.Tags_sel8_output6_0;

  iterateI_14 : block
    signal iterateI_ho1_0_res_14 : Spec_topEntity_types.PacingOut7;
    signal iterateI_ho1_1_res_14 : Spec_topEntity_types.PacingOut7;
    signal iterateI_ho1_2_res_14 : Spec_topEntity_types.PacingOut7;
    signal iterateI_ho1_3_res_14 : Spec_topEntity_types.PacingOut7;
  begin
    r_block_308 : block
        signal \c$bb_res_res_114\ : Spec_topEntity_types.PacingOut7 := (PacingOut7_sel0_boolean_0 => false, PacingOut7_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_114_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_114\ <= pOut7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_14 <= \c$bb_res_res_114\;


      end block;

    r_block_309 : block
        signal \c$bb_res_res_115\ : Spec_topEntity_types.PacingOut7 := (PacingOut7_sel0_boolean_0 => false, PacingOut7_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_115_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_115\ <= iterateI_ho1_0_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_14 <= \c$bb_res_res_115\;


      end block;

    r_block_310 : block
        signal \c$bb_res_res_116\ : Spec_topEntity_types.PacingOut7 := (PacingOut7_sel0_boolean_0 => false, PacingOut7_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_116_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_116\ <= iterateI_ho1_1_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_14 <= \c$bb_res_res_116\;


      end block;

    r_block_311 : block
        signal \c$bb_res_res_117\ : Spec_topEntity_types.PacingOut7 := (PacingOut7_sel0_boolean_0 => false, PacingOut7_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_117_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_117\ <= iterateI_ho1_2_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_14 <= \c$bb_res_res_117\;


      end block;

    \c$app_arg_14\ <= Spec_topEntity_types.array_of_PacingOut7'( pOut7
                                                               , iterateI_ho1_0_res_14
                                                               , iterateI_ho1_1_res_14
                                                               , iterateI_ho1_2_res_14
                                                               , iterateI_ho1_3_res_14 );


  end block;

  result_44 <=  \c$app_arg_14\(\c$app_arg_14\'high) ;

  tagToMatch_0 <= result_94.Tags_sel9_output7_0;

  \c$out5_case_alt_selection_res_0\ <= winTag_2 = tagToMatch_1;

  \c$out5_case_alt_0\ <= winData_2 when \c$out5_case_alt_selection_res_0\ else
                         to_signed(0,64);

  tagToMatch_1 <= result_94.Tags_sel3_output1_0;

  \c$out5_case_alt_selection_res_1\ <= tag_3 = tagToMatch_2;

  \c$out5_case_alt_1\ <= result_68.Tuple2_2_sel1_signed when \c$out5_case_alt_selection_res_1\ else
                         to_signed(0,64);

  tagToMatch_2 <= result_94.Tags_sel6_output4_0;

  \c$out5_case_alt_selection_res_2\ <= result_45 = winTag;

  \c$out5_case_alt_2\ <= winData when \c$out5_case_alt_selection_res_2\ else
                         to_signed(0,64);

  winTag <= result_35.Tuple2_2_sel0_unsigned;

  result_selection_res_5 <= result_94.Tags_sel7_output5_0 > to_unsigned(1,8);

  result_45 <= \c$app_arg_15\ when result_selection_res_5 else
               \c$app_arg_15\ + to_unsigned(4,8);

  \c$app_arg_15\ <= result_94.Tags_sel7_output5_0 - to_unsigned(1,8);

  iterateI_15 : block
    signal iterateI_ho1_0_res_15 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_1_res_15 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_2_res_15 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_3_res_15 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_4_res_14 : Spec_topEntity_types.PacingOut5;
  begin
    r_block_312 : block
        signal \c$bb_res_res_118\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_118_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_118\ <= pOut5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_15 <= \c$bb_res_res_118\;


      end block;

    r_block_313 : block
        signal \c$bb_res_res_119\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_119_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_119\ <= iterateI_ho1_0_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_15 <= \c$bb_res_res_119\;


      end block;

    r_block_314 : block
        signal \c$bb_res_res_120\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_120_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_120\ <= iterateI_ho1_1_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_15 <= \c$bb_res_res_120\;


      end block;

    r_block_315 : block
        signal \c$bb_res_res_121\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_121_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_121\ <= iterateI_ho1_2_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_15 <= \c$bb_res_res_121\;


      end block;

    r_block_316 : block
        signal \c$bb_res_res_122\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_122_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_122\ <= iterateI_ho1_3_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_14 <= \c$bb_res_res_122\;


      end block;

    \c$app_arg_16\ <= Spec_topEntity_types.array_of_PacingOut5'( pOut5
                                                               , iterateI_ho1_0_res_15
                                                               , iterateI_ho1_1_res_15
                                                               , iterateI_ho1_2_res_15
                                                               , iterateI_ho1_3_res_15
                                                               , iterateI_ho1_4_res_14 );


  end block;

  result_46 <=  \c$app_arg_16\(\c$app_arg_16\'high) ;

  iterateI_16 : block
    signal iterateI_ho1_0_arg0_10 : boolean;
    signal iterateI_ho1_0_res_16  : boolean;
    signal iterateI_ho1_1_res_16  : boolean;
    signal iterateI_ho1_2_res_16  : boolean;
    signal iterateI_ho1_3_res_16  : boolean;
    signal iterateI_ho1_4_res_15  : boolean;
    signal iterateI_ho1_5_res_14  : boolean;
  begin
    iterateI_ho1_0_arg0_10 <= slides.Slides_1_sel0_slide0;

    r_block_317 : block
        signal \c$bb_res_res_123\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_123_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_123\ <= iterateI_ho1_0_arg0_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_16 <= \c$bb_res_res_123\;


      end block;

    r_block_318 : block
        signal \c$bb_res_res_124\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_124_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_124\ <= iterateI_ho1_0_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_16 <= \c$bb_res_res_124\;


      end block;

    r_block_319 : block
        signal \c$bb_res_res_125\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_125_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_125\ <= iterateI_ho1_1_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_16 <= \c$bb_res_res_125\;


      end block;

    r_block_320 : block
        signal \c$bb_res_res_126\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_126_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_126\ <= iterateI_ho1_2_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_16 <= \c$bb_res_res_126\;


      end block;

    r_block_321 : block
        signal \c$bb_res_res_127\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_127_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_127\ <= iterateI_ho1_3_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_15 <= \c$bb_res_res_127\;


      end block;

    r_block_322 : block
        signal \c$bb_res_res_128\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_128_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_128\ <= iterateI_ho1_4_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_14 <= \c$bb_res_res_128\;


      end block;

    \c$app_arg_17\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel0_slide0
                                                            , iterateI_ho1_0_res_16
                                                            , iterateI_ho1_1_res_16
                                                            , iterateI_ho1_2_res_16
                                                            , iterateI_ho1_3_res_16
                                                            , iterateI_ho1_4_res_15
                                                            , iterateI_ho1_5_res_14 );


  end block;

  result_47 <=  \c$app_arg_17\(\c$app_arg_17\'high) ;

  slides <= result_130.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_Slides_1;

  iterateI_17 : block
    signal iterateI_ho1_0_res_17 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_1_res_17 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_2_res_17 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_3_res_17 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_4_res_16 : Spec_topEntity_types.PacingOut5;
    signal iterateI_ho1_5_res_15 : Spec_topEntity_types.PacingOut5;
  begin
    r_block_323 : block
        signal \c$bb_res_res_129\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_129_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_129\ <= pOut5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_17 <= \c$bb_res_res_129\;


      end block;

    r_block_324 : block
        signal \c$bb_res_res_130\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_130_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_130\ <= iterateI_ho1_0_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_17 <= \c$bb_res_res_130\;


      end block;

    r_block_325 : block
        signal \c$bb_res_res_131\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_131_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_131\ <= iterateI_ho1_1_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_17 <= \c$bb_res_res_131\;


      end block;

    r_block_326 : block
        signal \c$bb_res_res_132\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_132_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_132\ <= iterateI_ho1_2_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_17 <= \c$bb_res_res_132\;


      end block;

    r_block_327 : block
        signal \c$bb_res_res_133\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_133_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_133\ <= iterateI_ho1_3_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_16 <= \c$bb_res_res_133\;


      end block;

    r_block_328 : block
        signal \c$bb_res_res_134\ : Spec_topEntity_types.PacingOut5 := (PacingOut5_sel0_boolean_0 => false, PacingOut5_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_134_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_134\ <= iterateI_ho1_4_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_15 <= \c$bb_res_res_134\;


      end block;

    \c$app_arg_18\ <= Spec_topEntity_types.array_of_PacingOut5'( pOut5
                                                               , iterateI_ho1_0_res_17
                                                               , iterateI_ho1_1_res_17
                                                               , iterateI_ho1_2_res_17
                                                               , iterateI_ho1_3_res_17
                                                               , iterateI_ho1_4_res_16
                                                               , iterateI_ho1_5_res_15 );


  end block;

  result_48 <=  \c$app_arg_18\(\c$app_arg_18\'high) ;

  iterateI_18 : block
    signal iterateI_ho1_0_res_18 : boolean;
    signal iterateI_ho1_1_res_18 : boolean;
    signal iterateI_ho1_2_res_18 : boolean;
    signal iterateI_ho1_3_res_18 : boolean;
    signal iterateI_ho1_4_res_17 : boolean;
    signal iterateI_ho1_5_res_16 : boolean;
    signal iterateI_ho1_6_res_12 : boolean;
  begin
    r_block_329 : block
        signal \c$bb_res_res_135\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_135_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_135\ <= pOut11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_18 <= \c$bb_res_res_135\;


      end block;

    r_block_330 : block
        signal \c$bb_res_res_136\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_136_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_136\ <= iterateI_ho1_0_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_18 <= \c$bb_res_res_136\;


      end block;

    r_block_331 : block
        signal \c$bb_res_res_137\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_137_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_137\ <= iterateI_ho1_1_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_18 <= \c$bb_res_res_137\;


      end block;

    r_block_332 : block
        signal \c$bb_res_res_138\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_138_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_138\ <= iterateI_ho1_2_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_18 <= \c$bb_res_res_138\;


      end block;

    r_block_333 : block
        signal \c$bb_res_res_139\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_139_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_139\ <= iterateI_ho1_3_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_17 <= \c$bb_res_res_139\;


      end block;

    r_block_334 : block
        signal \c$bb_res_res_140\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_140_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_140\ <= iterateI_ho1_4_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_16 <= \c$bb_res_res_140\;


      end block;

    r_block_335 : block
        signal \c$bb_res_res_141\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_141_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_141\ <= iterateI_ho1_5_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_12 <= \c$bb_res_res_141\;


      end block;

    \c$app_arg_19\ <= Spec_topEntity_types.array_of_boolean'( pOut11
                                                            , iterateI_ho1_0_res_18
                                                            , iterateI_ho1_1_res_18
                                                            , iterateI_ho1_2_res_18
                                                            , iterateI_ho1_3_res_18
                                                            , iterateI_ho1_4_res_17
                                                            , iterateI_ho1_5_res_16
                                                            , iterateI_ho1_6_res_12 );


  end block;

  result_49 <=  \c$app_arg_19\(\c$app_arg_19\'high) ;

  result_selection_res_6 <= x0_2 and x1_2;

  result_50 <= t_4 when result_selection_res_6 else
               result_51;

  x0_2 <= result_52.PacingOut6_sel0_boolean_0;

  x1_2 <= result_52.PacingOut6_sel1_boolean_1;

  -- register begin
  result_51_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_51 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_51 <= result_50;
      end if;
    end if;
  end process;
  -- register end

  t_4 <= ( Tuple2_2_sel0_unsigned => result_97.Tags_sel8_output6_0
         , Tuple2_2_sel1_signed => (x_11 + y_8) );

  x_projection_13 <= (x_12 - y_9);

  x_11 <= x_projection_13;

  y_8 <= \c$out6_case_alt\;

  x_12 <= \c$out6_case_alt_1\;

  y_9 <= \c$out6_case_alt_0\;

  \c$out6_case_alt_selection_res\ <= tag_2 = tagToMatch_3;

  \c$out6_case_alt\ <= dta_2 when \c$out6_case_alt_selection_res\ else
                       to_signed(0,64);

  dta_2 <= result_54.Tuple2_2_sel1_signed;

  tag_2 <= result_54.Tuple2_2_sel0_unsigned;

  tagToMatch_3 <= result_97.Tags_sel10_output8_0;

  \c$out6_case_alt_selection_res_0\ <= winTag_0 = tagToMatch_4;

  \c$out6_case_alt_0\ <= winData_0 when \c$out6_case_alt_selection_res_0\ else
                         to_signed(0,64);

  tagToMatch_4 <= result_97.Tags_sel1_input1_0;

  \c$out6_case_alt_selection_res_1\ <= tag_3 = tagToMatch_5;

  \c$out6_case_alt_1\ <= result_68.Tuple2_2_sel1_signed when \c$out6_case_alt_selection_res_1\ else
                         to_signed(0,64);

  tag_3 <= result_68.Tuple2_2_sel0_unsigned;

  tagToMatch_5 <= result_97.Tags_sel6_output4_0;

  iterateI_19 : block
    signal iterateI_ho1_0_res_19 : Spec_topEntity_types.PacingOut6;
    signal iterateI_ho1_1_res_19 : Spec_topEntity_types.PacingOut6;
  begin
    r_block_336 : block
        signal \c$bb_res_res_142\ : Spec_topEntity_types.PacingOut6 := (PacingOut6_sel0_boolean_0 => false, PacingOut6_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_142_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_142\ <= pOut6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_19 <= \c$bb_res_res_142\;


      end block;

    r_block_337 : block
        signal \c$bb_res_res_143\ : Spec_topEntity_types.PacingOut6 := (PacingOut6_sel0_boolean_0 => false, PacingOut6_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_143_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_143\ <= iterateI_ho1_0_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_19 <= \c$bb_res_res_143\;


      end block;

    \c$app_arg_20\ <= Spec_topEntity_types.array_of_PacingOut6'( pOut6
                                                               , iterateI_ho1_0_res_19
                                                               , iterateI_ho1_1_res_19 );


  end block;

  result_52 <=  \c$app_arg_20\(\c$app_arg_20\'high) ;

  result_selection_13 <= x_13;

  result_53 <= t_5 when result_selection_13 else
               result_54;

  x_13 <= result_56;

  -- register begin
  result_54_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_54 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_54 <= result_53;
      end if;
    end if;
  end process;
  -- register end

  t_5 <= ( Tuple2_2_sel0_unsigned => t_22
         , Tuple2_2_sel1_signed => (x_15 + y_11) );

  x_14 <= \c$out8_case_alt_1\;

  y_10 <= \c$out8_case_alt_0\;

  x_projection_17 <= (x_14 + y_10);

  x_15 <= x_projection_17;

  y_11 <= \c$out8_case_alt\;

  \c$out8_case_alt\ <= to_signed(10,64) when \c$out8_case_scrut\ else
                       dta_3;

  dta_3 <= result_86.Tuple2_2_sel1_signed;

  \c$out8_case_scrut\ <= tag_4 = to_unsigned(5,8);

  tag_4 <= result_86.Tuple2_2_sel0_unsigned;

  \c$out8_case_alt_selection_res\ <= winTag_3 = to_unsigned(5,8);

  \c$out8_case_alt_0\ <= to_signed(0,64) when \c$out8_case_alt_selection_res\ else
                         winData_3;

  \c$out8_case_alt_selection_res_0\ <= result_55 = winTag_0;

  \c$out8_case_alt_1\ <= winData_0 when \c$out8_case_alt_selection_res_0\ else
                         to_signed(0,64);

  winData_0 <= result_58.Tuple2_2_sel1_signed;

  winTag_0 <= result_58.Tuple2_2_sel0_unsigned;

  result_selection_res_7 <= t_26 > to_unsigned(1,8);

  result_55 <= \c$app_arg_21\ when result_selection_res_7 else
               \c$app_arg_21\ + to_unsigned(4,8);

  \c$app_arg_21\ <= t_26 - to_unsigned(1,8);

  iterateI_20 : block
    signal iterateI_ho1_0_res_20 : boolean;
  begin
    r_block_338 : block
        signal \c$bb_res_res_144\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_144_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_144\ <= pOut8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_20 <= \c$bb_res_res_144\;


      end block;

    \c$app_arg_22\ <= Spec_topEntity_types.array_of_boolean'( pOut8
                                                            , iterateI_ho1_0_res_20 );


  end block;

  result_56 <=  \c$app_arg_22\(\c$app_arg_22\'high) ;

  result_57 <= ( Tuple2_2_sel0_unsigned => t_26
               , Tuple2_2_sel1_signed => \c$input1Win_app_arg\ ) when b_2 else
               result_58;

  -- register begin
  result_58_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_58 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_58 <= result_57;
      end if;
    end if;
  end process;
  -- register end

  b_2 <= result_59;

  -- delay begin
  cinput1Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input1Win_app_arg\ <= inputs_2.Inputs_3_sel1_input1.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_21 : block
    signal iterateI_ho1_0_res_21 : boolean;
  begin
    r_block_339 : block
        signal \c$bb_res_res_145\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_145_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_145\ <= pIn1_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_21 <= \c$bb_res_res_145\;


      end block;

    \c$app_arg_23\ <= Spec_topEntity_types.array_of_boolean'( pIn1_0
                                                            , iterateI_ho1_0_res_21 );


  end block;

  result_59 <=  \c$app_arg_23\(\c$app_arg_23\'high) ;

  tag_5 <= result_61.Tuple2_2_sel0_unsigned;

  result_selection_18 <= x_16;

  result_60 <= t_6 when result_selection_18 else
               result_61;

  x_16 <= result_80;

  -- register begin
  result_61_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_61 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_61 <= result_60;
      end if;
    end if;
  end process;
  -- register end

  t_6 <= ( Tuple2_2_sel0_unsigned => result_97.Tags_sel2_output0_0
         , Tuple2_2_sel1_signed => (x_17 + y_12) );

  x_17 <= result_65;

  y_12 <= \c$out0_case_alt\;

  \c$out0_case_alt_selection_res\ <= result_64 = winTag_1;

  \c$out0_case_alt\ <= winData_1 when \c$out0_case_alt_selection_res\ else
                       to_signed(10,64);

  winData_1 <= result_63.Tuple2_2_sel1_signed;

  winTag_1 <= result_63.Tuple2_2_sel0_unsigned;

  result_selection_21 <= x_18;

  result_62 <= t_7 when result_selection_21 else
               result_63;

  x_18 <= result_81;

  -- register begin
  result_63_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_63 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_63 <= result_62;
      end if;
    end if;
  end process;
  -- register end

  t_7 <= ( Tuple2_2_sel0_unsigned => result_96.Tags_sel4_output2_0
         , Tuple2_2_sel1_signed => (x_19 + to_signed(1,64)) );

  x_19 <= \c$out1_case_alt\;

  result_selection_res_8 <= tag_6 > to_unsigned(1,8);

  result_64 <= \c$app_arg_24\ when result_selection_res_8 else
               \c$app_arg_24\ + to_unsigned(4,8);

  \c$app_arg_24\ <= tag_6 - to_unsigned(1,8);

  tag_6 <= result_97.Tags_sel4_output2_0;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_6 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_6) <= ( Tuple2_4_sel0_Tuple2_2 => result_83(i_6)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_0\(i_6) );


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
    imap_2 : for i_7 in \c$ws_app_arg_2\'range generate
    begin
      fun_4 : block
      signal \c$case_alt_14\              : Spec_topEntity_types.Maybe_0;
      -- spec.hs:355:1-65
      signal t_30                         : unsigned(7 downto 0);
      -- spec.hs:355:1-65
      signal x_38                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_7) <= \c$case_alt_14\;

      \c$case_alt_selection_res_2\ <= t_30 = result_66;

      \c$case_alt_14\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_7,max_0(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_2\ else
                         \c$ws_app_arg_1\(i_7).Tuple2_4_sel1_Maybe_0;

      t_30 <= x_38.Tuple2_2_sel0_unsigned;

      x_38 <= \c$ws_app_arg_1\(i_7).Tuple2_4_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_0 : block
    signal vec_index_2 : integer range 0 to 3-1;
  begin
    vec_index_2 <= to_integer((signed(std_logic_vector(resize(i_0,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_1\ <= result_83(vec_index_2);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(2 downto 2)) select
    result_65 <= result_68.Tuple2_2_sel1_signed when "0",
                 \c$case_scrut_1\.Tuple2_2_sel1_signed when others;

  result_selection_res_9 <= result_97.Tags_sel0_input0_0 > to_unsigned(2,8);

  result_66 <= \c$app_arg_25\ when result_selection_res_9 else
               \c$app_arg_25\ + to_unsigned(4,8);

  \c$app_arg_25\ <= result_97.Tags_sel0_input0_0 - to_unsigned(2,8);

  i_0 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_2\(1 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  result_selection_27 <= x_20;

  result_67 <= t_8 when result_selection_27 else
               result_68;

  x_20 <= result_79;

  -- register begin
  result_68_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_68 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_68 <= result_67;
      end if;
    end if;
  end process;
  -- register end

  t_8 <= ( Tuple2_2_sel0_unsigned => t_23
         , Tuple2_2_sel1_signed => (x_21 + y_13) );

  x_21 <= \c$out4_case_alt\;

  y_13 <= result_69;

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_8 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_8) <= ( Tuple2_1_sel0_Tuple2_2 => result_72(i_8)
               , Tuple2_1_sel1_Maybe => \c$vec2_1\(i_8) );


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
    imap_4 : for i_9 in \c$ws_app_arg_4\'range generate
    begin
      fun_5 : block
      signal \c$case_alt_15\              : Spec_topEntity_types.Maybe;
      -- spec.hs:363:1-63
      signal t_31                         : unsigned(7 downto 0);
      -- spec.hs:363:1-63
      signal x_39                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_9) <= \c$case_alt_15\;

      \c$case_alt_selection_res_3\ <= t_31 = result_70;

      \c$case_alt_15\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_9,max_1(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_3\ else
                         \c$ws_app_arg_3\(i_9).Tuple2_1_sel1_Maybe;

      t_31 <= x_39.Tuple2_2_sel0_unsigned;

      x_39 <= \c$ws_app_arg_3\(i_9).Tuple2_1_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_1 : block
    signal vec_index_3 : integer range 0 to 2-1;
  begin
    vec_index_3 <= to_integer((signed(std_logic_vector(resize(i_1,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_3\ <= result_72(vec_index_3);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(1 downto 1)) select
    result_69 <= to_signed(-10,64) when "0",
                 \c$case_scrut_3\.Tuple2_2_sel1_signed when others;

  result_selection_res_10 <= t_24 > to_unsigned(2,8);

  result_70 <= \c$app_arg_26\ when result_selection_res_10 else
               \c$app_arg_26\ + to_unsigned(4,8);

  \c$app_arg_26\ <= t_24 - to_unsigned(2,8);

  i_1 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_4\(0 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  result_selection_32 <= x_22;

  result_71 <= t_9 when result_selection_32 else
               result_72;

  x_22 <= result_73;

  -- register begin
  result_72_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_72 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_72 <= result_71;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_5\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_72) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => result_96.Tags_sel5_output3_0
             , Tuple2_2_sel1_signed => (x_23 + to_signed(2,64)) )))));

  t_projection_2 <= (\c$vec_5\(0 to 1-1),\c$vec_5\(1 to \c$vec_5\'high));

  t_9 <= t_projection_2.Tuple2_6_sel1_array_of_Tuple2_2_1;

  x_23 <= \c$out1_case_alt\;

  iterateI_22 : block
    signal iterateI_ho1_0_res_22 : boolean;
    signal iterateI_ho1_1_res_20 : boolean;
    signal iterateI_ho1_2_res_19 : boolean;
  begin
    r_block_340 : block
        signal \c$bb_res_res_146\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_146_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_146\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_22 <= \c$bb_res_res_146\;


      end block;

    r_block_341 : block
        signal \c$bb_res_res_147\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_147_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_147\ <= iterateI_ho1_0_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_20 <= \c$bb_res_res_147\;


      end block;

    r_block_342 : block
        signal \c$bb_res_res_148\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_148_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_148\ <= iterateI_ho1_1_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_19 <= \c$bb_res_res_148\;


      end block;

    \c$app_arg_27\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                            , iterateI_ho1_0_res_22
                                                            , iterateI_ho1_1_res_20
                                                            , iterateI_ho1_2_res_19 );


  end block;

  result_73 <=  \c$app_arg_27\(\c$app_arg_27\'high) ;

  \c$out4_case_alt_selection_res\ <= result_78 = winTag_2;

  \c$out4_case_alt\ <= winData_2 when \c$out4_case_alt_selection_res\ else
                       to_signed(10,64);

  winData_2 <= result_75.Tuple2_2_sel1_signed;

  winTag_2 <= result_75.Tuple2_2_sel0_unsigned;

  result_selection_35 <= x_24;

  result_74 <= t_10 when result_selection_35 else
               result_75;

  x_24 <= result_77;

  -- register begin
  result_75_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_75 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_75 <= result_74;
      end if;
    end if;
  end process;
  -- register end

  t_10 <= ( Tuple2_2_sel0_unsigned => result_96.Tags_sel3_output1_0
          , Tuple2_2_sel1_signed => (x_25 + y_14) );

  x_25 <= result_76;

  y_14 <= \c$out1_case_alt\;

  \c$out1_case_alt_selection_res\ <= tag_5 = tagToMatch_6;

  \c$out1_case_alt\ <= dta_4 when \c$out1_case_alt_selection_res\ else
                       to_signed(0,64);

  dta_4 <= result_61.Tuple2_2_sel1_signed;

  \c$vec2_2\ <= (ws_2(1 to ws_2'high));

  -- zipWith begin
  zipWith_2 : for i_10 in \c$ws_app_arg_5\'range generate
  begin
    \c$ws_app_arg_5\(i_10) <= ( Tuple2_4_sel0_Tuple2_2 => result_83(i_10)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_2\(i_10) );


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
    imap_6 : for i_11 in \c$ws_app_arg_6\'range generate
    begin
      fun_6 : block
      signal \c$case_alt_16\              : Spec_topEntity_types.Maybe_0;
      -- spec.hs:355:1-65
      signal t_32                         : unsigned(7 downto 0);
      -- spec.hs:355:1-65
      signal x_40                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_4\ : boolean;
    begin
      \c$ws_app_arg_6\(i_11) <= \c$case_alt_16\;

      \c$case_alt_selection_res_4\ <= t_32 = result_96.Tags_sel0_input0_0;

      \c$case_alt_16\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_11,max_2(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_4\ else
                         \c$ws_app_arg_5\(i_11).Tuple2_4_sel1_Maybe_0;

      t_32 <= x_40.Tuple2_2_sel0_unsigned;

      x_40 <= \c$ws_app_arg_5\(i_11).Tuple2_4_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_2 : block
    signal vec_index_4 : integer range 0 to 3-1;
  begin
    vec_index_4 <= to_integer((signed(std_logic_vector(resize(i_2,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_5\ <= result_83(vec_index_4);
  end block;
  -- index end

  \c$case_scrut_6\ <=  ws_2(0) ;

  with (\c$case_scrut_6\(2 downto 2)) select
    result_76 <= to_signed(0,64) when "0",
                 \c$case_scrut_5\.Tuple2_2_sel1_signed when others;

  i_2 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_6\(1 downto 0)));

  ws_2 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_6\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  iterateI_23 : block
    signal iterateI_ho1_0_res_23 : boolean;
    signal iterateI_ho1_1_res_21 : boolean;
    signal iterateI_ho1_2_res_20 : boolean;
  begin
    r_block_343 : block
        signal \c$bb_res_res_149\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_149_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_149\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_23 <= \c$bb_res_res_149\;


      end block;

    r_block_344 : block
        signal \c$bb_res_res_150\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_150_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_150\ <= iterateI_ho1_0_res_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_21 <= \c$bb_res_res_150\;


      end block;

    r_block_345 : block
        signal \c$bb_res_res_151\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_151_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_151\ <= iterateI_ho1_1_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_20 <= \c$bb_res_res_151\;


      end block;

    \c$app_arg_28\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                            , iterateI_ho1_0_res_23
                                                            , iterateI_ho1_1_res_21
                                                            , iterateI_ho1_2_res_20 );


  end block;

  result_77 <=  \c$app_arg_28\(\c$app_arg_28\'high) ;

  result_selection_res_11 <= t_25 > to_unsigned(1,8);

  result_78 <= \c$app_arg_29\ when result_selection_res_11 else
               \c$app_arg_29\ + to_unsigned(4,8);

  \c$app_arg_29\ <= t_25 - to_unsigned(1,8);

  iterateI_24 : block
    signal iterateI_ho1_0_res_24 : boolean;
  begin
    r_block_346 : block
        signal \c$bb_res_res_152\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_152_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_152\ <= pOut4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_24 <= \c$bb_res_res_152\;


      end block;

    \c$app_arg_30\ <= Spec_topEntity_types.array_of_boolean'( pOut4
                                                            , iterateI_ho1_0_res_24 );


  end block;

  result_79 <=  \c$app_arg_30\(\c$app_arg_30\'high) ;

  iterateI_25 : block
    signal iterateI_ho1_0_res_25 : boolean;
    signal iterateI_ho1_1_res_22 : boolean;
  begin
    r_block_347 : block
        signal \c$bb_res_res_153\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_153_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_153\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_25 <= \c$bb_res_res_153\;


      end block;

    r_block_348 : block
        signal \c$bb_res_res_154\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_154_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_154\ <= iterateI_ho1_0_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_22 <= \c$bb_res_res_154\;


      end block;

    \c$app_arg_31\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                            , iterateI_ho1_0_res_25
                                                            , iterateI_ho1_1_res_22 );


  end block;

  result_80 <=  \c$app_arg_31\(\c$app_arg_31\'high) ;

  tagToMatch_6 <= result_96.Tags_sel2_output0_0;

  iterateI_26 : block
    signal iterateI_ho1_0_res_26 : boolean;
    signal iterateI_ho1_1_res_23 : boolean;
    signal iterateI_ho1_2_res_21 : boolean;
  begin
    r_block_349 : block
        signal \c$bb_res_res_155\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_155_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_155\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_26 <= \c$bb_res_res_155\;


      end block;

    r_block_350 : block
        signal \c$bb_res_res_156\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_156_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_156\ <= iterateI_ho1_0_res_26;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_23 <= \c$bb_res_res_156\;


      end block;

    r_block_351 : block
        signal \c$bb_res_res_157\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_157_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_157\ <= iterateI_ho1_1_res_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_21 <= \c$bb_res_res_157\;


      end block;

    \c$app_arg_32\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                            , iterateI_ho1_0_res_26
                                                            , iterateI_ho1_1_res_23
                                                            , iterateI_ho1_2_res_21 );


  end block;

  result_81 <=  \c$app_arg_32\(\c$app_arg_32\'high) ;

  result_82 <= t_11 when b_3 else
               result_83;

  -- register begin
  result_83_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_83 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_83 <= result_82;
      end if;
    end if;
  end process;
  -- register end

  b_3 <= result_84;

  \c$vec_6\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_83) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => t_27
             , Tuple2_2_sel1_signed => \c$input0Win_app_arg\ )))));

  t_projection_4 <= (\c$vec_6\(0 to 1-1),\c$vec_6\(1 to \c$vec_6\'high));

  t_11 <= t_projection_4.Tuple2_7_sel1_array_of_Tuple2_2_1;

  -- delay begin
  cinput0Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input0Win_app_arg\ <= inputs_2.Inputs_3_sel0_input0.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  inputs_2 <= result_130.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_Inputs_3;

  iterateI_27 : block
    signal iterateI_ho1_0_res_27 : boolean;
  begin
    r_block_352 : block
        signal \c$bb_res_res_158\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_158_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_158\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_27 <= \c$bb_res_res_158\;


      end block;

    \c$app_arg_33\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                            , iterateI_ho1_0_res_27 );


  end block;

  result_84 <=  \c$app_arg_33\(\c$app_arg_33\'high) ;

  result_85 <= t_12 when b_4 else
               result_86;

  -- register begin
  result_86_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_86 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_86 <= result_85;
      end if;
    end if;
  end process;
  -- register end

  b_4 <= result_87;

  t_12 <= ( Tuple2_2_sel0_unsigned => result_88.Tags_sel12_output10_0
          , Tuple2_2_sel1_signed => (x_26 + to_signed(1,64)) );

  x_26 <= \c$out10_case_alt\;

  \c$out10_case_alt_selection_res\ <= winTag_3 = tagToMatch_7;

  \c$out10_case_alt\ <= winData_3 when \c$out10_case_alt_selection_res\ else
                        to_signed(0,64);

  tagToMatch_7 <= result_88.Tags_sel11_output9_0;

  iterateI_28 : block
    signal iterateI_ho1_0_res_28 : boolean;
    signal iterateI_ho1_1_res_24 : boolean;
    signal iterateI_ho1_2_res_22 : boolean;
    signal iterateI_ho1_3_res_19 : boolean;
    signal iterateI_ho1_4_res_18 : boolean;
    signal iterateI_ho1_5_res_17 : boolean;
    signal iterateI_ho1_6_res_13 : boolean;
  begin
    r_block_353 : block
        signal \c$bb_res_res_159\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_159_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_159\ <= pOut10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_28 <= \c$bb_res_res_159\;


      end block;

    r_block_354 : block
        signal \c$bb_res_res_160\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_160_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_160\ <= iterateI_ho1_0_res_28;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_24 <= \c$bb_res_res_160\;


      end block;

    r_block_355 : block
        signal \c$bb_res_res_161\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_161_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_161\ <= iterateI_ho1_1_res_24;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_22 <= \c$bb_res_res_161\;


      end block;

    r_block_356 : block
        signal \c$bb_res_res_162\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_162_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_162\ <= iterateI_ho1_2_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_19 <= \c$bb_res_res_162\;


      end block;

    r_block_357 : block
        signal \c$bb_res_res_163\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_163_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_163\ <= iterateI_ho1_3_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_18 <= \c$bb_res_res_163\;


      end block;

    r_block_358 : block
        signal \c$bb_res_res_164\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_164_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_164\ <= iterateI_ho1_4_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_17 <= \c$bb_res_res_164\;


      end block;

    r_block_359 : block
        signal \c$bb_res_res_165\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_165_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_165\ <= iterateI_ho1_5_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_13 <= \c$bb_res_res_165\;


      end block;

    \c$app_arg_34\ <= Spec_topEntity_types.array_of_boolean'( pOut10
                                                            , iterateI_ho1_0_res_28
                                                            , iterateI_ho1_1_res_24
                                                            , iterateI_ho1_2_res_22
                                                            , iterateI_ho1_3_res_19
                                                            , iterateI_ho1_4_res_18
                                                            , iterateI_ho1_5_res_17
                                                            , iterateI_ho1_6_res_13 );


  end block;

  result_87 <=  \c$app_arg_34\(\c$app_arg_34\'high) ;

  iterateI_29 : block
    signal iterateI_ho1_0_arg0_11 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_29  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_25  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_23  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_20  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_19  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_5_res_18  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_11 <= ( Tags_sel0_input0_0 => t_27
                              , Tags_sel1_input1_0 => t_26
                              , Tags_sel2_output0_0 => t_21
                              , Tags_sel3_output1_0 => t_25
                              , Tags_sel4_output2_0 => t_20
                              , Tags_sel5_output3_0 => t_24
                              , Tags_sel6_output4_0 => t_23
                              , Tags_sel7_output5_0 => t_19
                              , Tags_sel8_output6_0 => t_18
                              , Tags_sel9_output7_0 => t_17
                              , Tags_sel10_output8_0 => t_22
                              , Tags_sel11_output9_0 => t_16
                              , Tags_sel12_output10_0 => t_15
                              , Tags_sel13_output11_0 => t_14
                              , Tags_sel14_slide0_0 => t_19
                              , Tags_sel15_slide1_0 => t_25 );

    r_block_360 : block
        signal \c$bb_res_res_166\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_166_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_166\ <= iterateI_ho1_0_arg0_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_29 <= \c$bb_res_res_166\;


      end block;

    r_block_361 : block
        signal \c$bb_res_res_167\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_167_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_167\ <= iterateI_ho1_0_res_29;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_25 <= \c$bb_res_res_167\;


      end block;

    r_block_362 : block
        signal \c$bb_res_res_168\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_168_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_168\ <= iterateI_ho1_1_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_23 <= \c$bb_res_res_168\;


      end block;

    r_block_363 : block
        signal \c$bb_res_res_169\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_169_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_169\ <= iterateI_ho1_2_res_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_20 <= \c$bb_res_res_169\;


      end block;

    r_block_364 : block
        signal \c$bb_res_res_170\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_170_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_170\ <= iterateI_ho1_3_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_19 <= \c$bb_res_res_170\;


      end block;

    r_block_365 : block
        signal \c$bb_res_res_171\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_171_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_171\ <= iterateI_ho1_4_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_18 <= \c$bb_res_res_171\;


      end block;

    \c$app_arg_35\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_27
                                                         , Tags_sel1_input1_0 => t_26
                                                         , Tags_sel2_output0_0 => t_21
                                                         , Tags_sel3_output1_0 => t_25
                                                         , Tags_sel4_output2_0 => t_20
                                                         , Tags_sel5_output3_0 => t_24
                                                         , Tags_sel6_output4_0 => t_23
                                                         , Tags_sel7_output5_0 => t_19
                                                         , Tags_sel8_output6_0 => t_18
                                                         , Tags_sel9_output7_0 => t_17
                                                         , Tags_sel10_output8_0 => t_22
                                                         , Tags_sel11_output9_0 => t_16
                                                         , Tags_sel12_output10_0 => t_15
                                                         , Tags_sel13_output11_0 => t_14
                                                         , Tags_sel14_slide0_0 => t_19
                                                         , Tags_sel15_slide1_0 => t_25 )
                                                         , iterateI_ho1_0_res_29
                                                         , iterateI_ho1_1_res_25
                                                         , iterateI_ho1_2_res_23
                                                         , iterateI_ho1_3_res_20
                                                         , iterateI_ho1_4_res_19
                                                         , iterateI_ho1_5_res_18 );


  end block;

  result_88 <=  \c$app_arg_35\(\c$app_arg_35\'high) ;

  winTag_3 <= result_90.Tuple2_2_sel0_unsigned;

  result_89 <= t_13 when b_5 else
               result_90;

  -- register begin
  result_90_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_90 <= ( Tuple2_2_sel0_unsigned => to_unsigned(5,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_90 <= result_89;
      end if;
    end if;
  end process;
  -- register end

  b_5 <= result_92;

  t_13 <= ( Tuple2_2_sel0_unsigned => result_93.Tags_sel11_output9_0
          , Tuple2_2_sel1_signed => (x_27 + to_signed(1,64)) );

  x_27 <= \c$out9_case_alt\;

  \c$out9_case_alt_selection_res\ <= result_91 = winTag_3;

  \c$out9_case_alt\ <= winData_3 when \c$out9_case_alt_selection_res\ else
                       to_signed(0,64);

  winData_3 <= result_90.Tuple2_2_sel1_signed;

  result_selection_res_12 <= result_93.Tags_sel11_output9_0 > to_unsigned(1,8);

  result_91 <= \c$app_arg_36\ when result_selection_res_12 else
               \c$app_arg_36\ + to_unsigned(4,8);

  \c$app_arg_36\ <= result_93.Tags_sel11_output9_0 - to_unsigned(1,8);

  iterateI_30 : block
    signal iterateI_ho1_0_res_30 : boolean;
    signal iterateI_ho1_1_res_26 : boolean;
    signal iterateI_ho1_2_res_24 : boolean;
    signal iterateI_ho1_3_res_21 : boolean;
    signal iterateI_ho1_4_res_20 : boolean;
    signal iterateI_ho1_5_res_19 : boolean;
  begin
    r_block_366 : block
        signal \c$bb_res_res_172\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_172_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_172\ <= pOut9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_30 <= \c$bb_res_res_172\;


      end block;

    r_block_367 : block
        signal \c$bb_res_res_173\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_173_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_173\ <= iterateI_ho1_0_res_30;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_26 <= \c$bb_res_res_173\;


      end block;

    r_block_368 : block
        signal \c$bb_res_res_174\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_174_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_174\ <= iterateI_ho1_1_res_26;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_24 <= \c$bb_res_res_174\;


      end block;

    r_block_369 : block
        signal \c$bb_res_res_175\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_175_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_175\ <= iterateI_ho1_2_res_24;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_21 <= \c$bb_res_res_175\;


      end block;

    r_block_370 : block
        signal \c$bb_res_res_176\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_176_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_176\ <= iterateI_ho1_3_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_20 <= \c$bb_res_res_176\;


      end block;

    r_block_371 : block
        signal \c$bb_res_res_177\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_177_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_177\ <= iterateI_ho1_4_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_19 <= \c$bb_res_res_177\;


      end block;

    \c$app_arg_37\ <= Spec_topEntity_types.array_of_boolean'( pOut9
                                                            , iterateI_ho1_0_res_30
                                                            , iterateI_ho1_1_res_26
                                                            , iterateI_ho1_2_res_24
                                                            , iterateI_ho1_3_res_21
                                                            , iterateI_ho1_4_res_20
                                                            , iterateI_ho1_5_res_19 );


  end block;

  result_92 <=  \c$app_arg_37\(\c$app_arg_37\'high) ;

  iterateI_31 : block
    signal iterateI_ho1_0_arg0_12 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_31  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_27  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_25  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_22  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_21  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_12 <= ( Tags_sel0_input0_0 => t_27
                              , Tags_sel1_input1_0 => t_26
                              , Tags_sel2_output0_0 => t_21
                              , Tags_sel3_output1_0 => t_25
                              , Tags_sel4_output2_0 => t_20
                              , Tags_sel5_output3_0 => t_24
                              , Tags_sel6_output4_0 => t_23
                              , Tags_sel7_output5_0 => t_19
                              , Tags_sel8_output6_0 => t_18
                              , Tags_sel9_output7_0 => t_17
                              , Tags_sel10_output8_0 => t_22
                              , Tags_sel11_output9_0 => t_16
                              , Tags_sel12_output10_0 => t_15
                              , Tags_sel13_output11_0 => t_14
                              , Tags_sel14_slide0_0 => t_19
                              , Tags_sel15_slide1_0 => t_25 );

    r_block_372 : block
        signal \c$bb_res_res_178\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_178_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_178\ <= iterateI_ho1_0_arg0_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_31 <= \c$bb_res_res_178\;


      end block;

    r_block_373 : block
        signal \c$bb_res_res_179\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_179_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_179\ <= iterateI_ho1_0_res_31;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_27 <= \c$bb_res_res_179\;


      end block;

    r_block_374 : block
        signal \c$bb_res_res_180\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_180_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_180\ <= iterateI_ho1_1_res_27;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_25 <= \c$bb_res_res_180\;


      end block;

    r_block_375 : block
        signal \c$bb_res_res_181\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_181_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_181\ <= iterateI_ho1_2_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_22 <= \c$bb_res_res_181\;


      end block;

    r_block_376 : block
        signal \c$bb_res_res_182\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_182_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_182\ <= iterateI_ho1_3_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_21 <= \c$bb_res_res_182\;


      end block;

    \c$app_arg_38\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_27
                                                         , Tags_sel1_input1_0 => t_26
                                                         , Tags_sel2_output0_0 => t_21
                                                         , Tags_sel3_output1_0 => t_25
                                                         , Tags_sel4_output2_0 => t_20
                                                         , Tags_sel5_output3_0 => t_24
                                                         , Tags_sel6_output4_0 => t_23
                                                         , Tags_sel7_output5_0 => t_19
                                                         , Tags_sel8_output6_0 => t_18
                                                         , Tags_sel9_output7_0 => t_17
                                                         , Tags_sel10_output8_0 => t_22
                                                         , Tags_sel11_output9_0 => t_16
                                                         , Tags_sel12_output10_0 => t_15
                                                         , Tags_sel13_output11_0 => t_14
                                                         , Tags_sel14_slide0_0 => t_19
                                                         , Tags_sel15_slide1_0 => t_25 )
                                                         , iterateI_ho1_0_res_31
                                                         , iterateI_ho1_1_res_27
                                                         , iterateI_ho1_2_res_25
                                                         , iterateI_ho1_3_res_22
                                                         , iterateI_ho1_4_res_21 );


  end block;

  result_93 <=  \c$app_arg_38\(\c$app_arg_38\'high) ;

  iterateI_32 : block
    signal iterateI_ho1_0_arg0_13 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_32  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_28  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_26  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_23  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_13 <= ( Tags_sel0_input0_0 => t_27
                              , Tags_sel1_input1_0 => t_26
                              , Tags_sel2_output0_0 => t_21
                              , Tags_sel3_output1_0 => t_25
                              , Tags_sel4_output2_0 => t_20
                              , Tags_sel5_output3_0 => t_24
                              , Tags_sel6_output4_0 => t_23
                              , Tags_sel7_output5_0 => t_19
                              , Tags_sel8_output6_0 => t_18
                              , Tags_sel9_output7_0 => t_17
                              , Tags_sel10_output8_0 => t_22
                              , Tags_sel11_output9_0 => t_16
                              , Tags_sel12_output10_0 => t_15
                              , Tags_sel13_output11_0 => t_14
                              , Tags_sel14_slide0_0 => t_19
                              , Tags_sel15_slide1_0 => t_25 );

    r_block_377 : block
        signal \c$bb_res_res_183\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_183_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_183\ <= iterateI_ho1_0_arg0_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_32 <= \c$bb_res_res_183\;


      end block;

    r_block_378 : block
        signal \c$bb_res_res_184\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_184_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_184\ <= iterateI_ho1_0_res_32;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_28 <= \c$bb_res_res_184\;


      end block;

    r_block_379 : block
        signal \c$bb_res_res_185\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_185_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_185\ <= iterateI_ho1_1_res_28;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_26 <= \c$bb_res_res_185\;


      end block;

    r_block_380 : block
        signal \c$bb_res_res_186\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_186_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_186\ <= iterateI_ho1_2_res_26;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_23 <= \c$bb_res_res_186\;


      end block;

    \c$app_arg_39\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_27
                                                         , Tags_sel1_input1_0 => t_26
                                                         , Tags_sel2_output0_0 => t_21
                                                         , Tags_sel3_output1_0 => t_25
                                                         , Tags_sel4_output2_0 => t_20
                                                         , Tags_sel5_output3_0 => t_24
                                                         , Tags_sel6_output4_0 => t_23
                                                         , Tags_sel7_output5_0 => t_19
                                                         , Tags_sel8_output6_0 => t_18
                                                         , Tags_sel9_output7_0 => t_17
                                                         , Tags_sel10_output8_0 => t_22
                                                         , Tags_sel11_output9_0 => t_16
                                                         , Tags_sel12_output10_0 => t_15
                                                         , Tags_sel13_output11_0 => t_14
                                                         , Tags_sel14_slide0_0 => t_19
                                                         , Tags_sel15_slide1_0 => t_25 )
                                                         , iterateI_ho1_0_res_32
                                                         , iterateI_ho1_1_res_28
                                                         , iterateI_ho1_2_res_26
                                                         , iterateI_ho1_3_res_23 );


  end block;

  result_94 <=  \c$app_arg_39\(\c$app_arg_39\'high) ;

  iterateI_33 : block
    signal iterateI_ho1_0_arg0_14 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_33  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_29  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_27  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_14 <= ( Tags_sel0_input0_0 => t_27
                              , Tags_sel1_input1_0 => t_26
                              , Tags_sel2_output0_0 => t_21
                              , Tags_sel3_output1_0 => t_25
                              , Tags_sel4_output2_0 => t_20
                              , Tags_sel5_output3_0 => t_24
                              , Tags_sel6_output4_0 => t_23
                              , Tags_sel7_output5_0 => t_19
                              , Tags_sel8_output6_0 => t_18
                              , Tags_sel9_output7_0 => t_17
                              , Tags_sel10_output8_0 => t_22
                              , Tags_sel11_output9_0 => t_16
                              , Tags_sel12_output10_0 => t_15
                              , Tags_sel13_output11_0 => t_14
                              , Tags_sel14_slide0_0 => t_19
                              , Tags_sel15_slide1_0 => t_25 );

    r_block_381 : block
        signal \c$bb_res_res_187\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_187_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_187\ <= iterateI_ho1_0_arg0_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_33 <= \c$bb_res_res_187\;


      end block;

    r_block_382 : block
        signal \c$bb_res_res_188\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_188_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_188\ <= iterateI_ho1_0_res_33;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_29 <= \c$bb_res_res_188\;


      end block;

    r_block_383 : block
        signal \c$bb_res_res_189\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_189_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_189\ <= iterateI_ho1_1_res_29;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_27 <= \c$bb_res_res_189\;


      end block;

    \c$app_arg_40\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_27
                                                         , Tags_sel1_input1_0 => t_26
                                                         , Tags_sel2_output0_0 => t_21
                                                         , Tags_sel3_output1_0 => t_25
                                                         , Tags_sel4_output2_0 => t_20
                                                         , Tags_sel5_output3_0 => t_24
                                                         , Tags_sel6_output4_0 => t_23
                                                         , Tags_sel7_output5_0 => t_19
                                                         , Tags_sel8_output6_0 => t_18
                                                         , Tags_sel9_output7_0 => t_17
                                                         , Tags_sel10_output8_0 => t_22
                                                         , Tags_sel11_output9_0 => t_16
                                                         , Tags_sel12_output10_0 => t_15
                                                         , Tags_sel13_output11_0 => t_14
                                                         , Tags_sel14_slide0_0 => t_19
                                                         , Tags_sel15_slide1_0 => t_25 )
                                                         , iterateI_ho1_0_res_33
                                                         , iterateI_ho1_1_res_29
                                                         , iterateI_ho1_2_res_27 );


  end block;

  result_95 <=  \c$app_arg_40\(\c$app_arg_40\'high) ;

  iterateI_34 : block
    signal iterateI_ho1_0_arg0_15 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_34  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_30  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_15 <= ( Tags_sel0_input0_0 => t_27
                              , Tags_sel1_input1_0 => t_26
                              , Tags_sel2_output0_0 => t_21
                              , Tags_sel3_output1_0 => t_25
                              , Tags_sel4_output2_0 => t_20
                              , Tags_sel5_output3_0 => t_24
                              , Tags_sel6_output4_0 => t_23
                              , Tags_sel7_output5_0 => t_19
                              , Tags_sel8_output6_0 => t_18
                              , Tags_sel9_output7_0 => t_17
                              , Tags_sel10_output8_0 => t_22
                              , Tags_sel11_output9_0 => t_16
                              , Tags_sel12_output10_0 => t_15
                              , Tags_sel13_output11_0 => t_14
                              , Tags_sel14_slide0_0 => t_19
                              , Tags_sel15_slide1_0 => t_25 );

    r_block_384 : block
        signal \c$bb_res_res_190\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_190_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_190\ <= iterateI_ho1_0_arg0_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_34 <= \c$bb_res_res_190\;


      end block;

    r_block_385 : block
        signal \c$bb_res_res_191\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_191_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_191\ <= iterateI_ho1_0_res_34;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_30 <= \c$bb_res_res_191\;


      end block;

    \c$app_arg_41\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_27
                                                         , Tags_sel1_input1_0 => t_26
                                                         , Tags_sel2_output0_0 => t_21
                                                         , Tags_sel3_output1_0 => t_25
                                                         , Tags_sel4_output2_0 => t_20
                                                         , Tags_sel5_output3_0 => t_24
                                                         , Tags_sel6_output4_0 => t_23
                                                         , Tags_sel7_output5_0 => t_19
                                                         , Tags_sel8_output6_0 => t_18
                                                         , Tags_sel9_output7_0 => t_17
                                                         , Tags_sel10_output8_0 => t_22
                                                         , Tags_sel11_output9_0 => t_16
                                                         , Tags_sel12_output10_0 => t_15
                                                         , Tags_sel13_output11_0 => t_14
                                                         , Tags_sel14_slide0_0 => t_19
                                                         , Tags_sel15_slide1_0 => t_25 )
                                                         , iterateI_ho1_0_res_34
                                                         , iterateI_ho1_1_res_30 );


  end block;

  result_96 <=  \c$app_arg_41\(\c$app_arg_41\'high) ;

  iterateI_35 : block
    signal iterateI_ho1_0_arg0_16 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_35  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_16 <= ( Tags_sel0_input0_0 => t_27
                              , Tags_sel1_input1_0 => t_26
                              , Tags_sel2_output0_0 => t_21
                              , Tags_sel3_output1_0 => t_25
                              , Tags_sel4_output2_0 => t_20
                              , Tags_sel5_output3_0 => t_24
                              , Tags_sel6_output4_0 => t_23
                              , Tags_sel7_output5_0 => t_19
                              , Tags_sel8_output6_0 => t_18
                              , Tags_sel9_output7_0 => t_17
                              , Tags_sel10_output8_0 => t_22
                              , Tags_sel11_output9_0 => t_16
                              , Tags_sel12_output10_0 => t_15
                              , Tags_sel13_output11_0 => t_14
                              , Tags_sel14_slide0_0 => t_19
                              , Tags_sel15_slide1_0 => t_25 );

    r_block_386 : block
        signal \c$bb_res_res_192\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(5,8)
      , Tags_sel1_input1_0 => to_unsigned(5,8)
      , Tags_sel2_output0_0 => to_unsigned(5,8)
      , Tags_sel3_output1_0 => to_unsigned(5,8)
      , Tags_sel4_output2_0 => to_unsigned(5,8)
      , Tags_sel5_output3_0 => to_unsigned(5,8)
      , Tags_sel6_output4_0 => to_unsigned(5,8)
      , Tags_sel7_output5_0 => to_unsigned(5,8)
      , Tags_sel8_output6_0 => to_unsigned(5,8)
      , Tags_sel9_output7_0 => to_unsigned(5,8)
      , Tags_sel10_output8_0 => to_unsigned(5,8)
      , Tags_sel11_output9_0 => to_unsigned(5,8)
      , Tags_sel12_output10_0 => to_unsigned(5,8)
      , Tags_sel13_output11_0 => to_unsigned(5,8)
      , Tags_sel14_slide0_0 => to_unsigned(5,8)
      , Tags_sel15_slide1_0 => to_unsigned(5,8) );
      begin
        -- delay begin
        cbb_res_res_192_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_192\ <= iterateI_ho1_0_arg0_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_35 <= \c$bb_res_res_192\;


      end block;

    \c$app_arg_42\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_27
                                                         , Tags_sel1_input1_0 => t_26
                                                         , Tags_sel2_output0_0 => t_21
                                                         , Tags_sel3_output1_0 => t_25
                                                         , Tags_sel4_output2_0 => t_20
                                                         , Tags_sel5_output3_0 => t_24
                                                         , Tags_sel6_output4_0 => t_23
                                                         , Tags_sel7_output5_0 => t_19
                                                         , Tags_sel8_output6_0 => t_18
                                                         , Tags_sel9_output7_0 => t_17
                                                         , Tags_sel10_output8_0 => t_22
                                                         , Tags_sel11_output9_0 => t_16
                                                         , Tags_sel12_output10_0 => t_15
                                                         , Tags_sel13_output11_0 => t_14
                                                         , Tags_sel14_slide0_0 => t_19
                                                         , Tags_sel15_slide1_0 => t_25 )
                                                         , iterateI_ho1_0_res_35 );


  end block;

  result_97 <=  \c$app_arg_42\(\c$app_arg_42\'high) ;

  -- register begin
  t_14_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_14 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_14 <= result_98;
      end if;
    end if;
  end process;
  -- register end

  result_selection_44 <= pOut11;

  result_98 <= result_99 when result_selection_44 else
               t_14;

  b_6 <= t_14 = to_unsigned(4,8);

  f1 <= t_14 + to_unsigned(1,8);

  result_99 <= to_unsigned(1,8) when b_6 else
               f1;

  pOut11 <= pacings.Pacings_1_sel13_pacingOut11;

  -- register begin
  t_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_15 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_15 <= result_100;
      end if;
    end if;
  end process;
  -- register end

  result_selection_48 <= pOut10;

  result_100 <= result_101 when result_selection_48 else
                t_15;

  b_7 <= t_15 = to_unsigned(4,8);

  f1_0 <= t_15 + to_unsigned(1,8);

  result_101 <= to_unsigned(1,8) when b_7 else
                f1_0;

  pOut10 <= pacings.Pacings_1_sel12_pacingOut10;

  -- register begin
  t_16_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_16 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_16 <= result_102;
      end if;
    end if;
  end process;
  -- register end

  result_selection_52 <= pOut9;

  result_102 <= result_103 when result_selection_52 else
                t_16;

  b_8 <= t_16 = to_unsigned(4,8);

  f1_1 <= t_16 + to_unsigned(1,8);

  result_103 <= to_unsigned(1,8) when b_8 else
                f1_1;

  pOut9 <= pacings.Pacings_1_sel11_pacingOut9;

  -- register begin
  t_17_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_17 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_17 <= result_104;
      end if;
    end if;
  end process;
  -- register end

  result_104 <= result_105 when \c$ds11_case_alt\ else
                t_17;

  b_9 <= t_17 = to_unsigned(4,8);

  f1_2 <= t_17 + to_unsigned(1,8);

  result_105 <= to_unsigned(1,8) when b_9 else
                f1_2;

  \c$ds11_case_alt\ <= x0_3 and x1_3;

  x1_3 <= pOut7.PacingOut7_sel1_boolean_1;

  x0_3 <= pOut7.PacingOut7_sel0_boolean_0;

  pOut7 <= pacings.Pacings_1_sel9_pacingOut7_0;

  -- register begin
  t_18_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_18 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_18 <= result_106;
      end if;
    end if;
  end process;
  -- register end

  result_106 <= result_107 when \c$ds10_case_alt\ else
                t_18;

  b_10 <= t_18 = to_unsigned(4,8);

  f1_3 <= t_18 + to_unsigned(1,8);

  result_107 <= to_unsigned(1,8) when b_10 else
                f1_3;

  \c$ds10_case_alt\ <= x0_4 and x1_4;

  x1_4 <= pOut6.PacingOut6_sel1_boolean_1;

  x0_4 <= pOut6.PacingOut6_sel0_boolean_0;

  pOut6 <= pacings.Pacings_1_sel8_pacingOut6_0;

  -- register begin
  t_19_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_19 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_19 <= result_108;
      end if;
    end if;
  end process;
  -- register end

  result_108 <= result_109 when \c$ds9_case_alt\ else
                t_19;

  b_11 <= t_19 = to_unsigned(4,8);

  f1_4 <= t_19 + to_unsigned(1,8);

  result_109 <= to_unsigned(1,8) when b_11 else
                f1_4;

  \c$ds9_case_alt\ <= x0_5 and x1_5;

  x1_5 <= pOut5.PacingOut5_sel1_boolean_1;

  x0_5 <= pOut5.PacingOut5_sel0_boolean_0;

  pOut5 <= pacings.Pacings_1_sel7_pacingOut5_0;

  -- register begin
  t_20_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_20 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_20 <= result_110;
      end if;
    end if;
  end process;
  -- register end

  result_selection_62 <= x_28;

  result_110 <= result_111 when result_selection_62 else
                t_20;

  b_12 <= t_20 = to_unsigned(4,8);

  f1_5 <= t_20 + to_unsigned(1,8);

  result_111 <= to_unsigned(1,8) when b_12 else
                f1_5;

  x_28 <= pOut2;

  pOut2 <= pacings.Pacings_1_sel4_pacingOut2;

  -- register begin
  t_21_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_21 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_21 <= result_112;
      end if;
    end if;
  end process;
  -- register end

  result_selection_66 <= x_29;

  result_112 <= result_113 when result_selection_66 else
                t_21;

  b_13 <= t_21 = to_unsigned(4,8);

  f1_6 <= t_21 + to_unsigned(1,8);

  result_113 <= to_unsigned(1,8) when b_13 else
                f1_6;

  x_29 <= pOut0;

  pOut0 <= pacings.Pacings_1_sel2_pacingOut0;

  -- register begin
  t_22_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_22 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_22 <= result_114;
      end if;
    end if;
  end process;
  -- register end

  result_selection_70 <= x_30;

  result_114 <= result_115 when result_selection_70 else
                t_22;

  b_14 <= t_22 = to_unsigned(4,8);

  f1_7 <= t_22 + to_unsigned(1,8);

  result_115 <= to_unsigned(1,8) when b_14 else
                f1_7;

  x_30 <= pOut8;

  pOut8 <= pacings.Pacings_1_sel10_pacingOut8;

  -- register begin
  t_23_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_23 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_23 <= result_116;
      end if;
    end if;
  end process;
  -- register end

  result_selection_74 <= x_31;

  result_116 <= result_117 when result_selection_74 else
                t_23;

  b_15 <= t_23 = to_unsigned(4,8);

  f1_8 <= t_23 + to_unsigned(1,8);

  result_117 <= to_unsigned(1,8) when b_15 else
                f1_8;

  x_31 <= pOut4;

  pOut4 <= pacings.Pacings_1_sel6_pacingOut4;

  -- register begin
  t_24_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_24 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_24 <= result_118;
      end if;
    end if;
  end process;
  -- register end

  result_selection_78 <= x_32;

  result_118 <= result_119 when result_selection_78 else
                t_24;

  b_16 <= t_24 = to_unsigned(4,8);

  f1_9 <= t_24 + to_unsigned(1,8);

  result_119 <= to_unsigned(1,8) when b_16 else
                f1_9;

  x_32 <= pOut3;

  pOut3 <= pacings.Pacings_1_sel5_pacingOut3;

  -- register begin
  t_25_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_25 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_25 <= result_120;
      end if;
    end if;
  end process;
  -- register end

  result_selection_82 <= x_33;

  result_120 <= result_121 when result_selection_82 else
                t_25;

  b_17 <= t_25 = to_unsigned(4,8);

  f1_10 <= t_25 + to_unsigned(1,8);

  result_121 <= to_unsigned(1,8) when b_17 else
                f1_10;

  x_33 <= pOut1;

  pOut1 <= pacings.Pacings_1_sel3_pacingOut1;

  -- register begin
  t_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_26 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_26 <= result_122;
      end if;
    end if;
  end process;
  -- register end

  result_selection_86 <= pIn1_0;

  result_122 <= result_123 when result_selection_86 else
                t_26;

  b_18 <= t_26 = to_unsigned(4,8);

  f1_11 <= t_26 + to_unsigned(1,8);

  result_123 <= to_unsigned(1,8) when b_18 else
                f1_11;

  pIn1_0 <= pacings.Pacings_1_sel1_pacingIn1;

  -- register begin
  t_27_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_27 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_27 <= result_124;
      end if;
    end if;
  end process;
  -- register end

  result_selection_90 <= pIn0_0;

  result_124 <= result_125 when result_selection_90 else
                t_27;

  b_19 <= t_27 = to_unsigned(4,8);

  f1_12 <= t_27 + to_unsigned(1,8);

  result_125 <= to_unsigned(1,8) when b_19 else
                f1_12;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_130.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  -- register begin
  toWait_register : process(clk,rst)
  begin
    if rst =  '1'  then
      toWait <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        toWait <= result_127;
      end if;
    end if;
  end process;
  -- register end

  result_126 <= toWait = (to_signed(0,64));

  result_127 <= to_signed(6,64) when result_129 else
                result_128;

  b_20 <= toWait > (to_signed(0,64));

  result_128 <= (x_34 - to_signed(1,64)) when b_20 else
                toWait;

  x_34 <= toWait;

  result_129 <= true when b_21 else
                false;

  b_21 <= result_126 and result_130.Tuple3_sel1_boolean_1;

  result_130 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_45\
                , Tuple3_sel1_boolean_1 => \c$app_arg_44\
                , Tuple3_sel2_Tuple3_0 => \c$app_arg_43\ );

  -- register begin
  capp_arg_43_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_43\ <= ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false ) )
  , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
  , Slides_1_sel1_slide1 => false )
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingIn1 => false
  , Pacings_1_sel2_pacingOut0 => false
  , Pacings_1_sel3_pacingOut1 => false
  , Pacings_1_sel4_pacingOut2 => false
  , Pacings_1_sel5_pacingOut3 => false
  , Pacings_1_sel6_pacingOut4 => false
  , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
  , PacingOut5_sel1_boolean_1 => false )
  , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
  , PacingOut6_sel1_boolean_1 => false )
  , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
  , PacingOut7_sel1_boolean_1 => false )
  , Pacings_1_sel10_pacingOut8 => false
  , Pacings_1_sel11_pacingOut9 => false
  , Pacings_1_sel12_pacingOut10 => false
  , Pacings_1_sel13_pacingOut11 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_43\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                    , Slides_1_sel1_slide1 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingOut0 => false
                    , Pacings_1_sel3_pacingOut1 => false
                    , Pacings_1_sel4_pacingOut2 => false
                    , Pacings_1_sel5_pacingOut3 => false
                    , Pacings_1_sel6_pacingOut4 => false
                    , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                    , PacingOut5_sel1_boolean_1 => false )
                    , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                    , PacingOut6_sel1_boolean_1 => false )
                    , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                    , PacingOut7_sel1_boolean_1 => false )
                    , Pacings_1_sel10_pacingOut8 => false
                    , Pacings_1_sel11_pacingOut9 => false
                    , Pacings_1_sel12_pacingOut10 => false
                    , Pacings_1_sel13_pacingOut11 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                    , Slides_1_sel1_slide1 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingOut0 => false
                    , Pacings_1_sel3_pacingOut1 => false
                    , Pacings_1_sel4_pacingOut2 => false
                    , Pacings_1_sel5_pacingOut3 => false
                    , Pacings_1_sel6_pacingOut4 => false
                    , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                    , PacingOut5_sel1_boolean_1 => false )
                    , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                    , PacingOut6_sel1_boolean_1 => false )
                    , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                    , PacingOut7_sel1_boolean_1 => false )
                    , Pacings_1_sel10_pacingOut8 => false
                    , Pacings_1_sel11_pacingOut9 => false
                    , Pacings_1_sel12_pacingOut10 => false
                    , Pacings_1_sel13_pacingOut11 => false ) );

  with (x_35) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_35) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false ) )
                      , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                      , Slides_1_sel1_slide1 => false )
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingIn1 => false
                      , Pacings_1_sel2_pacingOut0 => false
                      , Pacings_1_sel3_pacingOut1 => false
                      , Pacings_1_sel4_pacingOut2 => false
                      , Pacings_1_sel5_pacingOut3 => false
                      , Pacings_1_sel6_pacingOut4 => false
                      , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                      , PacingOut5_sel1_boolean_1 => false )
                      , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                      , PacingOut6_sel1_boolean_1 => false )
                      , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                      , PacingOut7_sel1_boolean_1 => false )
                      , Pacings_1_sel10_pacingOut8 => false
                      , Pacings_1_sel11_pacingOut9 => false
                      , Pacings_1_sel12_pacingOut10 => false
                      , Pacings_1_sel13_pacingOut11 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_3 : block
    signal vec_index_5 : integer range 0 to 8-1;
  begin
    vec_index_5 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 8
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_5);
  end block;
  -- index end

  -- register begin
  capp_arg_44_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_44\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_44\ <= \c$case_alt_5\;
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

  with (x_35) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_45_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_45\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_45\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_46\);

  \c$app_arg_46\ <= to_signed(1,64) when x_35 /= to_signed(8,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3 => false
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5_0 => ( PacingOut5_sel0_boolean_0 => false
                                         , PacingOut5_sel1_boolean_1 => false )
                                         , Pacings_1_sel8_pacingOut6_0 => ( PacingOut6_sel0_boolean_0 => false
                                         , PacingOut6_sel1_boolean_1 => false )
                                         , Pacings_1_sel9_pacingOut7_0 => ( PacingOut7_sel0_boolean_0 => false
                                         , PacingOut7_sel1_boolean_1 => false )
                                         , Pacings_1_sel10_pacingOut8 => false
                                         , Pacings_1_sel11_pacingOut9 => false
                                         , Pacings_1_sel12_pacingOut10 => false
                                         , Pacings_1_sel13_pacingOut11 => false ) ) );
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

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_7\(0 to 8-1),\c$vec_7\(8 to \c$vec_7\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_8_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_8\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_8\(0 to 8-1),\c$vec_8\(8 to \c$vec_8\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_8_sel0_array_of_Tuple3_0_0;

  qData <= result_16.Tuple2_sel1_Tuple3_0;

  x_35 <= cursor;

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

  push <= result_16.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_35 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_35 = to_signed(8,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_17.Tuple2_0_sel0_boolean;

  with (x_35) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_35 - to_signed(1,64));

  result <= result_17.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_3 <= result.Outputs_sel3_output3;

  result_4 <= result.Outputs_sel4_output4;

  result_5 <= result.Outputs_sel5_output5;

  result_6 <= result.Outputs_sel6_output6;

  result_7 <= result.Outputs_sel7_output7;

  result_8 <= result.Outputs_sel8_output8;

  result_9 <= result.Outputs_sel9_output9;

  result_10 <= result.Outputs_sel10_output10;

  result_11 <= result.Outputs_sel11_output11;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidInt_sel0_value;

  result_1_1 <= result_1.ValidInt_sel1_valid;

  result_2_0 <= result_2.ValidInt_sel0_value;

  result_2_1 <= result_2.ValidInt_sel1_valid;

  result_3_0 <= result_3.ValidInt_sel0_value;

  result_3_1 <= result_3.ValidInt_sel1_valid;

  result_4_0 <= result_4.ValidInt_sel0_value;

  result_4_1 <= result_4.ValidInt_sel1_valid;

  result_5_0 <= result_5.ValidInt_sel0_value;

  result_5_1 <= result_5.ValidInt_sel1_valid;

  result_6_0 <= result_6.ValidInt_sel0_value;

  result_6_1 <= result_6.ValidInt_sel1_valid;

  result_7_0 <= result_7.ValidInt_sel0_value;

  result_7_1 <= result_7.ValidInt_sel1_valid;

  result_8_0 <= result_8.ValidInt_sel0_value;

  result_8_1 <= result_8.ValidInt_sel1_valid;

  result_9_0 <= result_9.ValidInt_sel0_value;

  result_9_1 <= result_9.ValidInt_sel1_valid;

  result_10_0 <= result_10.ValidInt_sel0_value;

  result_10_1 <= result_10.ValidInt_sel1_valid;

  result_11_0 <= result_11.ValidInt_sel0_value;

  result_11_1 <= result_11.ValidInt_sel1_valid;


end;


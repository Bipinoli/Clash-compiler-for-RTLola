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
       result_3_0 : out signed(63 downto 0);
       result_3_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_4                        : signed(63 downto 0);
  signal x                               : signed(63 downto 0);
  signal result_5                        : signed(63 downto 0) := (to_signed(0,64));
  signal result_6                        : Spec_topEntity_types.Tuple2;
  -- spec.hs:197:1-82
  signal hasInput0                       : boolean;
  -- spec.hs:197:1-82
  signal pIn0                            : boolean;
  -- spec.hs:197:1-82
  signal timer0Over                      : boolean;
  signal result_7                        : Spec_topEntity_types.Tuple2_0;
  -- spec.hs:242:1-63
  signal \c$ws_app_arg\                  : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_0\                : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut\                  : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_0\                : Spec_topEntity_types.Maybe;
  signal result_8                        : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal i                               : Spec_topEntity_types.index_2;
  -- spec.hs:242:1-63
  signal ws                              : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg\                     : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_9                        : boolean;
  signal result_10                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:398:1-137
  signal result_11                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:398:1-137
  signal b                               : boolean;
  -- spec.hs:398:1-137
  signal t                               : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:276:1-91
  signal \c$ds1_case_alt\                : signed(63 downto 0);
  signal \c$app_arg_0\                   : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_12                       : boolean;
  signal \c$app_arg_1\                   : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_13                       : boolean;
  signal result_14                       : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:77:34-42
  signal x_0                             : boolean;
  -- spec.hs:390:1-147
  signal result_15                       : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:390:1-147
  signal t_0                             : Spec_topEntity_types.Tuple2_2;
  signal x_1                             : signed(63 downto 0);
  signal y                               : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_1\                : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_2\                : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_1\                : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_2\                : Spec_topEntity_types.Maybe;
  signal result_16                       : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal i_0                             : Spec_topEntity_types.index_2;
  -- spec.hs:242:1-63
  signal ws_0                            : Spec_topEntity_types.array_of_Maybe(0 to 2);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_3\                : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_4\                : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_3\                : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_4\                : Spec_topEntity_types.Maybe;
  signal result_17                       : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal i_1                             : Spec_topEntity_types.index_2;
  -- spec.hs:242:1-63
  signal ws_1                            : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_2\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_18                       : boolean;
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_5\                : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_6\                : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_5\                : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_6\                : Spec_topEntity_types.Maybe;
  signal result_19                       : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal i_2                             : Spec_topEntity_types.index_2;
  -- spec.hs:242:1-63
  signal ws_2                            : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_3\                   : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_20                       : boolean;
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_7\                : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_8\                : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_7\                : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_8\                : Spec_topEntity_types.Maybe;
  signal result_21                       : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal i_3                             : Spec_topEntity_types.index_2;
  -- spec.hs:242:1-63
  signal ws_3                            : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_4\                   : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_22                       : boolean;
  signal result_23                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:76:34-42
  signal x_2                             : boolean;
  -- spec.hs:381:1-137
  signal result_24                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:381:1-137
  signal t_1                             : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  signal x_3                             : signed(63 downto 0);
  signal \c$app_arg_5\                   : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_25                       : boolean;
  signal result_26                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:75:34-42
  signal x_4                             : boolean;
  -- spec.hs:372:1-137
  signal result_27                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:372:1-137
  signal t_2                             : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  signal x_5                             : signed(63 downto 0);
  -- spec.hs:276:1-91
  signal \c$out0_case_alt\               : signed(63 downto 0);
  -- spec.hs:255:1-53
  signal dta                             : signed(63 downto 0);
  -- spec.hs:255:1-53
  signal tagToMatch                      : unsigned(7 downto 0);
  -- spec.hs:255:1-53
  signal tag                             : unsigned(7 downto 0);
  signal \c$app_arg_6\                   : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_28                       : boolean;
  signal result_29                       : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:366:1-127
  signal result_30                       : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:366:1-127
  signal b_0                             : boolean;
  -- spec.hs:276:1-91
  signal \c$input0Win_app_arg\           : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_7\                   : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_31                       : boolean;
  signal \c$app_arg_8\                   : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_32                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_9\                   : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_33                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_10\                  : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_34                       : Spec_topEntity_types.Tags;
  -- spec.hs:356:9-81
  signal t_3                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_35                       : unsigned(7 downto 0);
  -- spec.hs:356:9-81
  signal b_1                             : boolean;
  -- spec.hs:356:9-81
  signal f1                              : unsigned(7 downto 0);
  signal result_36                       : unsigned(7 downto 0);
  -- spec.hs:276:1-91
  signal pOut3                           : boolean;
  -- spec.hs:356:9-81
  signal t_4                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_37                       : unsigned(7 downto 0);
  -- spec.hs:356:9-81
  signal b_2                             : boolean;
  -- spec.hs:356:9-81
  signal f1_0                            : unsigned(7 downto 0);
  signal result_38                       : unsigned(7 downto 0);
  -- spec.hs:77:34-42
  signal x_6                             : boolean;
  -- spec.hs:276:1-91
  signal pOut2                           : boolean;
  -- spec.hs:356:9-81
  signal t_5                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_39                       : unsigned(7 downto 0);
  -- spec.hs:356:9-81
  signal b_3                             : boolean;
  -- spec.hs:356:9-81
  signal f1_1                            : unsigned(7 downto 0);
  signal result_40                       : unsigned(7 downto 0);
  -- spec.hs:76:34-42
  signal x_7                             : boolean;
  -- spec.hs:276:1-91
  signal pOut1                           : boolean;
  -- spec.hs:356:9-81
  signal t_6                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_41                       : unsigned(7 downto 0);
  -- spec.hs:356:9-81
  signal b_4                             : boolean;
  -- spec.hs:356:9-81
  signal f1_2                            : unsigned(7 downto 0);
  signal result_42                       : unsigned(7 downto 0);
  -- spec.hs:75:34-42
  signal x_8                             : boolean;
  -- spec.hs:276:1-91
  signal pOut0                           : boolean;
  -- spec.hs:356:9-81
  signal t_7                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_43                       : unsigned(7 downto 0);
  -- spec.hs:356:9-81
  signal b_5                             : boolean;
  -- spec.hs:356:9-81
  signal f1_3                            : unsigned(7 downto 0);
  signal result_44                       : unsigned(7 downto 0);
  -- spec.hs:276:1-91
  signal pIn0_0                          : boolean;
  -- spec.hs:276:1-91
  signal pacings                         : Spec_topEntity_types.Pacings_1;
  signal result_45                       : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_11\                  : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingOut0 => false
, Pacings_1_sel2_pacingOut1 => false
, Pacings_1_sel3_pacingOut2 => false
, Pacings_1_sel4_pacingOut3 => false ) );
  signal \c$case_alt\                    : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_0\                  : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_1\                  : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_2\                  : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_3\                  : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_4\                  : Spec_topEntity_types.Tuple2_3;
  signal \c$app_arg_12\                  : boolean := false;
  signal \c$case_alt_5\                  : boolean;
  signal \c$case_alt_6\                  : boolean;
  signal \c$case_alt_7\                  : boolean;
  signal \c$case_alt_8\                  : boolean;
  signal \c$case_alt_9\                  : boolean;
  signal \c$app_arg_13\                  : boolean := false;
  signal \c$case_alt_10\                 : boolean;
  signal \c$case_alt_11\                 : boolean;
  signal \c$case_alt_12\                 : boolean;
  signal \c$app_arg_14\                  : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \buffer\                        : Spec_topEntity_types.array_of_Tuple2_3(0 to 1) := Spec_topEntity_types.array_of_Tuple2_3'( ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) )
                                       , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) ) );
  -- spec.hs:128:1-78
  signal \c$buffer_case_alt\             : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:128:1-78
  signal \c$buffer_case_alt_0\           : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:128:1-78
  signal \c$buffer_case_alt_1\           : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:128:1-78
  signal qData                           : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:128:1-78
  signal x_9                             : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal cursor                          : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt\             : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal push                            : boolean;
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_0\           : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_1\           : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_scrut\           : boolean;
  -- spec.hs:128:1-78
  signal \c$cursor_app_arg\              : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_2\           : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal pop                             : boolean;
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_3\           : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_4\           : signed(63 downto 0);
  signal inputs                          : Spec_topEntity_types.ValidInt;
  signal \c$vec2\                        : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$vec\                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 2);
  signal t_projection_0                  : Spec_topEntity_types.Tuple2_4;
  signal \c$ds1_case_alt_selection_res\  : boolean;
  signal result_selection_4              : boolean;
  signal \c$vec2_0\                      : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$vec2_1\                      : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$vec2_2\                      : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$vec2_3\                      : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal result_selection_11             : boolean;
  signal \c$vec_0\                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 2);
  signal t_projection_5                  : Spec_topEntity_types.Tuple2_4;
  signal result_selection_14             : boolean;
  signal \c$vec_1\                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 2);
  signal t_projection_6                  : Spec_topEntity_types.Tuple2_4;
  signal \c$out0_case_alt_selection_res\ : boolean;
  signal result_selection_18             : boolean;
  signal result_selection_22             : boolean;
  signal result_selection_26             : boolean;
  signal result_selection_30             : boolean;
  signal result_selection_34             : boolean;
  signal \c$vec_2\                       : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\   : Spec_topEntity_types.Tuple2_5;
  signal \c$vec_3\                       : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\   : Spec_topEntity_types.Tuple2_5;
  signal result                          : Spec_topEntity_types.Outputs;
  signal result_0                        : Spec_topEntity_types.ValidInt;
  signal result_1                        : Spec_topEntity_types.ValidInt;
  signal result_2                        : Spec_topEntity_types.ValidInt;
  signal result_3                        : Spec_topEntity_types.ValidInt;

begin
  inputs <= ( ValidInt_sel0_value => inputs_0
            , ValidInt_sel1_valid => inputs_1 );

  result_4 <= to_signed(2000,64) when timer0Over else
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

  result_6 <= ( Tuple2_sel0_boolean => hasInput0 or timer0Over
              , Tuple2_sel1_Tuple2_3 => ( Tuple2_3_sel0_ValidInt => inputs
              , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
              , Pacings_1_sel1_pacingOut0 => pIn0
              , Pacings_1_sel2_pacingOut1 => pIn0
              , Pacings_1_sel3_pacingOut2 => pIn0
              , Pacings_1_sel4_pacingOut3 => timer0Over ) ) );

  hasInput0 <= inputs.ValidInt_sel1_valid;

  pIn0 <= hasInput0;

  timer0Over <= result_5 >= (to_signed(1000000,64));

  result_7 <= ( Tuple2_0_sel0_boolean => true
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => result_21
              , ValidInt_sel1_valid => result_22 )
              , Outputs_sel1_output1 => ( ValidInt_sel0_value => result_19
              , ValidInt_sel1_valid => result_20 )
              , Outputs_sel2_output2 => ( ValidInt_sel0_value => result_15.Tuple2_2_sel1_signed
              , ValidInt_sel1_valid => result_13 )
              , Outputs_sel3_output3 => ( ValidInt_sel0_value => result_8
              , ValidInt_sel1_valid => result_9 ) ) );

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_4 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_4) <= ( Tuple2_1_sel0_Tuple2_2 => result_11(i_4)
               , Tuple2_1_sel1_Maybe => \c$vec2\(i_4) );


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
    imap_0 : for i_5 in \c$ws_app_arg_0\'range generate
    begin
      fun_4 : block
      signal \c$case_alt_13\            : Spec_topEntity_types.Maybe;
      -- spec.hs:242:1-63
      signal t_8                        : unsigned(7 downto 0);
      -- spec.hs:242:1-63
      signal x_11                       : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_5) <= \c$case_alt_13\;

      \c$case_alt_selection_res\ <= t_8 = result_32.Tags_sel4_output3_0;

      \c$case_alt_13\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_5,max(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res\ else
                         \c$ws_app_arg\(i_5).Tuple2_1_sel1_Maybe;

      t_8 <= x_11.Tuple2_2_sel0_unsigned;

      x_11 <= \c$ws_app_arg\(i_5).Tuple2_1_sel0_Tuple2_2;


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
    \c$case_scrut\ <= result_11(vec_index);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(1 downto 1)) select
    result_8 <= to_signed(0,64) when "0",
                \c$case_scrut\.Tuple2_2_sel1_signed when others;

  i <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_0\(0 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut3;

    r_block_31 : block
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

    r_block_32 : block
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

    r_block_33 : block
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

    r_block_34 : block
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

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res );


  end block;

  result_9 <=  \c$app_arg\(\c$app_arg\'high) ;

  result_10 <= t when b else
               result_11;

  -- register begin
  result_11_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_11 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_11 <= result_10;
      end if;
    end if;
  end process;
  -- register end

  b <= result_12;

  \c$vec\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_11) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => result_34.Tags_sel4_output3_0
           , Tuple2_2_sel1_signed => \c$ds1_case_alt\ )))));

  t_projection_0 <= (\c$vec\(0 to 1-1),\c$vec\(1 to \c$vec\'high));

  t <= t_projection_0.Tuple2_4_sel1_array_of_Tuple2_2_1;

  \c$ds1_case_alt_selection_res\ <= tag = to_unsigned(4,8);

  \c$ds1_case_alt\ <= to_signed(0,64) when \c$ds1_case_alt_selection_res\ else
                      dta;

  iterateI_0 : block
    signal iterateI_ho1_0_res_0 : boolean;
    signal iterateI_ho1_1_res_0 : boolean;
  begin
    r_block_35 : block
        signal \c$bb_res_res_3\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_3_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_3\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_3\;


      end block;

    r_block_36 : block
        signal \c$bb_res_res_4\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= iterateI_ho1_0_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_4\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                           , iterateI_ho1_0_res_0
                                                           , iterateI_ho1_1_res_0 );


  end block;

  result_12 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_1  : boolean;
    signal iterateI_ho1_1_res_1  : boolean;
    signal iterateI_ho1_2_res_0  : boolean;
    signal iterateI_ho1_3_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= x_6;

    r_block_37 : block
        signal \c$bb_res_res_5\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_5_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_5\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_5\;


      end block;

    r_block_38 : block
        signal \c$bb_res_res_6\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_6\;


      end block;

    r_block_39 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_7\;


      end block;

    r_block_40 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_2_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_8\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( x_6
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_0
                                                           , iterateI_ho1_3_res_0 );


  end block;

  result_13 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  result_selection_4 <= x_0;

  result_14 <= t_0 when result_selection_4 else
               result_15;

  x_0 <= result_18;

  -- register begin
  result_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_15 <= ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_15 <= result_14;
      end if;
    end if;
  end process;
  -- register end

  t_0 <= ( Tuple2_2_sel0_unsigned => result_33.Tags_sel3_output2_0
         , Tuple2_2_sel1_signed => (x_1 + y) );

  x_1 <= result_17;

  y <= result_16;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_7 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_7) <= ( Tuple2_1_sel0_Tuple2_2 => result_24(i_7)
               , Tuple2_1_sel1_Maybe => \c$vec2_0\(i_7) );


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
    imap_2 : for i_8 in \c$ws_app_arg_2\'range generate
    begin
      fun_5 : block
      signal \c$case_alt_14\              : Spec_topEntity_types.Maybe;
      -- spec.hs:242:1-63
      signal t_10                         : unsigned(7 downto 0);
      -- spec.hs:242:1-63
      signal x_12                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_8) <= \c$case_alt_14\;

      \c$case_alt_selection_res_2\ <= t_10 = result_33.Tags_sel2_output1_0;

      \c$case_alt_14\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_8,max_0(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_2\ else
                         \c$ws_app_arg_1\(i_8).Tuple2_1_sel1_Maybe;

      t_10 <= x_12.Tuple2_2_sel0_unsigned;

      x_12 <= \c$ws_app_arg_1\(i_8).Tuple2_1_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_0 : block
    signal vec_index_0 : integer range 0 to 2-1;
  begin
    vec_index_0 <= to_integer((signed(std_logic_vector(resize(i_0,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_1\ <= result_24(vec_index_0);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(1 downto 1)) select
    result_16 <= to_signed(0,64) when "0",
                 \c$case_scrut_1\.Tuple2_2_sel1_signed when others;

  i_0 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_2\(0 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_9 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_9) <= ( Tuple2_1_sel0_Tuple2_2 => result_27(i_9)
               , Tuple2_1_sel1_Maybe => \c$vec2_1\(i_9) );


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
    imap_4 : for i_10 in \c$ws_app_arg_4\'range generate
    begin
      fun_6 : block
      signal \c$case_alt_15\              : Spec_topEntity_types.Maybe;
      -- spec.hs:242:1-63
      signal t_11                         : unsigned(7 downto 0);
      -- spec.hs:242:1-63
      signal x_13                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_10) <= \c$case_alt_15\;

      \c$case_alt_selection_res_3\ <= t_11 = result_33.Tags_sel1_output0_0;

      \c$case_alt_15\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_10,max_1(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_3\ else
                         \c$ws_app_arg_3\(i_10).Tuple2_1_sel1_Maybe;

      t_11 <= x_13.Tuple2_2_sel0_unsigned;

      x_13 <= \c$ws_app_arg_3\(i_10).Tuple2_1_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_1 : block
    signal vec_index_1 : integer range 0 to 2-1;
  begin
    vec_index_1 <= to_integer((signed(std_logic_vector(resize(i_1,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_3\ <= result_27(vec_index_1);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(1 downto 1)) select
    result_17 <= to_signed(0,64) when "0",
                 \c$case_scrut_3\.Tuple2_2_sel1_signed when others;

  i_1 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_4\(0 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_2 : block
    signal iterateI_ho1_0_res_2 : boolean;
    signal iterateI_ho1_1_res_2 : boolean;
    signal iterateI_ho1_2_res_1 : boolean;
  begin
    r_block_41 : block
        signal \c$bb_res_res_9\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_9\;


      end block;

    r_block_42 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_10\;


      end block;

    r_block_43 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_11\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_1 );


  end block;

  result_18 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  \c$vec2_2\ <= (ws_2(1 to ws_2'high));

  -- zipWith begin
  zipWith_2 : for i_11 in \c$ws_app_arg_5\'range generate
  begin
    \c$ws_app_arg_5\(i_11) <= ( Tuple2_1_sel0_Tuple2_2 => result_24(i_11)
               , Tuple2_1_sel1_Maybe => \c$vec2_2\(i_11) );


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
    imap_6 : for i_12 in \c$ws_app_arg_6\'range generate
    begin
      fun_7 : block
      signal \c$case_alt_16\              : Spec_topEntity_types.Maybe;
      -- spec.hs:242:1-63
      signal t_12                         : unsigned(7 downto 0);
      -- spec.hs:242:1-63
      signal x_14                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_4\ : boolean;
    begin
      \c$ws_app_arg_6\(i_12) <= \c$case_alt_16\;

      \c$case_alt_selection_res_4\ <= t_12 = result_32.Tags_sel2_output1_0;

      \c$case_alt_16\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_12,max_2(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_4\ else
                         \c$ws_app_arg_5\(i_12).Tuple2_1_sel1_Maybe;

      t_12 <= x_14.Tuple2_2_sel0_unsigned;

      x_14 <= \c$ws_app_arg_5\(i_12).Tuple2_1_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_2 : block
    signal vec_index_2 : integer range 0 to 2-1;
  begin
    vec_index_2 <= to_integer((signed(std_logic_vector(resize(i_2,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_5\ <= result_24(vec_index_2);
  end block;
  -- index end

  \c$case_scrut_6\ <=  ws_2(0) ;

  with (\c$case_scrut_6\(1 downto 1)) select
    result_19 <= to_signed(0,64) when "0",
                 \c$case_scrut_5\.Tuple2_2_sel1_signed when others;

  i_2 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_6\(0 downto 0)));

  ws_2 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_6\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_3 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_3  : boolean;
    signal iterateI_ho1_1_res_3  : boolean;
    signal iterateI_ho1_2_res_2  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= x_7;

    r_block_44 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_12\;


      end block;

    r_block_45 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_13\;


      end block;

    r_block_46 : block
        signal \c$bb_res_res_14\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_2 <= \c$bb_res_res_14\;


      end block;

    r_block_47 : block
        signal \c$bb_res_res_15\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_2_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_15\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( x_7
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_2
                                                           , iterateI_ho1_3_res_1 );


  end block;

  result_20 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  \c$vec2_3\ <= (ws_3(1 to ws_3'high));

  -- zipWith begin
  zipWith_3 : for i_13 in \c$ws_app_arg_7\'range generate
  begin
    \c$ws_app_arg_7\(i_13) <= ( Tuple2_1_sel0_Tuple2_2 => result_27(i_13)
               , Tuple2_1_sel1_Maybe => \c$vec2_3\(i_13) );


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
    imap_8 : for i_14 in \c$ws_app_arg_8\'range generate
    begin
      fun_8 : block
      signal \c$case_alt_17\              : Spec_topEntity_types.Maybe;
      -- spec.hs:242:1-63
      signal t_13                         : unsigned(7 downto 0);
      -- spec.hs:242:1-63
      signal x_15                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_5\ : boolean;
    begin
      \c$ws_app_arg_8\(i_14) <= \c$case_alt_17\;

      \c$case_alt_selection_res_5\ <= t_13 = result_32.Tags_sel1_output0_0;

      \c$case_alt_17\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_14,max_3(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_5\ else
                         \c$ws_app_arg_7\(i_14).Tuple2_1_sel1_Maybe;

      t_13 <= x_15.Tuple2_2_sel0_unsigned;

      x_15 <= \c$ws_app_arg_7\(i_14).Tuple2_1_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_3 : block
    signal vec_index_3 : integer range 0 to 2-1;
  begin
    vec_index_3 <= to_integer((signed(std_logic_vector(resize(i_3,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_7\ <= result_27(vec_index_3);
  end block;
  -- index end

  \c$case_scrut_8\ <=  ws_3(0) ;

  with (\c$case_scrut_8\(1 downto 1)) select
    result_21 <= to_signed(0,64) when "0",
                 \c$case_scrut_7\.Tuple2_2_sel1_signed when others;

  i_3 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_8\(0 downto 0)));

  ws_3 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_8\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_4 : block
    signal iterateI_ho1_0_arg0_2 : boolean;
    signal iterateI_ho1_0_res_4  : boolean;
    signal iterateI_ho1_1_res_4  : boolean;
    signal iterateI_ho1_2_res_3  : boolean;
    signal iterateI_ho1_3_res_2  : boolean;
  begin
    iterateI_ho1_0_arg0_2 <= x_8;

    r_block_48 : block
        signal \c$bb_res_res_16\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_16\;


      end block;

    r_block_49 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_17\;


      end block;

    r_block_50 : block
        signal \c$bb_res_res_18\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_18\;


      end block;

    r_block_51 : block
        signal \c$bb_res_res_19\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_19\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( x_8
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_2 );


  end block;

  result_22 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  result_selection_11 <= x_2;

  result_23 <= t_1 when result_selection_11 else
               result_24;

  x_2 <= result_25;

  -- register begin
  result_24_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_24 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_24 <= result_23;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_0\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_24) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => result_34.Tags_sel2_output1_0
             , Tuple2_2_sel1_signed => (x_3 + to_signed(2,64)) )))));

  t_projection_5 <= (\c$vec_0\(0 to 1-1),\c$vec_0\(1 to \c$vec_0\'high));

  t_1 <= t_projection_5.Tuple2_4_sel1_array_of_Tuple2_2_1;

  x_3 <= \c$out0_case_alt\;

  iterateI_5 : block
    signal iterateI_ho1_0_res_5 : boolean;
    signal iterateI_ho1_1_res_5 : boolean;
  begin
    r_block_52 : block
        signal \c$bb_res_res_20\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_20\;


      end block;

    r_block_53 : block
        signal \c$bb_res_res_21\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_21\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5 );


  end block;

  result_25 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  result_selection_14 <= x_4;

  result_26 <= t_2 when result_selection_14 else
               result_27;

  x_4 <= result_28;

  -- register begin
  result_27_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_27 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_27 <= result_26;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_1\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_27) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => result_34.Tags_sel1_output0_0
             , Tuple2_2_sel1_signed => (x_5 + to_signed(1,64)) )))));

  t_projection_6 <= (\c$vec_1\(0 to 1-1),\c$vec_1\(1 to \c$vec_1\'high));

  t_2 <= t_projection_6.Tuple2_4_sel1_array_of_Tuple2_2_1;

  x_5 <= \c$out0_case_alt\;

  \c$out0_case_alt_selection_res\ <= tag = tagToMatch;

  \c$out0_case_alt\ <= dta when \c$out0_case_alt_selection_res\ else
                       to_signed(0,64);

  dta <= result_30.Tuple2_2_sel1_signed;

  tagToMatch <= result_34.Tags_sel0_input0;

  tag <= result_30.Tuple2_2_sel0_unsigned;

  iterateI_6 : block
    signal iterateI_ho1_0_res_6 : boolean;
    signal iterateI_ho1_1_res_6 : boolean;
  begin
    r_block_54 : block
        signal \c$bb_res_res_22\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_22\;


      end block;

    r_block_55 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_23\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                           , iterateI_ho1_0_res_6
                                                           , iterateI_ho1_1_res_6 );


  end block;

  result_28 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  result_29 <= ( Tuple2_2_sel0_unsigned => t_7
               , Tuple2_2_sel1_signed => \c$input0Win_app_arg\ ) when b_0 else
               result_30;

  -- register begin
  result_30_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_30 <= ( Tuple2_2_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_30 <= result_29;
      end if;
    end if;
  end process;
  -- register end

  b_0 <= result_31;

  -- delay begin
  cinput0Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input0Win_app_arg\ <= result_45.Tuple3_sel2_Tuple2_3.Tuple2_3_sel0_ValidInt.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_7 : block
    signal iterateI_ho1_0_res_7 : boolean;
  begin
    r_block_56 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_24\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                           , iterateI_ho1_0_res_7 );


  end block;

  result_31 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  iterateI_8 : block
    signal iterateI_ho1_0_arg0_3 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_7  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_4  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_3 <= ( Tags_sel0_input0 => t_7
                             , Tags_sel1_output0_0 => t_6
                             , Tags_sel2_output1_0 => t_5
                             , Tags_sel3_output2_0 => t_4
                             , Tags_sel4_output3_0 => t_3 );

    r_block_57 : block
        signal \c$bb_res_res_25\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_25\;


      end block;

    r_block_58 : block
        signal \c$bb_res_res_26\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_0_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_26\;


      end block;

    r_block_59 : block
        signal \c$bb_res_res_27\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_1_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_27\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_7
                                                        , Tags_sel1_output0_0 => t_6
                                                        , Tags_sel2_output1_0 => t_5
                                                        , Tags_sel3_output2_0 => t_4
                                                        , Tags_sel4_output3_0 => t_3 )
                                                        , iterateI_ho1_0_res_8
                                                        , iterateI_ho1_1_res_7
                                                        , iterateI_ho1_2_res_4 );


  end block;

  result_32 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_arg0_4 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_9  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_8  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_4 <= ( Tags_sel0_input0 => t_7
                             , Tags_sel1_output0_0 => t_6
                             , Tags_sel2_output1_0 => t_5
                             , Tags_sel3_output2_0 => t_4
                             , Tags_sel4_output3_0 => t_3 );

    r_block_60 : block
        signal \c$bb_res_res_28\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_28\;


      end block;

    r_block_61 : block
        signal \c$bb_res_res_29\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_29\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_7
                                                        , Tags_sel1_output0_0 => t_6
                                                        , Tags_sel2_output1_0 => t_5
                                                        , Tags_sel3_output2_0 => t_4
                                                        , Tags_sel4_output3_0 => t_3 )
                                                        , iterateI_ho1_0_res_9
                                                        , iterateI_ho1_1_res_8 );


  end block;

  result_33 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  iterateI_10 : block
    signal iterateI_ho1_0_arg0_5 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_10 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_5 <= ( Tags_sel0_input0 => t_7
                             , Tags_sel1_output0_0 => t_6
                             , Tags_sel2_output1_0 => t_5
                             , Tags_sel3_output2_0 => t_4
                             , Tags_sel4_output3_0 => t_3 );

    r_block_62 : block
        signal \c$bb_res_res_30\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_30\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_7
                                                         , Tags_sel1_output0_0 => t_6
                                                         , Tags_sel2_output1_0 => t_5
                                                         , Tags_sel3_output2_0 => t_4
                                                         , Tags_sel4_output3_0 => t_3 )
                                                         , iterateI_ho1_0_res_10 );


  end block;

  result_34 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  -- register begin
  t_3_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_3 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_3 <= result_35;
      end if;
    end if;
  end process;
  -- register end

  result_selection_18 <= pOut3;

  result_35 <= result_36 when result_selection_18 else
               t_3;

  b_1 <= t_3 = to_unsigned(3,8);

  f1 <= t_3 + to_unsigned(1,8);

  result_36 <= to_unsigned(1,8) when b_1 else
               f1;

  pOut3 <= pacings.Pacings_1_sel4_pacingOut3;

  -- register begin
  t_4_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_4 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_4 <= result_37;
      end if;
    end if;
  end process;
  -- register end

  result_selection_22 <= x_6;

  result_37 <= result_38 when result_selection_22 else
               t_4;

  b_2 <= t_4 = to_unsigned(3,8);

  f1_0 <= t_4 + to_unsigned(1,8);

  result_38 <= to_unsigned(1,8) when b_2 else
               f1_0;

  x_6 <= pOut2;

  pOut2 <= pacings.Pacings_1_sel3_pacingOut2;

  -- register begin
  t_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_5 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_5 <= result_39;
      end if;
    end if;
  end process;
  -- register end

  result_selection_26 <= x_7;

  result_39 <= result_40 when result_selection_26 else
               t_5;

  b_3 <= t_5 = to_unsigned(3,8);

  f1_1 <= t_5 + to_unsigned(1,8);

  result_40 <= to_unsigned(1,8) when b_3 else
               f1_1;

  x_7 <= pOut1;

  pOut1 <= pacings.Pacings_1_sel2_pacingOut1;

  -- register begin
  t_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_6 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_6 <= result_41;
      end if;
    end if;
  end process;
  -- register end

  result_selection_30 <= x_8;

  result_41 <= result_42 when result_selection_30 else
               t_6;

  b_4 <= t_6 = to_unsigned(3,8);

  f1_2 <= t_6 + to_unsigned(1,8);

  result_42 <= to_unsigned(1,8) when b_4 else
               f1_2;

  x_8 <= pOut0;

  pOut0 <= pacings.Pacings_1_sel1_pacingOut0;

  -- register begin
  t_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_7 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_7 <= result_43;
      end if;
    end if;
  end process;
  -- register end

  result_selection_34 <= pIn0_0;

  result_43 <= result_44 when result_selection_34 else
               t_7;

  b_5 <= t_7 = to_unsigned(3,8);

  f1_3 <= t_7 + to_unsigned(1,8);

  result_44 <= to_unsigned(1,8) when b_5 else
               f1_3;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_45.Tuple3_sel2_Tuple2_3.Tuple2_3_sel1_Pacings_1;

  result_45 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_13\
               , Tuple3_sel1_boolean_1 => \c$app_arg_12\
               , Tuple3_sel2_Tuple2_3 => \c$app_arg_11\ );

  -- register begin
  capp_arg_11_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_11\ <= ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingOut0 => false
  , Pacings_1_sel2_pacingOut1 => false
  , Pacings_1_sel3_pacingOut2 => false
  , Pacings_1_sel4_pacingOut3 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_11\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false
                    , Pacings_1_sel4_pacingOut3 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false
                    , Pacings_1_sel4_pacingOut3 => false ) );

  with (x_9) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_9) select
    \c$case_alt_3\ <= ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingOut0 => false
                      , Pacings_1_sel2_pacingOut1 => false
                      , Pacings_1_sel3_pacingOut2 => false
                      , Pacings_1_sel4_pacingOut3 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_4 : block
    signal vec_index_4 : integer range 0 to 2-1;
  begin
    vec_index_4 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_4);
  end block;
  -- index end

  -- register begin
  capp_arg_12_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_12\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_12\ <= \c$case_alt_5\;
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

  with (x_9) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_13_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_13\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_13\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_14\);

  \c$app_arg_14\ <= to_signed(1,64) when x_9 /= to_signed(2,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple2_3'( ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) )
                                         , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
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

  \c$vec_2\ <= (Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_3'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_2\(0 to 2-1),\c$vec_2\(2 to \c$vec_2\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_5_sel0_array_of_Tuple2_3_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_3\ <= (Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_3'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_3\(0 to 2-1),\c$vec_3\(2 to \c$vec_3\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_5_sel0_array_of_Tuple2_3_0;

  qData <= result_6.Tuple2_sel1_Tuple2_3;

  x_9 <= cursor;

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

  push <= result_6.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_9 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_9 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_7.Tuple2_0_sel0_boolean;

  with (x_9) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_9 - to_signed(1,64));

  result <= result_7.Tuple2_0_sel1_Outputs;

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

  result_3_0 <= result_3.ValidInt_sel0_value;

  result_3_1 <= result_3.ValidInt_sel1_valid;


end;


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
       clk      : in Spec_topEntity_types.clk_TestDomain;
       -- reset
       rst      : in Spec_topEntity_types.rst_TestDomain;
       -- enable
       en       : in Spec_topEntity_types.en_TestDomain;
       inputs_0 : in signed(63 downto 0);
       inputs_1 : in boolean;
       result_0 : out signed(63 downto 0);
       result_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_4                      : signed(63 downto 0);
  signal x                             : signed(63 downto 0);
  signal result_5                      : signed(63 downto 0) := (to_signed(0,64));
  signal result_6                      : Spec_topEntity_types.Tuple2;
  -- spec.hs:181:1-82
  signal hasInput0                     : boolean;
  -- spec.hs:181:1-82
  signal timer0Over                    : boolean;
  signal result_7                      : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                   : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_8                      : boolean;
  -- spec.hs:331:1-138
  signal \c$t_app_arg\                 : signed(63 downto 0);
  signal result_9                      : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:331:1-138
  signal result_10                     : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:331:1-138
  signal b                             : boolean;
  -- spec.hs:331:1-138
  signal t                             : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:346:1-157
  signal y                             : signed(63 downto 0);
  -- spec.hs:346:1-157
  signal x_0                           : signed(63 downto 0);
  -- spec.hs:346:1-157
  signal win                           : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:346:1-157
  signal window                        : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(13,8)
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
                                                                              , to_signed(0,64) ) );
  signal result_11                     : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:346:1-157
  signal t_0                           : Spec_topEntity_types.Tuple2_2;
  signal result_12                     : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:346:1-157
  signal \c$t_case_alt\                : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:346:1-157
  signal \c$t_case_alt_0\              : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:346:1-157
  signal lastBucketUpdated             : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:346:1-157
  signal b_0                           : boolean;
  signal result_13                     : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:325:1-127
  signal result_14                     : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:325:1-127
  signal b_1                           : boolean;
  -- spec.hs:259:1-91
  signal \c$ds1_app_arg\               : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_0\                 : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_15                     : boolean;
  signal \c$app_arg_1\                 : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_16                     : Spec_topEntity_types.Tags;
  signal \c$app_arg_2\                 : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_17                     : boolean;
  signal \c$app_arg_3\                 : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_18                     : boolean;
  signal \c$app_arg_4\                 : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_19                     : Spec_topEntity_types.Tags;
  signal \c$app_arg_5\                 : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_20                     : boolean;
  -- spec.hs:315:9-81
  signal t_1                           : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_21                     : unsigned(7 downto 0);
  -- spec.hs:315:9-81
  signal b_2                           : boolean;
  -- spec.hs:315:9-81
  signal f1                            : unsigned(7 downto 0);
  signal result_22                     : unsigned(7 downto 0);
  -- spec.hs:259:1-91
  signal pOut0                         : boolean;
  -- spec.hs:315:9-81
  signal t_2                           : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_23                     : unsigned(7 downto 0);
  -- spec.hs:315:9-81
  signal b_3                           : boolean;
  -- spec.hs:315:9-81
  signal f1_0                          : unsigned(7 downto 0);
  signal result_24                     : unsigned(7 downto 0);
  -- spec.hs:259:1-91
  signal pIn0                          : boolean;
  -- spec.hs:259:1-91
  signal pacings                       : Spec_topEntity_types.Pacings_1;
  signal result_25                     : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_6\                 : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Tuple3_0_sel1_boolean => false
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingOut0 => false ) );
  signal \c$case_alt\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_7\                 : boolean := false;
  signal \c$case_alt_5\                : boolean;
  signal \c$case_alt_6\                : boolean;
  signal \c$case_alt_7\                : boolean;
  signal \c$case_alt_8\                : boolean;
  signal \c$case_alt_9\                : boolean;
  signal \c$app_arg_8\                 : boolean := false;
  signal \c$case_alt_10\               : boolean;
  signal \c$case_alt_11\               : boolean;
  signal \c$case_alt_12\               : boolean;
  signal \c$app_arg_9\                 : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal \buffer\                      : Spec_topEntity_types.array_of_Tuple3_0(0 to 1) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false ) )
                                       , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false ) ) );
  -- spec.hs:112:1-78
  signal \c$buffer_case_alt\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:112:1-78
  signal \c$buffer_case_alt_0\         : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:112:1-78
  signal \c$buffer_case_alt_1\         : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:112:1-78
  signal qData                         : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:112:1-78
  signal x_1                           : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal cursor                        : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:112:1-78
  signal \c$cursor_case_alt\           : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal push                          : boolean;
  -- spec.hs:112:1-78
  signal \c$cursor_case_alt_0\         : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal \c$cursor_case_alt_1\         : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal \c$cursor_case_scrut\         : boolean;
  -- spec.hs:112:1-78
  signal \c$cursor_app_arg\            : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal \c$cursor_case_alt_2\         : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal pop                           : boolean;
  -- spec.hs:112:1-78
  signal \c$cursor_case_alt_3\         : signed(63 downto 0);
  -- spec.hs:112:1-78
  signal \c$cursor_case_alt_4\         : signed(63 downto 0);
  signal inputs                        : Spec_topEntity_types.ValidInt;
  signal \c$vec\                       : Spec_topEntity_types.array_of_signed_64(0 to 10);
  signal x_projection_9                : signed(63 downto 0);
  signal \c$t_case_alt_selection_1\    : boolean;
  signal \c$vec_0\                     : Spec_topEntity_types.array_of_signed_64(0 to 11);
  signal \c$t_case_alt_sel_alt_0\      : Spec_topEntity_types.Tuple2_3;
  signal \c$vec_1\                     : Spec_topEntity_types.array_of_signed_64(0 to 11);
  signal \c$t_case_alt_sel_alt_2\      : Spec_topEntity_types.Tuple2_3;
  signal \c$t_case_alt_selection_4\    : boolean;
  signal result_selection_6            : boolean;
  signal result_selection_10           : boolean;
  signal \c$vec_2\                     : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\ : Spec_topEntity_types.Tuple2_4;
  signal \c$vec_3\                     : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\ : Spec_topEntity_types.Tuple2_4;
  signal result                        : Spec_topEntity_types.ValidInt;

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
              , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_ValidInt => inputs
              , Tuple3_0_sel1_boolean => timer0Over
              , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => hasInput0
              , Pacings_1_sel1_pacingOut0 => timer0Over ) ) );

  hasInput0 <= inputs.ValidInt_sel1_valid;

  timer0Over <= result_5 >= (to_signed(1000000,64));

  result_7 <= ( Tuple2_0_sel0_boolean => true
              , Tuple2_0_sel1_ValidInt => ( ValidInt_sel0_value => result_10.Tuple2_1_sel1_signed
              , ValidInt_sel1_valid => result_8 ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut0;

    r_block_14 : block
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

    r_block_15 : block
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

    r_block_16 : block
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

    r_block_17 : block
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

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res );


  end block;

  result_8 <=  \c$app_arg\(\c$app_arg\'high) ;

  \c$vec\ <= window.Tuple2_2_sel1_array_of_signed_64;

  fold : block
    signal vec     : Spec_topEntity_types.array_of_signed_64(0 to 9);
    signal acc_4_0 : signed(63 downto 0);
    signal acc_1   : signed(63 downto 0);
    signal acc_2   : signed(63 downto 0);
    signal acc_3   : signed(63 downto 0);
    signal acc_4   : signed(63 downto 0);
    signal acc_5   : signed(63 downto 0);
    signal acc_6   : signed(63 downto 0);
    signal acc_7   : signed(63 downto 0);
    signal acc_8   : signed(63 downto 0);
    signal acc_9   : signed(63 downto 0);
    signal acc_10  : signed(63 downto 0);
    signal acc_1_0 : signed(63 downto 0);
    signal acc_1_1 : signed(63 downto 0);
    signal acc_1_2 : signed(63 downto 0);
    signal acc_1_3 : signed(63 downto 0);
    signal acc_2_0 : signed(63 downto 0);
    signal acc_2_1 : signed(63 downto 0);
    signal acc_3_0 : signed(63 downto 0);
    signal acc_1_4 : signed(63 downto 0);
  begin
    \c$t_app_arg\ <= acc_4_0;

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

    fun_8 : block
        signal x_3             : signed(63 downto 0);
        signal y_0             : signed(63 downto 0);
        signal \c$case_alt_13\ : signed(63 downto 0);
      begin
        acc_1_0 <= \c$case_alt_13\;

        x_3 <= acc_1;

        y_0 <= acc_2;

        \c$case_alt_13\ <= (x_3 + y_0);


      end block;

    fun_9 : block
        signal x_4             : signed(63 downto 0);
        signal y_1             : signed(63 downto 0);
        signal \c$case_alt_14\ : signed(63 downto 0);
      begin
        acc_1_1 <= \c$case_alt_14\;

        x_4 <= acc_3;

        y_1 <= acc_4;

        \c$case_alt_14\ <= (x_4 + y_1);


      end block;

    fun_10 : block
        signal x_5             : signed(63 downto 0);
        signal y_2             : signed(63 downto 0);
        signal \c$case_alt_15\ : signed(63 downto 0);
      begin
        acc_1_2 <= \c$case_alt_15\;

        x_5 <= acc_5;

        y_2 <= acc_6;

        \c$case_alt_15\ <= (x_5 + y_2);


      end block;

    fun_11 : block
        signal x_6             : signed(63 downto 0);
        signal y_3             : signed(63 downto 0);
        signal \c$case_alt_16\ : signed(63 downto 0);
      begin
        acc_1_3 <= \c$case_alt_16\;

        x_6 <= acc_7;

        y_3 <= acc_8;

        \c$case_alt_16\ <= (x_6 + y_3);


      end block;

    fun_12 : block
        signal x_7             : signed(63 downto 0);
        signal y_4             : signed(63 downto 0);
        signal \c$case_alt_17\ : signed(63 downto 0);
      begin
        acc_1_4 <= \c$case_alt_17\;

        x_7 <= acc_9;

        y_4 <= acc_10;

        \c$case_alt_17\ <= (x_7 + y_4);


      end block;

    fun_13 : block
        signal x_8             : signed(63 downto 0);
        signal y_5             : signed(63 downto 0);
        signal \c$case_alt_18\ : signed(63 downto 0);
      begin
        acc_2_0 <= \c$case_alt_18\;

        x_8 <= acc_1_0;

        y_5 <= acc_1_1;

        \c$case_alt_18\ <= (x_8 + y_5);


      end block;

    fun_14 : block
        signal x_9             : signed(63 downto 0);
        signal y_6             : signed(63 downto 0);
        signal \c$case_alt_19\ : signed(63 downto 0);
      begin
        acc_2_1 <= \c$case_alt_19\;

        x_9 <= acc_1_2;

        y_6 <= acc_1_3;

        \c$case_alt_19\ <= (x_9 + y_6);


      end block;

    fun_15 : block
        signal x_10            : signed(63 downto 0);
        signal y_7             : signed(63 downto 0);
        signal \c$case_alt_20\ : signed(63 downto 0);
      begin
        acc_3_0 <= \c$case_alt_20\;

        x_10 <= acc_2_0;

        y_7 <= acc_2_1;

        \c$case_alt_20\ <= (x_10 + y_7);


      end block;

    fun_16 : block
        signal x_11            : signed(63 downto 0);
        signal y_8             : signed(63 downto 0);
        signal \c$case_alt_21\ : signed(63 downto 0);
      begin
        acc_4_0 <= \c$case_alt_21\;

        x_11 <= acc_3_0;

        y_8 <= acc_1_4;

        \c$case_alt_21\ <= (x_11 + y_8);


      end block;


  end block;

  result_9 <= t when b else
              result_10;

  -- register begin
  result_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_10 <= ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_10 <= result_9;
      end if;
    end if;
  end process;
  -- register end

  b <= result_20;

  t <= ( Tuple2_1_sel0_unsigned => result_19.Tags_sel1_output0
       , Tuple2_1_sel1_signed => \c$t_app_arg\ );

  y <= result_14.Tuple2_1_sel1_signed;

  x_projection_9 <=  win(0) ;

  x_0 <= x_projection_9;

  win <= window.Tuple2_2_sel1_array_of_signed_64;

  -- register begin
  window_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window <= ( Tuple2_2_sel0_unsigned => to_unsigned(13,8)
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
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window <= result_11;
      end if;
    end if;
  end process;
  -- register end

  result_11 <= t_0 when b_0 else
               window;

  t_0 <= ( Tuple2_2_sel0_unsigned => result_16.Tags_sel2_slide0
         , Tuple2_2_sel1_array_of_signed_64 => result_12 );

  result_12 <= \c$t_case_alt\ when result_17 else
               \c$t_case_alt_0\;

  \c$t_case_alt_selection_1\ <= result_18;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec_0\(0 to 11-1),\c$vec_0\(11 to \c$vec_0\'high));

  \c$vec_1\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_1\(0 to 11-1),\c$vec_1\(11 to \c$vec_1\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_3_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_1\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_3_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_4\ <= result_18;

  \c$t_case_alt_0\ <= lastBucketUpdated when \c$t_case_alt_selection_4\ else
                      win;

  -- replace begin
  replaceVec : block
    signal vec_index : integer range 0 to 11-1;
  begin
    vec_index <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 11
    -- pragma translate_on
                 ;

    process(vec_index,win,x_0,y)
      variable ivec : Spec_topEntity_types.array_of_signed_64(0 to 10);
    begin
      ivec := win;
      ivec(vec_index) := ((x_0 + y));
      lastBucketUpdated <= ivec;
    end process;
  end block;
  -- replace end

  b_0 <= result_18 or result_17;

  result_13 <= ( Tuple2_1_sel0_unsigned => t_2
               , Tuple2_1_sel1_signed => \c$ds1_app_arg\ ) when b_1 else
               result_14;

  -- register begin
  result_14_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_14 <= ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_14 <= result_13;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_15;

  -- delay begin
  cds1_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$ds1_app_arg\ <= result_25.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_ValidInt.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_0 : block
    signal iterateI_ho1_0_res_0 : boolean;
  begin
    r_block_18 : block
        signal \c$bb_res_res_3\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_3_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_3\ <= pIn0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_3\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( pIn0
                                                           , iterateI_ho1_0_res_0 );


  end block;

  result_15 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_arg0_0 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_1  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_0 <= ( Tags_sel0_input0 => t_2
                             , Tags_sel1_output0 => t_1
                             , Tags_sel2_slide0 => t_2 );

    r_block_19 : block
        signal \c$bb_res_res_4\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(13,8)
      , Tags_sel1_output0 => to_unsigned(13,8)
      , Tags_sel2_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_4\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_2
                                                        , Tags_sel1_output0 => t_1
                                                        , Tags_sel2_slide0 => t_2 )
                                                        , iterateI_ho1_0_res_1 );


  end block;

  result_16 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_2  : boolean;
    signal iterateI_ho1_1_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= result_25.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean;

    r_block_20 : block
        signal \c$bb_res_res_5\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_5_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_5\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_5\;


      end block;

    r_block_21 : block
        signal \c$bb_res_res_6\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_6\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( result_25.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_0 );


  end block;

  result_17 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  iterateI_3 : block
    signal iterateI_ho1_0_res_3 : boolean;
    signal iterateI_ho1_1_res_1 : boolean;
  begin
    r_block_22 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= pIn0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_7\;


      end block;

    r_block_23 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_8\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( pIn0
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_1 );


  end block;

  result_18 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  iterateI_4 : block
    signal iterateI_ho1_0_arg0_2 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_4  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_2  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_2 <= ( Tags_sel0_input0 => t_2
                             , Tags_sel1_output0 => t_1
                             , Tags_sel2_slide0 => t_2 );

    r_block_24 : block
        signal \c$bb_res_res_9\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(13,8)
      , Tags_sel1_output0 => to_unsigned(13,8)
      , Tags_sel2_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_9\;


      end block;

    r_block_25 : block
        signal \c$bb_res_res_10\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(13,8)
      , Tags_sel1_output0 => to_unsigned(13,8)
      , Tags_sel2_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_10\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_2
                                                        , Tags_sel1_output0 => t_1
                                                        , Tags_sel2_slide0 => t_2 )
                                                        , iterateI_ho1_0_res_4
                                                        , iterateI_ho1_1_res_2 );


  end block;

  result_19 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_res_5 : boolean;
    signal iterateI_ho1_1_res_3 : boolean;
    signal iterateI_ho1_2_res_0 : boolean;
  begin
    r_block_26 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_11\;


      end block;

    r_block_27 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_12\;


      end block;

    r_block_28 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_13\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_0 );


  end block;

  result_20 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  -- register begin
  t_1_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_1 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_1 <= result_21;
      end if;
    end if;
  end process;
  -- register end

  result_selection_6 <= pOut0;

  result_21 <= result_22 when result_selection_6 else
               t_1;

  b_2 <= t_1 = to_unsigned(12,8);

  f1 <= t_1 + to_unsigned(1,8);

  result_22 <= to_unsigned(1,8) when b_2 else
               f1;

  pOut0 <= pacings.Pacings_1_sel1_pacingOut0;

  -- register begin
  t_2_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_2 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_2 <= result_23;
      end if;
    end if;
  end process;
  -- register end

  result_selection_10 <= pIn0;

  result_23 <= result_24 when result_selection_10 else
               t_2;

  b_3 <= t_2 = to_unsigned(12,8);

  f1_0 <= t_2 + to_unsigned(1,8);

  result_24 <= to_unsigned(1,8) when b_3 else
               f1_0;

  pIn0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_25.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  result_25 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_8\
               , Tuple3_sel1_boolean_1 => \c$app_arg_7\
               , Tuple3_sel2_Tuple3_0 => \c$app_arg_6\ );

  -- register begin
  capp_arg_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_6\ <= ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Tuple3_0_sel1_boolean => false
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingOut0 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_6\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple3_0_sel1_boolean => false
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple3_0_sel1_boolean => false
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false ) );

  with (x_1) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_1) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Tuple3_0_sel1_boolean => false
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingOut0 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec : block
    signal vec_index_0 : integer range 0 to 2-1;
  begin
    vec_index_0 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_0);
  end block;
  -- index end

  -- register begin
  capp_arg_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_7\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_7\ <= \c$case_alt_5\;
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

  with (x_1) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_8_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_8\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_8\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_9\);

  \c$app_arg_9\ <= to_signed(1,64) when x_1 /= to_signed(2,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_boolean => false
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false ) )
                                         , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_boolean => false
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec_2\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_2\(0 to 2-1),\c$vec_2\(2 to \c$vec_2\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_4_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_3\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_3\(0 to 2-1),\c$vec_3\(2 to \c$vec_3\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_4_sel0_array_of_Tuple3_0_0;

  qData <= result_6.Tuple2_sel1_Tuple3_0;

  x_1 <= cursor;

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
                           (x_1 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_1 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_7.Tuple2_0_sel0_boolean;

  with (x_1) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_1 - to_signed(1,64));

  result <= result_7.Tuple2_0_sel1_ValidInt;

  result_0 <= result.ValidInt_sel0_value;

  result_1 <= result.ValidInt_sel1_valid;


end;


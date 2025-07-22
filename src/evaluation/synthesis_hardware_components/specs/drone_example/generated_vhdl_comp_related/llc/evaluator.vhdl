library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity evaluator is
    port (
        clk, input_clk, rst : in std_logic;
        input_time : in unsigned(63 downto 0);
		acceleration_x : in signed(63 downto 0);
		acceleration_x_en : in std_logic;
		gps_sats : in unsigned(63 downto 0);
		gps_sats_en : in std_logic;
		lat_gps : in signed(63 downto 0);
		lat_gps_en : in std_logic;
		acceleration_x_periodic_en : in std_logic;
		acceleration_x_rising_en : in std_logic;
		acceleration_x_sinking_en : in std_logic;
		acceleration_x_direction_change_en : in std_logic;
		acceleration_x_changes_en : in std_logic;
		trigger_acc_en : in std_logic;
		gps_missed_beat_en : in std_logic;
		gps_medium_loss_en : in std_logic;
		gps_high_loss_en : in std_logic;
		gps_very_high_loss_en : in std_logic;
		trigger_gps_sats_en : in std_logic;
		acceleration_x_periodic : out signed(63 downto 0);
		acceleration_x_rising : out std_logic;
		acceleration_x_sinking : out std_logic;
		acceleration_x_direction_change : out std_logic;
		acceleration_x_changes : out unsigned(63 downto 0);
		trigger_acc : out std_logic;
		gps_missed_beat : out std_logic;
		gps_medium_loss : out std_logic;
		gps_high_loss : out std_logic;
		gps_very_high_loss : out std_logic;
		trigger_gps_sats : out std_logic;
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input acceleration_x : Int64
--* input gps_sats : UInt64
--* input lat_gps : Int64
--* output acceleration_x_periodic := acceleration_x.hold().defaults(to: 0)
--* output acceleration_x_rising := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) > 5)
--* output acceleration_x_sinking := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) < -5)
--* output acceleration_x_direction_change := ((acceleration_x_rising and acceleration_x_sinking.offset(by: neg1).defaults(to: '0')) or (acceleration_x_sinking and acceleration_x_rising.offset(by: neg1).defaults(to: '0')))
--* output acceleration_x_changes := acceleration_x_direction_change.aggregate(over: 0.05s, using: count)
--* output trigger_acc := (acceleration_x_changes > 5)
--* output gps_missed_beat := (lat_gps.aggregate(over: 0.055s, using: count) < 1)
--* output gps_medium_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 15) and (lat_gps.aggregate(over: 0.01s, using: count) >= 10))
--* output gps_high_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 10) and (lat_gps.aggregate(over: 0.01s, using: count) >= 5))
--* output gps_very_high_loss := (lat_gps.aggregate(over: 0.01s, using: count) < 5)
--* output trigger_gps_sats := (gps_sats.hold().defaults(to: 0) < 6)


architecture mixed of evaluator is

    -- Component Declaration
	--* input acceleration_x : Int64
    component acceleration_x_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* input gps_sats : UInt64
    component gps_sats_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in unsigned(63 downto 0);
		    data_out : out unsigned64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* input lat_gps : Int64
    component lat_gps_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output acceleration_x_periodic := acceleration_x.hold().defaults(to: 0)
    component acceleration_x_periodic_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			acceleration_x_0 : in signed(63 downto 0);
			acceleration_x_data_valid_0 : in std_logic;
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output acceleration_x_rising := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) > 5)
    component acceleration_x_rising_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			acceleration_x_periodic_0 : in signed(63 downto 0);
			acceleration_x_periodic_data_valid_0 : in std_logic;
			acceleration_x_periodic_neg1 : in signed(63 downto 0);
			acceleration_x_periodic_data_valid_neg1 : in std_logic;
		    data_out : out bit_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output acceleration_x_sinking := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) < -5)
    component acceleration_x_sinking_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			acceleration_x_periodic_0 : in signed(63 downto 0);
			acceleration_x_periodic_data_valid_0 : in std_logic;
			acceleration_x_periodic_neg1 : in signed(63 downto 0);
			acceleration_x_periodic_data_valid_neg1 : in std_logic;
		    data_out : out bit_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output acceleration_x_direction_change := ((acceleration_x_rising and acceleration_x_sinking.offset(by: neg1).defaults(to: '0')) or (acceleration_x_sinking and acceleration_x_rising.offset(by: neg1).defaults(to: '0')))
    component acceleration_x_direction_change_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			acceleration_x_rising_0 : in std_logic;
			acceleration_x_rising_data_valid_0 : in std_logic;
			acceleration_x_rising_neg1 : in std_logic;
			acceleration_x_rising_data_valid_neg1 : in std_logic;
			acceleration_x_sinking_neg1 : in std_logic;
			acceleration_x_sinking_data_valid_neg1 : in std_logic;
			acceleration_x_sinking_0 : in std_logic;
			acceleration_x_sinking_data_valid_0 : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output acceleration_x_changes := acceleration_x_direction_change.aggregate(over: 0.05s, using: count)
    component acceleration_x_changes_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			acceleration_x_direction_change_count_0_sw : in unsigned(63 downto 0);
			acceleration_x_direction_change_count_0_sw_data_valid : in std_logic;
		    data_out : out unsigned64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output trigger_acc := (acceleration_x_changes > 5)
    component trigger_acc_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			acceleration_x_changes_0 : in unsigned(63 downto 0);
			acceleration_x_changes_data_valid_0 : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output gps_missed_beat := (lat_gps.aggregate(over: 0.055s, using: count) < 1)
    component gps_missed_beat_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			lat_gps_count_1_sw : in unsigned(63 downto 0);
			lat_gps_count_1_sw_data_valid : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output gps_medium_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 15) and (lat_gps.aggregate(over: 0.01s, using: count) >= 10))
    component gps_medium_loss_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			lat_gps_count_2_sw : in unsigned(63 downto 0);
			lat_gps_count_2_sw_data_valid : in std_logic;
			lat_gps_count_3_sw : in unsigned(63 downto 0);
			lat_gps_count_3_sw_data_valid : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output gps_high_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 10) and (lat_gps.aggregate(over: 0.01s, using: count) >= 5))
    component gps_high_loss_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			lat_gps_count_4_sw : in unsigned(63 downto 0);
			lat_gps_count_4_sw_data_valid : in std_logic;
			lat_gps_count_5_sw : in unsigned(63 downto 0);
			lat_gps_count_5_sw_data_valid : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output gps_very_high_loss := (lat_gps.aggregate(over: 0.01s, using: count) < 5)
    component gps_very_high_loss_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			lat_gps_count_6_sw : in unsigned(63 downto 0);
			lat_gps_count_6_sw_data_valid : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output trigger_gps_sats := (gps_sats.hold().defaults(to: 0) < 6)
    component trigger_gps_sats_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			gps_sats_0 : in unsigned(63 downto 0);
			gps_sats_data_valid_0 : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* acceleration_x_direction_change.aggregate(over: 0.05 s, using: count)
    component acceleration_x_direction_change_count_0_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in std_logic;
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;

	--* lat_gps.aggregate(over: 0.055 s, using: count)
    component lat_gps_count_1_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    component lat_gps_count_2_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    component lat_gps_count_3_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    component lat_gps_count_4_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    component lat_gps_count_5_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    component lat_gps_count_6_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;


    -- Internal Signal Declarations
	signal acceleration_x_upd : std_logic;
	signal acceleration_x_upd_done : std_logic;
	signal acceleration_x_entity_data_0 : signed(63 downto 0);
	signal acceleration_x_entity_data_valid_0 : std_logic;
	signal gps_sats_upd : std_logic;
	signal gps_sats_upd_done : std_logic;
	signal gps_sats_entity_data_0 : unsigned(63 downto 0);
	signal gps_sats_entity_data_valid_0 : std_logic;
	signal lat_gps_upd : std_logic;
	signal lat_gps_upd_done : std_logic;
	signal lat_gps_entity_data_0 : signed(63 downto 0);
	signal lat_gps_entity_data_valid_0 : std_logic;
	signal acceleration_x_periodic_pe : std_logic;
	signal acceleration_x_periodic_eval : std_logic;
	signal acceleration_x_periodic_pe_done : std_logic;
	signal acceleration_x_periodic_eval_done : std_logic;
	signal acceleration_x_periodic_entity_data_0 : signed(63 downto 0);
	signal acceleration_x_periodic_entity_data_valid_0 : std_logic;
	signal acceleration_x_periodic_entity_data_1 : signed(63 downto 0);
	signal acceleration_x_periodic_entity_data_valid_1 : std_logic;
	signal acceleration_x_rising_pe : std_logic;
	signal acceleration_x_rising_eval : std_logic;
	signal acceleration_x_rising_pe_done : std_logic;
	signal acceleration_x_rising_eval_done : std_logic;
	signal acceleration_x_rising_entity_data_0 : std_logic;
	signal acceleration_x_rising_entity_data_valid_0 : std_logic;
	signal acceleration_x_rising_entity_data_1 : std_logic;
	signal acceleration_x_rising_entity_data_valid_1 : std_logic;
	signal acceleration_x_sinking_pe : std_logic;
	signal acceleration_x_sinking_eval : std_logic;
	signal acceleration_x_sinking_pe_done : std_logic;
	signal acceleration_x_sinking_eval_done : std_logic;
	signal acceleration_x_sinking_entity_data_0 : std_logic;
	signal acceleration_x_sinking_entity_data_valid_0 : std_logic;
	signal acceleration_x_sinking_entity_data_1 : std_logic;
	signal acceleration_x_sinking_entity_data_valid_1 : std_logic;
	signal acceleration_x_direction_change_pe : std_logic;
	signal acceleration_x_direction_change_eval : std_logic;
	signal acceleration_x_direction_change_pe_done : std_logic;
	signal acceleration_x_direction_change_eval_done : std_logic;
	signal acceleration_x_direction_change_entity_data_0 : std_logic;
	signal acceleration_x_direction_change_entity_data_valid_0 : std_logic;
	signal acceleration_x_changes_pe : std_logic;
	signal acceleration_x_changes_eval : std_logic;
	signal acceleration_x_changes_pe_done : std_logic;
	signal acceleration_x_changes_eval_done : std_logic;
	signal acceleration_x_changes_entity_data_0 : unsigned(63 downto 0);
	signal acceleration_x_changes_entity_data_valid_0 : std_logic;
	signal trigger_acc_pe : std_logic;
	signal trigger_acc_eval : std_logic;
	signal trigger_acc_pe_done : std_logic;
	signal trigger_acc_eval_done : std_logic;
	signal trigger_acc_entity_data_0 : std_logic;
	signal trigger_acc_entity_data_valid_0 : std_logic;
	signal gps_missed_beat_pe : std_logic;
	signal gps_missed_beat_eval : std_logic;
	signal gps_missed_beat_pe_done : std_logic;
	signal gps_missed_beat_eval_done : std_logic;
	signal gps_missed_beat_entity_data_0 : std_logic;
	signal gps_missed_beat_entity_data_valid_0 : std_logic;
	signal gps_medium_loss_pe : std_logic;
	signal gps_medium_loss_eval : std_logic;
	signal gps_medium_loss_pe_done : std_logic;
	signal gps_medium_loss_eval_done : std_logic;
	signal gps_medium_loss_entity_data_0 : std_logic;
	signal gps_medium_loss_entity_data_valid_0 : std_logic;
	signal gps_high_loss_pe : std_logic;
	signal gps_high_loss_eval : std_logic;
	signal gps_high_loss_pe_done : std_logic;
	signal gps_high_loss_eval_done : std_logic;
	signal gps_high_loss_entity_data_0 : std_logic;
	signal gps_high_loss_entity_data_valid_0 : std_logic;
	signal gps_very_high_loss_pe : std_logic;
	signal gps_very_high_loss_eval : std_logic;
	signal gps_very_high_loss_pe_done : std_logic;
	signal gps_very_high_loss_eval_done : std_logic;
	signal gps_very_high_loss_entity_data_0 : std_logic;
	signal gps_very_high_loss_entity_data_valid_0 : std_logic;
	signal trigger_gps_sats_pe : std_logic;
	signal trigger_gps_sats_eval : std_logic;
	signal trigger_gps_sats_pe_done : std_logic;
	signal trigger_gps_sats_eval_done : std_logic;
	signal trigger_gps_sats_entity_data_0 : std_logic;
	signal trigger_gps_sats_entity_data_valid_0 : std_logic;
	signal acceleration_x_direction_change_count_0_evict : std_logic;
	signal acceleration_x_direction_change_count_0_upd : std_logic;
	signal acceleration_x_direction_change_count_0_request : std_logic;
	signal acceleration_x_direction_change_count_0_entity_data : unsigned(63 downto 0);
	signal acceleration_x_direction_change_count_0_entity_data_valid : std_logic;
	signal acceleration_x_direction_change_count_0_evict_done : std_logic;
	signal acceleration_x_direction_change_count_0_upd_done : std_logic;
	signal acceleration_x_direction_change_count_0_request_done : std_logic;
	signal lat_gps_count_1_evict : std_logic;
	signal lat_gps_count_1_upd : std_logic;
	signal lat_gps_count_1_request : std_logic;
	signal lat_gps_count_1_entity_data : unsigned(63 downto 0);
	signal lat_gps_count_1_entity_data_valid : std_logic;
	signal lat_gps_count_1_evict_done : std_logic;
	signal lat_gps_count_1_upd_done : std_logic;
	signal lat_gps_count_1_request_done : std_logic;
	signal lat_gps_count_2_evict : std_logic;
	signal lat_gps_count_2_upd : std_logic;
	signal lat_gps_count_2_request : std_logic;
	signal lat_gps_count_2_entity_data : unsigned(63 downto 0);
	signal lat_gps_count_2_entity_data_valid : std_logic;
	signal lat_gps_count_2_evict_done : std_logic;
	signal lat_gps_count_2_upd_done : std_logic;
	signal lat_gps_count_2_request_done : std_logic;
	signal lat_gps_count_3_evict : std_logic;
	signal lat_gps_count_3_upd : std_logic;
	signal lat_gps_count_3_request : std_logic;
	signal lat_gps_count_3_entity_data : unsigned(63 downto 0);
	signal lat_gps_count_3_entity_data_valid : std_logic;
	signal lat_gps_count_3_evict_done : std_logic;
	signal lat_gps_count_3_upd_done : std_logic;
	signal lat_gps_count_3_request_done : std_logic;
	signal lat_gps_count_4_evict : std_logic;
	signal lat_gps_count_4_upd : std_logic;
	signal lat_gps_count_4_request : std_logic;
	signal lat_gps_count_4_entity_data : unsigned(63 downto 0);
	signal lat_gps_count_4_entity_data_valid : std_logic;
	signal lat_gps_count_4_evict_done : std_logic;
	signal lat_gps_count_4_upd_done : std_logic;
	signal lat_gps_count_4_request_done : std_logic;
	signal lat_gps_count_5_evict : std_logic;
	signal lat_gps_count_5_upd : std_logic;
	signal lat_gps_count_5_request : std_logic;
	signal lat_gps_count_5_entity_data : unsigned(63 downto 0);
	signal lat_gps_count_5_entity_data_valid : std_logic;
	signal lat_gps_count_5_evict_done : std_logic;
	signal lat_gps_count_5_upd_done : std_logic;
	signal lat_gps_count_5_request_done : std_logic;
	signal lat_gps_count_6_evict : std_logic;
	signal lat_gps_count_6_upd : std_logic;
	signal lat_gps_count_6_request : std_logic;
	signal lat_gps_count_6_entity_data : unsigned(63 downto 0);
	signal lat_gps_count_6_entity_data_valid : std_logic;
	signal lat_gps_count_6_evict_done : std_logic;
	signal lat_gps_count_6_upd_done : std_logic;
	signal lat_gps_count_6_request_done : std_logic;

    signal upd_and_pe_done : std_logic;
    signal evaluator_done : std_logic;
    signal valid_reg : std_logic;
    signal rst_en_done : std_logic;

begin
    -- Component Instantiation
	--* input acceleration_x : Int64
    acceleration_x_entity_instance: acceleration_x_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => acceleration_x_upd,
            data_in => acceleration_x,
			data_out(0) => acceleration_x_entity_data_0,
			data_valid_out(0) => acceleration_x_entity_data_valid_0,
            done_out => acceleration_x_upd_done
         );

	--* input gps_sats : UInt64
    gps_sats_entity_instance: gps_sats_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => gps_sats_upd,
            data_in => gps_sats,
			data_out(0) => gps_sats_entity_data_0,
			data_valid_out(0) => gps_sats_entity_data_valid_0,
            done_out => gps_sats_upd_done
         );

	--* input lat_gps : Int64
    lat_gps_entity_instance: lat_gps_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => lat_gps_upd,
            data_in => lat_gps,
			data_out(0) => lat_gps_entity_data_0,
			data_valid_out(0) => lat_gps_entity_data_valid_0,
            done_out => lat_gps_upd_done
         );

	--* output acceleration_x_periodic := acceleration_x.hold().defaults(to: 0)
    acceleration_x_periodic_entity_instance: acceleration_x_periodic_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => acceleration_x_periodic_pe,
            eval => acceleration_x_periodic_eval,
			acceleration_x_0 => acceleration_x_entity_data_0,
			acceleration_x_data_valid_0 => acceleration_x_entity_data_valid_0,
			data_out(0) => acceleration_x_periodic_entity_data_0,
			data_out(1) => acceleration_x_periodic_entity_data_1,
			data_valid_out(0) => acceleration_x_periodic_entity_data_valid_0,
			data_valid_out(1) => acceleration_x_periodic_entity_data_valid_1,
            pe_done_out => acceleration_x_periodic_pe_done,
            eval_done_out => acceleration_x_periodic_eval_done
        );

	--* output acceleration_x_rising := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) > 5)
    acceleration_x_rising_entity_instance: acceleration_x_rising_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => acceleration_x_rising_pe,
            eval => acceleration_x_rising_eval,
			acceleration_x_periodic_0 => acceleration_x_periodic_entity_data_0,
			acceleration_x_periodic_data_valid_0 => acceleration_x_periodic_entity_data_valid_0,
			acceleration_x_periodic_neg1 => acceleration_x_periodic_entity_data_1,
			acceleration_x_periodic_data_valid_neg1 => acceleration_x_periodic_entity_data_valid_1,
			data_out(0) => acceleration_x_rising_entity_data_0,
			data_out(1) => acceleration_x_rising_entity_data_1,
			data_valid_out(0) => acceleration_x_rising_entity_data_valid_0,
			data_valid_out(1) => acceleration_x_rising_entity_data_valid_1,
            pe_done_out => acceleration_x_rising_pe_done,
            eval_done_out => acceleration_x_rising_eval_done
        );

	--* output acceleration_x_sinking := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) < -5)
    acceleration_x_sinking_entity_instance: acceleration_x_sinking_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => acceleration_x_sinking_pe,
            eval => acceleration_x_sinking_eval,
			acceleration_x_periodic_0 => acceleration_x_periodic_entity_data_0,
			acceleration_x_periodic_data_valid_0 => acceleration_x_periodic_entity_data_valid_0,
			acceleration_x_periodic_neg1 => acceleration_x_periodic_entity_data_1,
			acceleration_x_periodic_data_valid_neg1 => acceleration_x_periodic_entity_data_valid_1,
			data_out(0) => acceleration_x_sinking_entity_data_0,
			data_out(1) => acceleration_x_sinking_entity_data_1,
			data_valid_out(0) => acceleration_x_sinking_entity_data_valid_0,
			data_valid_out(1) => acceleration_x_sinking_entity_data_valid_1,
            pe_done_out => acceleration_x_sinking_pe_done,
            eval_done_out => acceleration_x_sinking_eval_done
        );

	--* output acceleration_x_direction_change := ((acceleration_x_rising and acceleration_x_sinking.offset(by: neg1).defaults(to: '0')) or (acceleration_x_sinking and acceleration_x_rising.offset(by: neg1).defaults(to: '0')))
    acceleration_x_direction_change_entity_instance: acceleration_x_direction_change_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => acceleration_x_direction_change_pe,
            eval => acceleration_x_direction_change_eval,
			acceleration_x_rising_0 => acceleration_x_rising_entity_data_0,
			acceleration_x_rising_data_valid_0 => acceleration_x_rising_entity_data_valid_0,
			acceleration_x_rising_neg1 => acceleration_x_rising_entity_data_1,
			acceleration_x_rising_data_valid_neg1 => acceleration_x_rising_entity_data_valid_1,
			acceleration_x_sinking_neg1 => acceleration_x_sinking_entity_data_1,
			acceleration_x_sinking_data_valid_neg1 => acceleration_x_sinking_entity_data_valid_1,
			acceleration_x_sinking_0 => acceleration_x_sinking_entity_data_0,
			acceleration_x_sinking_data_valid_0 => acceleration_x_sinking_entity_data_valid_0,
			data_out(0) => acceleration_x_direction_change_entity_data_0,
			data_valid_out(0) => acceleration_x_direction_change_entity_data_valid_0,
            pe_done_out => acceleration_x_direction_change_pe_done,
            eval_done_out => acceleration_x_direction_change_eval_done
        );

	--* output acceleration_x_changes := acceleration_x_direction_change.aggregate(over: 0.05s, using: count)
    acceleration_x_changes_entity_instance: acceleration_x_changes_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => acceleration_x_changes_pe,
            eval => acceleration_x_changes_eval,
			acceleration_x_direction_change_count_0_sw => acceleration_x_direction_change_count_0_entity_data,
			acceleration_x_direction_change_count_0_sw_data_valid => acceleration_x_direction_change_count_0_entity_data_valid,
			data_out(0) => acceleration_x_changes_entity_data_0,
			data_valid_out(0) => acceleration_x_changes_entity_data_valid_0,
            pe_done_out => acceleration_x_changes_pe_done,
            eval_done_out => acceleration_x_changes_eval_done
        );

	--* output trigger_acc := (acceleration_x_changes > 5)
    trigger_acc_entity_instance: trigger_acc_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => trigger_acc_pe,
            eval => trigger_acc_eval,
			acceleration_x_changes_0 => acceleration_x_changes_entity_data_0,
			acceleration_x_changes_data_valid_0 => acceleration_x_changes_entity_data_valid_0,
			data_out(0) => trigger_acc_entity_data_0,
			data_valid_out(0) => trigger_acc_entity_data_valid_0,
            pe_done_out => trigger_acc_pe_done,
            eval_done_out => trigger_acc_eval_done
        );

	--* output gps_missed_beat := (lat_gps.aggregate(over: 0.055s, using: count) < 1)
    gps_missed_beat_entity_instance: gps_missed_beat_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => gps_missed_beat_pe,
            eval => gps_missed_beat_eval,
			lat_gps_count_1_sw => lat_gps_count_1_entity_data,
			lat_gps_count_1_sw_data_valid => lat_gps_count_1_entity_data_valid,
			data_out(0) => gps_missed_beat_entity_data_0,
			data_valid_out(0) => gps_missed_beat_entity_data_valid_0,
            pe_done_out => gps_missed_beat_pe_done,
            eval_done_out => gps_missed_beat_eval_done
        );

	--* output gps_medium_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 15) and (lat_gps.aggregate(over: 0.01s, using: count) >= 10))
    gps_medium_loss_entity_instance: gps_medium_loss_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => gps_medium_loss_pe,
            eval => gps_medium_loss_eval,
			lat_gps_count_2_sw => lat_gps_count_2_entity_data,
			lat_gps_count_2_sw_data_valid => lat_gps_count_2_entity_data_valid,
			lat_gps_count_3_sw => lat_gps_count_3_entity_data,
			lat_gps_count_3_sw_data_valid => lat_gps_count_3_entity_data_valid,
			data_out(0) => gps_medium_loss_entity_data_0,
			data_valid_out(0) => gps_medium_loss_entity_data_valid_0,
            pe_done_out => gps_medium_loss_pe_done,
            eval_done_out => gps_medium_loss_eval_done
        );

	--* output gps_high_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 10) and (lat_gps.aggregate(over: 0.01s, using: count) >= 5))
    gps_high_loss_entity_instance: gps_high_loss_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => gps_high_loss_pe,
            eval => gps_high_loss_eval,
			lat_gps_count_4_sw => lat_gps_count_4_entity_data,
			lat_gps_count_4_sw_data_valid => lat_gps_count_4_entity_data_valid,
			lat_gps_count_5_sw => lat_gps_count_5_entity_data,
			lat_gps_count_5_sw_data_valid => lat_gps_count_5_entity_data_valid,
			data_out(0) => gps_high_loss_entity_data_0,
			data_valid_out(0) => gps_high_loss_entity_data_valid_0,
            pe_done_out => gps_high_loss_pe_done,
            eval_done_out => gps_high_loss_eval_done
        );

	--* output gps_very_high_loss := (lat_gps.aggregate(over: 0.01s, using: count) < 5)
    gps_very_high_loss_entity_instance: gps_very_high_loss_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => gps_very_high_loss_pe,
            eval => gps_very_high_loss_eval,
			lat_gps_count_6_sw => lat_gps_count_6_entity_data,
			lat_gps_count_6_sw_data_valid => lat_gps_count_6_entity_data_valid,
			data_out(0) => gps_very_high_loss_entity_data_0,
			data_valid_out(0) => gps_very_high_loss_entity_data_valid_0,
            pe_done_out => gps_very_high_loss_pe_done,
            eval_done_out => gps_very_high_loss_eval_done
        );

	--* output trigger_gps_sats := (gps_sats.hold().defaults(to: 0) < 6)
    trigger_gps_sats_entity_instance: trigger_gps_sats_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => trigger_gps_sats_pe,
            eval => trigger_gps_sats_eval,
			gps_sats_0 => gps_sats_entity_data_0,
			gps_sats_data_valid_0 => gps_sats_entity_data_valid_0,
			data_out(0) => trigger_gps_sats_entity_data_0,
			data_valid_out(0) => trigger_gps_sats_entity_data_valid_0,
            pe_done_out => trigger_gps_sats_pe_done,
            eval_done_out => trigger_gps_sats_eval_done
        );

	--* acceleration_x_direction_change.aggregate(over: 0.05 s, using: count)
    acceleration_x_direction_change_count_0_sliding_window_entity_instance: acceleration_x_direction_change_count_0_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => acceleration_x_direction_change_count_0_evict,
            upd => acceleration_x_direction_change_count_0_upd,
            request => acceleration_x_direction_change_count_0_request,
            time_in => input_time,
            data_in => acceleration_x_direction_change_entity_data_0,
            data_out => acceleration_x_direction_change_count_0_entity_data,
            data_valid_out => acceleration_x_direction_change_count_0_entity_data_valid,
            evict_done_out => acceleration_x_direction_change_count_0_evict_done,
            upd_done_out => acceleration_x_direction_change_count_0_upd_done,
            request_done_out => acceleration_x_direction_change_count_0_request_done
        );

	--* lat_gps.aggregate(over: 0.055 s, using: count)
    lat_gps_count_1_sliding_window_entity_instance: lat_gps_count_1_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => lat_gps_count_1_evict,
            upd => lat_gps_count_1_upd,
            request => lat_gps_count_1_request,
            time_in => input_time,
            data_in => lat_gps_entity_data_0,
            data_out => lat_gps_count_1_entity_data,
            data_valid_out => lat_gps_count_1_entity_data_valid,
            evict_done_out => lat_gps_count_1_evict_done,
            upd_done_out => lat_gps_count_1_upd_done,
            request_done_out => lat_gps_count_1_request_done
        );

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    lat_gps_count_2_sliding_window_entity_instance: lat_gps_count_2_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => lat_gps_count_2_evict,
            upd => lat_gps_count_2_upd,
            request => lat_gps_count_2_request,
            time_in => input_time,
            data_in => lat_gps_entity_data_0,
            data_out => lat_gps_count_2_entity_data,
            data_valid_out => lat_gps_count_2_entity_data_valid,
            evict_done_out => lat_gps_count_2_evict_done,
            upd_done_out => lat_gps_count_2_upd_done,
            request_done_out => lat_gps_count_2_request_done
        );

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    lat_gps_count_3_sliding_window_entity_instance: lat_gps_count_3_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => lat_gps_count_3_evict,
            upd => lat_gps_count_3_upd,
            request => lat_gps_count_3_request,
            time_in => input_time,
            data_in => lat_gps_entity_data_0,
            data_out => lat_gps_count_3_entity_data,
            data_valid_out => lat_gps_count_3_entity_data_valid,
            evict_done_out => lat_gps_count_3_evict_done,
            upd_done_out => lat_gps_count_3_upd_done,
            request_done_out => lat_gps_count_3_request_done
        );

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    lat_gps_count_4_sliding_window_entity_instance: lat_gps_count_4_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => lat_gps_count_4_evict,
            upd => lat_gps_count_4_upd,
            request => lat_gps_count_4_request,
            time_in => input_time,
            data_in => lat_gps_entity_data_0,
            data_out => lat_gps_count_4_entity_data,
            data_valid_out => lat_gps_count_4_entity_data_valid,
            evict_done_out => lat_gps_count_4_evict_done,
            upd_done_out => lat_gps_count_4_upd_done,
            request_done_out => lat_gps_count_4_request_done
        );

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    lat_gps_count_5_sliding_window_entity_instance: lat_gps_count_5_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => lat_gps_count_5_evict,
            upd => lat_gps_count_5_upd,
            request => lat_gps_count_5_request,
            time_in => input_time,
            data_in => lat_gps_entity_data_0,
            data_out => lat_gps_count_5_entity_data,
            data_valid_out => lat_gps_count_5_entity_data_valid,
            evict_done_out => lat_gps_count_5_evict_done,
            upd_done_out => lat_gps_count_5_upd_done,
            request_done_out => lat_gps_count_5_request_done
        );

	--* lat_gps.aggregate(over: 0.01 s, using: count)
    lat_gps_count_6_sliding_window_entity_instance: lat_gps_count_6_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => lat_gps_count_6_evict,
            upd => lat_gps_count_6_upd,
            request => lat_gps_count_6_request,
            time_in => input_time,
            data_in => lat_gps_entity_data_0,
            data_out => lat_gps_count_6_entity_data,
            data_valid_out => lat_gps_count_6_entity_data_valid,
            evict_done_out => lat_gps_count_6_evict_done,
            upd_done_out => lat_gps_count_6_upd_done,
            request_done_out => lat_gps_count_6_request_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				acceleration_x_upd <= '0';
				gps_sats_upd <= '0';
				lat_gps_upd <= '0';
				acceleration_x_periodic_pe <= '0';
				acceleration_x_periodic_eval <= '0';
				acceleration_x_rising_pe <= '0';
				acceleration_x_rising_eval <= '0';
				acceleration_x_sinking_pe <= '0';
				acceleration_x_sinking_eval <= '0';
				acceleration_x_direction_change_pe <= '0';
				acceleration_x_direction_change_eval <= '0';
				acceleration_x_changes_pe <= '0';
				acceleration_x_changes_eval <= '0';
				trigger_acc_pe <= '0';
				trigger_acc_eval <= '0';
				gps_missed_beat_pe <= '0';
				gps_missed_beat_eval <= '0';
				gps_medium_loss_pe <= '0';
				gps_medium_loss_eval <= '0';
				gps_high_loss_pe <= '0';
				gps_high_loss_eval <= '0';
				gps_very_high_loss_pe <= '0';
				gps_very_high_loss_eval <= '0';
				trigger_gps_sats_pe <= '0';
				trigger_gps_sats_eval <= '0';
				acceleration_x_direction_change_count_0_evict <= '0';
				acceleration_x_direction_change_count_0_upd <= '0';
				acceleration_x_direction_change_count_0_request <= '0';
				lat_gps_count_1_evict <= '0';
				lat_gps_count_1_upd <= '0';
				lat_gps_count_1_request <= '0';
				lat_gps_count_2_evict <= '0';
				lat_gps_count_2_upd <= '0';
				lat_gps_count_2_request <= '0';
				lat_gps_count_3_evict <= '0';
				lat_gps_count_3_upd <= '0';
				lat_gps_count_3_request <= '0';
				lat_gps_count_4_evict <= '0';
				lat_gps_count_4_upd <= '0';
				lat_gps_count_4_request <= '0';
				lat_gps_count_5_evict <= '0';
				lat_gps_count_5_upd <= '0';
				lat_gps_count_5_request <= '0';
				lat_gps_count_6_evict <= '0';
				lat_gps_count_6_upd <= '0';
				lat_gps_count_6_request <= '0';
            upd_and_pe_done <= '1';
            evaluator_done <= '1';
            rst_en_done <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase
            if input_clk = '1' then
                if upd_and_pe_done = '0' then
                    -- Input Stream Updates
                    --* Input Streams in Specification 
					--* - acceleration_x 
					--* - gps_sats 
					--* - lat_gps 
					acceleration_x_upd <= acceleration_x_en;
					gps_sats_upd <= gps_sats_en;
					lat_gps_upd <= lat_gps_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - acceleration_x_periodic
					--* - acceleration_x_rising
					--* - acceleration_x_sinking
					--* - acceleration_x_direction_change
					--* - acceleration_x_changes
					--* - trigger_acc
					--* - gps_missed_beat
					--* - gps_medium_loss
					--* - gps_high_loss
					--* - gps_very_high_loss
					--* - trigger_gps_sats
					acceleration_x_periodic_pe <= acceleration_x_periodic_en;
					acceleration_x_rising_pe <= acceleration_x_rising_en;
					acceleration_x_sinking_pe <= acceleration_x_sinking_en;
					acceleration_x_direction_change_pe <= acceleration_x_direction_change_en;
					acceleration_x_changes_pe <= acceleration_x_changes_en;
					trigger_acc_pe <= trigger_acc_en;
					gps_missed_beat_pe <= gps_missed_beat_en;
					gps_medium_loss_pe <= gps_medium_loss_en;
					gps_high_loss_pe <= gps_high_loss_en;
					gps_very_high_loss_pe <= gps_very_high_loss_en;
					trigger_gps_sats_pe <= trigger_gps_sats_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
					--* - acceleration_x_direction_change.aggregate(over: 0.05 s, using: count)
					--* - lat_gps.aggregate(over: 0.055 s, using: count)
					--* - lat_gps.aggregate(over: 0.01 s, using: count)
					--* - lat_gps.aggregate(over: 0.01 s, using: count)
					--* - lat_gps.aggregate(over: 0.01 s, using: count)
					--* - lat_gps.aggregate(over: 0.01 s, using: count)
					--* - lat_gps.aggregate(over: 0.01 s, using: count)
					acceleration_x_direction_change_count_0_evict <= '1';
					lat_gps_count_1_evict <= '1';
					lat_gps_count_2_evict <= '1';
					lat_gps_count_3_evict <= '1';
					lat_gps_count_4_evict <= '1';
					lat_gps_count_5_evict <= '1';
					lat_gps_count_6_evict <= '1';
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output acceleration_x_periodic := acceleration_x.hold().defaults(to: 0)
					--* Evaluation Phase of Output Stream acceleration_x_periodic is Influenced by No Lookup
					acceleration_x_periodic_eval <= acceleration_x_periodic_en and upd_and_pe_done;
					--* output acceleration_x_rising := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) > 5)
					--* Evaluation Phase of Output Stream acceleration_x_rising is Influenced by the following Lookups: 
					--* - Synchronous Lookup: acceleration_x_periodic
					acceleration_x_rising_eval <= acceleration_x_rising_en and upd_and_pe_done and acceleration_x_periodic_eval_done;
					--* output acceleration_x_sinking := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) < -5)
					--* Evaluation Phase of Output Stream acceleration_x_sinking is Influenced by the following Lookups: 
					--* - Synchronous Lookup: acceleration_x_periodic
					acceleration_x_sinking_eval <= acceleration_x_sinking_en and upd_and_pe_done and acceleration_x_periodic_eval_done;
					--* output acceleration_x_direction_change := ((acceleration_x_rising and acceleration_x_sinking.offset(by: neg1).defaults(to: '0')) or (acceleration_x_sinking and acceleration_x_rising.offset(by: neg1).defaults(to: '0')))
					--* Evaluation Phase of Output Stream acceleration_x_direction_change is Influenced by the following Lookups: 
					--* - Synchronous Lookup: acceleration_x_rising
					--* - Synchronous Lookup: acceleration_x_sinking
					acceleration_x_direction_change_eval <= acceleration_x_direction_change_en and upd_and_pe_done and acceleration_x_rising_eval_done and acceleration_x_sinking_eval_done;
					--* output acceleration_x_changes := acceleration_x_direction_change.aggregate(over: 0.05s, using: count)
					--* Evaluation Phase of Output Stream acceleration_x_changes is Influenced by the following Lookups: 
					--* - Window Lookup: acceleration_x_direction_change.aggregate(over: 0.05 s, using: count)
					acceleration_x_changes_eval <= acceleration_x_changes_en and upd_and_pe_done and acceleration_x_direction_change_count_0_request_done;
					--* output trigger_acc := (acceleration_x_changes > 5)
					--* Evaluation Phase of Output Stream trigger_acc is Influenced by the following Lookups: 
					--* - Synchronous Lookup: acceleration_x_changes
					trigger_acc_eval <= trigger_acc_en and upd_and_pe_done and acceleration_x_changes_eval_done;
					--* output gps_missed_beat := (lat_gps.aggregate(over: 0.055s, using: count) < 1)
					--* Evaluation Phase of Output Stream gps_missed_beat is Influenced by the following Lookups: 
					--* - Window Lookup: lat_gps.aggregate(over: 0.055 s, using: count)
					gps_missed_beat_eval <= gps_missed_beat_en and upd_and_pe_done and lat_gps_count_1_request_done;
					--* output gps_medium_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 15) and (lat_gps.aggregate(over: 0.01s, using: count) >= 10))
					--* Evaluation Phase of Output Stream gps_medium_loss is Influenced by the following Lookups: 
					--* - Window Lookup: lat_gps.aggregate(over: 0.01 s, using: count)
					--* - Window Lookup: lat_gps.aggregate(over: 0.01 s, using: count)
					gps_medium_loss_eval <= gps_medium_loss_en and upd_and_pe_done and lat_gps_count_2_request_done and lat_gps_count_3_request_done;
					--* output gps_high_loss := ((lat_gps.aggregate(over: 0.01s, using: count) < 10) and (lat_gps.aggregate(over: 0.01s, using: count) >= 5))
					--* Evaluation Phase of Output Stream gps_high_loss is Influenced by the following Lookups: 
					--* - Window Lookup: lat_gps.aggregate(over: 0.01 s, using: count)
					--* - Window Lookup: lat_gps.aggregate(over: 0.01 s, using: count)
					gps_high_loss_eval <= gps_high_loss_en and upd_and_pe_done and lat_gps_count_4_request_done and lat_gps_count_5_request_done;
					--* output gps_very_high_loss := (lat_gps.aggregate(over: 0.01s, using: count) < 5)
					--* Evaluation Phase of Output Stream gps_very_high_loss is Influenced by the following Lookups: 
					--* - Window Lookup: lat_gps.aggregate(over: 0.01 s, using: count)
					gps_very_high_loss_eval <= gps_very_high_loss_en and upd_and_pe_done and lat_gps_count_6_request_done;
					--* output trigger_gps_sats := (gps_sats.hold().defaults(to: 0) < 6)
					--* Evaluation Phase of Output Stream trigger_gps_sats is Influenced by No Lookup
					trigger_gps_sats_eval <= trigger_gps_sats_en and upd_and_pe_done;
                    -- SW Update Phase
					--* - acceleration_x_direction_change.aggregate(over: 0.05 s, using: count) aggregates over acceleration_x_direction_change
					acceleration_x_direction_change_count_0_upd <= acceleration_x_direction_change_eval_done and upd_and_pe_done;
					--* - lat_gps.aggregate(over: 0.055 s, using: count) aggregates over lat_gps
					lat_gps_count_1_upd <= lat_gps_upd_done and upd_and_pe_done;
					--* - lat_gps.aggregate(over: 0.01 s, using: count) aggregates over lat_gps
					lat_gps_count_2_upd <= lat_gps_upd_done and upd_and_pe_done;
					--* - lat_gps.aggregate(over: 0.01 s, using: count) aggregates over lat_gps
					lat_gps_count_3_upd <= lat_gps_upd_done and upd_and_pe_done;
					--* - lat_gps.aggregate(over: 0.01 s, using: count) aggregates over lat_gps
					lat_gps_count_4_upd <= lat_gps_upd_done and upd_and_pe_done;
					--* - lat_gps.aggregate(over: 0.01 s, using: count) aggregates over lat_gps
					lat_gps_count_5_upd <= lat_gps_upd_done and upd_and_pe_done;
					--* - lat_gps.aggregate(over: 0.01 s, using: count) aggregates over lat_gps
					lat_gps_count_6_upd <= lat_gps_upd_done and upd_and_pe_done;
                    -- SW Request Phase
					--* acceleration_x_direction_change.aggregate(over: 0.05 s, using: count) has Source acceleration_x_changes
					acceleration_x_direction_change_count_0_request <= acceleration_x_changes_en and upd_and_pe_done and (not acceleration_x_direction_change_en or acceleration_x_direction_change_count_0_upd_done);
					--* lat_gps.aggregate(over: 0.055 s, using: count) has Source gps_missed_beat
					lat_gps_count_1_request <= gps_missed_beat_en and upd_and_pe_done and (not lat_gps_en or lat_gps_count_1_upd_done);
					--* lat_gps.aggregate(over: 0.01 s, using: count) has Source gps_medium_loss
					lat_gps_count_2_request <= gps_medium_loss_en and upd_and_pe_done and (not lat_gps_en or lat_gps_count_2_upd_done);
					--* lat_gps.aggregate(over: 0.01 s, using: count) has Source gps_medium_loss
					lat_gps_count_3_request <= gps_medium_loss_en and upd_and_pe_done and (not lat_gps_en or lat_gps_count_3_upd_done);
					--* lat_gps.aggregate(over: 0.01 s, using: count) has Source gps_high_loss
					lat_gps_count_4_request <= gps_high_loss_en and upd_and_pe_done and (not lat_gps_en or lat_gps_count_4_upd_done);
					--* lat_gps.aggregate(over: 0.01 s, using: count) has Source gps_high_loss
					lat_gps_count_5_request <= gps_high_loss_en and upd_and_pe_done and (not lat_gps_en or lat_gps_count_5_upd_done);
					--* lat_gps.aggregate(over: 0.01 s, using: count) has Source gps_very_high_loss
					lat_gps_count_6_request <= gps_very_high_loss_en and upd_and_pe_done and (not lat_gps_en or lat_gps_count_6_upd_done);
                    -- Valid Assignment
					valid_reg <= '1' and acceleration_x_periodic_entity_data_valid_0 and acceleration_x_rising_entity_data_valid_0 and acceleration_x_sinking_entity_data_valid_0 and acceleration_x_direction_change_entity_data_valid_0 and acceleration_x_changes_entity_data_valid_0 and trigger_acc_entity_data_valid_0 and gps_missed_beat_entity_data_valid_0 and gps_medium_loss_entity_data_valid_0 and gps_high_loss_entity_data_valid_0 and gps_very_high_loss_entity_data_valid_0 and trigger_gps_sats_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not acceleration_x_en or acceleration_x_upd_done) and (not gps_sats_en or gps_sats_upd_done) and (not lat_gps_en or lat_gps_upd_done) and (not acceleration_x_periodic_en or acceleration_x_periodic_pe_done) and (not acceleration_x_rising_en or acceleration_x_rising_pe_done) and (not acceleration_x_sinking_en or acceleration_x_sinking_pe_done) and (not acceleration_x_direction_change_en or acceleration_x_direction_change_pe_done) and (not acceleration_x_changes_en or acceleration_x_changes_pe_done) and (not trigger_acc_en or trigger_acc_pe_done) and (not gps_missed_beat_en or gps_missed_beat_pe_done) and (not gps_medium_loss_en or gps_medium_loss_pe_done) and (not gps_high_loss_en or gps_high_loss_pe_done) and (not gps_very_high_loss_en or gps_very_high_loss_pe_done) and (not trigger_gps_sats_en or trigger_gps_sats_pe_done) and (not acceleration_x_direction_change_en or acceleration_x_direction_change_count_0_evict_done) and (not lat_gps_en or lat_gps_count_1_evict_done) and (not lat_gps_en or lat_gps_count_2_evict_done) and (not lat_gps_en or lat_gps_count_3_evict_done) and (not lat_gps_en or lat_gps_count_4_evict_done) and (not lat_gps_en or lat_gps_count_5_evict_done) and (not lat_gps_en or lat_gps_count_6_evict_done);
					evaluator_done <= upd_and_pe_done and (not acceleration_x_periodic_en or acceleration_x_periodic_eval_done) and (not acceleration_x_rising_en or acceleration_x_rising_eval_done) and (not acceleration_x_sinking_en or acceleration_x_sinking_eval_done) and (not acceleration_x_direction_change_en or acceleration_x_direction_change_eval_done) and (not acceleration_x_changes_en or acceleration_x_direction_change_count_0_request_done) and (not acceleration_x_changes_en or acceleration_x_changes_eval_done) and (not trigger_acc_en or trigger_acc_eval_done) and (not gps_missed_beat_en or lat_gps_count_1_request_done) and (not gps_missed_beat_en or gps_missed_beat_eval_done) and (not gps_medium_loss_en or lat_gps_count_2_request_done) and (not gps_medium_loss_en or lat_gps_count_3_request_done) and (not gps_medium_loss_en or gps_medium_loss_eval_done) and (not gps_high_loss_en or lat_gps_count_4_request_done) and (not gps_high_loss_en or lat_gps_count_5_request_done) and (not gps_high_loss_en or gps_high_loss_eval_done) and (not gps_very_high_loss_en or lat_gps_count_6_request_done) and (not gps_very_high_loss_en or gps_very_high_loss_eval_done) and (not trigger_gps_sats_en or trigger_gps_sats_eval_done) and (not acceleration_x_direction_change_en or acceleration_x_direction_change_count_0_upd_done) and (not lat_gps_en or lat_gps_count_1_upd_done) and (not lat_gps_en or lat_gps_count_2_upd_done) and (not lat_gps_en or lat_gps_count_3_upd_done) and (not lat_gps_en or lat_gps_count_4_upd_done) and (not lat_gps_en or lat_gps_count_5_upd_done) and (not lat_gps_en or lat_gps_count_6_upd_done);
                end if;
            else
                upd_and_pe_done <= '0';
				acceleration_x_upd <= '0';
				gps_sats_upd <= '0';
				lat_gps_upd <= '0';
				acceleration_x_periodic_pe <= '0';
				acceleration_x_periodic_eval <= '0';
				acceleration_x_rising_pe <= '0';
				acceleration_x_rising_eval <= '0';
				acceleration_x_sinking_pe <= '0';
				acceleration_x_sinking_eval <= '0';
				acceleration_x_direction_change_pe <= '0';
				acceleration_x_direction_change_eval <= '0';
				acceleration_x_changes_pe <= '0';
				acceleration_x_changes_eval <= '0';
				trigger_acc_pe <= '0';
				trigger_acc_eval <= '0';
				gps_missed_beat_pe <= '0';
				gps_missed_beat_eval <= '0';
				gps_medium_loss_pe <= '0';
				gps_medium_loss_eval <= '0';
				gps_high_loss_pe <= '0';
				gps_high_loss_eval <= '0';
				gps_very_high_loss_pe <= '0';
				gps_very_high_loss_eval <= '0';
				trigger_gps_sats_pe <= '0';
				trigger_gps_sats_eval <= '0';
				acceleration_x_direction_change_count_0_evict <= '0';
				acceleration_x_direction_change_count_0_upd <= '0';
				acceleration_x_direction_change_count_0_request <= '0';
				lat_gps_count_1_evict <= '0';
				lat_gps_count_1_upd <= '0';
				lat_gps_count_1_request <= '0';
				lat_gps_count_2_evict <= '0';
				lat_gps_count_2_upd <= '0';
				lat_gps_count_2_request <= '0';
				lat_gps_count_3_evict <= '0';
				lat_gps_count_3_upd <= '0';
				lat_gps_count_3_request <= '0';
				lat_gps_count_4_evict <= '0';
				lat_gps_count_4_upd <= '0';
				lat_gps_count_4_request <= '0';
				lat_gps_count_5_evict <= '0';
				lat_gps_count_5_upd <= '0';
				lat_gps_count_5_request <= '0';
				lat_gps_count_6_evict <= '0';
				lat_gps_count_6_upd <= '0';
				lat_gps_count_6_request <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	acceleration_x_periodic <= acceleration_x_periodic_entity_data_0;
	acceleration_x_rising <= acceleration_x_rising_entity_data_0;
	acceleration_x_sinking <= acceleration_x_sinking_entity_data_0;
	acceleration_x_direction_change <= acceleration_x_direction_change_entity_data_0;
	acceleration_x_changes <= acceleration_x_changes_entity_data_0;
	trigger_acc <= trigger_acc_entity_data_0;
	gps_missed_beat <= gps_missed_beat_entity_data_0;
	gps_medium_loss <= gps_medium_loss_entity_data_0;
	gps_high_loss <= gps_high_loss_entity_data_0;
	gps_very_high_loss <= gps_very_high_loss_entity_data_0;
	trigger_gps_sats <= trigger_gps_sats_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;
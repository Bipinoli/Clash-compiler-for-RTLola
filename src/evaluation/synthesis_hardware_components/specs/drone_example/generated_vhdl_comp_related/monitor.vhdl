library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity monitor is
    port (
        clk, tclk, qclk, eclk, rst : in std_logic;
        input_time : in std_logic_vector(63 downto 0);
        offline : in std_logic;
        new_input : in std_logic;
		acceleration_x_data_in : in std_logic_vector(63 downto 0);
		acceleration_x_data_in_new_input : in std_logic;
		gps_sats_data_in : in std_logic_vector(63 downto 0);
		gps_sats_data_in_new_input : in std_logic;
		lat_gps_data_in : in std_logic_vector(63 downto 0);
		lat_gps_data_in_new_input : in std_logic;
        time_stream : out std_logic_vector(63 downto 0);
		acceleration_x_stream: out std_logic_vector(63 downto 0);
		gps_sats_stream: out std_logic_vector(63 downto 0);
		lat_gps_stream: out std_logic_vector(63 downto 0);
		acceleration_x_periodic_stream: out std_logic_vector(63 downto 0);
		acceleration_x_rising_stream: out std_logic;
		acceleration_x_sinking_stream: out std_logic;
		acceleration_x_direction_change_stream: out std_logic;
		acceleration_x_changes_stream: out std_logic_vector(63 downto 0);
		trigger_acc_stream: out std_logic;
		gps_missed_beat_stream: out std_logic;
		gps_medium_loss_stream: out std_logic;
		gps_high_loss_stream: out std_logic;
		gps_very_high_loss_stream: out std_logic;
		trigger_gps_sats_stream: out std_logic;
        lost_data : out std_logic
    );
end monitor;

architecture mixed of monitor is

    -- component declaration
    component high_level_controller is
        port (
            clk, rst : in std_logic;
            system_clk : in std_logic;
            time_data_in : in std_logic_vector(63 downto 0);
            new_input : in std_logic;
			acceleration_x_data_in : in std_logic_vector(63 downto 0);
			acceleration_x_push_in : std_logic;
			gps_sats_data_in : in std_logic_vector(63 downto 0);
			gps_sats_push_in : std_logic;
			lat_gps_data_in : in std_logic_vector(63 downto 0);
			lat_gps_push_in : std_logic;
			acceleration_x_data_out : out signed(63 downto 0);
			acceleration_x_en_out : out std_logic;
			gps_sats_data_out : out unsigned(63 downto 0);
			gps_sats_en_out : out std_logic;
			lat_gps_data_out : out signed(63 downto 0);
			lat_gps_en_out : out std_logic;
			acceleration_x_periodic_en_out : out std_logic;
			acceleration_x_rising_en_out : out std_logic;
			acceleration_x_sinking_en_out : out std_logic;
			acceleration_x_direction_change_en_out : out std_logic;
			acceleration_x_changes_en_out : out std_logic;
			trigger_acc_en_out : out std_logic;
			gps_missed_beat_en_out : out std_logic;
			gps_medium_loss_en_out : out std_logic;
			gps_high_loss_en_out : out std_logic;
			gps_very_high_loss_en_out : out std_logic;
			trigger_gps_sats_en_out : out std_logic;
            time_data_out : out unsigned(63 downto 0);
            push_data_in_query : out std_logic;
            lost_data : out std_logic
        );
    end component;

    component queue is
        port (
            clk, rst : in std_logic;
            push : in std_logic;
            time_data_in : in unsigned(63 downto 0);
			acceleration_x_data_in : in signed(63 downto 0);
			acceleration_x_en_in : in std_logic;
			gps_sats_data_in : in unsigned(63 downto 0);
			gps_sats_en_in : in std_logic;
			lat_gps_data_in : in signed(63 downto 0);
			lat_gps_en_in : in std_logic;
			acceleration_x_periodic_en_in : in std_logic;
			acceleration_x_rising_en_in : in std_logic;
			acceleration_x_sinking_en_in : in std_logic;
			acceleration_x_direction_change_en_in : in std_logic;
			acceleration_x_changes_en_in : in std_logic;
			trigger_acc_en_in : in std_logic;
			gps_missed_beat_en_in : in std_logic;
			gps_medium_loss_en_in : in std_logic;
			gps_high_loss_en_in : in std_logic;
			gps_very_high_loss_en_in : in std_logic;
			trigger_gps_sats_en_in : in std_logic;
            full : out std_logic;
            pop : in std_logic;
            time_data_out : out unsigned(63 downto 0);
			acceleration_x_data_out : out signed(63 downto 0);
			acceleration_x_en_out : out std_logic;
			gps_sats_data_out : out unsigned(63 downto 0);
			gps_sats_en_out : out std_logic;
			lat_gps_data_out : out signed(63 downto 0);
			lat_gps_en_out : out std_logic;
			acceleration_x_periodic_en_out : out std_logic;
			acceleration_x_rising_en_out : out std_logic;
			acceleration_x_sinking_en_out : out std_logic;
			acceleration_x_direction_change_en_out : out std_logic;
			acceleration_x_changes_en_out : out std_logic;
			trigger_acc_en_out : out std_logic;
			gps_missed_beat_en_out : out std_logic;
			gps_medium_loss_en_out : out std_logic;
			gps_high_loss_en_out : out std_logic;
			gps_very_high_loss_en_out : out std_logic;
			trigger_gps_sats_en_out : out std_logic;
            available : out std_logic
        );
    end component;

    component low_level_controller is
        port (
            clk, eclk, rst : in std_logic;
            time_in : in unsigned(63 downto 0);
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
            data_available : in std_logic;
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
		    pop : out std_logic;
            eval_done : out std_logic
        );
    end component;

    -- signal declarationq
    -- timing_manager
    signal push_data_timing : std_logic;
    signal timing_manager_time : unsigned(63 downto 0);
	signal acceleration_x_data_timing : signed(63 downto 0);
	signal acceleration_x_en_timing : std_logic;
	signal gps_sats_data_timing : unsigned(63 downto 0);
	signal gps_sats_en_timing : std_logic;
	signal lat_gps_data_timing : signed(63 downto 0);
	signal lat_gps_en_timing : std_logic;
	signal acceleration_x_periodic_en_timing : std_logic;
	signal acceleration_x_rising_en_timing : std_logic;
	signal acceleration_x_sinking_en_timing : std_logic;
	signal acceleration_x_direction_change_en_timing : std_logic;
	signal acceleration_x_changes_en_timing : std_logic;
	signal trigger_acc_en_timing : std_logic;
	signal gps_missed_beat_en_timing : std_logic;
	signal gps_medium_loss_en_timing : std_logic;
	signal gps_high_loss_en_timing : std_logic;
	signal gps_very_high_loss_en_timing : std_logic;
	signal trigger_gps_sats_en_timing : std_logic;
    -- query
    signal queue_is_full : std_logic;
    signal queue_time : unsigned(63 downto 0);
    signal pop_queue : std_logic;
	signal acceleration_x_data_queue : signed(63 downto 0);
	signal acceleration_x_en_queue : std_logic;
	signal gps_sats_data_queue : unsigned(63 downto 0);
	signal gps_sats_en_queue : std_logic;
	signal lat_gps_data_queue : signed(63 downto 0);
	signal lat_gps_en_queue : std_logic;
	signal acceleration_x_periodic_en_queue : std_logic;
	signal acceleration_x_rising_en_queue : std_logic;
	signal acceleration_x_sinking_en_queue : std_logic;
	signal acceleration_x_direction_change_en_queue : std_logic;
	signal acceleration_x_changes_en_queue : std_logic;
	signal trigger_acc_en_queue : std_logic;
	signal gps_missed_beat_en_queue : std_logic;
	signal gps_medium_loss_en_queue : std_logic;
	signal gps_high_loss_en_queue : std_logic;
	signal gps_very_high_loss_en_queue : std_logic;
	signal trigger_gps_sats_en_queue : std_logic;
    signal queue_data_available : std_logic;
    -- evaluator
	signal acceleration_x_periodic_stream_evaluator : signed(63 downto 0);
	signal acceleration_x_rising_stream_evaluator : std_logic;
	signal acceleration_x_sinking_stream_evaluator : std_logic;
	signal acceleration_x_direction_change_stream_evaluator : std_logic;
	signal acceleration_x_changes_stream_evaluator : unsigned(63 downto 0);
	signal trigger_acc_stream_evaluator : std_logic;
	signal gps_missed_beat_stream_evaluator : std_logic;
	signal gps_medium_loss_stream_evaluator : std_logic;
	signal gps_high_loss_stream_evaluator : std_logic;
	signal gps_very_high_loss_stream_evaluator : std_logic;
	signal trigger_gps_sats_stream_evaluator : std_logic;
    -- monitor
    signal time_stream_reg : std_logic_vector(63 downto 0);
	signal acceleration_x_stream_reg : std_logic_vector(63 downto 0);
	signal gps_sats_stream_reg : std_logic_vector(63 downto 0);
	signal lat_gps_stream_reg : std_logic_vector(63 downto 0);
	signal acceleration_x_periodic_stream_reg : std_logic_vector(63 downto 0);
	signal acceleration_x_rising_stream_reg : std_logic;
	signal acceleration_x_sinking_stream_reg : std_logic;
	signal acceleration_x_direction_change_stream_reg : std_logic;
	signal acceleration_x_changes_stream_reg : std_logic_vector(63 downto 0);
	signal trigger_acc_stream_reg : std_logic;
	signal gps_missed_beat_stream_reg : std_logic;
	signal gps_medium_loss_stream_reg : std_logic;
	signal gps_high_loss_stream_reg : std_logic;
	signal gps_very_high_loss_stream_reg : std_logic;
	signal trigger_gps_sats_stream_reg : std_logic;

    signal print : std_logic;

begin
    -- component instantiation
    high_level_controller_instance: high_level_controller
        port map (
            clk => tclk,
            system_clk => clk,
            rst => rst,
            time_data_in => input_time,
            new_input => new_input,
			acceleration_x_data_in => acceleration_x_data_in,
			acceleration_x_push_in => acceleration_x_data_in_new_input,
			gps_sats_data_in => gps_sats_data_in,
			gps_sats_push_in => gps_sats_data_in_new_input,
			lat_gps_data_in => lat_gps_data_in,
			lat_gps_push_in => lat_gps_data_in_new_input,
			acceleration_x_data_out => acceleration_x_data_timing,
			acceleration_x_en_out => acceleration_x_en_timing,
			gps_sats_data_out => gps_sats_data_timing,
			gps_sats_en_out => gps_sats_en_timing,
			lat_gps_data_out => lat_gps_data_timing,
			lat_gps_en_out => lat_gps_en_timing,
			acceleration_x_periodic_en_out => acceleration_x_periodic_en_timing,
			acceleration_x_rising_en_out => acceleration_x_rising_en_timing,
			acceleration_x_sinking_en_out => acceleration_x_sinking_en_timing,
			acceleration_x_direction_change_en_out => acceleration_x_direction_change_en_timing,
			acceleration_x_changes_en_out => acceleration_x_changes_en_timing,
			trigger_acc_en_out => trigger_acc_en_timing,
			gps_missed_beat_en_out => gps_missed_beat_en_timing,
			gps_medium_loss_en_out => gps_medium_loss_en_timing,
			gps_high_loss_en_out => gps_high_loss_en_timing,
			gps_very_high_loss_en_out => gps_very_high_loss_en_timing,
			trigger_gps_sats_en_out => trigger_gps_sats_en_timing,
            time_data_out => timing_manager_time,
            push_data_in_query => push_data_timing,
            lost_data => lost_data
        );

    queue_instance: queue
        port map (
            clk => qclk,
            rst => rst,
            push => push_data_timing,
            time_data_in => timing_manager_time,
			acceleration_x_data_in => acceleration_x_data_timing,
			acceleration_x_en_in => acceleration_x_en_timing,
			gps_sats_data_in => gps_sats_data_timing,
			gps_sats_en_in => gps_sats_en_timing,
			lat_gps_data_in => lat_gps_data_timing,
			lat_gps_en_in => lat_gps_en_timing,
			acceleration_x_periodic_en_in => acceleration_x_periodic_en_timing,
			acceleration_x_rising_en_in => acceleration_x_rising_en_timing,
			acceleration_x_sinking_en_in => acceleration_x_sinking_en_timing,
			acceleration_x_direction_change_en_in => acceleration_x_direction_change_en_timing,
			acceleration_x_changes_en_in => acceleration_x_changes_en_timing,
			trigger_acc_en_in => trigger_acc_en_timing,
			gps_missed_beat_en_in => gps_missed_beat_en_timing,
			gps_medium_loss_en_in => gps_medium_loss_en_timing,
			gps_high_loss_en_in => gps_high_loss_en_timing,
			gps_very_high_loss_en_in => gps_very_high_loss_en_timing,
			trigger_gps_sats_en_in => trigger_gps_sats_en_timing,
            full => queue_is_full,
            pop => pop_queue,
            time_data_out => queue_time,
			acceleration_x_data_out => acceleration_x_data_queue,
			acceleration_x_en_out => acceleration_x_en_queue,
			gps_sats_data_out => gps_sats_data_queue,
			gps_sats_en_out => gps_sats_en_queue,
			lat_gps_data_out => lat_gps_data_queue,
			lat_gps_en_out => lat_gps_en_queue,
			acceleration_x_periodic_en_out => acceleration_x_periodic_en_queue,
			acceleration_x_rising_en_out => acceleration_x_rising_en_queue,
			acceleration_x_sinking_en_out => acceleration_x_sinking_en_queue,
			acceleration_x_direction_change_en_out => acceleration_x_direction_change_en_queue,
			acceleration_x_changes_en_out => acceleration_x_changes_en_queue,
			trigger_acc_en_out => trigger_acc_en_queue,
			gps_missed_beat_en_out => gps_missed_beat_en_queue,
			gps_medium_loss_en_out => gps_medium_loss_en_queue,
			gps_high_loss_en_out => gps_high_loss_en_queue,
			gps_very_high_loss_en_out => gps_very_high_loss_en_queue,
			trigger_gps_sats_en_out => trigger_gps_sats_en_queue,
            available => queue_data_available
        );

    low_level_controller_instance: low_level_controller
        port map (
            clk => clk,
            eclk => eclk,
            rst => rst,
            time_in => queue_time,
			acceleration_x => acceleration_x_data_queue,
			acceleration_x_en => acceleration_x_en_queue,
			gps_sats => gps_sats_data_queue,
			gps_sats_en => gps_sats_en_queue,
			lat_gps => lat_gps_data_queue,
			lat_gps_en => lat_gps_en_queue,
			acceleration_x_periodic_en => acceleration_x_periodic_en_queue,
			acceleration_x_rising_en => acceleration_x_rising_en_queue,
			acceleration_x_sinking_en => acceleration_x_sinking_en_queue,
			acceleration_x_direction_change_en => acceleration_x_direction_change_en_queue,
			acceleration_x_changes_en => acceleration_x_changes_en_queue,
			trigger_acc_en => trigger_acc_en_queue,
			gps_missed_beat_en => gps_missed_beat_en_queue,
			gps_medium_loss_en => gps_medium_loss_en_queue,
			gps_high_loss_en => gps_high_loss_en_queue,
			gps_very_high_loss_en => gps_very_high_loss_en_queue,
			trigger_gps_sats_en => trigger_gps_sats_en_queue,
            data_available => queue_data_available,
			acceleration_x_periodic => acceleration_x_periodic_stream_evaluator,
			acceleration_x_rising => acceleration_x_rising_stream_evaluator,
			acceleration_x_sinking => acceleration_x_sinking_stream_evaluator,
			acceleration_x_direction_change => acceleration_x_direction_change_stream_evaluator,
			acceleration_x_changes => acceleration_x_changes_stream_evaluator,
			trigger_acc => trigger_acc_stream_evaluator,
			gps_missed_beat => gps_missed_beat_stream_evaluator,
			gps_medium_loss => gps_medium_loss_stream_evaluator,
			gps_high_loss => gps_high_loss_stream_evaluator,
			gps_very_high_loss => gps_very_high_loss_stream_evaluator,
			trigger_gps_sats => trigger_gps_sats_stream_evaluator,
            pop => pop_queue,
            eval_done => print
        );

    process(rst, print) begin
        if (rst = '1') then
            time_stream_reg <= (others => '0');
			acceleration_x_stream_reg <= (others => '0');
			gps_sats_stream_reg <= (others => '0');
			lat_gps_stream_reg <= (others => '0');
			acceleration_x_periodic_stream_reg <= (others => '0');
			acceleration_x_rising_stream_reg <= '0';
			acceleration_x_sinking_stream_reg <= '0';
			acceleration_x_direction_change_stream_reg <= '0';
			acceleration_x_changes_stream_reg <= (others => '0');
			trigger_acc_stream_reg <= '0';
			gps_missed_beat_stream_reg <= '0';
			gps_medium_loss_stream_reg <= '0';
			gps_high_loss_stream_reg <= '0';
			gps_very_high_loss_stream_reg <= '0';
			trigger_gps_sats_stream_reg <= '0';
        elsif falling_edge(print) then
            time_stream_reg <= std_logic_vector(queue_time);
			acceleration_x_stream_reg <= std_logic_vector(acceleration_x_data_queue);
			gps_sats_stream_reg <= std_logic_vector(gps_sats_data_queue);
			lat_gps_stream_reg <= std_logic_vector(lat_gps_data_queue);
			acceleration_x_periodic_stream_reg <= std_logic_vector(acceleration_x_periodic_stream_evaluator);
			acceleration_x_rising_stream_reg <= acceleration_x_rising_stream_evaluator;
			acceleration_x_sinking_stream_reg <= acceleration_x_sinking_stream_evaluator;
			acceleration_x_direction_change_stream_reg <= acceleration_x_direction_change_stream_evaluator;
			acceleration_x_changes_stream_reg <= std_logic_vector(acceleration_x_changes_stream_evaluator);
			trigger_acc_stream_reg <= trigger_acc_stream_evaluator;
			gps_missed_beat_stream_reg <= gps_missed_beat_stream_evaluator;
			gps_medium_loss_stream_reg <= gps_medium_loss_stream_evaluator;
			gps_high_loss_stream_reg <= gps_high_loss_stream_evaluator;
			gps_very_high_loss_stream_reg <= gps_very_high_loss_stream_evaluator;
			trigger_gps_sats_stream_reg <= trigger_gps_sats_stream_evaluator;
        end if;
    end process;

    time_stream <= time_stream_reg;
	acceleration_x_stream <= acceleration_x_stream_reg;
	gps_sats_stream <= gps_sats_stream_reg;
	lat_gps_stream <= lat_gps_stream_reg;
	acceleration_x_periodic_stream <= acceleration_x_periodic_stream_reg;
	acceleration_x_rising_stream <= acceleration_x_rising_stream_reg;
	acceleration_x_sinking_stream <= acceleration_x_sinking_stream_reg;
	acceleration_x_direction_change_stream <= acceleration_x_direction_change_stream_reg;
	acceleration_x_changes_stream <= acceleration_x_changes_stream_reg;
	trigger_acc_stream <= trigger_acc_stream_reg;
	gps_missed_beat_stream <= gps_missed_beat_stream_reg;
	gps_medium_loss_stream <= gps_medium_loss_stream_reg;
	gps_high_loss_stream <= gps_high_loss_stream_reg;
	gps_very_high_loss_stream <= gps_very_high_loss_stream_reg;
	trigger_gps_sats_stream <= trigger_gps_sats_stream_reg;

end mixed;

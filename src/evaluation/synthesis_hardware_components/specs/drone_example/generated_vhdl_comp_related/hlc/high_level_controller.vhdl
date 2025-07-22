library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity high_level_controller is
    port(
        clk, rst : in std_logic;
        system_clk : in std_logic;
        time_data_in : in std_logic_vector(63 downto 0);
        new_input : in std_logic;
		acceleration_x_data_in : in std_logic_vector(63 downto 0);
		acceleration_x_push_in : in std_logic;
		gps_sats_data_in : in std_logic_vector(63 downto 0);
		gps_sats_push_in : in std_logic;
		lat_gps_data_in : in std_logic_vector(63 downto 0);
		lat_gps_push_in : in std_logic;
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
end high_level_controller;

architecture mixed of high_level_controller is

component extInterface
    port (
        clk, rst : in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		acceleration_x_data_in : in std_logic_vector(63 downto 0);
		acceleration_x_push_in : in std_logic;
		gps_sats_data_in : in std_logic_vector(63 downto 0);
		gps_sats_push_in : in std_logic;
		lat_gps_data_in : in std_logic_vector(63 downto 0);
		lat_gps_push_in : in std_logic;
		acceleration_x_data_out : out signed(63 downto 0);
		acceleration_x_push_out : out std_logic;
		gps_sats_data_out : out unsigned(63 downto 0);
		gps_sats_push_out : out std_logic;
		lat_gps_data_out : out signed(63 downto 0);
		lat_gps_push_out : out std_logic;
        time_out : out unsigned(63 downto 0)
    );
end component;


component check_new_input
    port (
        clk, rst : in std_logic;
        new_input_in : in std_logic;
        new_input_out : out std_logic
    );
end component;


component event_delay
    port (
        clk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		acceleration_x_data_in : in signed(63 downto 0);
		acceleration_x_push_in : in std_logic;
		gps_sats_data_in : in unsigned(63 downto 0);
		gps_sats_push_in : in std_logic;
		lat_gps_data_in : in signed(63 downto 0);
		lat_gps_push_in : in std_logic;
        push_event_in : in std_logic;
        time_out : out unsigned(63 downto 0);
		acceleration_x_data_out : out signed(63 downto 0);
		acceleration_x_push_out : out std_logic;
		gps_sats_data_out : out unsigned(63 downto 0);
		gps_sats_push_out : out std_logic;
		lat_gps_data_out : out signed(63 downto 0);
		lat_gps_push_out : out std_logic;
        push_event_out : out std_logic
    );
end component;


component time_unit
    port(
        rst : in std_logic;
        clk : in std_logic;
        sys_time_out : out unsigned(63 downto 0)
    );
end component;


component scheduler
    port (
        clk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
        time_last_deadline_out : out unsigned(63 downto 0);
        hit_deadline_out : out std_logic
    );
end component;


component hlQInterface is
    port (
        clk, rst : in std_logic;
        time_for_event : in unsigned(63 downto 0);
        time_for_deadline : in unsigned(63 downto 0);
        push_event : in std_logic;
        push_deadline : in std_logic;
		acceleration_x_data_in : in signed(63 downto 0);
		acceleration_x_en_in : in std_logic;
		gps_sats_data_in : in unsigned(63 downto 0);
		gps_sats_en_in : in std_logic;
		lat_gps_data_in : in signed(63 downto 0);
		lat_gps_en_in : in std_logic;
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
        time_out : out unsigned(63 downto 0);
        push_out : out std_logic
    );
end component;

    -- Internal Signal Declarations
    signal time_from_extInterface : unsigned(63 downto 0);
    signal time_from_time_unit : unsigned(63 downto 0);
    signal time_from_eventDelay : unsigned(63 downto 0);
    signal time_from_eventScheduler : unsigned(63 downto 0);
    signal time_from_scheduler : unsigned(63 downto 0);
    signal push_from_new_input_check : std_logic;
    signal push_from_eventDelay : std_logic;
    signal push_from_eventScheduler : std_logic;
    
	signal acceleration_x_data_from_extInterface : signed(63 downto 0);
	signal acceleration_x_push_from_extInterface : std_logic;
	signal acceleration_x_data_from_eventDelay : signed(63 downto 0);
	signal acceleration_x_push_from_eventDelay : std_logic;
	signal acceleration_x_data_from_eventScheduler : signed(63 downto 0);
	signal acceleration_x_push_from_eventScheduler : std_logic;
	signal gps_sats_data_from_extInterface : unsigned(63 downto 0);
	signal gps_sats_push_from_extInterface : std_logic;
	signal gps_sats_data_from_eventDelay : unsigned(63 downto 0);
	signal gps_sats_push_from_eventDelay : std_logic;
	signal gps_sats_data_from_eventScheduler : unsigned(63 downto 0);
	signal gps_sats_push_from_eventScheduler : std_logic;
	signal lat_gps_data_from_extInterface : signed(63 downto 0);
	signal lat_gps_push_from_extInterface : std_logic;
	signal lat_gps_data_from_eventDelay : signed(63 downto 0);
	signal lat_gps_push_from_eventDelay : std_logic;
	signal lat_gps_data_from_eventScheduler : signed(63 downto 0);
	signal lat_gps_push_from_eventScheduler : std_logic;

    signal hit_deadline : std_logic;
    signal hlc_clk_count : integer;
    signal slow_hlc_clk : std_logic;

begin
    extInterface_instance: extInterface
        port map (
            clk => slow_hlc_clk,
            rst => rst,
            time_in => time_data_in,
			acceleration_x_data_in => acceleration_x_data_in,
			acceleration_x_push_in => acceleration_x_push_in,
			gps_sats_data_in => gps_sats_data_in,
			gps_sats_push_in => gps_sats_push_in,
			lat_gps_data_in => lat_gps_data_in,
			lat_gps_push_in => lat_gps_push_in,
			acceleration_x_data_out => acceleration_x_data_from_extInterface,
			acceleration_x_push_out => acceleration_x_push_from_extInterface,
			gps_sats_data_out => gps_sats_data_from_extInterface,
			gps_sats_push_out => gps_sats_push_from_extInterface,
			lat_gps_data_out => lat_gps_data_from_extInterface,
			lat_gps_push_out => lat_gps_push_from_extInterface,
            time_out => time_from_extInterface
        );

    check_new_input_instance: check_new_input
        port map (
            clk => slow_hlc_clk,
            rst => rst,
            new_input_in => new_input,
            new_input_out => push_from_new_input_check
        );

    event_delay_instance: event_delay
        port map(
            clk => slow_hlc_clk,
            rst => rst,
            time_in => time_from_time_unit,
			acceleration_x_data_in => acceleration_x_data_from_extInterface,
			acceleration_x_push_in => acceleration_x_push_from_extInterface,
			gps_sats_data_in => gps_sats_data_from_extInterface,
			gps_sats_push_in => gps_sats_push_from_extInterface,
			lat_gps_data_in => lat_gps_data_from_extInterface,
			lat_gps_push_in => lat_gps_push_from_extInterface,
            push_event_in => push_from_new_input_check,
			acceleration_x_data_out => acceleration_x_data_from_eventDelay,
			acceleration_x_push_out => acceleration_x_push_from_eventDelay,
			gps_sats_data_out => gps_sats_data_from_eventDelay,
			gps_sats_push_out => gps_sats_push_from_eventDelay,
			lat_gps_data_out => lat_gps_data_from_eventDelay,
			lat_gps_push_out => lat_gps_push_from_eventDelay,
            time_out => time_from_eventDelay,
            push_event_out => push_from_eventDelay
        );




    time_unit_instance: time_unit
           port map (
               clk => system_clk,
               rst => rst,
               sys_time_out => time_from_time_unit
           );


    scheduler_instance: scheduler
        port map(
            clk => slow_hlc_clk,
            rst => rst,
            time_in => time_from_time_unit,
            time_last_deadline_out => time_from_scheduler,
            hit_deadline_out => hit_deadline
        );

    hlQInterface_instance: hlQInterface
        port map (
            clk => clk,
            rst => rst,
            time_for_event => time_from_eventDelay,
            time_for_deadline => time_from_scheduler,
            push_event => push_from_eventDelay,
            push_deadline => hit_deadline,
			acceleration_x_data_in => acceleration_x_data_from_eventDelay,
			acceleration_x_en_in => acceleration_x_push_from_eventDelay,
			gps_sats_data_in => gps_sats_data_from_eventDelay,
			gps_sats_en_in => gps_sats_push_from_eventDelay,
			lat_gps_data_in => lat_gps_data_from_eventDelay,
			lat_gps_en_in => lat_gps_push_from_eventDelay,
			acceleration_x_data_out => acceleration_x_data_out,
			acceleration_x_en_out => acceleration_x_en_out,
			gps_sats_data_out => gps_sats_data_out,
			gps_sats_en_out => gps_sats_en_out,
			lat_gps_data_out => lat_gps_data_out,
			lat_gps_en_out => lat_gps_en_out,
			acceleration_x_periodic_en_out => acceleration_x_periodic_en_out,
			acceleration_x_rising_en_out => acceleration_x_rising_en_out,
			acceleration_x_sinking_en_out => acceleration_x_sinking_en_out,
			acceleration_x_direction_change_en_out => acceleration_x_direction_change_en_out,
			acceleration_x_changes_en_out => acceleration_x_changes_en_out,
			trigger_acc_en_out => trigger_acc_en_out,
			gps_missed_beat_en_out => gps_missed_beat_en_out,
			gps_medium_loss_en_out => gps_medium_loss_en_out,
			gps_high_loss_en_out => gps_high_loss_en_out,
			gps_very_high_loss_en_out => gps_very_high_loss_en_out,
			trigger_gps_sats_en_out => trigger_gps_sats_en_out,
            time_out => time_data_out,
            push_out => push_data_in_query
        );


    process(clk, rst) begin
        if (rst = '1') then
            -- Reset Phase
            slow_hlc_clk <= '0';
            hlc_clk_count <= 0;
        elsif rising_edge(clk) then
            -- Logic Phase: Raise Slow Clock Signal Every Fourth Cycle
            hlc_clk_count <= (hlc_clk_count + 1) mod 4;
            if hlc_clk_count = 3 then
                slow_hlc_clk <= '1';
            else
                slow_hlc_clk <= '0';
            end if;
         end if;
    end process;



end mixed;

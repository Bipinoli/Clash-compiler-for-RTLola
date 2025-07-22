library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity hlQInterface is
    port (
        clk, rst : in std_logic;
        time_for_event : in unsigned(63 downto 0);
        time_for_deadline : in unsigned(63 downto 0);
        push_event : std_logic;
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
end hlQInterface;

--* Streams and their Activation Conditions:
--* Input Streams 
--* - acceleration_x @ { acceleration_x }
--* - gps_sats @ { gps_sats }
--* - lat_gps @ { lat_gps }
--* Event-based Output Streams 
--* Periodic Output Streams 
--* - acceleration_x_periodic @ 2000Hz
--* - acceleration_x_rising @ 2000Hz
--* - acceleration_x_sinking @ 2000Hz
--* - acceleration_x_direction_change @ 2000Hz
--* - acceleration_x_changes @ 2000Hz
--* - trigger_acc @ 2000Hz
--* - gps_missed_beat @ 2000Hz
--* - gps_medium_loss @ 100Hz
--* - gps_high_loss @ 100Hz
--* - gps_very_high_loss @ 100Hz
--* - trigger_gps_sats @ 100Hz
--* Resulting Deadline Array
--* || acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, gps_medium_loss, gps_high_loss, gps_very_high_loss, trigger_gps_sats, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc ||

architecture behavioral of hlQInterface is

    -- Internal Signal Declarations
    signal clock_state_machine : integer;
    signal push_to_queue : std_logic;
	signal acceleration_x_data_push : signed(63 downto 0);
	signal acceleration_x_en_push: std_logic;
	signal gps_sats_data_push : unsigned(63 downto 0);
	signal gps_sats_en_push: std_logic;
	signal lat_gps_data_push : signed(63 downto 0);
	signal lat_gps_en_push: std_logic;
	signal acceleration_x_periodic_en_push : std_logic;
	signal acceleration_x_rising_en_push : std_logic;
	signal acceleration_x_sinking_en_push : std_logic;
	signal acceleration_x_direction_change_en_push : std_logic;
	signal acceleration_x_changes_en_push : std_logic;
	signal trigger_acc_en_push : std_logic;
	signal gps_missed_beat_en_push : std_logic;
	signal gps_medium_loss_en_push : std_logic;
	signal gps_high_loss_en_push : std_logic;
	signal gps_very_high_loss_en_push : std_logic;
	signal trigger_gps_sats_en_push : std_logic;
	signal acceleration_x_periodic_en_array : bit_array(19 downto 0);
	signal acceleration_x_rising_en_array : bit_array(19 downto 0);
	signal acceleration_x_sinking_en_array : bit_array(19 downto 0);
	signal acceleration_x_direction_change_en_array : bit_array(19 downto 0);
	signal acceleration_x_changes_en_array : bit_array(19 downto 0);
	signal trigger_acc_en_array : bit_array(19 downto 0);
	signal gps_missed_beat_en_array : bit_array(19 downto 0);
	signal gps_medium_loss_en_array : bit_array(19 downto 0);
	signal gps_high_loss_en_array : bit_array(19 downto 0);
	signal gps_very_high_loss_en_array : bit_array(19 downto 0);
	signal trigger_gps_sats_en_array : bit_array(19 downto 0);
    signal last_deadline_id : integer;
    signal time_to_queue : unsigned(63 downto 0);

begin

    process(clk, rst) begin
        if (rst = '1') then
            -- Reset Phase
            clock_state_machine <= 0;
            time_to_queue <= (others => '0');
            push_to_queue <= '0';
            last_deadline_id <= 0;
			acceleration_x_data_push <= (others => '0');
			acceleration_x_en_push <= '0';
			gps_sats_data_push <= (others => '0');
			gps_sats_en_push <= '0';
			lat_gps_data_push <= (others => '0');
			lat_gps_en_push <= '0';
			acceleration_x_periodic_en_push <= '0';
			acceleration_x_rising_en_push <= '0';
			acceleration_x_sinking_en_push <= '0';
			acceleration_x_direction_change_en_push <= '0';
			acceleration_x_changes_en_push <= '0';
			trigger_acc_en_push <= '0';
			gps_missed_beat_en_push <= '0';
			gps_medium_loss_en_push <= '0';
			gps_high_loss_en_push <= '0';
			gps_very_high_loss_en_push <= '0';
			trigger_gps_sats_en_push <= '0';
            -- Initialize Deadline Arrays
            --* Deadline Array
            --* || acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc | acceleration_x_periodic, gps_missed_beat, gps_medium_loss, gps_high_loss, gps_very_high_loss, trigger_gps_sats, acceleration_x_rising, acceleration_x_sinking, acceleration_x_direction_change, acceleration_x_changes, trigger_acc ||
			acceleration_x_periodic_en_array(0) <= '1';
			acceleration_x_rising_en_array(0) <= '1';
			acceleration_x_sinking_en_array(0) <= '1';
			acceleration_x_direction_change_en_array(0) <= '1';
			acceleration_x_changes_en_array(0) <= '1';
			trigger_acc_en_array(0) <= '1';
			gps_missed_beat_en_array(0) <= '1';
			gps_medium_loss_en_array(0) <= '0';
			gps_high_loss_en_array(0) <= '0';
			gps_very_high_loss_en_array(0) <= '0';
			trigger_gps_sats_en_array(0) <= '0';
			acceleration_x_periodic_en_array(1) <= '1';
			acceleration_x_rising_en_array(1) <= '1';
			acceleration_x_sinking_en_array(1) <= '1';
			acceleration_x_direction_change_en_array(1) <= '1';
			acceleration_x_changes_en_array(1) <= '1';
			trigger_acc_en_array(1) <= '1';
			gps_missed_beat_en_array(1) <= '1';
			gps_medium_loss_en_array(1) <= '0';
			gps_high_loss_en_array(1) <= '0';
			gps_very_high_loss_en_array(1) <= '0';
			trigger_gps_sats_en_array(1) <= '0';
			acceleration_x_periodic_en_array(2) <= '1';
			acceleration_x_rising_en_array(2) <= '1';
			acceleration_x_sinking_en_array(2) <= '1';
			acceleration_x_direction_change_en_array(2) <= '1';
			acceleration_x_changes_en_array(2) <= '1';
			trigger_acc_en_array(2) <= '1';
			gps_missed_beat_en_array(2) <= '1';
			gps_medium_loss_en_array(2) <= '0';
			gps_high_loss_en_array(2) <= '0';
			gps_very_high_loss_en_array(2) <= '0';
			trigger_gps_sats_en_array(2) <= '0';
			acceleration_x_periodic_en_array(3) <= '1';
			acceleration_x_rising_en_array(3) <= '1';
			acceleration_x_sinking_en_array(3) <= '1';
			acceleration_x_direction_change_en_array(3) <= '1';
			acceleration_x_changes_en_array(3) <= '1';
			trigger_acc_en_array(3) <= '1';
			gps_missed_beat_en_array(3) <= '1';
			gps_medium_loss_en_array(3) <= '0';
			gps_high_loss_en_array(3) <= '0';
			gps_very_high_loss_en_array(3) <= '0';
			trigger_gps_sats_en_array(3) <= '0';
			acceleration_x_periodic_en_array(4) <= '1';
			acceleration_x_rising_en_array(4) <= '1';
			acceleration_x_sinking_en_array(4) <= '1';
			acceleration_x_direction_change_en_array(4) <= '1';
			acceleration_x_changes_en_array(4) <= '1';
			trigger_acc_en_array(4) <= '1';
			gps_missed_beat_en_array(4) <= '1';
			gps_medium_loss_en_array(4) <= '0';
			gps_high_loss_en_array(4) <= '0';
			gps_very_high_loss_en_array(4) <= '0';
			trigger_gps_sats_en_array(4) <= '0';
			acceleration_x_periodic_en_array(5) <= '1';
			acceleration_x_rising_en_array(5) <= '1';
			acceleration_x_sinking_en_array(5) <= '1';
			acceleration_x_direction_change_en_array(5) <= '1';
			acceleration_x_changes_en_array(5) <= '1';
			trigger_acc_en_array(5) <= '1';
			gps_missed_beat_en_array(5) <= '1';
			gps_medium_loss_en_array(5) <= '0';
			gps_high_loss_en_array(5) <= '0';
			gps_very_high_loss_en_array(5) <= '0';
			trigger_gps_sats_en_array(5) <= '0';
			acceleration_x_periodic_en_array(6) <= '1';
			acceleration_x_rising_en_array(6) <= '1';
			acceleration_x_sinking_en_array(6) <= '1';
			acceleration_x_direction_change_en_array(6) <= '1';
			acceleration_x_changes_en_array(6) <= '1';
			trigger_acc_en_array(6) <= '1';
			gps_missed_beat_en_array(6) <= '1';
			gps_medium_loss_en_array(6) <= '0';
			gps_high_loss_en_array(6) <= '0';
			gps_very_high_loss_en_array(6) <= '0';
			trigger_gps_sats_en_array(6) <= '0';
			acceleration_x_periodic_en_array(7) <= '1';
			acceleration_x_rising_en_array(7) <= '1';
			acceleration_x_sinking_en_array(7) <= '1';
			acceleration_x_direction_change_en_array(7) <= '1';
			acceleration_x_changes_en_array(7) <= '1';
			trigger_acc_en_array(7) <= '1';
			gps_missed_beat_en_array(7) <= '1';
			gps_medium_loss_en_array(7) <= '0';
			gps_high_loss_en_array(7) <= '0';
			gps_very_high_loss_en_array(7) <= '0';
			trigger_gps_sats_en_array(7) <= '0';
			acceleration_x_periodic_en_array(8) <= '1';
			acceleration_x_rising_en_array(8) <= '1';
			acceleration_x_sinking_en_array(8) <= '1';
			acceleration_x_direction_change_en_array(8) <= '1';
			acceleration_x_changes_en_array(8) <= '1';
			trigger_acc_en_array(8) <= '1';
			gps_missed_beat_en_array(8) <= '1';
			gps_medium_loss_en_array(8) <= '0';
			gps_high_loss_en_array(8) <= '0';
			gps_very_high_loss_en_array(8) <= '0';
			trigger_gps_sats_en_array(8) <= '0';
			acceleration_x_periodic_en_array(9) <= '1';
			acceleration_x_rising_en_array(9) <= '1';
			acceleration_x_sinking_en_array(9) <= '1';
			acceleration_x_direction_change_en_array(9) <= '1';
			acceleration_x_changes_en_array(9) <= '1';
			trigger_acc_en_array(9) <= '1';
			gps_missed_beat_en_array(9) <= '1';
			gps_medium_loss_en_array(9) <= '0';
			gps_high_loss_en_array(9) <= '0';
			gps_very_high_loss_en_array(9) <= '0';
			trigger_gps_sats_en_array(9) <= '0';
			acceleration_x_periodic_en_array(10) <= '1';
			acceleration_x_rising_en_array(10) <= '1';
			acceleration_x_sinking_en_array(10) <= '1';
			acceleration_x_direction_change_en_array(10) <= '1';
			acceleration_x_changes_en_array(10) <= '1';
			trigger_acc_en_array(10) <= '1';
			gps_missed_beat_en_array(10) <= '1';
			gps_medium_loss_en_array(10) <= '0';
			gps_high_loss_en_array(10) <= '0';
			gps_very_high_loss_en_array(10) <= '0';
			trigger_gps_sats_en_array(10) <= '0';
			acceleration_x_periodic_en_array(11) <= '1';
			acceleration_x_rising_en_array(11) <= '1';
			acceleration_x_sinking_en_array(11) <= '1';
			acceleration_x_direction_change_en_array(11) <= '1';
			acceleration_x_changes_en_array(11) <= '1';
			trigger_acc_en_array(11) <= '1';
			gps_missed_beat_en_array(11) <= '1';
			gps_medium_loss_en_array(11) <= '0';
			gps_high_loss_en_array(11) <= '0';
			gps_very_high_loss_en_array(11) <= '0';
			trigger_gps_sats_en_array(11) <= '0';
			acceleration_x_periodic_en_array(12) <= '1';
			acceleration_x_rising_en_array(12) <= '1';
			acceleration_x_sinking_en_array(12) <= '1';
			acceleration_x_direction_change_en_array(12) <= '1';
			acceleration_x_changes_en_array(12) <= '1';
			trigger_acc_en_array(12) <= '1';
			gps_missed_beat_en_array(12) <= '1';
			gps_medium_loss_en_array(12) <= '0';
			gps_high_loss_en_array(12) <= '0';
			gps_very_high_loss_en_array(12) <= '0';
			trigger_gps_sats_en_array(12) <= '0';
			acceleration_x_periodic_en_array(13) <= '1';
			acceleration_x_rising_en_array(13) <= '1';
			acceleration_x_sinking_en_array(13) <= '1';
			acceleration_x_direction_change_en_array(13) <= '1';
			acceleration_x_changes_en_array(13) <= '1';
			trigger_acc_en_array(13) <= '1';
			gps_missed_beat_en_array(13) <= '1';
			gps_medium_loss_en_array(13) <= '0';
			gps_high_loss_en_array(13) <= '0';
			gps_very_high_loss_en_array(13) <= '0';
			trigger_gps_sats_en_array(13) <= '0';
			acceleration_x_periodic_en_array(14) <= '1';
			acceleration_x_rising_en_array(14) <= '1';
			acceleration_x_sinking_en_array(14) <= '1';
			acceleration_x_direction_change_en_array(14) <= '1';
			acceleration_x_changes_en_array(14) <= '1';
			trigger_acc_en_array(14) <= '1';
			gps_missed_beat_en_array(14) <= '1';
			gps_medium_loss_en_array(14) <= '0';
			gps_high_loss_en_array(14) <= '0';
			gps_very_high_loss_en_array(14) <= '0';
			trigger_gps_sats_en_array(14) <= '0';
			acceleration_x_periodic_en_array(15) <= '1';
			acceleration_x_rising_en_array(15) <= '1';
			acceleration_x_sinking_en_array(15) <= '1';
			acceleration_x_direction_change_en_array(15) <= '1';
			acceleration_x_changes_en_array(15) <= '1';
			trigger_acc_en_array(15) <= '1';
			gps_missed_beat_en_array(15) <= '1';
			gps_medium_loss_en_array(15) <= '0';
			gps_high_loss_en_array(15) <= '0';
			gps_very_high_loss_en_array(15) <= '0';
			trigger_gps_sats_en_array(15) <= '0';
			acceleration_x_periodic_en_array(16) <= '1';
			acceleration_x_rising_en_array(16) <= '1';
			acceleration_x_sinking_en_array(16) <= '1';
			acceleration_x_direction_change_en_array(16) <= '1';
			acceleration_x_changes_en_array(16) <= '1';
			trigger_acc_en_array(16) <= '1';
			gps_missed_beat_en_array(16) <= '1';
			gps_medium_loss_en_array(16) <= '0';
			gps_high_loss_en_array(16) <= '0';
			gps_very_high_loss_en_array(16) <= '0';
			trigger_gps_sats_en_array(16) <= '0';
			acceleration_x_periodic_en_array(17) <= '1';
			acceleration_x_rising_en_array(17) <= '1';
			acceleration_x_sinking_en_array(17) <= '1';
			acceleration_x_direction_change_en_array(17) <= '1';
			acceleration_x_changes_en_array(17) <= '1';
			trigger_acc_en_array(17) <= '1';
			gps_missed_beat_en_array(17) <= '1';
			gps_medium_loss_en_array(17) <= '0';
			gps_high_loss_en_array(17) <= '0';
			gps_very_high_loss_en_array(17) <= '0';
			trigger_gps_sats_en_array(17) <= '0';
			acceleration_x_periodic_en_array(18) <= '1';
			acceleration_x_rising_en_array(18) <= '1';
			acceleration_x_sinking_en_array(18) <= '1';
			acceleration_x_direction_change_en_array(18) <= '1';
			acceleration_x_changes_en_array(18) <= '1';
			trigger_acc_en_array(18) <= '1';
			gps_missed_beat_en_array(18) <= '1';
			gps_medium_loss_en_array(18) <= '0';
			gps_high_loss_en_array(18) <= '0';
			gps_very_high_loss_en_array(18) <= '0';
			trigger_gps_sats_en_array(18) <= '0';
			acceleration_x_periodic_en_array(19) <= '1';
			acceleration_x_rising_en_array(19) <= '1';
			acceleration_x_sinking_en_array(19) <= '1';
			acceleration_x_direction_change_en_array(19) <= '1';
			acceleration_x_changes_en_array(19) <= '1';
			trigger_acc_en_array(19) <= '1';
			gps_missed_beat_en_array(19) <= '1';
			gps_medium_loss_en_array(19) <= '1';
			gps_high_loss_en_array(19) <= '1';
			gps_very_high_loss_en_array(19) <= '1';
			trigger_gps_sats_en_array(19) <= '1';
        elsif (rising_edge(clk)) then
            clock_state_machine <= (clock_state_machine + 1) mod 4;
            if push_deadline = '1' and clock_state_machine = 0 then
                -- Deadline Handling
                push_to_queue <= '1';
                last_deadline_id <= (last_deadline_id + 1) mod 20;
                time_to_queue <= time_for_deadline;
				--* acceleration_x @ { acceleration_x }
				acceleration_x_en_push <= '0';
				--* gps_sats @ { gps_sats }
				gps_sats_en_push <= '0';
				--* lat_gps @ { lat_gps }
				lat_gps_en_push <= '0';
				--* acceleration_x_periodic @ 2000Hz
				acceleration_x_periodic_en_push <= acceleration_x_periodic_en_array(last_deadline_id);
				--* acceleration_x_rising @ 2000Hz
				acceleration_x_rising_en_push <= acceleration_x_rising_en_array(last_deadline_id);
				--* acceleration_x_sinking @ 2000Hz
				acceleration_x_sinking_en_push <= acceleration_x_sinking_en_array(last_deadline_id);
				--* acceleration_x_direction_change @ 2000Hz
				acceleration_x_direction_change_en_push <= acceleration_x_direction_change_en_array(last_deadline_id);
				--* acceleration_x_changes @ 2000Hz
				acceleration_x_changes_en_push <= acceleration_x_changes_en_array(last_deadline_id);
				--* trigger_acc @ 2000Hz
				trigger_acc_en_push <= trigger_acc_en_array(last_deadline_id);
				--* gps_missed_beat @ 2000Hz
				gps_missed_beat_en_push <= gps_missed_beat_en_array(last_deadline_id);
				--* gps_medium_loss @ 100Hz
				gps_medium_loss_en_push <= gps_medium_loss_en_array(last_deadline_id);
				--* gps_high_loss @ 100Hz
				gps_high_loss_en_push <= gps_high_loss_en_array(last_deadline_id);
				--* gps_very_high_loss @ 100Hz
				gps_very_high_loss_en_push <= gps_very_high_loss_en_array(last_deadline_id);
				--* trigger_gps_sats @ 100Hz
				trigger_gps_sats_en_push <= trigger_gps_sats_en_array(last_deadline_id);
            elsif push_event = '1' and clock_state_machine = 2 then
                -- Event Handling
                push_to_queue <= '1';
                time_to_queue <= time_for_event;
				--* acceleration_x @ { acceleration_x }
				acceleration_x_data_push <= acceleration_x_data_in;
				acceleration_x_en_push <= acceleration_x_en_in;
				--* gps_sats @ { gps_sats }
				gps_sats_data_push <= gps_sats_data_in;
				gps_sats_en_push <= gps_sats_en_in;
				--* lat_gps @ { lat_gps }
				lat_gps_data_push <= lat_gps_data_in;
				lat_gps_en_push <= lat_gps_en_in;
				--* acceleration_x_periodic @ 2000Hz
				acceleration_x_periodic_en_push <= '0';
				--* acceleration_x_rising @ 2000Hz
				acceleration_x_rising_en_push <= '0';
				--* acceleration_x_sinking @ 2000Hz
				acceleration_x_sinking_en_push <= '0';
				--* acceleration_x_direction_change @ 2000Hz
				acceleration_x_direction_change_en_push <= '0';
				--* acceleration_x_changes @ 2000Hz
				acceleration_x_changes_en_push <= '0';
				--* trigger_acc @ 2000Hz
				trigger_acc_en_push <= '0';
				--* gps_missed_beat @ 2000Hz
				gps_missed_beat_en_push <= '0';
				--* gps_medium_loss @ 100Hz
				gps_medium_loss_en_push <= '0';
				--* gps_high_loss @ 100Hz
				gps_high_loss_en_push <= '0';
				--* gps_very_high_loss @ 100Hz
				gps_very_high_loss_en_push <= '0';
				--* trigger_gps_sats @ 100Hz
				trigger_gps_sats_en_push <= '0';
            else
                -- Enable No Stream
                push_to_queue <= '0';
				acceleration_x_en_push <= '0';
				gps_sats_en_push <= '0';
				lat_gps_en_push <= '0';
				acceleration_x_periodic_en_push <= '0';
				acceleration_x_rising_en_push <= '0';
				acceleration_x_sinking_en_push <= '0';
				acceleration_x_direction_change_en_push <= '0';
				acceleration_x_changes_en_push <= '0';
				trigger_acc_en_push <= '0';
				gps_missed_beat_en_push <= '0';
				gps_medium_loss_en_push <= '0';
				gps_high_loss_en_push <= '0';
				gps_very_high_loss_en_push <= '0';
				trigger_gps_sats_en_push <= '0';
            end if;
        end if;
    end process;

    push_out <= push_to_queue;
    time_out <= time_to_queue;
	acceleration_x_data_out <= acceleration_x_data_push;
	acceleration_x_en_out <= acceleration_x_en_push;
	gps_sats_data_out <= gps_sats_data_push;
	gps_sats_en_out <= gps_sats_en_push;
	lat_gps_data_out <= lat_gps_data_push;
	lat_gps_en_out <= lat_gps_en_push;
	acceleration_x_periodic_en_out <= acceleration_x_periodic_en_push;
	acceleration_x_rising_en_out <= acceleration_x_rising_en_push;
	acceleration_x_sinking_en_out <= acceleration_x_sinking_en_push;
	acceleration_x_direction_change_en_out <= acceleration_x_direction_change_en_push;
	acceleration_x_changes_en_out <= acceleration_x_changes_en_push;
	trigger_acc_en_out <= trigger_acc_en_push;
	gps_missed_beat_en_out <= gps_missed_beat_en_push;
	gps_medium_loss_en_out <= gps_medium_loss_en_push;
	gps_high_loss_en_out <= gps_high_loss_en_push;
	gps_very_high_loss_en_out <= gps_very_high_loss_en_push;
	trigger_gps_sats_en_out <= trigger_gps_sats_en_push;

end behavioral;

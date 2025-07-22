library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity implementation is
    port (
        clk: in std_logic;
        rst: in std_logic;
        offline : in std_logic;
        input_time: in std_logic_vector(63 downto 0);
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
end entity;

architecture structural of implementation is

    component clock_pre_processing is
        port (
            clk : in std_logic;
            rst : in std_logic;
            sys_clk : out std_logic;
            tclk : out std_logic;
            eclk : out std_logic
        );
    end component;

    component input_pre_processing is
        port (
            clk : in std_logic;
            rst : in std_logic;
            time_in : in std_logic_vector(63 downto 0);
            new_input_in : in std_logic;
			acceleration_x_in : in std_logic_vector(63 downto 0);
			acceleration_x_in_new_input : in std_logic;
			gps_sats_in : in std_logic_vector(63 downto 0);
			gps_sats_in_new_input : in std_logic;
			lat_gps_in : in std_logic_vector(63 downto 0);
			lat_gps_in_new_input : in std_logic;
            time_out : out std_logic_vector(63 downto 0);
			acceleration_x_out : out std_logic_vector(63 downto 0);
			acceleration_x_out_new_input : out std_logic;
			gps_sats_out : out std_logic_vector(63 downto 0);
			gps_sats_out_new_input : out std_logic;
			lat_gps_out : out std_logic_vector(63 downto 0);
			lat_gps_out_new_input : out std_logic;
            new_input_out : out std_logic
        );
    end component;

    component monitor is
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
            lost_data: out std_logic
        );
    end component;

    signal sys_clk : std_logic;
    signal tclk : std_logic;
    signal eclk : std_logic;
    signal time_reg : std_logic_vector(63 downto 0);
    signal new_input_reg : std_logic;
	signal acceleration_x_data_reg : std_logic_vector(63 downto 0);
	signal acceleration_x_data_reg_new_input : std_logic;
	signal gps_sats_data_reg : std_logic_vector(63 downto 0);
	signal gps_sats_data_reg_new_input : std_logic;
	signal lat_gps_data_reg : std_logic_vector(63 downto 0);
	signal lat_gps_data_reg_new_input : std_logic;

begin

    clock_pre_processing_instance: clock_pre_processing
        port map (
            clk => clk,
            rst => rst,
            sys_clk => sys_clk,
            tclk => tclk,
            eclk => eclk
        );

    input_pre_processing_instance: input_pre_processing
        port map (
            clk => clk,
            rst => rst,
            time_in => input_time,
            new_input_in => new_input,
			acceleration_x_in => acceleration_x_data_in,
			acceleration_x_in_new_input => acceleration_x_data_in_new_input,
			gps_sats_in => gps_sats_data_in,
			gps_sats_in_new_input => gps_sats_data_in_new_input,
			lat_gps_in => lat_gps_data_in,
			lat_gps_in_new_input => lat_gps_data_in_new_input,
            time_out => time_reg,
			acceleration_x_out => acceleration_x_data_reg,
			acceleration_x_out_new_input => acceleration_x_data_reg_new_input,
			gps_sats_out => gps_sats_data_reg,
			gps_sats_out_new_input => gps_sats_data_reg_new_input,
			lat_gps_out => lat_gps_data_reg,
			lat_gps_out_new_input => lat_gps_data_reg_new_input,
            new_input_out => new_input_reg
        );

    monitor_instance: monitor
        port map (
            clk => sys_clk,
            tclk => tclk,
            qclk => sys_clk,
            eclk => eclk,
            rst => rst,
            input_time => time_reg,
            offline => offline,
            new_input => new_input_reg,
			acceleration_x_data_in => acceleration_x_data_reg,
			acceleration_x_data_in_new_input => acceleration_x_data_reg_new_input,
			gps_sats_data_in => gps_sats_data_reg,
			gps_sats_data_in_new_input => gps_sats_data_reg_new_input,
			lat_gps_data_in => lat_gps_data_reg,
			lat_gps_data_in_new_input => lat_gps_data_reg_new_input,
            time_stream => time_stream,
			acceleration_x_stream => acceleration_x_stream,
			gps_sats_stream => gps_sats_stream,
			lat_gps_stream => lat_gps_stream,
			acceleration_x_periodic_stream => acceleration_x_periodic_stream,
			acceleration_x_rising_stream => acceleration_x_rising_stream,
			acceleration_x_sinking_stream => acceleration_x_sinking_stream,
			acceleration_x_direction_change_stream => acceleration_x_direction_change_stream,
			acceleration_x_changes_stream => acceleration_x_changes_stream,
			trigger_acc_stream => trigger_acc_stream,
			gps_missed_beat_stream => gps_missed_beat_stream,
			gps_medium_loss_stream => gps_medium_loss_stream,
			gps_high_loss_stream => gps_high_loss_stream,
			gps_very_high_loss_stream => gps_very_high_loss_stream,
			trigger_gps_sats_stream => trigger_gps_sats_stream,
            lost_data => lost_data
        );

end structural;
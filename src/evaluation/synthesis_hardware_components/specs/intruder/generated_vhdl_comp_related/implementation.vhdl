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
		lat_data_in : in std_logic_vector(63 downto 0);
		lat_data_in_new_input : in std_logic;
		lon_data_in : in std_logic_vector(63 downto 0);
		lon_data_in_new_input : in std_logic;
        time_stream : out std_logic_vector(63 downto 0);
		lat_stream: out std_logic_vector(63 downto 0);
		lon_stream: out std_logic_vector(63 downto 0);
		distance_stream: out std_logic_vector(63 downto 0);
		closer_stream: out std_logic;
		trigger_closer_stream: out std_logic;
		is_good_stream: out std_logic;
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
			lat_in : in std_logic_vector(63 downto 0);
			lat_in_new_input : in std_logic;
			lon_in : in std_logic_vector(63 downto 0);
			lon_in_new_input : in std_logic;
            time_out : out std_logic_vector(63 downto 0);
			lat_out : out std_logic_vector(63 downto 0);
			lat_out_new_input : out std_logic;
			lon_out : out std_logic_vector(63 downto 0);
			lon_out_new_input : out std_logic;
            new_input_out : out std_logic
        );
    end component;

    component monitor is
        port (
            clk, tclk, qclk, eclk, rst : in std_logic;
            input_time : in std_logic_vector(63 downto 0);
            offline : in std_logic;
            new_input : in std_logic;
			lat_data_in : in std_logic_vector(63 downto 0);
			lat_data_in_new_input : in std_logic;
			lon_data_in : in std_logic_vector(63 downto 0);
			lon_data_in_new_input : in std_logic;
            time_stream : out std_logic_vector(63 downto 0);
			lat_stream: out std_logic_vector(63 downto 0);
			lon_stream: out std_logic_vector(63 downto 0);
			distance_stream: out std_logic_vector(63 downto 0);
			closer_stream: out std_logic;
			trigger_closer_stream: out std_logic;
			is_good_stream: out std_logic;
            lost_data: out std_logic
        );
    end component;

    signal sys_clk : std_logic;
    signal tclk : std_logic;
    signal eclk : std_logic;
    signal time_reg : std_logic_vector(63 downto 0);
    signal new_input_reg : std_logic;
	signal lat_data_reg : std_logic_vector(63 downto 0);
	signal lat_data_reg_new_input : std_logic;
	signal lon_data_reg : std_logic_vector(63 downto 0);
	signal lon_data_reg_new_input : std_logic;

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
			lat_in => lat_data_in,
			lat_in_new_input => lat_data_in_new_input,
			lon_in => lon_data_in,
			lon_in_new_input => lon_data_in_new_input,
            time_out => time_reg,
			lat_out => lat_data_reg,
			lat_out_new_input => lat_data_reg_new_input,
			lon_out => lon_data_reg,
			lon_out_new_input => lon_data_reg_new_input,
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
			lat_data_in => lat_data_reg,
			lat_data_in_new_input => lat_data_reg_new_input,
			lon_data_in => lon_data_reg,
			lon_data_in_new_input => lon_data_reg_new_input,
            time_stream => time_stream,
			lat_stream => lat_stream,
			lon_stream => lon_stream,
			distance_stream => distance_stream,
			closer_stream => closer_stream,
			trigger_closer_stream => trigger_closer_stream,
			is_good_stream => is_good_stream,
            lost_data => lost_data
        );

end structural;
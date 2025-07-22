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
		gps_x_data_in : in std_logic_vector(63 downto 0);
		gps_x_data_in_new_input : in std_logic;
		num_satellites_data_in : in std_logic_vector(63 downto 0);
		num_satellites_data_in_new_input : in std_logic;
		imu_acc_x_data_in : in std_logic_vector(63 downto 0);
		imu_acc_x_data_in_new_input : in std_logic;
        time_stream : out std_logic_vector(63 downto 0);
		gps_x_stream: out std_logic_vector(63 downto 0);
		num_satellites_stream: out std_logic_vector(63 downto 0);
		imu_acc_x_stream: out std_logic_vector(63 downto 0);
		gps_emitted_enough_stream: out std_logic;
		few_satellites_stream: out std_logic;
		is_unreliable_gps_data_stream: out std_logic;
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
			gps_x_in : in std_logic_vector(63 downto 0);
			gps_x_in_new_input : in std_logic;
			num_satellites_in : in std_logic_vector(63 downto 0);
			num_satellites_in_new_input : in std_logic;
			imu_acc_x_in : in std_logic_vector(63 downto 0);
			imu_acc_x_in_new_input : in std_logic;
            time_out : out std_logic_vector(63 downto 0);
			gps_x_out : out std_logic_vector(63 downto 0);
			gps_x_out_new_input : out std_logic;
			num_satellites_out : out std_logic_vector(63 downto 0);
			num_satellites_out_new_input : out std_logic;
			imu_acc_x_out : out std_logic_vector(63 downto 0);
			imu_acc_x_out_new_input : out std_logic;
            new_input_out : out std_logic
        );
    end component;

    component monitor is
        port (
            clk, tclk, qclk, eclk, rst : in std_logic;
            input_time : in std_logic_vector(63 downto 0);
            offline : in std_logic;
            new_input : in std_logic;
			gps_x_data_in : in std_logic_vector(63 downto 0);
			gps_x_data_in_new_input : in std_logic;
			num_satellites_data_in : in std_logic_vector(63 downto 0);
			num_satellites_data_in_new_input : in std_logic;
			imu_acc_x_data_in : in std_logic_vector(63 downto 0);
			imu_acc_x_data_in_new_input : in std_logic;
            time_stream : out std_logic_vector(63 downto 0);
			gps_x_stream: out std_logic_vector(63 downto 0);
			num_satellites_stream: out std_logic_vector(63 downto 0);
			imu_acc_x_stream: out std_logic_vector(63 downto 0);
			gps_emitted_enough_stream: out std_logic;
			few_satellites_stream: out std_logic;
			is_unreliable_gps_data_stream: out std_logic;
            lost_data: out std_logic
        );
    end component;

    signal sys_clk : std_logic;
    signal tclk : std_logic;
    signal eclk : std_logic;
    signal time_reg : std_logic_vector(63 downto 0);
    signal new_input_reg : std_logic;
	signal gps_x_data_reg : std_logic_vector(63 downto 0);
	signal gps_x_data_reg_new_input : std_logic;
	signal num_satellites_data_reg : std_logic_vector(63 downto 0);
	signal num_satellites_data_reg_new_input : std_logic;
	signal imu_acc_x_data_reg : std_logic_vector(63 downto 0);
	signal imu_acc_x_data_reg_new_input : std_logic;

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
			gps_x_in => gps_x_data_in,
			gps_x_in_new_input => gps_x_data_in_new_input,
			num_satellites_in => num_satellites_data_in,
			num_satellites_in_new_input => num_satellites_data_in_new_input,
			imu_acc_x_in => imu_acc_x_data_in,
			imu_acc_x_in_new_input => imu_acc_x_data_in_new_input,
            time_out => time_reg,
			gps_x_out => gps_x_data_reg,
			gps_x_out_new_input => gps_x_data_reg_new_input,
			num_satellites_out => num_satellites_data_reg,
			num_satellites_out_new_input => num_satellites_data_reg_new_input,
			imu_acc_x_out => imu_acc_x_data_reg,
			imu_acc_x_out_new_input => imu_acc_x_data_reg_new_input,
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
			gps_x_data_in => gps_x_data_reg,
			gps_x_data_in_new_input => gps_x_data_reg_new_input,
			num_satellites_data_in => num_satellites_data_reg,
			num_satellites_data_in_new_input => num_satellites_data_reg_new_input,
			imu_acc_x_data_in => imu_acc_x_data_reg,
			imu_acc_x_data_in_new_input => imu_acc_x_data_reg_new_input,
            time_stream => time_stream,
			gps_x_stream => gps_x_stream,
			num_satellites_stream => num_satellites_stream,
			imu_acc_x_stream => imu_acc_x_stream,
			gps_emitted_enough_stream => gps_emitted_enough_stream,
			few_satellites_stream => few_satellites_stream,
			is_unreliable_gps_data_stream => is_unreliable_gps_data_stream,
            lost_data => lost_data
        );

end structural;
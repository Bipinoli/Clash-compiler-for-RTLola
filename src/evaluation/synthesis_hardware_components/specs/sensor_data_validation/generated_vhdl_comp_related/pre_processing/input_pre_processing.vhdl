library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity input_pre_processing is
    port(
        clk, rst: in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		gps_x_in : in std_logic_vector(63 downto 0);
		gps_x_in_new_input : in std_logic;
		num_satellites_in : in std_logic_vector(63 downto 0);
		num_satellites_in_new_input : in std_logic;
		imu_acc_x_in : in std_logic_vector(63 downto 0);
		imu_acc_x_in_new_input : in std_logic;
        new_input_in : in std_logic;
        time_out : out std_logic_vector(63 downto 0);
		gps_x_out : out std_logic_vector(63 downto 0);
		gps_x_out_new_input : out std_logic;
		num_satellites_out : out std_logic_vector(63 downto 0);
		num_satellites_out_new_input : out std_logic;
		imu_acc_x_out : out std_logic_vector(63 downto 0);
		imu_acc_x_out_new_input : out std_logic;
        new_input_out : out std_logic
    );
end input_pre_processing;

architecture behavioral of input_pre_processing is

    signal new_input : std_logic;
    signal time_reg : std_logic_vector(63 downto 0);
	signal gps_x_reg : std_logic_vector(63 downto 0);
	signal gps_x_reg_new_input : std_logic;
	signal num_satellites_reg : std_logic_vector(63 downto 0);
	signal num_satellites_reg_new_input : std_logic;
	signal imu_acc_x_reg : std_logic_vector(63 downto 0);
	signal imu_acc_x_reg_new_input : std_logic;

begin

    process(clk, rst) begin
        if (rst = '1') then
            -- set default
            new_input <= '0';
            time_reg <= (others => '0');
			gps_x_reg <= (others => '0');
			gps_x_reg_new_input <= '0';
			num_satellites_reg <= (others => '0');
			num_satellites_reg_new_input <= '0';
			imu_acc_x_reg <= (others => '0');
			imu_acc_x_reg_new_input <= '0';
        elsif (rising_edge(clk)) then
            new_input <= new_input_in;
            time_reg <= time_in;
				gps_x_reg <= gps_x_in;
				gps_x_reg_new_input <= gps_x_in_new_input;
				num_satellites_reg <= num_satellites_in;
				num_satellites_reg_new_input <= num_satellites_in_new_input;
				imu_acc_x_reg <= imu_acc_x_in;
				imu_acc_x_reg_new_input <= imu_acc_x_in_new_input;
        end if;
    end process;

    new_input_out <= new_input;
    time_out <= time_reg;
	gps_x_out <= gps_x_reg;
	gps_x_out_new_input <= gps_x_reg_new_input;
	num_satellites_out <= num_satellites_reg;
	num_satellites_out_new_input <= num_satellites_reg_new_input;
	imu_acc_x_out <= imu_acc_x_reg;
	imu_acc_x_out_new_input <= imu_acc_x_reg_new_input;

end behavioral;
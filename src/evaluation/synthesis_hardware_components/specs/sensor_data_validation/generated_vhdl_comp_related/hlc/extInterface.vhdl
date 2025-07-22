library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity extInterface is
    port(
        clk, rst: in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		gps_x_data_in : in std_logic_vector(63 downto 0);
		gps_x_push_in : in std_logic;
		num_satellites_data_in : in std_logic_vector(63 downto 0);
		num_satellites_push_in : in std_logic;
		imu_acc_x_data_in : in std_logic_vector(63 downto 0);
		imu_acc_x_push_in : in std_logic;
		gps_x_data_out : out signed(63 downto 0);
		gps_x_push_out : out std_logic;
		num_satellites_data_out : out signed(63 downto 0);
		num_satellites_push_out : out std_logic;
		imu_acc_x_data_out : out signed(63 downto 0);
		imu_acc_x_push_out : out std_logic;
        time_out : out unsigned(63 downto 0)
    );
end extInterface;

--* Input Streams and their Types in the Specification: 
--* - input gps_x : Int64
--* - input num_satellites : Int64
--* - input imu_acc_x : Int64

architecture behavioral of extInterface is

    -- Internal Signal Declarations
    signal time_converted : unsigned(63 downto 0);
	signal gps_x_parsed : signed(63 downto 0);
	signal gps_x_push_delayed : std_logic;
	signal num_satellites_parsed : signed(63 downto 0);
	signal num_satellites_push_delayed : std_logic;
	signal imu_acc_x_parsed : signed(63 downto 0);
	signal imu_acc_x_push_delayed : std_logic;

begin

    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            time_converted <= to_unsigned(0, time_converted'length);
			gps_x_parsed <= (others => '0');
			gps_x_push_delayed <= '0';
			num_satellites_parsed <= (others => '0');
			num_satellites_push_delayed <= '0';
			imu_acc_x_parsed <= (others => '0');
			imu_acc_x_push_delayed <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase: Convert Input in Numeric Types
            time_converted <= unsigned(time_in);
			--* input gps_x : Int64
			gps_x_parsed <= signed(gps_x_data_in);
			gps_x_push_delayed <= gps_x_push_in;
			--* input num_satellites : Int64
			num_satellites_parsed <= signed(num_satellites_data_in);
			num_satellites_push_delayed <= num_satellites_push_in;
			--* input imu_acc_x : Int64
			imu_acc_x_parsed <= signed(imu_acc_x_data_in);
			imu_acc_x_push_delayed <= imu_acc_x_push_in;
        end if;
    end process;

    time_out <= time_converted;
	gps_x_data_out <= gps_x_parsed;
	gps_x_push_out <= gps_x_push_delayed;
	num_satellites_data_out <= num_satellites_parsed;
	num_satellites_push_out <= num_satellites_push_delayed;
	imu_acc_x_data_out <= imu_acc_x_parsed;
	imu_acc_x_push_out <= imu_acc_x_push_delayed;

end behavioral;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity event_delay is
    port(
        clk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		gps_x_data_in : in signed(63 downto 0);
		gps_x_push_in : in std_logic;
		num_satellites_data_in : in signed(63 downto 0);
		num_satellites_push_in : in std_logic;
		imu_acc_x_data_in : in signed(63 downto 0);
		imu_acc_x_push_in : in std_logic;
        push_event_in : std_logic;
        time_out : out unsigned(63 downto 0);
		gps_x_data_out : out signed(63 downto 0);
		gps_x_push_out : out std_logic;
		num_satellites_data_out : out signed(63 downto 0);
		num_satellites_push_out : out std_logic;
		imu_acc_x_data_out : out signed(63 downto 0);
		imu_acc_x_push_out : out std_logic;
        push_event_out : out std_logic
    );
end event_delay;

--* Input Streams and their Types in the Specification:
--* - gps_x : Int64 *--
--* - num_satellites : Int64 *--
--* - imu_acc_x : Int64 *--

architecture behavioral of event_delay is

    -- Internal Signal Declarations
    signal time_value : unsigned(63 downto 0);
    signal push_event : std_logic;
	signal gps_x_data_delayed : signed(63 downto 0);
	signal gps_x_push_delayed : std_logic;
	signal num_satellites_data_delayed : signed(63 downto 0);
	signal num_satellites_push_delayed : std_logic;
	signal imu_acc_x_data_delayed : signed(63 downto 0);
	signal imu_acc_x_push_delayed : std_logic;

    begin

    process(clk, rst) begin
        if (rst = '1') then
            -- Reset Phase
            time_value <= (others => '0');
            push_event <= '0';
			gps_x_data_delayed <= to_signed(0, gps_x_data_delayed'length);
			gps_x_push_delayed <= '0';
			num_satellites_data_delayed <= to_signed(0, num_satellites_data_delayed'length);
			num_satellites_push_delayed <= '0';
			imu_acc_x_data_delayed <= to_signed(0, imu_acc_x_data_delayed'length);
			imu_acc_x_push_delayed <= '0';
        elsif (rising_edge(clk)) then
            -- Logic Phase: Map Inputs to Internal Signals to Receive a Delay
            if (push_event_in = '1') then
                time_value <= time_in;
                push_event <= push_event_in;
				gps_x_data_delayed <= gps_x_data_in;
				gps_x_push_delayed <= gps_x_push_in;
				num_satellites_data_delayed <= num_satellites_data_in;
				num_satellites_push_delayed <= num_satellites_push_in;
				imu_acc_x_data_delayed <= imu_acc_x_data_in;
				imu_acc_x_push_delayed <= imu_acc_x_push_in;
            else
                time_value <= (others => '0');
                push_event <= '0';
				gps_x_data_delayed <= to_signed(0, gps_x_data_delayed'length);
				gps_x_push_delayed <= '0';
				num_satellites_data_delayed <= to_signed(0, num_satellites_data_delayed'length);
				num_satellites_push_delayed <= '0';
				imu_acc_x_data_delayed <= to_signed(0, imu_acc_x_data_delayed'length);
				imu_acc_x_push_delayed <= '0';
            end if;
        end if;
    end process;

    time_out <= time_value;
    push_event_out <= push_event;
	gps_x_data_out <= gps_x_data_delayed;
	gps_x_push_out <= gps_x_push_delayed;
	num_satellites_data_out <= num_satellites_data_delayed;
	num_satellites_push_out <= num_satellites_push_delayed;
	imu_acc_x_data_out <= imu_acc_x_data_delayed;
	imu_acc_x_push_out <= imu_acc_x_push_delayed;

end behavioral;
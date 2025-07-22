library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity event_delay is
    port(
        clk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		acceleration_x_data_in : in signed(63 downto 0);
		acceleration_x_push_in : in std_logic;
		gps_sats_data_in : in unsigned(63 downto 0);
		gps_sats_push_in : in std_logic;
		lat_gps_data_in : in signed(63 downto 0);
		lat_gps_push_in : in std_logic;
        push_event_in : std_logic;
        time_out : out unsigned(63 downto 0);
		acceleration_x_data_out : out signed(63 downto 0);
		acceleration_x_push_out : out std_logic;
		gps_sats_data_out : out unsigned(63 downto 0);
		gps_sats_push_out : out std_logic;
		lat_gps_data_out : out signed(63 downto 0);
		lat_gps_push_out : out std_logic;
        push_event_out : out std_logic
    );
end event_delay;

--* Input Streams and their Types in the Specification:
--* - acceleration_x : Int64 *--
--* - gps_sats : UInt64 *--
--* - lat_gps : Int64 *--

architecture behavioral of event_delay is

    -- Internal Signal Declarations
    signal time_value : unsigned(63 downto 0);
    signal push_event : std_logic;
	signal acceleration_x_data_delayed : signed(63 downto 0);
	signal acceleration_x_push_delayed : std_logic;
	signal gps_sats_data_delayed : unsigned(63 downto 0);
	signal gps_sats_push_delayed : std_logic;
	signal lat_gps_data_delayed : signed(63 downto 0);
	signal lat_gps_push_delayed : std_logic;

    begin

    process(clk, rst) begin
        if (rst = '1') then
            -- Reset Phase
            time_value <= (others => '0');
            push_event <= '0';
			acceleration_x_data_delayed <= to_signed(0, acceleration_x_data_delayed'length);
			acceleration_x_push_delayed <= '0';
			gps_sats_data_delayed <= to_unsigned(0, gps_sats_data_delayed'length);
			gps_sats_push_delayed <= '0';
			lat_gps_data_delayed <= to_signed(0, lat_gps_data_delayed'length);
			lat_gps_push_delayed <= '0';
        elsif (rising_edge(clk)) then
            -- Logic Phase: Map Inputs to Internal Signals to Receive a Delay
            if (push_event_in = '1') then
                time_value <= time_in;
                push_event <= push_event_in;
				acceleration_x_data_delayed <= acceleration_x_data_in;
				acceleration_x_push_delayed <= acceleration_x_push_in;
				gps_sats_data_delayed <= gps_sats_data_in;
				gps_sats_push_delayed <= gps_sats_push_in;
				lat_gps_data_delayed <= lat_gps_data_in;
				lat_gps_push_delayed <= lat_gps_push_in;
            else
                time_value <= (others => '0');
                push_event <= '0';
				acceleration_x_data_delayed <= to_signed(0, acceleration_x_data_delayed'length);
				acceleration_x_push_delayed <= '0';
				gps_sats_data_delayed <= to_unsigned(0, gps_sats_data_delayed'length);
				gps_sats_push_delayed <= '0';
				lat_gps_data_delayed <= to_signed(0, lat_gps_data_delayed'length);
				lat_gps_push_delayed <= '0';
            end if;
        end if;
    end process;

    time_out <= time_value;
    push_event_out <= push_event;
	acceleration_x_data_out <= acceleration_x_data_delayed;
	acceleration_x_push_out <= acceleration_x_push_delayed;
	gps_sats_data_out <= gps_sats_data_delayed;
	gps_sats_push_out <= gps_sats_push_delayed;
	lat_gps_data_out <= lat_gps_data_delayed;
	lat_gps_push_out <= lat_gps_push_delayed;

end behavioral;
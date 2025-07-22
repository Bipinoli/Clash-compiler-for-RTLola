library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity extInterface is
    port(
        clk, rst: in std_logic;
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
end extInterface;

--* Input Streams and their Types in the Specification: 
--* - input acceleration_x : Int64
--* - input gps_sats : UInt64
--* - input lat_gps : Int64

architecture behavioral of extInterface is

    -- Internal Signal Declarations
    signal time_converted : unsigned(63 downto 0);
	signal acceleration_x_parsed : signed(63 downto 0);
	signal acceleration_x_push_delayed : std_logic;
	signal gps_sats_parsed : unsigned(63 downto 0);
	signal gps_sats_push_delayed : std_logic;
	signal lat_gps_parsed : signed(63 downto 0);
	signal lat_gps_push_delayed : std_logic;

begin

    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            time_converted <= to_unsigned(0, time_converted'length);
			acceleration_x_parsed <= (others => '0');
			acceleration_x_push_delayed <= '0';
			gps_sats_parsed <= (others => '0');
			gps_sats_push_delayed <= '0';
			lat_gps_parsed <= (others => '0');
			lat_gps_push_delayed <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase: Convert Input in Numeric Types
            time_converted <= unsigned(time_in);
			--* input acceleration_x : Int64
			acceleration_x_parsed <= signed(acceleration_x_data_in);
			acceleration_x_push_delayed <= acceleration_x_push_in;
			--* input gps_sats : UInt64
			gps_sats_parsed <= unsigned(gps_sats_data_in);
			gps_sats_push_delayed <= gps_sats_push_in;
			--* input lat_gps : Int64
			lat_gps_parsed <= signed(lat_gps_data_in);
			lat_gps_push_delayed <= lat_gps_push_in;
        end if;
    end process;

    time_out <= time_converted;
	acceleration_x_data_out <= acceleration_x_parsed;
	acceleration_x_push_out <= acceleration_x_push_delayed;
	gps_sats_data_out <= gps_sats_parsed;
	gps_sats_push_out <= gps_sats_push_delayed;
	lat_gps_data_out <= lat_gps_parsed;
	lat_gps_push_out <= lat_gps_push_delayed;

end behavioral;
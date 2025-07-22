library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity input_pre_processing is
    port(
        clk, rst: in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		acceleration_x_in : in std_logic_vector(63 downto 0);
		acceleration_x_in_new_input : in std_logic;
		gps_sats_in : in std_logic_vector(63 downto 0);
		gps_sats_in_new_input : in std_logic;
		lat_gps_in : in std_logic_vector(63 downto 0);
		lat_gps_in_new_input : in std_logic;
        new_input_in : in std_logic;
        time_out : out std_logic_vector(63 downto 0);
		acceleration_x_out : out std_logic_vector(63 downto 0);
		acceleration_x_out_new_input : out std_logic;
		gps_sats_out : out std_logic_vector(63 downto 0);
		gps_sats_out_new_input : out std_logic;
		lat_gps_out : out std_logic_vector(63 downto 0);
		lat_gps_out_new_input : out std_logic;
        new_input_out : out std_logic
    );
end input_pre_processing;

architecture behavioral of input_pre_processing is

    signal new_input : std_logic;
    signal time_reg : std_logic_vector(63 downto 0);
	signal acceleration_x_reg : std_logic_vector(63 downto 0);
	signal acceleration_x_reg_new_input : std_logic;
	signal gps_sats_reg : std_logic_vector(63 downto 0);
	signal gps_sats_reg_new_input : std_logic;
	signal lat_gps_reg : std_logic_vector(63 downto 0);
	signal lat_gps_reg_new_input : std_logic;

begin

    process(clk, rst) begin
        if (rst = '1') then
            -- set default
            new_input <= '0';
            time_reg <= (others => '0');
			acceleration_x_reg <= (others => '0');
			acceleration_x_reg_new_input <= '0';
			gps_sats_reg <= (others => '0');
			gps_sats_reg_new_input <= '0';
			lat_gps_reg <= (others => '0');
			lat_gps_reg_new_input <= '0';
        elsif (rising_edge(clk)) then
            new_input <= new_input_in;
            time_reg <= time_in;
				acceleration_x_reg <= acceleration_x_in;
				acceleration_x_reg_new_input <= acceleration_x_in_new_input;
				gps_sats_reg <= gps_sats_in;
				gps_sats_reg_new_input <= gps_sats_in_new_input;
				lat_gps_reg <= lat_gps_in;
				lat_gps_reg_new_input <= lat_gps_in_new_input;
        end if;
    end process;

    new_input_out <= new_input;
    time_out <= time_reg;
	acceleration_x_out <= acceleration_x_reg;
	acceleration_x_out_new_input <= acceleration_x_reg_new_input;
	gps_sats_out <= gps_sats_reg;
	gps_sats_out_new_input <= gps_sats_reg_new_input;
	lat_gps_out <= lat_gps_reg;
	lat_gps_out_new_input <= lat_gps_reg_new_input;

end behavioral;
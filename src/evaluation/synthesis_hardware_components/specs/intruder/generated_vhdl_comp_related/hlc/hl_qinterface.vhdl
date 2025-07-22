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
		lat_data_in : in signed(63 downto 0);
		lat_en_in : in std_logic;
		lon_data_in : in signed(63 downto 0);
		lon_en_in : in std_logic;
		lat_data_out : out signed(63 downto 0);
		lat_en_out : out std_logic;
		lon_data_out : out signed(63 downto 0);
		lon_en_out : out std_logic;
		distance_en_out : out std_logic;
		closer_en_out : out std_logic;
		trigger_closer_en_out : out std_logic;
		is_good_en_out : out std_logic;
        time_out : out unsigned(63 downto 0);
        push_out : out std_logic
    );
end hlQInterface;

--* Streams and their Activation Conditions:
--* Input Streams 
--* - lat @ { lat }
--* - lon @ { lon }
--* Event-based Output Streams 
--* - distance @ { lat, lon }
--* - closer @ { lat, lon }
--* - trigger_closer @ { lat, lon }
--* Periodic Output Streams 
--* - is_good @ 1000Hz
--* Resulting Deadline Array
--* || is_good ||

architecture behavioral of hlQInterface is

    -- Internal Signal Declarations
    signal clock_state_machine : integer;
    signal push_to_queue : std_logic;
	signal lat_data_push : signed(63 downto 0);
	signal lat_en_push: std_logic;
	signal lon_data_push : signed(63 downto 0);
	signal lon_en_push: std_logic;
	signal distance_en_push : std_logic;
	signal closer_en_push : std_logic;
	signal trigger_closer_en_push : std_logic;
	signal is_good_en_push : std_logic;
	signal is_good_en_array : bit_array(0 downto 0);
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
			lat_data_push <= (others => '0');
			lat_en_push <= '0';
			lon_data_push <= (others => '0');
			lon_en_push <= '0';
			distance_en_push <= '0';
			closer_en_push <= '0';
			trigger_closer_en_push <= '0';
			is_good_en_push <= '0';
            -- Initialize Deadline Arrays
            --* Deadline Array
            --* || is_good ||
			is_good_en_array(0) <= '1';
        elsif (rising_edge(clk)) then
            clock_state_machine <= (clock_state_machine + 1) mod 4;
            if push_deadline = '1' and clock_state_machine = 0 then
                -- Deadline Handling
                push_to_queue <= '1';
                last_deadline_id <= (last_deadline_id + 1) mod 1;
                time_to_queue <= time_for_deadline;
				--* lat @ { lat }
				lat_en_push <= '0';
				--* lon @ { lon }
				lon_en_push <= '0';
				--* distance @ { lat, lon }
				distance_en_push <= '0';
				--* closer @ { lat, lon }
				closer_en_push <= '0';
				--* trigger_closer @ { lat, lon }
				trigger_closer_en_push <= '0';
				--* is_good @ 1000Hz
				is_good_en_push <= is_good_en_array(last_deadline_id);
            elsif push_event = '1' and clock_state_machine = 2 then
                -- Event Handling
                push_to_queue <= '1';
                time_to_queue <= time_for_event;
				--* lat @ { lat }
				lat_data_push <= lat_data_in;
				lat_en_push <= lat_en_in;
				--* lon @ { lon }
				lon_data_push <= lon_data_in;
				lon_en_push <= lon_en_in;
				--* distance @ { lat, lon }
				distance_en_push <= '1' and lat_en_in and lon_en_in;
				--* closer @ { lat, lon }
				closer_en_push <= '1' and lat_en_in and lon_en_in;
				--* trigger_closer @ { lat, lon }
				trigger_closer_en_push <= '1' and lat_en_in and lon_en_in;
				--* is_good @ 1000Hz
				is_good_en_push <= '0';
            else
                -- Enable No Stream
                push_to_queue <= '0';
				lat_en_push <= '0';
				lon_en_push <= '0';
				distance_en_push <= '0';
				closer_en_push <= '0';
				trigger_closer_en_push <= '0';
				is_good_en_push <= '0';
            end if;
        end if;
    end process;

    push_out <= push_to_queue;
    time_out <= time_to_queue;
	lat_data_out <= lat_data_push;
	lat_en_out <= lat_en_push;
	lon_data_out <= lon_data_push;
	lon_en_out <= lon_en_push;
	distance_en_out <= distance_en_push;
	closer_en_out <= closer_en_push;
	trigger_closer_en_out <= trigger_closer_en_push;
	is_good_en_out <= is_good_en_push;

end behavioral;

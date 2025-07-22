library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity queue is
    port (
        clk, rst : in std_logic;
        push : in std_logic;
        time_data_in : in unsigned(63 downto 0);
		lat_data_in : in signed(63 downto 0);
		lat_en_in : in std_logic;
		lon_data_in : in signed(63 downto 0);
		lon_en_in : in std_logic;
		distance_en_in : in std_logic;
		closer_en_in : in std_logic;
		trigger_closer_en_in : in std_logic;
		is_good_en_in : in std_logic;
        full : out std_logic;
        pop : in std_logic;
        time_data_out : out unsigned(63 downto 0);
		lat_data_out : out signed(63 downto 0);
		lat_en_out : out std_logic;
		lon_data_out : out signed(63 downto 0);
		lon_en_out : out std_logic;
		distance_en_out : out std_logic;
		closer_en_out : out std_logic;
		trigger_closer_en_out : out std_logic;
		is_good_en_out : out std_logic;
        available : out std_logic
    );
end queue;

architecture behavioral of queue is

    signal is_full : std_logic;
    signal time_data_reg : unsigned64_array(1 downto 0);
    signal time_data : unsigned(63 downto 0);
	signal lat_data_reg : signed64_array(1 downto 0);
	signal lat_en_reg : bit_array(1 downto 0);
	signal lat_data : signed(63 downto 0);
	signal lat_en: std_logic;
	signal lon_data_reg : signed64_array(1 downto 0);
	signal lon_en_reg : bit_array(1 downto 0);
	signal lon_data : signed(63 downto 0);
	signal lon_en: std_logic;
	signal distance_en_reg : bit_array(1 downto 0);
	signal distance_en : std_logic;
	signal closer_en_reg : bit_array(1 downto 0);
	signal closer_en : std_logic;
	signal trigger_closer_en_reg : bit_array(1 downto 0);
	signal trigger_closer_en : std_logic;
	signal is_good_en_reg : bit_array(1 downto 0);
	signal is_good_en : std_logic;
    signal av : std_logic;
    signal size : integer;
    signal clk_reg : std_logic;
    signal push_done : std_logic;
    signal pop_done : std_logic;

begin

    process(rst, clk) begin
        if (rst = '1') then
            is_full <= '0';
            time_data_reg(time_data_reg'high downto 0) <= (others => (others => '0'));
            time_data <= (others => '0');
			lat_data_reg(lat_data_reg'high downto 0) <= (others => (others => '0'));
			lat_en_reg(lat_en_reg'high downto 0) <= (others => '0');
			lat_data <= (others => '0');
			lat_en <= '0';
			lon_data_reg(lon_data_reg'high downto 0) <= (others => (others => '0'));
			lon_en_reg(lon_en_reg'high downto 0) <= (others => '0');
			lon_data <= (others => '0');
			lon_en <= '0';
			distance_en_reg <= (others => '0');
			distance_en <= '0';
			closer_en_reg <= (others => '0');
			closer_en <= '0';
			trigger_closer_en_reg <= (others => '0');
			trigger_closer_en <= '0';
			is_good_en_reg <= (others => '0');
			is_good_en <= '0';
            size <= 0;
            av <= '0';
            clk_reg <= '0';
            push_done <= '0';
            pop_done <= '0';
        elsif rising_edge(clk) then
            clk_reg <= not clk_reg;
            if clk_reg = '0' then
                if push = '1' and push_done = '0' and pop = '1' and pop_done = '0' and size > 0 and size < 2 then
                    -- perform push and pop
                    time_data_reg <= time_data_reg(time_data_reg'high - 1 downto 0) & time_data_in;
					lat_data_reg <= lat_data_reg(lat_data_reg'high - 1 downto 0) & lat_data_in;
					lat_en_reg <= lat_en_reg(lat_en_reg'high - 1 downto 0) & lat_en_in;
					lon_data_reg <= lon_data_reg(lon_data_reg'high - 1 downto 0) & lon_data_in;
					lon_en_reg <= lon_en_reg(lon_en_reg'high - 1 downto 0) & lon_en_in;
					distance_en_reg <= distance_en_reg(distance_en_reg'high - 1 downto 0) & distance_en_in;
					closer_en_reg <= closer_en_reg(closer_en_reg'high - 1 downto 0) & closer_en_in;
					trigger_closer_en_reg <= trigger_closer_en_reg(trigger_closer_en_reg'high - 1 downto 0) & trigger_closer_en_in;
					is_good_en_reg <= is_good_en_reg(is_good_en_reg'high - 1 downto 0) & is_good_en_in;

                    time_data <= time_data_reg(size-1);
					lat_data <= lat_data_reg(size-1);
					lat_en <= lat_en_reg(size-1);
					lon_data <= lon_data_reg(size-1);
					lon_en <= lon_en_reg(size-1);
					distance_en <= distance_en_reg(size-1);
					closer_en <= closer_en_reg(size-1);
					trigger_closer_en <= trigger_closer_en_reg(size-1);
					is_good_en <= is_good_en_reg(size-1);
                    push_done <= '1';
                    pop_done <= '1';
                elsif push = '1' and push_done = '0' and size < 2 then
                    -- perform push
                    time_data_reg <= time_data_reg(time_data_reg'high - 1 downto 0) & time_data_in;
					lat_data_reg <= lat_data_reg(lat_data_reg'high - 1 downto 0) & lat_data_in;
					lat_en_reg <= lat_en_reg(lat_en_reg'high - 1 downto 0) & lat_en_in;
					lon_data_reg <= lon_data_reg(lon_data_reg'high - 1 downto 0) & lon_data_in;
					lon_en_reg <= lon_en_reg(lon_en_reg'high - 1 downto 0) & lon_en_in;
					distance_en_reg <= distance_en_reg(distance_en_reg'high - 1 downto 0) & distance_en_in;
					closer_en_reg <= closer_en_reg(closer_en_reg'high - 1 downto 0) & closer_en_in;
					trigger_closer_en_reg <= trigger_closer_en_reg(trigger_closer_en_reg'high - 1 downto 0) & trigger_closer_en_in;
					is_good_en_reg <= is_good_en_reg(is_good_en_reg'high - 1 downto 0) & is_good_en_in;

                    size <= size + 1;
                    av <= '1';
                    is_full <= to_std_logic(size = 1);
                    push_done <= '1';
                elsif pop = '1' and pop_done = '0' and size > 0 then
                    --perform pop
                    time_data <= time_data_reg(size-1);
					lat_data <= lat_data_reg(size-1);
					lat_en <= lat_en_reg(size-1);
					lon_data <= lon_data_reg(size-1);
					lon_en <= lon_en_reg(size-1);
					distance_en <= distance_en_reg(size-1);
					closer_en <= closer_en_reg(size-1);
					trigger_closer_en <= trigger_closer_en_reg(size-1);
					is_good_en <= is_good_en_reg(size-1);

                    size <= size - 1;
                    is_full <= '0';
                    av <= to_std_logic(size > 1);
                    pop_done <= '1';
                end if;
            else
                if push = '0' then
                    push_done <= '0';
                end if;
                if pop = '0' then 
                    pop_done <= '0';
                end if;
            end if;
        end if;
    end process;

    full <= is_full;
    time_data_out <= time_data;
	lat_data_out <= lat_data;
	lat_en_out <= lat_en;
	lon_data_out <= lon_data;
	lon_en_out <= lon_en;
	distance_en_out <= distance_en;
	closer_en_out <= closer_en;
	trigger_closer_en_out <= trigger_closer_en;
	is_good_en_out <= is_good_en;
    available <= av;

end behavioral;

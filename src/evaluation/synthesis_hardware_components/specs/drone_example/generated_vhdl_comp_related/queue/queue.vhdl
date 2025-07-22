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
		acceleration_x_data_in : in signed(63 downto 0);
		acceleration_x_en_in : in std_logic;
		gps_sats_data_in : in unsigned(63 downto 0);
		gps_sats_en_in : in std_logic;
		lat_gps_data_in : in signed(63 downto 0);
		lat_gps_en_in : in std_logic;
		acceleration_x_periodic_en_in : in std_logic;
		acceleration_x_rising_en_in : in std_logic;
		acceleration_x_sinking_en_in : in std_logic;
		acceleration_x_direction_change_en_in : in std_logic;
		acceleration_x_changes_en_in : in std_logic;
		trigger_acc_en_in : in std_logic;
		gps_missed_beat_en_in : in std_logic;
		gps_medium_loss_en_in : in std_logic;
		gps_high_loss_en_in : in std_logic;
		gps_very_high_loss_en_in : in std_logic;
		trigger_gps_sats_en_in : in std_logic;
        full : out std_logic;
        pop : in std_logic;
        time_data_out : out unsigned(63 downto 0);
		acceleration_x_data_out : out signed(63 downto 0);
		acceleration_x_en_out : out std_logic;
		gps_sats_data_out : out unsigned(63 downto 0);
		gps_sats_en_out : out std_logic;
		lat_gps_data_out : out signed(63 downto 0);
		lat_gps_en_out : out std_logic;
		acceleration_x_periodic_en_out : out std_logic;
		acceleration_x_rising_en_out : out std_logic;
		acceleration_x_sinking_en_out : out std_logic;
		acceleration_x_direction_change_en_out : out std_logic;
		acceleration_x_changes_en_out : out std_logic;
		trigger_acc_en_out : out std_logic;
		gps_missed_beat_en_out : out std_logic;
		gps_medium_loss_en_out : out std_logic;
		gps_high_loss_en_out : out std_logic;
		gps_very_high_loss_en_out : out std_logic;
		trigger_gps_sats_en_out : out std_logic;
        available : out std_logic
    );
end queue;

architecture behavioral of queue is

    signal is_full : std_logic;
    signal time_data_reg : unsigned64_array(1 downto 0);
    signal time_data : unsigned(63 downto 0);
	signal acceleration_x_data_reg : signed64_array(1 downto 0);
	signal acceleration_x_en_reg : bit_array(1 downto 0);
	signal acceleration_x_data : signed(63 downto 0);
	signal acceleration_x_en: std_logic;
	signal gps_sats_data_reg : unsigned64_array(1 downto 0);
	signal gps_sats_en_reg : bit_array(1 downto 0);
	signal gps_sats_data : unsigned(63 downto 0);
	signal gps_sats_en: std_logic;
	signal lat_gps_data_reg : signed64_array(1 downto 0);
	signal lat_gps_en_reg : bit_array(1 downto 0);
	signal lat_gps_data : signed(63 downto 0);
	signal lat_gps_en: std_logic;
	signal acceleration_x_periodic_en_reg : bit_array(1 downto 0);
	signal acceleration_x_periodic_en : std_logic;
	signal acceleration_x_rising_en_reg : bit_array(1 downto 0);
	signal acceleration_x_rising_en : std_logic;
	signal acceleration_x_sinking_en_reg : bit_array(1 downto 0);
	signal acceleration_x_sinking_en : std_logic;
	signal acceleration_x_direction_change_en_reg : bit_array(1 downto 0);
	signal acceleration_x_direction_change_en : std_logic;
	signal acceleration_x_changes_en_reg : bit_array(1 downto 0);
	signal acceleration_x_changes_en : std_logic;
	signal trigger_acc_en_reg : bit_array(1 downto 0);
	signal trigger_acc_en : std_logic;
	signal gps_missed_beat_en_reg : bit_array(1 downto 0);
	signal gps_missed_beat_en : std_logic;
	signal gps_medium_loss_en_reg : bit_array(1 downto 0);
	signal gps_medium_loss_en : std_logic;
	signal gps_high_loss_en_reg : bit_array(1 downto 0);
	signal gps_high_loss_en : std_logic;
	signal gps_very_high_loss_en_reg : bit_array(1 downto 0);
	signal gps_very_high_loss_en : std_logic;
	signal trigger_gps_sats_en_reg : bit_array(1 downto 0);
	signal trigger_gps_sats_en : std_logic;
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
			acceleration_x_data_reg(acceleration_x_data_reg'high downto 0) <= (others => (others => '0'));
			acceleration_x_en_reg(acceleration_x_en_reg'high downto 0) <= (others => '0');
			acceleration_x_data <= (others => '0');
			acceleration_x_en <= '0';
			gps_sats_data_reg(gps_sats_data_reg'high downto 0) <= (others => (others => '0'));
			gps_sats_en_reg(gps_sats_en_reg'high downto 0) <= (others => '0');
			gps_sats_data <= (others => '0');
			gps_sats_en <= '0';
			lat_gps_data_reg(lat_gps_data_reg'high downto 0) <= (others => (others => '0'));
			lat_gps_en_reg(lat_gps_en_reg'high downto 0) <= (others => '0');
			lat_gps_data <= (others => '0');
			lat_gps_en <= '0';
			acceleration_x_periodic_en_reg <= (others => '0');
			acceleration_x_periodic_en <= '0';
			acceleration_x_rising_en_reg <= (others => '0');
			acceleration_x_rising_en <= '0';
			acceleration_x_sinking_en_reg <= (others => '0');
			acceleration_x_sinking_en <= '0';
			acceleration_x_direction_change_en_reg <= (others => '0');
			acceleration_x_direction_change_en <= '0';
			acceleration_x_changes_en_reg <= (others => '0');
			acceleration_x_changes_en <= '0';
			trigger_acc_en_reg <= (others => '0');
			trigger_acc_en <= '0';
			gps_missed_beat_en_reg <= (others => '0');
			gps_missed_beat_en <= '0';
			gps_medium_loss_en_reg <= (others => '0');
			gps_medium_loss_en <= '0';
			gps_high_loss_en_reg <= (others => '0');
			gps_high_loss_en <= '0';
			gps_very_high_loss_en_reg <= (others => '0');
			gps_very_high_loss_en <= '0';
			trigger_gps_sats_en_reg <= (others => '0');
			trigger_gps_sats_en <= '0';
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
					acceleration_x_data_reg <= acceleration_x_data_reg(acceleration_x_data_reg'high - 1 downto 0) & acceleration_x_data_in;
					acceleration_x_en_reg <= acceleration_x_en_reg(acceleration_x_en_reg'high - 1 downto 0) & acceleration_x_en_in;
					gps_sats_data_reg <= gps_sats_data_reg(gps_sats_data_reg'high - 1 downto 0) & gps_sats_data_in;
					gps_sats_en_reg <= gps_sats_en_reg(gps_sats_en_reg'high - 1 downto 0) & gps_sats_en_in;
					lat_gps_data_reg <= lat_gps_data_reg(lat_gps_data_reg'high - 1 downto 0) & lat_gps_data_in;
					lat_gps_en_reg <= lat_gps_en_reg(lat_gps_en_reg'high - 1 downto 0) & lat_gps_en_in;
					acceleration_x_periodic_en_reg <= acceleration_x_periodic_en_reg(acceleration_x_periodic_en_reg'high - 1 downto 0) & acceleration_x_periodic_en_in;
					acceleration_x_rising_en_reg <= acceleration_x_rising_en_reg(acceleration_x_rising_en_reg'high - 1 downto 0) & acceleration_x_rising_en_in;
					acceleration_x_sinking_en_reg <= acceleration_x_sinking_en_reg(acceleration_x_sinking_en_reg'high - 1 downto 0) & acceleration_x_sinking_en_in;
					acceleration_x_direction_change_en_reg <= acceleration_x_direction_change_en_reg(acceleration_x_direction_change_en_reg'high - 1 downto 0) & acceleration_x_direction_change_en_in;
					acceleration_x_changes_en_reg <= acceleration_x_changes_en_reg(acceleration_x_changes_en_reg'high - 1 downto 0) & acceleration_x_changes_en_in;
					trigger_acc_en_reg <= trigger_acc_en_reg(trigger_acc_en_reg'high - 1 downto 0) & trigger_acc_en_in;
					gps_missed_beat_en_reg <= gps_missed_beat_en_reg(gps_missed_beat_en_reg'high - 1 downto 0) & gps_missed_beat_en_in;
					gps_medium_loss_en_reg <= gps_medium_loss_en_reg(gps_medium_loss_en_reg'high - 1 downto 0) & gps_medium_loss_en_in;
					gps_high_loss_en_reg <= gps_high_loss_en_reg(gps_high_loss_en_reg'high - 1 downto 0) & gps_high_loss_en_in;
					gps_very_high_loss_en_reg <= gps_very_high_loss_en_reg(gps_very_high_loss_en_reg'high - 1 downto 0) & gps_very_high_loss_en_in;
					trigger_gps_sats_en_reg <= trigger_gps_sats_en_reg(trigger_gps_sats_en_reg'high - 1 downto 0) & trigger_gps_sats_en_in;

                    time_data <= time_data_reg(size-1);
					acceleration_x_data <= acceleration_x_data_reg(size-1);
					acceleration_x_en <= acceleration_x_en_reg(size-1);
					gps_sats_data <= gps_sats_data_reg(size-1);
					gps_sats_en <= gps_sats_en_reg(size-1);
					lat_gps_data <= lat_gps_data_reg(size-1);
					lat_gps_en <= lat_gps_en_reg(size-1);
					acceleration_x_periodic_en <= acceleration_x_periodic_en_reg(size-1);
					acceleration_x_rising_en <= acceleration_x_rising_en_reg(size-1);
					acceleration_x_sinking_en <= acceleration_x_sinking_en_reg(size-1);
					acceleration_x_direction_change_en <= acceleration_x_direction_change_en_reg(size-1);
					acceleration_x_changes_en <= acceleration_x_changes_en_reg(size-1);
					trigger_acc_en <= trigger_acc_en_reg(size-1);
					gps_missed_beat_en <= gps_missed_beat_en_reg(size-1);
					gps_medium_loss_en <= gps_medium_loss_en_reg(size-1);
					gps_high_loss_en <= gps_high_loss_en_reg(size-1);
					gps_very_high_loss_en <= gps_very_high_loss_en_reg(size-1);
					trigger_gps_sats_en <= trigger_gps_sats_en_reg(size-1);
                    push_done <= '1';
                    pop_done <= '1';
                elsif push = '1' and push_done = '0' and size < 2 then
                    -- perform push
                    time_data_reg <= time_data_reg(time_data_reg'high - 1 downto 0) & time_data_in;
					acceleration_x_data_reg <= acceleration_x_data_reg(acceleration_x_data_reg'high - 1 downto 0) & acceleration_x_data_in;
					acceleration_x_en_reg <= acceleration_x_en_reg(acceleration_x_en_reg'high - 1 downto 0) & acceleration_x_en_in;
					gps_sats_data_reg <= gps_sats_data_reg(gps_sats_data_reg'high - 1 downto 0) & gps_sats_data_in;
					gps_sats_en_reg <= gps_sats_en_reg(gps_sats_en_reg'high - 1 downto 0) & gps_sats_en_in;
					lat_gps_data_reg <= lat_gps_data_reg(lat_gps_data_reg'high - 1 downto 0) & lat_gps_data_in;
					lat_gps_en_reg <= lat_gps_en_reg(lat_gps_en_reg'high - 1 downto 0) & lat_gps_en_in;
					acceleration_x_periodic_en_reg <= acceleration_x_periodic_en_reg(acceleration_x_periodic_en_reg'high - 1 downto 0) & acceleration_x_periodic_en_in;
					acceleration_x_rising_en_reg <= acceleration_x_rising_en_reg(acceleration_x_rising_en_reg'high - 1 downto 0) & acceleration_x_rising_en_in;
					acceleration_x_sinking_en_reg <= acceleration_x_sinking_en_reg(acceleration_x_sinking_en_reg'high - 1 downto 0) & acceleration_x_sinking_en_in;
					acceleration_x_direction_change_en_reg <= acceleration_x_direction_change_en_reg(acceleration_x_direction_change_en_reg'high - 1 downto 0) & acceleration_x_direction_change_en_in;
					acceleration_x_changes_en_reg <= acceleration_x_changes_en_reg(acceleration_x_changes_en_reg'high - 1 downto 0) & acceleration_x_changes_en_in;
					trigger_acc_en_reg <= trigger_acc_en_reg(trigger_acc_en_reg'high - 1 downto 0) & trigger_acc_en_in;
					gps_missed_beat_en_reg <= gps_missed_beat_en_reg(gps_missed_beat_en_reg'high - 1 downto 0) & gps_missed_beat_en_in;
					gps_medium_loss_en_reg <= gps_medium_loss_en_reg(gps_medium_loss_en_reg'high - 1 downto 0) & gps_medium_loss_en_in;
					gps_high_loss_en_reg <= gps_high_loss_en_reg(gps_high_loss_en_reg'high - 1 downto 0) & gps_high_loss_en_in;
					gps_very_high_loss_en_reg <= gps_very_high_loss_en_reg(gps_very_high_loss_en_reg'high - 1 downto 0) & gps_very_high_loss_en_in;
					trigger_gps_sats_en_reg <= trigger_gps_sats_en_reg(trigger_gps_sats_en_reg'high - 1 downto 0) & trigger_gps_sats_en_in;

                    size <= size + 1;
                    av <= '1';
                    is_full <= to_std_logic(size = 1);
                    push_done <= '1';
                elsif pop = '1' and pop_done = '0' and size > 0 then
                    --perform pop
                    time_data <= time_data_reg(size-1);
					acceleration_x_data <= acceleration_x_data_reg(size-1);
					acceleration_x_en <= acceleration_x_en_reg(size-1);
					gps_sats_data <= gps_sats_data_reg(size-1);
					gps_sats_en <= gps_sats_en_reg(size-1);
					lat_gps_data <= lat_gps_data_reg(size-1);
					lat_gps_en <= lat_gps_en_reg(size-1);
					acceleration_x_periodic_en <= acceleration_x_periodic_en_reg(size-1);
					acceleration_x_rising_en <= acceleration_x_rising_en_reg(size-1);
					acceleration_x_sinking_en <= acceleration_x_sinking_en_reg(size-1);
					acceleration_x_direction_change_en <= acceleration_x_direction_change_en_reg(size-1);
					acceleration_x_changes_en <= acceleration_x_changes_en_reg(size-1);
					trigger_acc_en <= trigger_acc_en_reg(size-1);
					gps_missed_beat_en <= gps_missed_beat_en_reg(size-1);
					gps_medium_loss_en <= gps_medium_loss_en_reg(size-1);
					gps_high_loss_en <= gps_high_loss_en_reg(size-1);
					gps_very_high_loss_en <= gps_very_high_loss_en_reg(size-1);
					trigger_gps_sats_en <= trigger_gps_sats_en_reg(size-1);

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
	acceleration_x_data_out <= acceleration_x_data;
	acceleration_x_en_out <= acceleration_x_en;
	gps_sats_data_out <= gps_sats_data;
	gps_sats_en_out <= gps_sats_en;
	lat_gps_data_out <= lat_gps_data;
	lat_gps_en_out <= lat_gps_en;
	acceleration_x_periodic_en_out <= acceleration_x_periodic_en;
	acceleration_x_rising_en_out <= acceleration_x_rising_en;
	acceleration_x_sinking_en_out <= acceleration_x_sinking_en;
	acceleration_x_direction_change_en_out <= acceleration_x_direction_change_en;
	acceleration_x_changes_en_out <= acceleration_x_changes_en;
	trigger_acc_en_out <= trigger_acc_en;
	gps_missed_beat_en_out <= gps_missed_beat_en;
	gps_medium_loss_en_out <= gps_medium_loss_en;
	gps_high_loss_en_out <= gps_high_loss_en;
	gps_very_high_loss_en_out <= gps_very_high_loss_en;
	trigger_gps_sats_en_out <= trigger_gps_sats_en;
    available <= av;

end behavioral;

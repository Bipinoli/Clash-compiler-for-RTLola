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
		gps_x_data_in : in signed(63 downto 0);
		gps_x_en_in : in std_logic;
		num_satellites_data_in : in signed(63 downto 0);
		num_satellites_en_in : in std_logic;
		imu_acc_x_data_in : in signed(63 downto 0);
		imu_acc_x_en_in : in std_logic;
		gps_emitted_enough_en_in : in std_logic;
		few_satellites_en_in : in std_logic;
		is_unreliable_gps_data_en_in : in std_logic;
        full : out std_logic;
        pop : in std_logic;
        time_data_out : out unsigned(63 downto 0);
		gps_x_data_out : out signed(63 downto 0);
		gps_x_en_out : out std_logic;
		num_satellites_data_out : out signed(63 downto 0);
		num_satellites_en_out : out std_logic;
		imu_acc_x_data_out : out signed(63 downto 0);
		imu_acc_x_en_out : out std_logic;
		gps_emitted_enough_en_out : out std_logic;
		few_satellites_en_out : out std_logic;
		is_unreliable_gps_data_en_out : out std_logic;
        available : out std_logic
    );
end queue;

architecture behavioral of queue is

    signal is_full : std_logic;
    signal time_data_reg : unsigned64_array(1 downto 0);
    signal time_data : unsigned(63 downto 0);
	signal gps_x_data_reg : signed64_array(1 downto 0);
	signal gps_x_en_reg : bit_array(1 downto 0);
	signal gps_x_data : signed(63 downto 0);
	signal gps_x_en: std_logic;
	signal num_satellites_data_reg : signed64_array(1 downto 0);
	signal num_satellites_en_reg : bit_array(1 downto 0);
	signal num_satellites_data : signed(63 downto 0);
	signal num_satellites_en: std_logic;
	signal imu_acc_x_data_reg : signed64_array(1 downto 0);
	signal imu_acc_x_en_reg : bit_array(1 downto 0);
	signal imu_acc_x_data : signed(63 downto 0);
	signal imu_acc_x_en: std_logic;
	signal gps_emitted_enough_en_reg : bit_array(1 downto 0);
	signal gps_emitted_enough_en : std_logic;
	signal few_satellites_en_reg : bit_array(1 downto 0);
	signal few_satellites_en : std_logic;
	signal is_unreliable_gps_data_en_reg : bit_array(1 downto 0);
	signal is_unreliable_gps_data_en : std_logic;
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
			gps_x_data_reg(gps_x_data_reg'high downto 0) <= (others => (others => '0'));
			gps_x_en_reg(gps_x_en_reg'high downto 0) <= (others => '0');
			gps_x_data <= (others => '0');
			gps_x_en <= '0';
			num_satellites_data_reg(num_satellites_data_reg'high downto 0) <= (others => (others => '0'));
			num_satellites_en_reg(num_satellites_en_reg'high downto 0) <= (others => '0');
			num_satellites_data <= (others => '0');
			num_satellites_en <= '0';
			imu_acc_x_data_reg(imu_acc_x_data_reg'high downto 0) <= (others => (others => '0'));
			imu_acc_x_en_reg(imu_acc_x_en_reg'high downto 0) <= (others => '0');
			imu_acc_x_data <= (others => '0');
			imu_acc_x_en <= '0';
			gps_emitted_enough_en_reg <= (others => '0');
			gps_emitted_enough_en <= '0';
			few_satellites_en_reg <= (others => '0');
			few_satellites_en <= '0';
			is_unreliable_gps_data_en_reg <= (others => '0');
			is_unreliable_gps_data_en <= '0';
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
					gps_x_data_reg <= gps_x_data_reg(gps_x_data_reg'high - 1 downto 0) & gps_x_data_in;
					gps_x_en_reg <= gps_x_en_reg(gps_x_en_reg'high - 1 downto 0) & gps_x_en_in;
					num_satellites_data_reg <= num_satellites_data_reg(num_satellites_data_reg'high - 1 downto 0) & num_satellites_data_in;
					num_satellites_en_reg <= num_satellites_en_reg(num_satellites_en_reg'high - 1 downto 0) & num_satellites_en_in;
					imu_acc_x_data_reg <= imu_acc_x_data_reg(imu_acc_x_data_reg'high - 1 downto 0) & imu_acc_x_data_in;
					imu_acc_x_en_reg <= imu_acc_x_en_reg(imu_acc_x_en_reg'high - 1 downto 0) & imu_acc_x_en_in;
					gps_emitted_enough_en_reg <= gps_emitted_enough_en_reg(gps_emitted_enough_en_reg'high - 1 downto 0) & gps_emitted_enough_en_in;
					few_satellites_en_reg <= few_satellites_en_reg(few_satellites_en_reg'high - 1 downto 0) & few_satellites_en_in;
					is_unreliable_gps_data_en_reg <= is_unreliable_gps_data_en_reg(is_unreliable_gps_data_en_reg'high - 1 downto 0) & is_unreliable_gps_data_en_in;

                    time_data <= time_data_reg(size-1);
					gps_x_data <= gps_x_data_reg(size-1);
					gps_x_en <= gps_x_en_reg(size-1);
					num_satellites_data <= num_satellites_data_reg(size-1);
					num_satellites_en <= num_satellites_en_reg(size-1);
					imu_acc_x_data <= imu_acc_x_data_reg(size-1);
					imu_acc_x_en <= imu_acc_x_en_reg(size-1);
					gps_emitted_enough_en <= gps_emitted_enough_en_reg(size-1);
					few_satellites_en <= few_satellites_en_reg(size-1);
					is_unreliable_gps_data_en <= is_unreliable_gps_data_en_reg(size-1);
                    push_done <= '1';
                    pop_done <= '1';
                elsif push = '1' and push_done = '0' and size < 2 then
                    -- perform push
                    time_data_reg <= time_data_reg(time_data_reg'high - 1 downto 0) & time_data_in;
					gps_x_data_reg <= gps_x_data_reg(gps_x_data_reg'high - 1 downto 0) & gps_x_data_in;
					gps_x_en_reg <= gps_x_en_reg(gps_x_en_reg'high - 1 downto 0) & gps_x_en_in;
					num_satellites_data_reg <= num_satellites_data_reg(num_satellites_data_reg'high - 1 downto 0) & num_satellites_data_in;
					num_satellites_en_reg <= num_satellites_en_reg(num_satellites_en_reg'high - 1 downto 0) & num_satellites_en_in;
					imu_acc_x_data_reg <= imu_acc_x_data_reg(imu_acc_x_data_reg'high - 1 downto 0) & imu_acc_x_data_in;
					imu_acc_x_en_reg <= imu_acc_x_en_reg(imu_acc_x_en_reg'high - 1 downto 0) & imu_acc_x_en_in;
					gps_emitted_enough_en_reg <= gps_emitted_enough_en_reg(gps_emitted_enough_en_reg'high - 1 downto 0) & gps_emitted_enough_en_in;
					few_satellites_en_reg <= few_satellites_en_reg(few_satellites_en_reg'high - 1 downto 0) & few_satellites_en_in;
					is_unreliable_gps_data_en_reg <= is_unreliable_gps_data_en_reg(is_unreliable_gps_data_en_reg'high - 1 downto 0) & is_unreliable_gps_data_en_in;

                    size <= size + 1;
                    av <= '1';
                    is_full <= to_std_logic(size = 1);
                    push_done <= '1';
                elsif pop = '1' and pop_done = '0' and size > 0 then
                    --perform pop
                    time_data <= time_data_reg(size-1);
					gps_x_data <= gps_x_data_reg(size-1);
					gps_x_en <= gps_x_en_reg(size-1);
					num_satellites_data <= num_satellites_data_reg(size-1);
					num_satellites_en <= num_satellites_en_reg(size-1);
					imu_acc_x_data <= imu_acc_x_data_reg(size-1);
					imu_acc_x_en <= imu_acc_x_en_reg(size-1);
					gps_emitted_enough_en <= gps_emitted_enough_en_reg(size-1);
					few_satellites_en <= few_satellites_en_reg(size-1);
					is_unreliable_gps_data_en <= is_unreliable_gps_data_en_reg(size-1);

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
	gps_x_data_out <= gps_x_data;
	gps_x_en_out <= gps_x_en;
	num_satellites_data_out <= num_satellites_data;
	num_satellites_en_out <= num_satellites_en;
	imu_acc_x_data_out <= imu_acc_x_data;
	imu_acc_x_en_out <= imu_acc_x_en;
	gps_emitted_enough_en_out <= gps_emitted_enough_en;
	few_satellites_en_out <= few_satellites_en;
	is_unreliable_gps_data_en_out <= is_unreliable_gps_data_en;
    available <= av;

end behavioral;

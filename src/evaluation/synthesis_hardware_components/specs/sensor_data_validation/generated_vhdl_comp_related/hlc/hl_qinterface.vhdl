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
		gps_x_data_in : in signed(63 downto 0);
		gps_x_en_in : in std_logic;
		num_satellites_data_in : in signed(63 downto 0);
		num_satellites_en_in : in std_logic;
		imu_acc_x_data_in : in signed(63 downto 0);
		imu_acc_x_en_in : in std_logic;
		gps_x_data_out : out signed(63 downto 0);
		gps_x_en_out : out std_logic;
		num_satellites_data_out : out signed(63 downto 0);
		num_satellites_en_out : out std_logic;
		imu_acc_x_data_out : out signed(63 downto 0);
		imu_acc_x_en_out : out std_logic;
		gps_emitted_enough_en_out : out std_logic;
		few_satellites_en_out : out std_logic;
		is_unreliable_gps_data_en_out : out std_logic;
        time_out : out unsigned(63 downto 0);
        push_out : out std_logic
    );
end hlQInterface;

--* Streams and their Activation Conditions:
--* Input Streams 
--* - gps_x @ { gps_x }
--* - num_satellites @ { num_satellites }
--* - imu_acc_x @ { imu_acc_x }
--* Event-based Output Streams 
--* - few_satellites @ { num_satellites }
--* Periodic Output Streams 
--* - gps_emitted_enough @ 1Hz
--* - is_unreliable_gps_data @ 1Hz
--* Resulting Deadline Array
--* || gps_emitted_enough, is_unreliable_gps_data ||

architecture behavioral of hlQInterface is

    -- Internal Signal Declarations
    signal clock_state_machine : integer;
    signal push_to_queue : std_logic;
	signal gps_x_data_push : signed(63 downto 0);
	signal gps_x_en_push: std_logic;
	signal num_satellites_data_push : signed(63 downto 0);
	signal num_satellites_en_push: std_logic;
	signal imu_acc_x_data_push : signed(63 downto 0);
	signal imu_acc_x_en_push: std_logic;
	signal gps_emitted_enough_en_push : std_logic;
	signal few_satellites_en_push : std_logic;
	signal is_unreliable_gps_data_en_push : std_logic;
	signal gps_emitted_enough_en_array : bit_array(0 downto 0);
	signal is_unreliable_gps_data_en_array : bit_array(0 downto 0);
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
			gps_x_data_push <= (others => '0');
			gps_x_en_push <= '0';
			num_satellites_data_push <= (others => '0');
			num_satellites_en_push <= '0';
			imu_acc_x_data_push <= (others => '0');
			imu_acc_x_en_push <= '0';
			gps_emitted_enough_en_push <= '0';
			few_satellites_en_push <= '0';
			is_unreliable_gps_data_en_push <= '0';
            -- Initialize Deadline Arrays
            --* Deadline Array
            --* || gps_emitted_enough, is_unreliable_gps_data ||
			gps_emitted_enough_en_array(0) <= '1';
			is_unreliable_gps_data_en_array(0) <= '1';
        elsif (rising_edge(clk)) then
            clock_state_machine <= (clock_state_machine + 1) mod 4;
            if push_deadline = '1' and clock_state_machine = 0 then
                -- Deadline Handling
                push_to_queue <= '1';
                last_deadline_id <= (last_deadline_id + 1) mod 1;
                time_to_queue <= time_for_deadline;
				--* gps_x @ { gps_x }
				gps_x_en_push <= '0';
				--* num_satellites @ { num_satellites }
				num_satellites_en_push <= '0';
				--* imu_acc_x @ { imu_acc_x }
				imu_acc_x_en_push <= '0';
				--* few_satellites @ { num_satellites }
				few_satellites_en_push <= '0';
				--* gps_emitted_enough @ 1Hz
				gps_emitted_enough_en_push <= gps_emitted_enough_en_array(last_deadline_id);
				--* is_unreliable_gps_data @ 1Hz
				is_unreliable_gps_data_en_push <= is_unreliable_gps_data_en_array(last_deadline_id);
            elsif push_event = '1' and clock_state_machine = 2 then
                -- Event Handling
                push_to_queue <= '1';
                time_to_queue <= time_for_event;
				--* gps_x @ { gps_x }
				gps_x_data_push <= gps_x_data_in;
				gps_x_en_push <= gps_x_en_in;
				--* num_satellites @ { num_satellites }
				num_satellites_data_push <= num_satellites_data_in;
				num_satellites_en_push <= num_satellites_en_in;
				--* imu_acc_x @ { imu_acc_x }
				imu_acc_x_data_push <= imu_acc_x_data_in;
				imu_acc_x_en_push <= imu_acc_x_en_in;
				--* few_satellites @ { num_satellites }
				few_satellites_en_push <= '1' and num_satellites_en_in;
				--* gps_emitted_enough @ 1Hz
				gps_emitted_enough_en_push <= '0';
				--* is_unreliable_gps_data @ 1Hz
				is_unreliable_gps_data_en_push <= '0';
            else
                -- Enable No Stream
                push_to_queue <= '0';
				gps_x_en_push <= '0';
				num_satellites_en_push <= '0';
				imu_acc_x_en_push <= '0';
				gps_emitted_enough_en_push <= '0';
				few_satellites_en_push <= '0';
				is_unreliable_gps_data_en_push <= '0';
            end if;
        end if;
    end process;

    push_out <= push_to_queue;
    time_out <= time_to_queue;
	gps_x_data_out <= gps_x_data_push;
	gps_x_en_out <= gps_x_en_push;
	num_satellites_data_out <= num_satellites_data_push;
	num_satellites_en_out <= num_satellites_en_push;
	imu_acc_x_data_out <= imu_acc_x_data_push;
	imu_acc_x_en_out <= imu_acc_x_en_push;
	gps_emitted_enough_en_out <= gps_emitted_enough_en_push;
	few_satellites_en_out <= few_satellites_en_push;
	is_unreliable_gps_data_en_out <= is_unreliable_gps_data_en_push;

end behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity low_level_controller is
    port (
        clk, eclk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		gps_x : in signed(63 downto 0);
		gps_x_en : in std_logic;
		num_satellites : in signed(63 downto 0);
		num_satellites_en : in std_logic;
		imu_acc_x : in signed(63 downto 0);
		imu_acc_x_en : in std_logic;
		gps_emitted_enough_en : in std_logic;
		few_satellites_en : in std_logic;
		is_unreliable_gps_data_en : in std_logic;
		data_available : in std_logic;
		gps_emitted_enough : out std_logic;
		few_satellites : out std_logic;
		is_unreliable_gps_data : out std_logic;
		pop : out std_logic;
		eval_done : out std_logic
    );
end low_level_controller;

architecture mixed of low_level_controller is

	-- component declaration
	component evaluator is
		port (
			clk, input_clk, rst : in std_logic;
			input_time : in unsigned(63 downto 0);
			gps_x : in signed(63 downto 0);
			gps_x_en : in std_logic;
			num_satellites : in signed(63 downto 0);
			num_satellites_en : in std_logic;
			imu_acc_x : in signed(63 downto 0);
			imu_acc_x_en : in std_logic;
			gps_emitted_enough_en : in std_logic;
			few_satellites_en : in std_logic;
			is_unreliable_gps_data_en : in std_logic;
			gps_emitted_enough : out std_logic;
			few_satellites : out std_logic;
			is_unreliable_gps_data : out std_logic;
			done : out std_logic;
			valid : out std_logic
		);
	end component;

	-- signal declaration
	signal input_clk : std_logic;
	signal current_state : integer;
	signal evaluator_done : std_logic;
	signal evaluator_valid : std_logic;
	signal pop_data : std_logic;

begin
    -- component instantiation
    evaluator_instance: evaluator
        port map (
			clk => clk,
			input_clk => input_clk,
			rst => rst,
			input_time => time_in,
			gps_x => gps_x,
			gps_x_en => gps_x_en,
			num_satellites => num_satellites,
			num_satellites_en => num_satellites_en,
			imu_acc_x => imu_acc_x,
			imu_acc_x_en => imu_acc_x_en,
			gps_emitted_enough_en => gps_emitted_enough_en,
			few_satellites_en => few_satellites_en,
			is_unreliable_gps_data_en => is_unreliable_gps_data_en,
			gps_emitted_enough => gps_emitted_enough,
			few_satellites => few_satellites,
			is_unreliable_gps_data => is_unreliable_gps_data,
			done => evaluator_done,
			valid => evaluator_valid
        );

    process(eclk, rst) begin
		if rst='1' then
			input_clk <= '0';
			current_state <= 0;
			pop_data <= '0';
		elsif rising_edge(eclk) then
            if (current_state = 0 and data_available = '1') then
                -- idle
                pop_data <= '1';
                input_clk <= '0';
                current_state <= 1;
            elsif current_state = 1 then
                -- pop
                input_clk <= '1';
                pop_data <= '0';
                current_state <= 2;
            elsif current_state = 2 and evaluator_done = '1' then
                -- evaluate_done
                if data_available = '1' then
                    pop_data <= '1';
                    input_clk <= '0';
                    current_state <= 1;
                else
                    input_clk <= '0';
                    current_state <= 0;
                end if;
            end if;
        end if;
	end process;

	pop <= pop_data;
	eval_done <= input_clk;

end mixed;
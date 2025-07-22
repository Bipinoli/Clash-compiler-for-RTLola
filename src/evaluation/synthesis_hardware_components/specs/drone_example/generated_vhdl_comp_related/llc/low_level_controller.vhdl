library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity low_level_controller is
    port (
        clk, eclk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		acceleration_x : in signed(63 downto 0);
		acceleration_x_en : in std_logic;
		gps_sats : in unsigned(63 downto 0);
		gps_sats_en : in std_logic;
		lat_gps : in signed(63 downto 0);
		lat_gps_en : in std_logic;
		acceleration_x_periodic_en : in std_logic;
		acceleration_x_rising_en : in std_logic;
		acceleration_x_sinking_en : in std_logic;
		acceleration_x_direction_change_en : in std_logic;
		acceleration_x_changes_en : in std_logic;
		trigger_acc_en : in std_logic;
		gps_missed_beat_en : in std_logic;
		gps_medium_loss_en : in std_logic;
		gps_high_loss_en : in std_logic;
		gps_very_high_loss_en : in std_logic;
		trigger_gps_sats_en : in std_logic;
		data_available : in std_logic;
		acceleration_x_periodic : out signed(63 downto 0);
		acceleration_x_rising : out std_logic;
		acceleration_x_sinking : out std_logic;
		acceleration_x_direction_change : out std_logic;
		acceleration_x_changes : out unsigned(63 downto 0);
		trigger_acc : out std_logic;
		gps_missed_beat : out std_logic;
		gps_medium_loss : out std_logic;
		gps_high_loss : out std_logic;
		gps_very_high_loss : out std_logic;
		trigger_gps_sats : out std_logic;
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
			acceleration_x : in signed(63 downto 0);
			acceleration_x_en : in std_logic;
			gps_sats : in unsigned(63 downto 0);
			gps_sats_en : in std_logic;
			lat_gps : in signed(63 downto 0);
			lat_gps_en : in std_logic;
			acceleration_x_periodic_en : in std_logic;
			acceleration_x_rising_en : in std_logic;
			acceleration_x_sinking_en : in std_logic;
			acceleration_x_direction_change_en : in std_logic;
			acceleration_x_changes_en : in std_logic;
			trigger_acc_en : in std_logic;
			gps_missed_beat_en : in std_logic;
			gps_medium_loss_en : in std_logic;
			gps_high_loss_en : in std_logic;
			gps_very_high_loss_en : in std_logic;
			trigger_gps_sats_en : in std_logic;
			acceleration_x_periodic : out signed(63 downto 0);
			acceleration_x_rising : out std_logic;
			acceleration_x_sinking : out std_logic;
			acceleration_x_direction_change : out std_logic;
			acceleration_x_changes : out unsigned(63 downto 0);
			trigger_acc : out std_logic;
			gps_missed_beat : out std_logic;
			gps_medium_loss : out std_logic;
			gps_high_loss : out std_logic;
			gps_very_high_loss : out std_logic;
			trigger_gps_sats : out std_logic;
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
			acceleration_x => acceleration_x,
			acceleration_x_en => acceleration_x_en,
			gps_sats => gps_sats,
			gps_sats_en => gps_sats_en,
			lat_gps => lat_gps,
			lat_gps_en => lat_gps_en,
			acceleration_x_periodic_en => acceleration_x_periodic_en,
			acceleration_x_rising_en => acceleration_x_rising_en,
			acceleration_x_sinking_en => acceleration_x_sinking_en,
			acceleration_x_direction_change_en => acceleration_x_direction_change_en,
			acceleration_x_changes_en => acceleration_x_changes_en,
			trigger_acc_en => trigger_acc_en,
			gps_missed_beat_en => gps_missed_beat_en,
			gps_medium_loss_en => gps_medium_loss_en,
			gps_high_loss_en => gps_high_loss_en,
			gps_very_high_loss_en => gps_very_high_loss_en,
			trigger_gps_sats_en => trigger_gps_sats_en,
			acceleration_x_periodic => acceleration_x_periodic,
			acceleration_x_rising => acceleration_x_rising,
			acceleration_x_sinking => acceleration_x_sinking,
			acceleration_x_direction_change => acceleration_x_direction_change,
			acceleration_x_changes => acceleration_x_changes,
			trigger_acc => trigger_acc,
			gps_missed_beat => gps_missed_beat,
			gps_medium_loss => gps_medium_loss,
			gps_high_loss => gps_high_loss,
			gps_very_high_loss => gps_very_high_loss,
			trigger_gps_sats => trigger_gps_sats,
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
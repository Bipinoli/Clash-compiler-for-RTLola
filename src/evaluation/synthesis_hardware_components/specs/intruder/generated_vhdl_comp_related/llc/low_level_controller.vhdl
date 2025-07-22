library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity low_level_controller is
    port (
        clk, eclk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		lat : in signed(63 downto 0);
		lat_en : in std_logic;
		lon : in signed(63 downto 0);
		lon_en : in std_logic;
		distance_en : in std_logic;
		closer_en : in std_logic;
		trigger_closer_en : in std_logic;
		is_good_en : in std_logic;
		data_available : in std_logic;
		distance : out signed(63 downto 0);
		closer : out std_logic;
		trigger_closer : out std_logic;
		is_good : out std_logic;
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
			lat : in signed(63 downto 0);
			lat_en : in std_logic;
			lon : in signed(63 downto 0);
			lon_en : in std_logic;
			distance_en : in std_logic;
			closer_en : in std_logic;
			trigger_closer_en : in std_logic;
			is_good_en : in std_logic;
			distance : out signed(63 downto 0);
			closer : out std_logic;
			trigger_closer : out std_logic;
			is_good : out std_logic;
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
			lat => lat,
			lat_en => lat_en,
			lon => lon,
			lon_en => lon_en,
			distance_en => distance_en,
			closer_en => closer_en,
			trigger_closer_en => trigger_closer_en,
			is_good_en => is_good_en,
			distance => distance,
			closer => closer,
			trigger_closer => trigger_closer,
			is_good => is_good,
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
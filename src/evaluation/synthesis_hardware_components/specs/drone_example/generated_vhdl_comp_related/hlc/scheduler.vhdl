library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity scheduler is
    port(
        clk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
        time_last_deadline_out : out unsigned(63 downto 0);
        hit_deadline_out : out std_logic
    );
end scheduler;

--* Periodic Streams in Specification: 
--* - acceleration_x_periodic @ 2000Hz *--
--* - acceleration_x_rising @ 2000Hz *--
--* - acceleration_x_sinking @ 2000Hz *--
--* - acceleration_x_direction_change @ 2000Hz *--
--* - acceleration_x_changes @ 2000Hz *--
--* - trigger_acc @ 2000Hz *--
--* - gps_missed_beat @ 2000Hz *--
--* - gps_medium_loss @ 100Hz *--
--* - gps_high_loss @ 100Hz *--
--* - gps_very_high_loss @ 100Hz *--
--* - trigger_gps_sats @ 100Hz *--
--* Resulting Hyper Period in Seconds: 0.01 seconds
--* Resulting Offset Array in Seconds:
--* || 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 ||

architecture behavioral of scheduler is

    -- Internal Signal Declarations
    signal time_of_next_deadline : unsigned(63 downto 0);
    signal offset_per_deadline : unsigned64_array(19 downto 0);
    signal last_deadline_id : integer;
    signal hit_deadline : std_logic;
    signal time_last_deadline : unsigned(63 downto 0);

begin

    process(clk, rst) begin
        if (rst = '1') then
            -- Reset Phase
            time_of_next_deadline <= to_unsigned(500000, time_of_next_deadline'length);
            last_deadline_id <= 0;
            time_last_deadline <= (others => '0');
            hit_deadline <= '0';
            -- Initialization of the Deadline Offset Array
            --* Offset Array in seconds:
            --* || 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 | 0.0005 ||
			offset_per_deadline(0) <= to_unsigned(500000, offset_per_deadline(0)'length);
			offset_per_deadline(1) <= to_unsigned(500000, offset_per_deadline(1)'length);
			offset_per_deadline(2) <= to_unsigned(500000, offset_per_deadline(2)'length);
			offset_per_deadline(3) <= to_unsigned(500000, offset_per_deadline(3)'length);
			offset_per_deadline(4) <= to_unsigned(500000, offset_per_deadline(4)'length);
			offset_per_deadline(5) <= to_unsigned(500000, offset_per_deadline(5)'length);
			offset_per_deadline(6) <= to_unsigned(500000, offset_per_deadline(6)'length);
			offset_per_deadline(7) <= to_unsigned(500000, offset_per_deadline(7)'length);
			offset_per_deadline(8) <= to_unsigned(500000, offset_per_deadline(8)'length);
			offset_per_deadline(9) <= to_unsigned(500000, offset_per_deadline(9)'length);
			offset_per_deadline(10) <= to_unsigned(500000, offset_per_deadline(10)'length);
			offset_per_deadline(11) <= to_unsigned(500000, offset_per_deadline(11)'length);
			offset_per_deadline(12) <= to_unsigned(500000, offset_per_deadline(12)'length);
			offset_per_deadline(13) <= to_unsigned(500000, offset_per_deadline(13)'length);
			offset_per_deadline(14) <= to_unsigned(500000, offset_per_deadline(14)'length);
			offset_per_deadline(15) <= to_unsigned(500000, offset_per_deadline(15)'length);
			offset_per_deadline(16) <= to_unsigned(500000, offset_per_deadline(16)'length);
			offset_per_deadline(17) <= to_unsigned(500000, offset_per_deadline(17)'length);
			offset_per_deadline(18) <= to_unsigned(500000, offset_per_deadline(18)'length);
			offset_per_deadline(19) <= to_unsigned(500000, offset_per_deadline(19)'length);
        elsif (rising_edge(clk)) then
            -- Logic Phase: Decision, if Arrival of a Deadline
            if (time_in >= time_of_next_deadline) then
                -- Deadline is Reached
                time_of_next_deadline <= time_of_next_deadline + offset_per_deadline(last_deadline_id);
                last_deadline_id <= (last_deadline_id + 1) mod 20;
                hit_deadline <= '1';
                time_last_deadline <= time_of_next_deadline;
            else
                -- No Deadline is Reached
                hit_deadline <= '0';
            end if;
        end if;
    end process;

    hit_deadline_out <= hit_deadline;
    time_last_deadline_out <= time_last_deadline;

end behavioral;
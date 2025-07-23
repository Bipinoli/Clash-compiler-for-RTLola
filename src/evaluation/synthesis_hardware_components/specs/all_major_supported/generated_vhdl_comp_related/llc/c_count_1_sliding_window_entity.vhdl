library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

--* Sliding window in the specification:
--* c.aggregate(over: 0.05 s, using: count)
--* Source Stream: d @2000Hz
--* Number of Buckets: 100
--* Time per Bucket: 0.0005s
--* Input Type: Int64
--* Return Type: UInt64

entity c_count_1_sliding_window_entity is
    port (
        clk, rst : in std_logic;
        evict, upd, request : in std_logic;
        time_in : in unsigned(63 downto 0);
        data_in : in signed(63 downto 0);
        data_out : out unsigned(63 downto 0);
        data_valid_out : out std_logic;
        evict_done_out : out std_logic;
        upd_done_out : out std_logic;
        request_done_out : out std_logic
    );
end c_count_1_sliding_window_entity;

architecture behavioral of c_count_1_sliding_window_entity is

    -- Internal Signal Declarations
    signal last_ts_before_upd : unsigned(63 downto 0);
    signal sw_data : unsigned(63 downto 0);
    signal sw_data_valid : std_logic;
    -- Done Signals
    signal evict_done : std_logic;
    signal upd_done : std_logic;
    signal request_done : std_logic;
    -- Buckets
    signal ts_buckets : unsigned64_array(99 downto 0);
    signal count_buckets : unsigned64_array(99 downto 0);
    signal data_valid_buckets : bit_array(99 downto 0);

    begin

    process (clk, rst) begin
        if (rst='1') then
            -- Reset Phase
            evict_done <= '0';
            upd_done <= '0';
            request_done <= '0';
            sw_data <= to_unsigned(0, sw_data'length);
            sw_data_valid <= '1';
            last_ts_before_upd <= to_unsigned(0, last_ts_before_upd'length);
            -- Reset Buckets
            ts_buckets(ts_buckets'high downto 0) <= (others => (others => '0'));
            data_valid_buckets(data_valid_buckets'high downto 0) <= (others => '1');
            count_buckets(count_buckets'high downto 0) <= (others => to_unsigned(0, sw_data'length));
        elsif (rising_edge(clk)) then
            -- Logic Phase
            if (evict = '1' and evict_done = '0') then
                -- Evict Case: New TimeStamp
                if time_in > last_ts_before_upd then
                    -- Update Timestamp
                    last_ts_before_upd <= last_ts_before_upd + to_unsigned(500000, last_ts_before_upd'length);
                    -- Create New Bucket and Shift Bucket Array
                    ts_buckets <= ts_buckets(ts_buckets'high-1 downto 0) & (last_ts_before_upd + to_unsigned(500000, ts_buckets(0)'length));
                    data_valid_buckets <= data_valid_buckets(data_valid_buckets'high-1 downto 0) & '1';
                    count_buckets <= count_buckets(count_buckets'high-1 downto 0) & to_unsigned(0, sw_data'length);
                else
                    evict_done <= '1';
                end if;
            elsif (upd = '1' and upd_done = '0') then
                -- Update Case: Map New Input and Update Last Buckets Entry
                count_buckets(0) <= count_buckets(0) + to_unsigned(1, count_buckets(0)'length);
                upd_done <= '1';
            elsif (request = '1' and request_done = '0') then
                -- Request Case: Finalize Buckets
                sw_data <= count_buckets(0) + count_buckets(1) + count_buckets(2) + count_buckets(3) + count_buckets(4) + count_buckets(5) + count_buckets(6) + count_buckets(7) + count_buckets(8) + count_buckets(9) + count_buckets(10) + count_buckets(11) + count_buckets(12) + count_buckets(13) + count_buckets(14) + count_buckets(15) + count_buckets(16) + count_buckets(17) + count_buckets(18) + count_buckets(19) + count_buckets(20) + count_buckets(21) + count_buckets(22) + count_buckets(23) + count_buckets(24) + count_buckets(25) + count_buckets(26) + count_buckets(27) + count_buckets(28) + count_buckets(29) + count_buckets(30) + count_buckets(31) + count_buckets(32) + count_buckets(33) + count_buckets(34) + count_buckets(35) + count_buckets(36) + count_buckets(37) + count_buckets(38) + count_buckets(39) + count_buckets(40) + count_buckets(41) + count_buckets(42) + count_buckets(43) + count_buckets(44) + count_buckets(45) + count_buckets(46) + count_buckets(47) + count_buckets(48) + count_buckets(49) + count_buckets(50) + count_buckets(51) + count_buckets(52) + count_buckets(53) + count_buckets(54) + count_buckets(55) + count_buckets(56) + count_buckets(57) + count_buckets(58) + count_buckets(59) + count_buckets(60) + count_buckets(61) + count_buckets(62) + count_buckets(63) + count_buckets(64) + count_buckets(65) + count_buckets(66) + count_buckets(67) + count_buckets(68) + count_buckets(69) + count_buckets(70) + count_buckets(71) + count_buckets(72) + count_buckets(73) + count_buckets(74) + count_buckets(75) + count_buckets(76) + count_buckets(77) + count_buckets(78) + count_buckets(79) + count_buckets(80) + count_buckets(81) + count_buckets(82) + count_buckets(83) + count_buckets(84) + count_buckets(85) + count_buckets(86) + count_buckets(87) + count_buckets(88) + count_buckets(89) + count_buckets(90) + count_buckets(91) + count_buckets(92) + count_buckets(93) + count_buckets(94) + count_buckets(95) + count_buckets(96) + count_buckets(97) + count_buckets(98) + count_buckets(99);
                sw_data_valid <= '1';
                request_done <= '1';
            elsif (evict ='0' and upd = '0' and request = '0') then
                evict_done <= '0';
                upd_done <= '0';
                request_done <= '0';
            end if;
        end if;
    end process;

    -- Map Internal Signals to Output Wires
    data_out <= sw_data;
    data_valid_out <= sw_data_valid;
    evict_done_out <= evict_done;
    upd_done_out <= upd_done;
    request_done_out <= request_done;

end behavioral;

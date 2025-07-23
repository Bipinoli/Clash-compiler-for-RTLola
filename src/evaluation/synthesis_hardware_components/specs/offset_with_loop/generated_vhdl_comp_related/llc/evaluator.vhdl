library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity evaluator is
    port (
        clk, input_clk, rst : in std_logic;
        input_time : in unsigned(63 downto 0);
		x : in signed(63 downto 0);
		x_en : in std_logic;
		a_en : in std_logic;
		b_en : in std_logic;
		c_en : in std_logic;
		d_en : in std_logic;
		a : out signed(63 downto 0);
		b : out signed(63 downto 0);
		c : out signed(63 downto 0);
		d : out signed(63 downto 0);
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input x : Int64
--* output a := ((x.offset(by: neg1).defaults(to: 0) + b.offset(by: neg1).defaults(to: 0)) + c.offset(by: neg1).defaults(to: 0))
--* output b := (a.offset(by: neg3).defaults(to: 0) + x.offset(by: neg2).defaults(to: 0))
--* output c := ((x + a) + b)
--* output d := c.hold().defaults(to: 0)


architecture mixed of evaluator is

    -- Component Declaration
	--* input x : Int64
    component x_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(2 downto 0);
		    data_valid_out : out bit_array(2 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output a := ((x.offset(by: neg1).defaults(to: 0) + b.offset(by: neg1).defaults(to: 0)) + c.offset(by: neg1).defaults(to: 0))
    component a_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			x_neg1 : in signed(63 downto 0);
			x_data_valid_neg1 : in std_logic;
			b_neg1 : in signed(63 downto 0);
			b_data_valid_neg1 : in std_logic;
			c_neg1 : in signed(63 downto 0);
			c_data_valid_neg1 : in std_logic;
		    data_out : out signed64_array(3 downto 0);
		    data_valid_out : out bit_array(3 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output b := (a.offset(by: neg3).defaults(to: 0) + x.offset(by: neg2).defaults(to: 0))
    component b_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			x_neg2 : in signed(63 downto 0);
			x_data_valid_neg2 : in std_logic;
			a_neg3 : in signed(63 downto 0);
			a_data_valid_neg3 : in std_logic;
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output c := ((x + a) + b)
    component c_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			x_0 : in signed(63 downto 0);
			x_data_valid_0 : in std_logic;
			a_0 : in signed(63 downto 0);
			a_data_valid_0 : in std_logic;
			b_0 : in signed(63 downto 0);
			b_data_valid_0 : in std_logic;
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output d := c.hold().defaults(to: 0)
    component d_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			c_0 : in signed(63 downto 0);
			c_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;


    -- Internal Signal Declarations
	signal x_upd : std_logic;
	signal x_upd_done : std_logic;
	signal x_entity_data_0 : signed(63 downto 0);
	signal x_entity_data_valid_0 : std_logic;
	signal x_entity_data_1 : signed(63 downto 0);
	signal x_entity_data_valid_1 : std_logic;
	signal x_entity_data_2 : signed(63 downto 0);
	signal x_entity_data_valid_2 : std_logic;
	signal a_pe : std_logic;
	signal a_eval : std_logic;
	signal a_pe_done : std_logic;
	signal a_eval_done : std_logic;
	signal a_entity_data_0 : signed(63 downto 0);
	signal a_entity_data_valid_0 : std_logic;
	signal a_entity_data_1 : signed(63 downto 0);
	signal a_entity_data_valid_1 : std_logic;
	signal a_entity_data_2 : signed(63 downto 0);
	signal a_entity_data_valid_2 : std_logic;
	signal a_entity_data_3 : signed(63 downto 0);
	signal a_entity_data_valid_3 : std_logic;
	signal b_pe : std_logic;
	signal b_eval : std_logic;
	signal b_pe_done : std_logic;
	signal b_eval_done : std_logic;
	signal b_entity_data_0 : signed(63 downto 0);
	signal b_entity_data_valid_0 : std_logic;
	signal b_entity_data_1 : signed(63 downto 0);
	signal b_entity_data_valid_1 : std_logic;
	signal c_pe : std_logic;
	signal c_eval : std_logic;
	signal c_pe_done : std_logic;
	signal c_eval_done : std_logic;
	signal c_entity_data_0 : signed(63 downto 0);
	signal c_entity_data_valid_0 : std_logic;
	signal c_entity_data_1 : signed(63 downto 0);
	signal c_entity_data_valid_1 : std_logic;
	signal d_pe : std_logic;
	signal d_eval : std_logic;
	signal d_pe_done : std_logic;
	signal d_eval_done : std_logic;
	signal d_entity_data_0 : signed(63 downto 0);
	signal d_entity_data_valid_0 : std_logic;

    signal upd_and_pe_done : std_logic;
    signal evaluator_done : std_logic;
    signal valid_reg : std_logic;
    signal rst_en_done : std_logic;

begin
    -- Component Instantiation
	--* input x : Int64
    x_entity_instance: x_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => x_upd,
            data_in => x,
			data_out(0) => x_entity_data_0,
			data_out(1) => x_entity_data_1,
			data_out(2) => x_entity_data_2,
			data_valid_out(0) => x_entity_data_valid_0,
			data_valid_out(1) => x_entity_data_valid_1,
			data_valid_out(2) => x_entity_data_valid_2,
            done_out => x_upd_done
         );

	--* output a := ((x.offset(by: neg1).defaults(to: 0) + b.offset(by: neg1).defaults(to: 0)) + c.offset(by: neg1).defaults(to: 0))
    a_entity_instance: a_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => a_pe,
            eval => a_eval,
			x_neg1 => x_entity_data_1,
			x_data_valid_neg1 => x_entity_data_valid_1,
			b_neg1 => b_entity_data_1,
			b_data_valid_neg1 => b_entity_data_valid_1,
			c_neg1 => c_entity_data_1,
			c_data_valid_neg1 => c_entity_data_valid_1,
			data_out(0) => a_entity_data_0,
			data_out(1) => a_entity_data_1,
			data_out(2) => a_entity_data_2,
			data_out(3) => a_entity_data_3,
			data_valid_out(0) => a_entity_data_valid_0,
			data_valid_out(1) => a_entity_data_valid_1,
			data_valid_out(2) => a_entity_data_valid_2,
			data_valid_out(3) => a_entity_data_valid_3,
            pe_done_out => a_pe_done,
            eval_done_out => a_eval_done
        );

	--* output b := (a.offset(by: neg3).defaults(to: 0) + x.offset(by: neg2).defaults(to: 0))
    b_entity_instance: b_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => b_pe,
            eval => b_eval,
			x_neg2 => x_entity_data_2,
			x_data_valid_neg2 => x_entity_data_valid_2,
			a_neg3 => a_entity_data_3,
			a_data_valid_neg3 => a_entity_data_valid_3,
			data_out(0) => b_entity_data_0,
			data_out(1) => b_entity_data_1,
			data_valid_out(0) => b_entity_data_valid_0,
			data_valid_out(1) => b_entity_data_valid_1,
            pe_done_out => b_pe_done,
            eval_done_out => b_eval_done
        );

	--* output c := ((x + a) + b)
    c_entity_instance: c_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => c_pe,
            eval => c_eval,
			x_0 => x_entity_data_0,
			x_data_valid_0 => x_entity_data_valid_0,
			a_0 => a_entity_data_0,
			a_data_valid_0 => a_entity_data_valid_0,
			b_0 => b_entity_data_0,
			b_data_valid_0 => b_entity_data_valid_0,
			data_out(0) => c_entity_data_0,
			data_out(1) => c_entity_data_1,
			data_valid_out(0) => c_entity_data_valid_0,
			data_valid_out(1) => c_entity_data_valid_1,
            pe_done_out => c_pe_done,
            eval_done_out => c_eval_done
        );

	--* output d := c.hold().defaults(to: 0)
    d_entity_instance: d_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => d_pe,
            eval => d_eval,
			c_0 => c_entity_data_0,
			c_data_valid_0 => c_entity_data_valid_0,
			data_out(0) => d_entity_data_0,
			data_valid_out(0) => d_entity_data_valid_0,
            pe_done_out => d_pe_done,
            eval_done_out => d_eval_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				x_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				b_pe <= '0';
				b_eval <= '0';
				c_pe <= '0';
				c_eval <= '0';
				d_pe <= '0';
				d_eval <= '0';
            upd_and_pe_done <= '1';
            evaluator_done <= '1';
            rst_en_done <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase
            if input_clk = '1' then
                if upd_and_pe_done = '0' then
                    -- Input Stream Updates
                    --* Input Streams in Specification 
					--* - x 
					x_upd <= x_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - a
					--* - b
					--* - c
					--* - d
					a_pe <= a_en;
					b_pe <= b_en;
					c_pe <= c_en;
					d_pe <= d_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output a := ((x.offset(by: neg1).defaults(to: 0) + b.offset(by: neg1).defaults(to: 0)) + c.offset(by: neg1).defaults(to: 0))
					--* Evaluation Phase of Output Stream a is Influenced by No Lookup
					a_eval <= a_en and upd_and_pe_done;
					--* output b := (a.offset(by: neg3).defaults(to: 0) + x.offset(by: neg2).defaults(to: 0))
					--* Evaluation Phase of Output Stream b is Influenced by No Lookup
					b_eval <= b_en and upd_and_pe_done;
					--* output c := ((x + a) + b)
					--* Evaluation Phase of Output Stream c is Influenced by the following Lookups: 
					--* - Synchronous Lookup: a
					--* - Synchronous Lookup: b
					c_eval <= c_en and upd_and_pe_done and a_eval_done and b_eval_done;
					--* output d := c.hold().defaults(to: 0)
					--* Evaluation Phase of Output Stream d is Influenced by No Lookup
					d_eval <= d_en and upd_and_pe_done;
                    -- SW Update Phase
                    -- SW Request Phase
                    -- Valid Assignment
					valid_reg <= '1' and a_entity_data_valid_0 and b_entity_data_valid_0 and c_entity_data_valid_0 and d_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not x_en or x_upd_done) and (not a_en or a_pe_done) and (not b_en or b_pe_done) and (not c_en or c_pe_done) and (not d_en or d_pe_done);
					evaluator_done <= upd_and_pe_done and (not a_en or a_eval_done) and (not b_en or b_eval_done) and (not c_en or c_eval_done) and (not d_en or d_eval_done);
                end if;
            else
                upd_and_pe_done <= '0';
				x_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				b_pe <= '0';
				b_eval <= '0';
				c_pe <= '0';
				c_eval <= '0';
				d_pe <= '0';
				d_eval <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	a <= a_entity_data_0;
	b <= b_entity_data_0;
	c <= c_entity_data_0;
	d <= d_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;
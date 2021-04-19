
-------------------------------------------------------------------------------
-- Objective
-- Construct a finite impulse response (FIR) filter with 4 coefficients (taps) in HDL along with a test-bench.  
-- Perform synthesis, placement and routing, and static timing analysis.  
-- Input data is assumed to have been digitized from a 16-bit analog-to-digital converter (ADC) at a sample rate of 2000 MHz.  
-- The FPGA logic is operating at a slower rate of 500 MHz.  
-- Output data is assumed to be driving a digital-to-analog converter at a sample rate of 2000 MHz.  
-- A block diagram of the FIR filter and interfaces is shown below:

-- Author : Surinder Pal Singh

-- Dependencies : Need four FIFO to buffer up ADC data and additional four FIFO TO queue up the data in order to sample it 
--                @ DAC clock rate

-- Limitation : This design won't able to sustain input sample rate due to limited storage capacity of available hardware. 

-- Implementation : Design has been implemented using 4 FIFO to hold input ADC stream into 4 FIFO's which then will be pushed into 
--                  FIR_FILTER module by fetching one DW from each fifo to generate filter output @500MHz. Output of FIR FILTER 
--                  saved into another set of 4 FIFO's. Once these FIFO's reached half capacity, another process will  start fetching  
--                  data from these FIFO to sustain the input sample rate of DAC. 

-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

USE work.fir_filter_pkg.ALL;

LIBRARY unisim;
USE unisim.vcomponents.ALL;

ENTITY FIR_FILTER_TOP_TB IS
  --  GENERIC (
  --    SIM_MODE : STRING := "FALSE";
  --    g_Num_Bits : INTEGER := 16
  --  );
  --  PORT (
  --    --    FPGA_CLK_P : IN STD_LOGIC;
  --    --    FPGA_CLK_N : IN STD_LOGIC;
  --    ADC_CLK_P : IN STD_LOGIC;
  --    ADC_CLK_N : IN STD_LOGIC;
  --    fir_data_out_IO_P : OUT STD_LOGIC;    -- _VECTOR(g_NUM_Bits-1 DOWNTO 0);
  --    fir_data_out_IO_N : OUT STD_LOGIC    -- _VECTOR(g_NUM_Bits-1 DOWNTO 0)

  --    --    DAC_CLK_P : IN STD_LOGIC;
  --    --    DAC_CLK_N : IN STD_LOGIC
  --  );
END ENTITY FIR_FILTER_TOP_TB;

ARCHITECTURE behave OF FIR_FILTER_TOP_TB IS
  COMPONENT FIR_FIFO1
    PORT (
      clk : IN STD_LOGIC;
      srst : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
      full : OUT STD_LOGIC;
      almost_full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      data_count : OUT STD_LOGIC_VECTOR(DAC_FIFO_SIZE - 1 DOWNTO 0)
    );
  END COMPONENT;
  COMPONENT FIR_FIFO2
    PORT (
      clk : IN STD_LOGIC;
      srst : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
      full : OUT STD_LOGIC;
      almost_full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      data_count : OUT STD_LOGIC_VECTOR(DAC_FIFO_SIZE - 1 DOWNTO 0)
    );
  END COMPONENT;

  COMPONENT FIR_FILTER IS
    GENERIC (
      NUM_OF_TAPS : INTEGER := 4;
      C_COEFS_LENGTH : INTEGER := 16;
      C_MULT_LENGTH : INTEGER := 32;
      C_ADD_LENGTH : INTEGER := 33;
      C_DATA_IN_LENGTH : INTEGER := 16;
      C_DATA_OUT_LENGTH : INTEGER := 16
    );
    PORT (
      i_clk : IN STD_LOGIC;
      i_rstb : IN STD_LOGIC;
      -- coefficient
      i_coeff_0 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      i_coeff_1 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      i_coeff_2 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      i_coeff_3 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      -- data input
      i_data : IN STD_LOGIC_VECTOR(C_DATA_IN_LENGTH - 1 DOWNTO 0);
      -- filtered data 
      o_data : OUT STD_LOGIC_VECTOR(C_DATA_OUT_LENGTH - 1 DOWNTO 0));
  END COMPONENT;
  COMPONENT IBUFGDS
    GENERIC (
      CAPACITANCE : STRING := "DONT_CARE";
      DIFF_TERM : BOOLEAN := FALSE;
      IBUF_DELAY_VALUE : STRING := "0";
      IBUF_LOW_PWR : BOOLEAN := TRUE;
      IOSTANDARD : STRING := "DEFAULT");

    PORT (
      O : OUT STD_LOGIC;
      I : IN STD_LOGIC;
      IB : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL fir_data_in : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);

  SIGNAL DAC_DATA_IN : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);

  SIGNAL w_LFSR_Done : STD_LOGIC;
  SIGNAL i_Enable : STD_LOGIC;

  -- seed related
  SIGNAL i_Seed_DV : STD_LOGIC;
  SIGNAL i_Seed_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL seed_counter : STD_LOGIC_VECTOR(c_SEED_COUNTER_BITS - 1 DOWNTO 0) := (OTHERS => '0');

  --reset 
  SIGNAL RST_N : STD_LOGIC := '0';
  SIGNAL reset_counter : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
  SIGNAL d_valid_counter : STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '1');
  SIGNAL d_valid_counter_en : STD_LOGIC;

  SIGNAL d_read_counter : STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '1');
  SIGNAL d_read_counter_en : STD_LOGIC;
  SIGNAL d_wr_counter, d_rd_counter : STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '1');
  SIGNAL d_wr_counter_en, d_rd_counter_en : STD_LOGIC;

  -- clk
  SIGNAL FPGA_CLK : STD_LOGIC;
  SIGNAL ADC_CLK : STD_LOGIC;
  SIGNAL DAC_CLK : STD_LOGIC;

  SIGNAL stop_reset_timer : STD_LOGIC;

  SIGNAL INT_FPGA_CLK_P : STD_LOGIC := '0';
  SIGNAL INT_FPGA_CLK_N : STD_LOGIC := '0';
  SIGNAL INT_ADC_CLK_P : STD_LOGIC := '0';
  SIGNAL INT_ADC_CLK_N : STD_LOGIC := '0';
  SIGNAL INT_DAC_CLK_P : STD_LOGIC := '0';
  SIGNAL INT_DAC_CLK_N : STD_LOGIC := '0';
  SIGNAL MULT : MULT_TYPE;
  SIGNAL ADD : ADD_TYPE;

  SIGNAL fifo_data_valid : STD_LOGIC;

  SIGNAL Valid_in : STD_LOGIC;
  SIGNAL Valid_in_cnt : STD_LOGIC_VECTOR(3 DOWNTO 0);

  SIGNAL s_adc_data_Valid : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  -- fir fifo 

  SIGNAL adc_fifo_din : FIR_FILTER_DATA_TYPE;
  SIGNAL adc_fifo_wr_en : FIR_FILTER_LOGIC_TYPE;
  SIGNAL adc_fifo_rd_en : FIR_FILTER_LOGIC_TYPE;
  SIGNAL adc_fifo_dout : FIR_FILTER_DATA_TYPE;
  SIGNAL adc_fifo_rst : FIR_FILTER_LOGIC_TYPE;
  SIGNAL adc_fifo_empty : FIR_FILTER_LOGIC_TYPE;

  SIGNAL dac_fifo_din : FIR_FILTER_DATA_TYPE;
  SIGNAL dac_fifo_wr_en : FIR_FILTER_LOGIC_TYPE;
  SIGNAL dac_fifo_rd_en : FIR_FILTER_LOGIC_TYPE;
  SIGNAL dac_fifo_dout : FIR_FILTER_DATA_TYPE;
  SIGNAL dac_fifo_rst : FIR_FILTER_LOGIC_TYPE;
  SIGNAL dac_fifo_empty : FIR_FILTER_LOGIC_TYPE;
  SIGNAL dac_fifo1_data_count : DAC_FIFO_DATA_CNT_TYPE;
  SIGNAL adc_fifo1_data_count : ADC_FIFO_DATA_CNT_TYPE;

  SIGNAL d_strb_Q, d1_strb_Q, d2_strb_Q, d3_strb_Q, d4_strb_Q : STD_LOGIC;
  SIGNAL d1_strb_QQ, d2_strb_QQ, d3_strb_QQ, d4_strb_QQ : STD_LOGIC;
  SIGNAL d1_Q, d2_Q, d3_Q, d4_Q : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL d1_QQ, d2_QQ, d3_QQ, d4_QQ : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL d1_QQQ, d2_QQQ, d3_QQQ, d4_QQQ : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);

  SIGNAL d_out_Q : FIR_FILTER_DATA_TYPE;
  SIGNAL FIR_DATA_o : FIR_FILTER_DATA_TYPE;

  SIGNAL s_fir_data_in, s_fir_data_in_Q, s_fir_data_in_QQ : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL s_fir_data_out, s_fir_data_out2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL s_dac_fifo_wr_strb, s_dac_fifo_rd_strb : STD_LOGIC_VECTOR(11 DOWNTO 0);
BEGIN
  i_Seed_Data <= x"DEAD";
  -- DAC_DATA_IN <= FIR_DATA_o;
  --  G_DIFF_OBUG : FOR I IN 0 TO 0 GENERATE
  --  DIFF_OBUF : OBUFDS PORT MAP (
  --    I => DAC_DATA_IN(I),
  --    O => fir_data_out_IO_P,
  --    OB => fir_data_out_IO_N    
  --  );
  --  END GENERATE G_DIFF_OBUG;
  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT

  adc_ibuf : IBUFGDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    IBUF_LOW_PWR => FALSE)
  PORT MAP(
    O => adc_clk,
    I => INT_ADC_CLK_P,
    IB => INT_ADC_CLK_N
  );

  -- GENERATE SINGLE ENDED dac CLK FROM DIFFERENTIAL INPUT
  dac_clk_ibuf : IBUFGDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    IBUF_LOW_PWR => FALSE)
  PORT MAP(
    O => DAC_CLK,
    I => INT_DAC_CLK_P,
    IB => INT_DAC_CLK_N
  );

  -- GENERATE SINGLE ENDED fpga CLK FROM DIFFERENTIAL INPUT
  fpga_clk_ibuf : IBUFGDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    IBUF_LOW_PWR => FALSE)
  PORT MAP(
    O => FPGA_CLK,
    I => INT_FPGA_CLK_P,
    IB => INT_FPGA_CLK_N
  );
  INT_FPGA_CLK_P <= NOT INT_FPGA_CLK_P AFTER c_CLK_PERIOD/2;
  INT_FPGA_CLK_N <= NOT INT_FPGA_CLK_P;
  INT_ADC_CLK_P <= NOT INT_ADC_CLK_P AFTER c_CLK_PERIOD/8;
  INT_ADC_CLK_N <= NOT INT_ADC_CLK_P;
  INT_DAC_CLK_P <= NOT INT_DAC_CLK_P AFTER c_CLK_PERIOD/8;
  INT_DAC_CLK_N <= NOT INT_DAC_CLK_P;

  G_ADC_FIFO : FOR I IN 0 TO NUM_OF_TAPS - 1 GENERATE
    U_FIR_FIFO1 : FIR_FIFO1
    PORT MAP(
      clk => FPGA_CLK,
      srst => adc_fifo_rst(I),
      din => adc_fifo_din(I),
      wr_en => adc_fifo_wr_en(I),
      rd_en => adc_fifo_rd_en(I),
      dout => adc_fifo_dout(I),
      full => OPEN,
      almost_full => OPEN,
      empty => adc_fifo_empty(I),
      data_count => adc_fifo1_data_count(I)
    );
  END GENERATE G_ADC_FIFO;

  -- Generate Fake Valid strb every 16th sample
  p_valid : PROCESS (adc_clk, RST_N)
  BEGIN
    IF (RST_N = '0') THEN
      Valid_in <= '0';
      Valid_in_cnt <= (OTHERS => '0');
    ELSIF rising_edge(adc_clk) THEN

      Valid_in <= '0';
      IF (Valid_in_cnt = 0) THEN
        Valid_in <= '1';
        Valid_in_cnt <= (OTHERS => '1');
      ELSE
        Valid_in_cnt <= Valid_in_cnt - b"1";
      END IF;
    END IF;
  END PROCESS p_valid;

  p_reset : PROCESS (adc_clk, stop_reset_timer)
  BEGIN
    IF rising_edge(adc_clk) THEN
      RST_N <= reset_counter(11);
      IF (stop_reset_timer = '0') THEN
        reset_counter <= reset_counter + b"1";
      END IF;
    END IF;
  END PROCESS p_reset;

  stop_reset_timer <= reset_counter(11);

  p_Seed : PROCESS (adc_clk, RST_N)
  BEGIN
    IF (RST_N = '0') THEN
      i_Seed_DV <= '1';
      i_Enable <= '0';
    ELSIF rising_edge(adc_clk) THEN
      --default
      i_Seed_DV <= '0';
      i_Enable <= '1';
      -- seed counter
      IF (seed_counter = 0) THEN
        i_Seed_DV <= '1';
        seed_counter <= (OTHERS => '1');
      ELSE
        seed_counter <= seed_counter - b"1";
      END IF;
    END IF;
  END PROCESS p_Seed;

  p_adc_data : PROCESS (adc_clk, RST_N)
  BEGIN
    IF (RST_N = '0') THEN
      d_valid_counter_en <= '0';
      d_valid_counter <= (OTHERS => '1');
      s_adc_data_Valid <= (OTHERS => '0');
    ELSIF rising_edge(adc_clk) THEN

      adc_fifo_rst(0) <= NOT RST_N;
      adc_fifo_rst(1) <= NOT RST_N;
      adc_fifo_rst(2) <= NOT RST_N;
      adc_fifo_rst(3) <= NOT RST_N;
      dac_fifo_rst(0) <= NOT RST_N;
      dac_fifo_rst(1) <= NOT RST_N;
      dac_fifo_rst(2) <= NOT RST_N;
      dac_fifo_rst(3) <= NOT RST_N;

      -- adc data acquisation into fir fifo cntrl
      IF (Valid_in = '1') THEN
        s_adc_data_Valid <= x"0001";
        d_strb_Q <= '0';
      ELSE
        s_adc_data_Valid <= s_adc_data_Valid(c_NUM_BITS - 2 DOWNTO 0) & b"0";
      END IF;
      -- determine where to load incoming adc data 
      d1_strb_Q <= s_adc_data_Valid(0) OR s_adc_data_Valid(4) OR s_adc_data_Valid(8) OR s_adc_data_Valid(12);
      d2_strb_Q <= s_adc_data_Valid(1) OR s_adc_data_Valid(5) OR s_adc_data_Valid(9) OR s_adc_data_Valid(13);
      d3_strb_Q <= s_adc_data_Valid(2) OR s_adc_data_Valid(6) OR s_adc_data_Valid(10) OR s_adc_data_Valid(14);
      d4_strb_Q <= s_adc_data_Valid(3) OR s_adc_data_Valid(7) OR s_adc_data_Valid(11) OR s_adc_data_Valid(15);

      -- TRIGGER FIFO ENABLE LOGIC AFTER LAST VALID REGISTER WR AND HOLD IT HIGH FOR VALID DATA PERIOD
      IF (d4_strb_Q = '1') THEN
        d_strb_Q <= '1';
        d_valid_counter_en <= '1';
      ELSIF (d_valid_counter = 0) THEN
        d_strb_Q <= '0';
        d_valid_counter_en <= '0';
      END IF;

      IF (d_valid_counter_en = '0') THEN
        d_valid_counter <= (OTHERS => '1');
      ELSIF (d_valid_counter_en <= '1') THEN
        d_valid_counter <= d_valid_counter - b"1";
      END IF;

      -- direct incoming adc data into four different register for parallelism to save and process it at slower clock rate
      IF (d1_strb_Q = '1') THEN
        d1_Q <= fir_data_in;
      END IF;

      IF (d2_strb_Q = '1') THEN
        d2_Q <= fir_data_in;
      END IF;

      IF (d3_strb_Q = '1') THEN
        d3_Q <= fir_data_in;
      END IF;

      IF (d4_strb_Q = '1') THEN
        d4_Q <= fir_data_in;
      END IF;

      d1_QQ <= d1_Q;
      d2_QQ <= d2_Q;
      d3_QQ <= d3_Q;
      -- d4_QQ <= d4_Q;

      d1_QQQ <= d1_QQ;
      d2_QQQ <= d2_QQ;
      -- d3_QQQ <= d3_QQ;
      -- d4_QQQ <= d4_QQ;
    END IF;
  END PROCESS p_adc_data;

  p_load_fifo : PROCESS (FPGA_CLK, RST_N)
  BEGIN
    IF (RST_N = '0') THEN
      adc_fifo_din(0) <= x"DEAD";
      adc_fifo_wr_en(0) <= '0';
      adc_fifo_din(1) <= x"BABE";
      adc_fifo_wr_en(1) <= '0';
      adc_fifo_din(2) <= x"BEEF";
      adc_fifo_wr_en(2) <= '0';
      adc_fifo_din(3) <= x"FABE";
      adc_fifo_wr_en(3) <= '0';
      -- adc_fifo_rd_en(2)  
      -- adc_fifo_dout(2)  

    ELSIF rising_edge(FPGA_CLK) THEN
      --default values 
      adc_fifo_wr_en(0) <= '0';
      adc_fifo_wr_en(1) <= '0';
      adc_fifo_wr_en(2) <= '0';
      adc_fifo_wr_en(3) <= '0';

      -- GENERATE WR STRB FOR FIR FIFO
      IF (d_strb_Q = '1') THEN
        adc_fifo_din(0) <= d1_QQQ;
        adc_fifo_wr_en(0) <= '1';

        adc_fifo_din(1) <= d2_QQQ;
        adc_fifo_wr_en(1) <= '1';

        adc_fifo_din(2) <= d3_QQ;
        adc_fifo_wr_en(2) <= '1';

        adc_fifo_din(3) <= d4_Q;
        adc_fifo_wr_en(3) <= '1';
      END IF;
    END IF;
  END PROCESS p_load_fifo;

  p_fetch_fifo : PROCESS (FPGA_CLK, RST_N)
  BEGIN
    IF (RST_N = '0') THEN
      adc_fifo_rd_en(0) <= '0';
      adc_fifo_rd_en(1) <= '0';
      adc_fifo_rd_en(2) <= '0';
      adc_fifo_rd_en(3) <= '0';
      d_read_counter <= (OTHERS => '1');
      d_read_counter_en <= '0';
      s_fir_data_in_Q <= (OTHERS => '0');
      s_fir_data_in_QQ <= (OTHERS => '0');
      d_out_Q <= ( others =>(OTHERS => '0'));
    ELSIF rising_edge(FPGA_CLK) THEN
      --default values 
      adc_fifo_rd_en(0) <= '0';
      adc_fifo_rd_en(1) <= '0';
      adc_fifo_rd_en(2) <= '0';
      adc_fifo_rd_en(3) <= '0';

      d_out_Q(0) <= adc_fifo_dout(0);
      d_out_Q(1) <= adc_fifo_dout(1);
      d_out_Q(2) <= adc_fifo_dout(2);
      d_out_Q(3) <= adc_fifo_dout(3);
      s_fir_data_in <= x"1";

      fifo_data_valid <= NOT (adc_fifo_empty(0) OR adc_fifo_empty(1) OR adc_fifo_empty(2) OR adc_fifo_empty(3));
      -- GENERATE rd STRB FOR FIR FIFO
      s_fir_data_in_Q <= s_fir_data_in;
      s_fir_data_in_QQ <= s_fir_data_in_Q;
      IF (d_read_counter = 0) THEN
        adc_fifo_rd_en(0) <= '1';
        adc_fifo_rd_en(1) <= '1';
        adc_fifo_rd_en(2) <= '1';
        adc_fifo_rd_en(3) <= '1';
      ELSIF (fifo_data_valid = '1') THEN
        d_read_counter_en <= '1';
        s_fir_data_in <= s_fir_data_in(2 DOWNTO 0) & b"0";
      ELSIF (fifo_data_valid = '0') THEN
        d_read_counter_en <= '0';
      END IF;

      IF (d_read_counter_en = '0') THEN
        d_read_counter <= (OTHERS => '1');
      ELSIF (d_read_counter_en = '1') THEN
        d_read_counter <= d_read_counter - b"1";
      END IF;

    END IF;
  END PROCESS p_fetch_fifo;

  p_upconv_data_sample : PROCESS (DAC_CLK, RST_N)
  BEGIN
    IF (RST_N = '0') THEN
      s_fir_data_out2 <= x"1";
    ELSIF rising_edge(DAC_CLK) THEN
      s_fir_data_out2 <= s_fir_data_out2(2 DOWNTO 0) & b"0";

      IF (s_fir_data_out2(0) = '1') THEN
        DAC_DATA_IN <= FIR_DATA_o(0);
      END IF;

      IF (s_fir_data_out2(1) = '1') THEN
        DAC_DATA_IN <= FIR_DATA_o(1);
      END IF;

      IF (s_fir_data_out2(2) = '1') THEN
        DAC_DATA_IN <= FIR_DATA_o(2);
      END IF;

      IF (s_fir_data_out2(3) = '1') THEN
        DAC_DATA_IN <= FIR_DATA_o(3);

        s_fir_data_out2 <= x"1";
      END IF;
    END IF;
  END PROCESS p_upconv_data_sample;

  LFSR_1 : ENTITY work.LFSR
    GENERIC MAP(
      g_Num_Bits => c_NUM_BITS)
    PORT MAP(
      i_Clk => adc_clk,
      i_Enable => i_Enable,
      i_Seed_DV => i_Seed_DV,
      i_Seed_Data => i_Seed_Data,
      o_LFSR_Data => fir_data_in,
      o_LFSR_Done => w_LFSR_Done
    );

  G_FIR_FILTER : FOR I IN 0 TO NUM_OF_TAPS - 1 GENERATE
    U_FIR_FILTER : FIR_FILTER
    GENERIC MAP(
      NUM_OF_TAPS => NUM_OF_TAPS,
      C_COEFS_LENGTH => C_COEFS_LENGTH,
      C_MULT_LENGTH => C_MULT_LENGTH,
      C_ADD_LENGTH => C_ADD_LENGTH,
      C_DATA_IN_LENGTH => C_DATA_IN_LENGTH,
      C_DATA_OUT_LENGTH => C_DATA_OUT_LENGTH)
    PORT MAP(
      i_clk => FPGA_CLK,
      i_rstb => RST_N,
      -- coefficient
      i_coeff_0 => C_FIR_COEFS(0),
      i_coeff_1 => C_FIR_COEFS(1),
      i_coeff_2 => C_FIR_COEFS(2),
      i_coeff_3 => C_FIR_COEFS(3),
      -- data input
      i_data => d_out_Q(I),
      -- filtered data 
      o_data => FIR_DATA_o(I)
    );
  END GENERATE G_FIR_FILTER;

END ARCHITECTURE behave;
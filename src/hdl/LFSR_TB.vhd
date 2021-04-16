
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

USE work.fir_filter_pkg.ALL;

LIBRARY unisim;

USE unisim.vcomponents.ALL;

ENTITY LFSR_TB IS
END ENTITY LFSR_TB;

ARCHITECTURE behave OF LFSR_TB IS

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

  CONSTANT c_NUM_BITS : INTEGER := 16;
  CONSTANT c_SEED_COUNTER_BITS : INTEGER := 16;
  CONSTANT c_CLK_PERIOD : TIME := 2 ns;
  SIGNAL fir_data_in : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL fir_data_out : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL LFSR_Data_Q : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL w_LFSR_Done : STD_LOGIC;
  SIGNAL i_Enable : STD_LOGIC;
  SIGNAL i_Seed_DV : STD_LOGIC;
  SIGNAL i_Seed_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL seed_counter : STD_LOGIC_VECTOR(c_SEED_COUNTER_BITS - 1 DOWNTO 0) := (OTHERS => '0');
  SIGNAL reset_counter : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');

  SIGNAL FPGA_CLK : STD_LOGIC := '0';
  SIGNAL ADC_CLK : STD_LOGIC;
  SIGNAL DAC_CLK : STD_LOGIC;
  SIGNAL rst_n : STD_LOGIC := '0';
  SIGNAL stop_reset_timer : STD_LOGIC;

  SIGNAL INT_FPGA_CLK_P : STD_LOGIC := '0';
  SIGNAL INT_FPGA_CLK_N : STD_LOGIC := '0';
  SIGNAL INT_ADC_CLK_P : STD_LOGIC := '0';
  SIGNAL INT_ADC_CLK_N : STD_LOGIC := '0';
  SIGNAL INT_DAC_CLK_P : STD_LOGIC := '0';
  SIGNAL INT_DAC_CLK_N : STD_LOGIC := '0';
  SIGNAL LFSR_DataArray_Q : COEFS_TYPE := (OTHERS => (OTHERS => '0'));

  SIGNAL MULT : MULT_TYPE;
  SIGNAL ADD : ADD_TYPE;

  -- ATTRIBUTE syn_multstyle : STRING;
  -- ATTRIBUTE syn_multstyle OF MULT : SIGNAL IS "logic";
BEGIN
  i_Seed_Data <= x"DEAD";

  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT
  adc_clk_ibug : unisim.vcomponents.IBUFDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    DQS_BIAS => "FALSE",
    IOSTANDARD => "DEFAULT"
  )
  PORT MAP(
    O => adc_clk,
    I => INT_ADC_CLK_P,
    IB => INT_ADC_CLK_N
  );
  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT
  dac_clk_ibug : unisim.vcomponents.IBUFDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    DQS_BIAS => "FALSE",
    IOSTANDARD => "DEFAULT"
  )
  PORT MAP(
    O => dac_clk,
    I => INT_DAC_CLK_P,
    IB => INT_DAC_CLK_N
  );
  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT
  fpga_clk_ibug : unisim.vcomponents.IBUFDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    DQS_BIAS => "FALSE",
    IOSTANDARD => "DEFAULT"
  )
  PORT MAP(
    O => fpga_clk,
    I => INT_FPGA_CLK_P,
    IB => INT_FPGA_CLK_N
  );


    INT_FPGA_CLK_P <= NOT INT_FPGA_CLK_P AFTER c_CLK_PERIOD/2;
    INT_FPGA_CLK_N <= NOT INT_FPGA_CLK_P;
    INT_ADC_CLK_P <= NOT INT_ADC_CLK_P AFTER c_CLK_PERIOD/8;
    INT_ADC_CLK_N <= NOT INT_ADC_CLK_P;
    INT_DAC_CLK_P <= NOT INT_DAC_CLK_P AFTER c_CLK_PERIOD/8;
    INT_DAC_CLK_N <= NOT INT_DAC_CLK_P;



  p_reset : PROCESS (adc_clk, stop_reset_timer)
  BEGIN
    IF rising_edge(adc_clk) THEN
      rst_n <= reset_counter(11);
      IF (stop_reset_timer = '0') THEN
        reset_counter <= reset_counter + b"1";
      END IF;
    END IF;
  END PROCESS p_reset;

  stop_reset_timer <= reset_counter(11);

  p_Seed : PROCESS (adc_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
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
  U_FIR_FILTER : FIR_FILTER
  GENERIC MAP(
    NUM_OF_TAPS => NUM_OF_TAPS,
    C_COEFS_LENGTH => C_COEFS_LENGTH,
    C_MULT_LENGTH => C_MULT_LENGTH,
    C_ADD_LENGTH => C_ADD_LENGTH,
    C_DATA_IN_LENGTH => C_DATA_IN_LENGTH,
    C_DATA_OUT_LENGTH => C_DATA_OUT_LENGTH)
  PORT MAP(
    i_clk => adc_clk,
    i_rstb => rst_n,
    -- coefficient
    i_coeff_0 => C_FIR_COEFS(0),
    i_coeff_1 => C_FIR_COEFS(1),
    i_coeff_2 => C_FIR_COEFS(2),
    i_coeff_3 => C_FIR_COEFS(3),
    -- data input
    i_data => fir_data_in,
    -- filtered data 
    o_data => fir_data_out
  );
  -- FIR_1 : PROCESS (adc_clk) BEGIN
  --   IF adc_clk'event AND adc_clk = '1' THEN
  --     LFSR_Data_Q <= fir_data_in;
  --     LFSR_DataArray_Q <= LFSR_DataArray_Q(NUM_OF_TAPS - 2 DOWNTO 0) & LFSR_Data_Q;
  --     FOR I IN NUM_OF_TAPS DOWNTO 0 LOOP
  --       MULT(I) <= LFSR_DataArray_Q(N - 1) * C_FIR_COEFS(NUM_OF_TAPS - I);
  --       IF I = 0 THEN
  --         ADD(I) <= ZERO + MULT(0);
  --       ELSE
  --         ADD(I) <= MULT(I) + ADD(I - 1);
  --       END IF;
  --     END LOOP;
  --     DOUT <= ADD(59);
  --   END IF;
  -- END PROCESS FIR_1;
END ARCHITECTURE behave;
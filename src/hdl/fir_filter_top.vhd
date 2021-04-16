
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

--USE work.fir_filter_pkg.all;

LIBRARY unisim;

USE unisim.vcomponents.ALL;

ENTITY fir_filter_top IS
    GENERIC (
        SIM_MODE : STRING := "TRUE";
        g_Num_Bits : INTEGER := 16
    );
    PORT (
        FPGA_CLK_P : IN STD_LOGIC;
        FPGA_CLK_N : IN STD_LOGIC;
        ADC_CLK_P : IN STD_LOGIC;
        ADC_CLK_N : IN STD_LOGIC;
        DAC_CLK_P : IN STD_LOGIC;
        DAC_CLK_N : IN STD_LOGIC
    );

END ENTITY fir_filter_top;
ARCHITECTURE behave OF fir_filter_top IS

    CONSTANT c_NUM_BITS : INTEGER := 16;
    CONSTANT c_SEED_COUNTER_BITS : INTEGER := 16;
    CONSTANT c_CLK_PERIOD : TIME :=2 ns;

    
    SIGNAL w_LFSR_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
    SIGNAL w_LFSR_Done : STD_LOGIC;
    SIGNAL i_Enable : STD_LOGIC;
    SIGNAL i_Seed_DV : STD_LOGIC;
    SIGNAL i_Seed_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
    SIGNAL seed_counter : STD_LOGIC_VECTOR(c_SEED_COUNTER_BITS - 1 DOWNTO 0) := (OTHERS => '0');

    SIGNAL FPGA_CLK : STD_LOGIC := '0';
    SIGNAL ADC_CLK : STD_LOGIC;
    SIGNAL DAC_CLK : STD_LOGIC;

    SIGNAL INT_FPGA_CLK_P : STD_LOGIC := '0';
    SIGNAL INT_FPGA_CLK_N : STD_LOGIC := '0';
    SIGNAL INT_ADC_CLK_P : STD_LOGIC := '0';
    SIGNAL INT_ADC_CLK_N : STD_LOGIC := '0';
    SIGNAL INT_DAC_CLK_P : STD_LOGIC := '0';
    SIGNAL INT_DAC_CLK_N : STD_LOGIC := '0';

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

--    G_SIM_MODE : IF SIM_MODE = "TRUE" GENERATE
        INT_FPGA_CLK_P <= NOT INT_FPGA_CLK_P AFTER c_CLK_PERIOD/2;
        INT_FPGA_CLK_N <= NOT INT_FPGA_CLK_P;
        INT_ADC_CLK_P  <= NOT INT_ADC_CLK_P AFTER c_CLK_PERIOD/8;
        INT_ADC_CLK_N  <= NOT INT_ADC_CLK_P;
        INT_DAC_CLK_P  <= NOT INT_DAC_CLK_P AFTER c_CLK_PERIOD/8;
        INT_DAC_CLK_N  <= NOT INT_DAC_CLK_P;
--    END GENERATE G_SIM_MODE;

--    G_REAL_MODE : IF SIM_MODE = "TRUE" GENERATE
--        INT_FPGA_CLK_P <= FPGA_CLK_P;
--        INT_FPGA_CLK_N <= FPGA_CLK_N;
--        INT_ADC_CLK_P <= ADC_CLK_P;
--        INT_ADC_CLK_N <= ADC_CLK_N;
--        INT_DAC_CLK_P <= DAC_CLK_P;
--        INT_DAC_CLK_N <= DAC_CLK_N;
--    END GENERATE G_REAL_MODE;

    p_Seed : PROCESS (fpga_clk)
    BEGIN
      IF rising_edge(fpga_clk) THEN
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
      i_Clk => FPGA_CLK,
      i_Enable => i_Enable,
      i_Seed_DV => i_Seed_DV,
      i_Seed_Data => i_Seed_Data,
      o_LFSR_Data => w_LFSR_Data,
      o_LFSR_Done => w_LFSR_Done
    );
END ARCHITECTURE behave;
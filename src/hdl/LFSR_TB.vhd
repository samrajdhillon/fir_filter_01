
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

--LIBRARY work
--use work.fir_filter_pkg.all;

ENTITY LFSR_TB IS
END ENTITY LFSR_TB;

ARCHITECTURE behave OF LFSR_TB IS

  CONSTANT c_NUM_BITS : INTEGER := 16;
  CONSTANT c_CLK_PERIOD : TIME := 0.5 ns; -- 2000 MHz
  CONSTANT c_SEED_COUNTER_BITS : INTEGER := 16;
  SIGNAL FPGA_CLK : STD_LOGIC := '0';
  SIGNAL w_LFSR_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL w_LFSR_Done : STD_LOGIC;
  SIGNAL i_Enable : STD_LOGIC := '0';
  SIGNAL i_Seed_DV : STD_LOGIC := '1';
  SIGNAL i_Seed_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL seed_counter : STD_LOGIC_VECTOR(c_SEED_COUNTER_BITS - 1 DOWNTO 0) := (OTHERS => '1');

BEGIN
  i_Seed_Data <= x"DEAD";
  FPGA_CLK <= NOT FPGA_CLK AFTER c_CLK_PERIOD/2;

  p_Seed : PROCESS (fpga_clk)
  BEGIN
    IF rising_edge(fpga_clk) THEN
      --default
      i_Seed_DV <= '0';
      i_Enable <= '1';

      -- enable seed
      IF (seed_counter = x"FFFF") THEN
        i_Seed_DV <= '1';
      END IF;

      -- seed counter
      IF (seed_counter = 0) THEN
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
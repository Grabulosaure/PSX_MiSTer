library IEEE;
use IEEE.std_logic_1164.all;  
use IEEE.numeric_std.all;     

library MEM;
use work.pProc_bus.all;
use work.pexport.all;

entity psx_top is
   generic
   (
      is_simu               : std_logic := '0'
   );
   port 
   (
      clk1x                 : in  std_logic;  
      clk2x                 : in  std_logic;   
      reset                 : in  std_logic; 
      -- commands 
      pause                 : in  std_logic;
      loadExe               : in  std_logic;
      fastboot              : in  std_logic;
      REPRODUCIBLEGPUTIMING : in  std_logic;
      REPRODUCIBLEDMATIMING : in  std_logic;
      DMABLOCKATONCE        : in  std_logic;
      INSTANTSEEK           : in  std_logic;
      FAKESPU               : in  std_logic;
      ditherOff             : in  std_logic;
      analogPad             : in  std_logic;
      fpscountOn            : in  std_logic;
      errorOn               : in  std_logic;
      -- RAM/BIOS interface      
      ram_refresh           : out std_logic;
      ram_dataWrite         : out std_logic_vector(31 downto 0);
      ram_dataRead          : in  std_logic_vector(127 downto 0);
      ram_Adr               : out std_logic_vector(22 downto 0);
      ram_be                : out std_logic_vector(3 downto 0) := (others => '0');
      ram_rnw               : out std_logic;
      ram_ena               : out std_logic;
      ram_128               : out std_logic;
      ram_done              : in  std_logic;
      ram_reqprocessed      : in  std_logic;
      ram_idle              : in  std_logic;
      -- vram/savestate interface
      ddr3_BUSY             : in  std_logic;                    
      ddr3_DOUT             : in  std_logic_vector(63 downto 0);
      ddr3_DOUT_READY       : in  std_logic;
      ddr3_BURSTCNT         : out std_logic_vector(7 downto 0) := (others => '0'); 
      ddr3_ADDR             : out std_logic_vector(27 downto 0) := (others => '0');                       
      ddr3_DIN              : out std_logic_vector(63 downto 0) := (others => '0');
      ddr3_BE               : out std_logic_vector(7 downto 0) := (others => '0'); 
      ddr3_WE               : out std_logic := '0';
      ddr3_RD               : out std_logic := '0'; 
      -- cd
      hasCD                 : in  std_logic;
      fastCD                : in  std_logic;
      cd_Size               : in  unsigned(29 downto 0);
      cd_req                : out std_logic := '0';
      cd_addr               : out std_logic_vector(26 downto 0) := (others => '0');
      cd_data               : in  std_logic_vector(31 downto 0);
      cd_done               : in  std_logic;
      cd_hps_on             : in  std_logic;
      cd_hps_req            : out std_logic := '0';
      cd_hps_lba            : out std_logic_vector(31 downto 0);
      cd_hps_ack            : in  std_logic;
      cd_hps_write          : in  std_logic;
      cd_hps_data           : in  std_logic_vector(15 downto 0);
      -- video
      videoout_on           : in  std_logic;
      isPal                 : in  std_logic;
      hsync                 : out std_logic;
      vsync                 : out std_logic;
      hblank                : out std_logic;
      vblank                : out std_logic;
      DisplayWidth          : out unsigned( 9 downto 0);
      DisplayHeight         : out unsigned( 8 downto 0);
      DisplayOffsetX        : out unsigned( 9 downto 0);
      DisplayOffsetY        : out unsigned( 8 downto 0);
      video_ce              : out std_logic;
      video_interlace       : out std_logic;
      video_r               : out std_logic_vector(7 downto 0);
      video_g               : out std_logic_vector(7 downto 0);
      video_b               : out std_logic_vector(7 downto 0);
      -- Keys - all active high   
      KeyTriangle           : in  std_logic_vector(1 downto 0); 
      KeyCircle             : in  std_logic_vector(1 downto 0); 
      KeyCross              : in  std_logic_vector(1 downto 0); 
      KeySquare             : in  std_logic_vector(1 downto 0);
      KeySelect             : in  std_logic_vector(1 downto 0);
      KeyStart              : in  std_logic_vector(1 downto 0);
      KeyRight              : in  std_logic_vector(1 downto 0);
      KeyLeft               : in  std_logic_vector(1 downto 0);
      KeyUp                 : in  std_logic_vector(1 downto 0);
      KeyDown               : in  std_logic_vector(1 downto 0);
      KeyR1                 : in  std_logic_vector(1 downto 0);
      KeyR2                 : in  std_logic_vector(1 downto 0);
      KeyR3                 : in  std_logic_vector(1 downto 0);
      KeyL1                 : in  std_logic_vector(1 downto 0);
      KeyL2                 : in  std_logic_vector(1 downto 0);
      KeyL3                 : in  std_logic_vector(1 downto 0);
      Analog1XP1            : in  signed(7 downto 0);
      Analog1YP1            : in  signed(7 downto 0);
      Analog2XP1            : in  signed(7 downto 0);
      Analog2YP1            : in  signed(7 downto 0);         
      Analog1XP2            : in  signed(7 downto 0);
      Analog1YP2            : in  signed(7 downto 0);
      Analog2XP2            : in  signed(7 downto 0);
      Analog2YP2            : in  signed(7 downto 0);              
      -- sound                            
      sound_out_left        : out std_logic_vector(15 downto 0) := (others => '0');
      sound_out_right       : out std_logic_vector(15 downto 0) := (others => '0');
       -- savestates
      increaseSSHeaderCount : in  std_logic;
      save_state            : in  std_logic;
      load_state            : in  std_logic;
      savestate_number      : in  integer range 0 to 3;
      state_loaded          : out std_logic;
      rewind_on             : in  std_logic;
      rewind_active         : in  std_logic
   );
end entity;

architecture arch of psx_top is

   signal reset_in               : std_logic := '0';
   signal reset_intern           : std_logic := '0';
   signal reset_exe              : std_logic;
   
   signal ce                     : std_logic := '0';
   signal clk1xToggle            : std_logic := '0';
   signal clk1xToggle2X          : std_logic := '0';
   signal clk2xIndex             : std_logic := '0';
   
   signal pausing                : std_logic := '0';
   
   -- ddr3 arbiter
   type tddr3State is
   (
      ARBITERIDLE,
      WAITGPUPAUSED,
      REQUEST,
      WAITDONE
   );
   signal ddr3state              : tddr3State := ARBITERIDLE;
   
   signal arbiter_active         : std_logic := '0';
   
   signal memcard1_acknext       : std_logic := '0';
   signal memcard2_acknext       : std_logic := '0';
   
   signal arbiter_BURSTCNT       : std_logic_vector(7 downto 0) := (others => '0'); 
   signal arbiter_ADDR           : std_logic_vector(27 downto 0) := (others => '0');                       
   signal arbiter_DIN            : std_logic_vector(63 downto 0) := (others => '0');
   signal arbiter_BE             : std_logic_vector(7 downto 0) := (others => '0'); 
   signal arbiter_WE             : std_logic := '0';
   signal arbiter_RD             : std_logic := '0';
   
   signal memcard1_request       : std_logic;
   signal memcard1_ack           : std_logic := '0';
   signal memcard1_BURSTCNT      : std_logic_vector(7 downto 0) := (others => '0'); 
   signal memcard1_ADDR          : std_logic_vector(19 downto 0) := (others => '0');                       
   signal memcard1_DIN           : std_logic_vector(63 downto 0) := (others => '0');
   signal memcard1_BE            : std_logic_vector(7 downto 0) := (others => '0'); 
   signal memcard1_WE            : std_logic := '0';
   signal memcard1_RD            : std_logic := '0';
   
   signal memcard2_request       : std_logic;
   signal memcard2_ack           : std_logic := '0';
   signal memcard2_BURSTCNT      : std_logic_vector(7 downto 0) := (others => '0'); 
   signal memcard2_ADDR          : std_logic_vector(19 downto 0) := (others => '0');                       
   signal memcard2_DIN           : std_logic_vector(63 downto 0) := (others => '0');
   signal memcard2_BE            : std_logic_vector(7 downto 0) := (others => '0'); 
   signal memcard2_WE            : std_logic := '0';
   signal memcard2_RD            : std_logic := '0';

   -- Busses
   --signal bus_exp1_addr          : unsigned(22 downto 0); 
   --signal bus_exp1_dataWrite     : std_logic_vector(31 downto 0);
   signal bus_exp1_read          : std_logic;
   --signal bus_exp1_write         : std_logic;
   signal bus_exp1_dataRead      : std_logic_vector(31 downto 0);
   
   signal bus_memc_addr          : unsigned(5 downto 0); 
   signal bus_memc_dataWrite     : std_logic_vector(31 downto 0);
   signal bus_memc_read          : std_logic;
   signal bus_memc_write         : std_logic;
   signal bus_memc_dataRead      : std_logic_vector(31 downto 0);
   
   signal bus_pad_addr           : unsigned(3 downto 0); 
   signal bus_pad_dataWrite      : std_logic_vector(31 downto 0);
   signal bus_pad_read           : std_logic;
   signal bus_pad_write          : std_logic;
   signal bus_pad_writeMask      : std_logic_vector(3 downto 0);
   signal bus_pad_dataRead       : std_logic_vector(31 downto 0);   
   
   signal bus_sio_addr           : unsigned(3 downto 0); 
   signal bus_sio_dataWrite      : std_logic_vector(31 downto 0);
   signal bus_sio_read           : std_logic;
   signal bus_sio_write          : std_logic;
   signal bus_sio_writeMask      : std_logic_vector(3 downto 0);
   signal bus_sio_dataRead       : std_logic_vector(31 downto 0);
   
   signal bus_memc2_addr         : unsigned(3 downto 0); 
   signal bus_memc2_dataWrite    : std_logic_vector(31 downto 0);
   signal bus_memc2_read         : std_logic;
   signal bus_memc2_write        : std_logic;
   signal bus_memc2_dataRead     : std_logic_vector(31 downto 0);
   
   signal bus_irq_addr           : unsigned(3 downto 0); 
   signal bus_irq_dataWrite      : std_logic_vector(31 downto 0);
   signal bus_irq_read           : std_logic;
   signal bus_irq_write          : std_logic;
   signal bus_irq_dataRead       : std_logic_vector(31 downto 0);   
   
   signal bus_dma_addr           : unsigned(6 downto 0); 
   signal bus_dma_dataWrite      : std_logic_vector(31 downto 0);
   signal bus_dma_read           : std_logic;
   signal bus_dma_write          : std_logic;
   signal bus_dma_dataRead       : std_logic_vector(31 downto 0);
   
   signal bus_tmr_addr           : unsigned(5 downto 0); 
   signal bus_tmr_dataWrite      : std_logic_vector(31 downto 0);
   signal bus_tmr_read           : std_logic;
   signal bus_tmr_write          : std_logic;
   signal bus_tmr_dataRead       : std_logic_vector(31 downto 0);
   
   signal bus_cd_addr            : unsigned(3 downto 0); 
   signal bus_cd_dataWrite       : std_logic_vector(7 downto 0);
   signal bus_cd_read            : std_logic;
   signal bus_cd_write           : std_logic;
   signal bus_cd_dataRead        : std_logic_vector(7 downto 0);
   
   signal bus_gpu_addr           : unsigned(3 downto 0); 
   signal bus_gpu_dataWrite      : std_logic_vector(31 downto 0);
   signal bus_gpu_read           : std_logic;
   signal bus_gpu_write          : std_logic;
   signal bus_gpu_dataRead       : std_logic_vector(31 downto 0);
   
   signal bus_mdec_addr          : unsigned(3 downto 0); 
   signal bus_mdec_dataWrite     : std_logic_vector(31 downto 0);
   signal bus_mdec_read          : std_logic;
   signal bus_mdec_write         : std_logic;
   signal bus_mdec_dataRead      : std_logic_vector(31 downto 0);
   
   signal bus_spu_addr           : unsigned(9 downto 0); 
   signal bus_spu_dataWrite      : std_logic_vector(15 downto 0);
   signal bus_spu_read           : std_logic;
   signal bus_spu_write          : std_logic;
   signal bus_spu_dataRead       : std_logic_vector(15 downto 0);
   
   signal bus_exp2_addr          : unsigned(12 downto 0); 
   signal bus_exp2_dataWrite     : std_logic_vector(31 downto 0);
   signal bus_exp2_read          : std_logic;
   signal bus_exp2_write         : std_logic;
   signal bus_exp2_dataRead      : std_logic_vector(31 downto 0);
   signal bus_exp2_writeMask     : std_logic_vector(3 downto 0);   
   
   signal bus_exp3_dataWrite     : std_logic_vector(31 downto 0);
   signal bus_exp3_read          : std_logic;
   signal bus_exp3_write         : std_logic;
   signal bus_exp3_dataRead      : std_logic_vector(31 downto 0);
   signal bus_exp3_writeMask     : std_logic_vector(3 downto 0);
   
   -- Memory mux
   signal memMuxIdle             : std_logic;
   
   signal mem_request            : std_logic;
   signal mem_rnw                : std_logic; 
   signal mem_isData             : std_logic; 
   signal mem_isCache            : std_logic; 
   signal mem_addressInstr       : unsigned(31 downto 0); 
   signal mem_addressData        : unsigned(31 downto 0); 
   signal mem_reqsize            : unsigned(1 downto 0); 
   signal mem_writeMask          : std_logic_vector(3 downto 0);
   signal mem_dataWrite          : std_logic_vector(31 downto 0); 
   signal mem_dataRead           : std_logic_vector(31 downto 0); 
   signal mem_dataCache          : std_logic_vector(127 downto 0); 
   signal mem_done               : std_logic;
   
   signal ram_next_dma           : std_logic;
   signal ram_next_cpu           : std_logic;
   
   signal ram_cpu_dataWrite      : std_logic_vector(31 downto 0);
   signal ram_cpu_Adr            : std_logic_vector(22 downto 0);
   signal ram_cpu_be             : std_logic_vector(3 downto 0);
   signal ram_cpu_rnw            : std_logic;
   signal ram_cpu_ena            : std_logic;
   signal ram_cpu_128            : std_logic;
   signal ram_cpu_done           : std_logic;
   
   -- gpu
   signal hblank_intern          : std_logic;
   signal vblank_intern          : std_logic;
   signal hblank_tmr             : std_logic;
   
   signal vram_pause             : std_logic; 
   signal vram_paused            : std_logic; 
   signal vram_BURSTCNT          : std_logic_vector(7 downto 0) := (others => '0'); 
   signal vram_ADDR              : std_logic_vector(19 downto 0) := (others => '0');                       
   signal vram_DIN               : std_logic_vector(63 downto 0) := (others => '0');
   signal vram_BE                : std_logic_vector(7 downto 0) := (others => '0'); 
   signal vram_WE                : std_logic := '0';
   signal vram_RD                : std_logic := '0'; 
   
   -- irq
   signal irqRequest             : std_logic;
   signal irq_VBLANK             : std_logic;
   signal irq_GPU                : std_logic;
   signal irq_CDROM              : std_logic;
   signal irq_DMA                : std_logic;
   signal irq_TIMER0             : std_logic;
   signal irq_TIMER1             : std_logic;
   signal irq_TIMER2             : std_logic;
   signal irq_PAD                : std_logic;
   signal irq_SIO                : std_logic;
   signal irq_SPU                : std_logic;
   signal irq_LIGHTPEN           : std_logic;
   
   signal enaSPUirq              : std_logic;
   
   -- dma
   signal cpuPaused              : std_logic;
   signal dmaOn                  : std_logic;
   
   signal ram_refresh_dma        : std_logic;
   signal ram_dma_dataWrite      : std_logic_vector(31 downto 0);
   signal ram_dma_Adr            : std_logic_vector(22 downto 0);
   signal ram_dma_be             : std_logic_vector(3 downto 0);
   signal ram_dma_rnw            : std_logic;
   signal ram_dma_ena            : std_logic;
   signal ram_dma_128            : std_logic;
   signal ram_dma_done           : std_logic;
   
   signal gpu_dmaRequest         : std_logic;
   signal DMA_GPU_waiting        : std_logic;
   signal DMA_GPU_writeEna       : std_logic;
   signal DMA_GPU_readEna        : std_logic;
   signal DMA_GPU_write          : std_logic_vector(31 downto 0);
   signal DMA_GPU_read           : std_logic_vector(31 downto 0);
   
   signal mdec_dmaWriteRequest   : std_logic;
   signal mdec_dmaReadRequest    : std_logic;
   signal DMA_MDEC_writeEna      : std_logic := '0';
   signal DMA_MDEC_readEna       : std_logic := '0';
   signal DMA_MDEC_write         : std_logic_vector(31 downto 0);
   signal DMA_MDEC_read          : std_logic_vector(31 downto 0);
   
   signal DMA_CD_readEna         : std_logic;
   signal DMA_CD_read            : std_logic_vector(7 downto 0);
   
   signal spu_dmaRequest         : std_logic;
   signal DMA_SPU_writeEna       : std_logic := '0';
   signal DMA_SPU_readEna        : std_logic := '0';
   signal DMA_SPU_write          : std_logic_vector(15 downto 0);
   signal DMA_SPU_read           : std_logic_vector(15 downto 0);
   
   -- cpu
   signal ce_intern              : std_logic := '0';
   signal ce_cpu                 : std_logic := '0';
   signal stallNext              : std_logic;
   
   -- GTE
   signal gte_busy               : std_logic;
   signal gte_readEna            : std_logic;
   signal gte_readAddr           : unsigned(5 downto 0);
   signal gte_readData           : unsigned(31 downto 0);
   signal gte_writeAddr          : unsigned(5 downto 0);
   signal gte_writeData          : unsigned(31 downto 0);
   signal gte_writeEna           : std_logic; 
   signal gte_cmdData            : unsigned(31 downto 0);
   signal gte_cmdEna             : std_logic; 

   -- overlay + error codes
   signal cdSlow                 : std_logic;
   signal errorEna               : std_logic;
   signal errorCode              : unsigned(3 downto 0);
   
   signal errorCD                : std_logic;
   signal errorCPU               : std_logic;
   signal errorLINE              : std_logic;
   signal errorRECT              : std_logic;
   signal errorPOLY              : std_logic;
   signal errorGPU               : std_logic;

   -- savestates
   signal loading_savestate      : std_logic;
   signal sleep_savestate        : std_logic;
   signal sleep_rewind           : std_logic;
   signal savestate_pause        : std_logic;
   signal ddr3_savestate         : std_logic;
   
   signal SS_reset               : std_logic;
   
   signal savestate_savestate    : std_logic; 
   signal savestate_loadstate    : std_logic; 
   signal savestate_address      : integer; 
   signal savestate_busy         : std_logic; 
   
   signal SS_DataWrite           : std_logic_vector(31 downto 0);
   signal SS_Adr                 : unsigned(18 downto 0);
   signal SS_wren                : std_logic_vector(16 downto 0);
   signal SS_rden                : std_logic_vector(16 downto 0);
   signal SS_DataRead_CPU        : std_logic_vector(31 downto 0);
   signal SS_DataRead_GPU        : std_logic_vector(31 downto 0);
   signal SS_DataRead_GPUTiming  : std_logic_vector(31 downto 0);
   signal SS_DataRead_DMA        : std_logic_vector(31 downto 0);
   signal SS_DataRead_GTE        : std_logic_vector(31 downto 0);
   signal SS_DataRead_JOYPAD     : std_logic_vector(31 downto 0);
   signal SS_DataRead_MDEC       : std_logic_vector(31 downto 0);
   signal SS_DataRead_MEMORY     : std_logic_vector(31 downto 0);
   signal SS_DataRead_TIMER      : std_logic_vector(31 downto 0);
   signal SS_DataRead_SOUND      : std_logic_vector(31 downto 0);
   signal SS_DataRead_IRQ        : std_logic_vector(31 downto 0);
   signal SS_DataRead_SIO        : std_logic_vector(31 downto 0);
   signal SS_DataRead_SCP        : std_logic_vector(31 downto 0);
   signal SS_DataRead_CD         : std_logic_vector(31 downto 0);
   
   signal ss_ram_BUSY            : std_logic;                    
   signal ss_ram_DOUT            : std_logic_vector(63 downto 0);
   signal ss_ram_DOUT_READY      : std_logic;
   signal ss_ram_BURSTCNT        : std_logic_vector(7 downto 0) := (others => '0'); 
   signal ss_ram_ADDR            : std_logic_vector(25 downto 0) := (others => '0');                       
   signal ss_ram_DIN             : std_logic_vector(63 downto 0) := (others => '0');
   signal ss_ram_BE              : std_logic_vector(7 downto 0) := (others => '0'); 
   signal ss_ram_WE              : std_logic := '0';
   signal ss_ram_RD              : std_logic := '0'; 
   
   signal SS_Idle                : std_logic; 
   signal SS_Idle_gpu            : std_logic; 
   signal SS_Idle_mdec           : std_logic; 
   signal SS_Idle_cd             : std_logic; 
   signal SS_Idle_spu            : std_logic; 
   signal SS_idle_pad            : std_logic; 
   signal SS_idle_irq            : std_logic; 
   signal SS_idle_cpu            : std_logic; 
   signal SS_idle_gte            : std_logic; 
   signal SS_idle_dma            : std_logic; 

-- synthesis translate_off
   -- export
   signal cpu_done               : std_logic; 
   signal new_export             : std_logic; 
   signal cpu_export             : cpu_export_type;
   signal export_8               : std_logic_vector(7 downto 0);
   signal export_16              : std_logic_vector(15 downto 0);
   signal export_32              : std_logic_vector(31 downto 0);
   signal export_irq             : unsigned(15 downto 0);
   signal export_gtm             : unsigned(11 downto 0);
   signal export_line            : unsigned(11 downto 0);
   signal export_gpus            : unsigned(31 downto 0);
   signal export_gobj            : unsigned(15 downto 0);
   signal export_t_current0      : unsigned(15 downto 0);
   signal export_t_current1      : unsigned(15 downto 0);
   signal export_t_current2      : unsigned(15 downto 0);
-- synthesis translate_on
   
   signal debug_firstGTE         : std_logic;
   
begin 

   sound_out_left   <= (others => '0');
   sound_out_right  <= (others => '0');
   
   -- reset
   process (clk1x)
   begin
      if rising_edge(clk1x) then
         reset_in <= reset or reset_exe;
      end if;
   end process;
   

   -- clock index
   process (clk1x)
   begin
      if rising_edge(clk1x) then
         clk1xToggle <= not clk1xToggle;
      end if;
   end process;
   
   process (clk2x)
   begin
      if rising_edge(clk2x) then
         clk1xToggle2x <= clk1xToggle;
         clk2xIndex    <= '0';
         if (clk1xToggle2x = clk1xToggle) then
            clk2xIndex <= '1';
         end if;
      end if;
   end process;

   -- busses
   process (clk1x)
   begin
      if rising_edge(clk1x) then
      
         bus_exp1_dataRead <= (others => '0');
         if (bus_exp1_read = '1') then
            bus_exp1_dataRead <= (others => '1');
         end if;
      
         bus_exp3_dataRead <= (others => '0');
         if (bus_exp3_read = '1') then
            bus_exp3_dataRead <= (others => '1');
         end if;
      
      end if;
   end process;
 
   SS_idle <= SS_Idle_gpu and SS_Idle_mdec and SS_Idle_cd and SS_idle_spu and SS_idle_pad and SS_idle_irq and SS_idle_cpu and SS_idle_gte and SS_idle_dma;
   
   -- ce generation
   process (clk1x)
   begin
      if rising_edge(clk1x) then
      
         if (reset = '1' or pausing = '1' or sleep_savestate = '1' or sleep_rewind = '1') then
         
            ce        <= '0';
            ce_cpu    <= '0';
            if (reset_intern = '1') then
               cpuPaused <= '0';
            end if;
            
            if (pause = '1') then
               pausing <= '1';
            end if;
            
            if (pause = '0' and savestate_pause = '0') then
               pausing <= '0';
            end if;
         
         else
      
            ce        <= '1';
            ce_cpu    <= '1';
         
            if (reset_intern = '1') then
               cpuPaused <= '0';
            else
         
               if ((pause = '1' or savestate_pause = '1') and cpuPaused = '0' and dmaOn = '0' and stallNext = '0' and memMuxIdle = '1' and SS_idle = '1') then -- switch to pause/savestate pausing
                  pausing <= '1';
                  ce      <= '0';
                  ce_cpu  <= '0';
               elsif ((cpuPaused = '1' and dmaOn = '1') or (dmaOn = '1' and memMuxIdle = '1' and stallNext = '0')) then -- switch to dma
                  cpuPaused <= '1';
                  ce_cpu    <= '0';
               elsif (dmaOn = '0') then -- switch to CPU
                  cpuPaused <= '0';
                  ce_cpu    <= '1';
               end if;
               
            end if;
            
         end if;   
         
         if (reset_in = '1') then
            pausing <= '0';
         end if;
         
      end if;
   end process;
   
   -- error codes
   process (clk1x)
   begin
      if rising_edge(clk1x) then
         if (reset_intern = '1') then
            errorEna  <= '0';
            errorCode <= x"0";
         else
         
            if (errorEna = '0') then
               if (errorCD   = '1') then errorEna  <= '1'; errorCode <= x"1"; end if;
               if (errorCPU  = '1') then errorEna  <= '1'; errorCode <= x"2"; end if;
               if (errorGPU  = '1') then errorEna  <= '1'; errorCode <= x"3"; end if;
            end if;
            
            if (errorEna = '0' or errorCode = x"3") then
               if (errorLINE = '1') then errorEna  <= '1'; errorCode <= x"4"; end if;
               if (errorRECT = '1') then errorEna  <= '1'; errorCode <= x"5"; end if;
               if (errorPOLY = '1') then errorEna  <= '1'; errorCode <= x"6"; end if;
            end if;
            
         end if;
      end if;
   end process;
   
   -- DDR3 arbiter
   process (clk2x)
   begin
      if rising_edge(clk2x) then
      
         memcard1_ack    <= '0';
         memcard2_ack    <= '0';
      
         if (reset_intern = '1') then
            arbiter_active    <= '0';
            vram_pause        <= '0';
            ddr3state         <= ARBITERIDLE;
            memcard1_acknext  <= '0';
            memcard2_acknext  <= '0';
         else
         
            case (ddr3state) is
            
               when ARBITERIDLE =>
                  memcard1_acknext  <= '0';
                  memcard2_acknext  <= '0';
                  if (memcard1_request = '1' or memcard2_request = '1') then
                     vram_pause <= '1';
                     ddr3state  <= WAITGPUPAUSED;
                  end if;
                  
               when WAITGPUPAUSED =>
                  if (vram_paused = '1') then
                     ddr3state      <= REQUEST; 
                     arbiter_active <= '1';
                     if (memcard1_request = '1') then
                        memcard1_acknext <= '1';
                        arbiter_BURSTCNT <= memcard1_BURSTCNT;
                        arbiter_ADDR     <= x"01" & memcard1_ADDR;    
                        arbiter_DIN      <= memcard1_DIN;     
                        arbiter_BE       <= memcard1_BE;      
                        arbiter_WE       <= memcard1_WE;      
                        arbiter_RD       <= memcard1_RD;
                     elsif (memcard2_request = '1') then
                        memcard2_acknext <= '1';
                        arbiter_BURSTCNT <= memcard2_BURSTCNT;
                        arbiter_ADDR     <= x"02" & memcard2_ADDR;    
                        arbiter_DIN      <= memcard2_DIN;     
                        arbiter_BE       <= memcard2_BE;      
                        arbiter_WE       <= memcard2_WE;      
                        arbiter_RD       <= memcard2_RD;
                     end if;
                  end if;
               
               when REQUEST =>
                  if (ddr3_BUSY = '0') then
                     ddr3state  <= WAITDONE; 
                     arbiter_WE <= '0';     
                     arbiter_RD <= '0';
                     if (memcard1_acknext = '1') then memcard1_ack <= '1'; end if;
                     if (memcard2_acknext = '1') then memcard2_ack <= '1'; end if;
                  end if;
               
               when WAITDONE =>
                  if ((memcard1_request and memcard1_acknext) = '0' and (memcard2_request and memcard2_acknext) = '0') then
                     ddr3state      <= ARBITERIDLE;
                     arbiter_active <= '0';
                     vram_pause     <= '0';
                  end if;
               
            end case;
         end if;
      end if;
   end process;
   
   
   imemctrl : entity work.memctrl
   port map
   (
      clk1x                => clk1x,
      ce                   => ce,   
      reset                => reset_intern,

      bus_addr             => bus_memc_addr,     
      bus_dataWrite        => bus_memc_dataWrite,
      bus_read             => bus_memc_read,     
      bus_write            => bus_memc_write,    
      bus_dataRead         => bus_memc_dataRead,      
      
      bus2_addr            => bus_memc2_addr,     
      bus2_dataWrite       => bus_memc2_dataWrite,
      bus2_read            => bus_memc2_read,     
      bus2_write           => bus_memc2_write,    
      bus2_dataRead        => bus_memc2_dataRead,

      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(4 downto 0),      
      SS_wren              => SS_wren(7),     
      SS_rden              => SS_rden(7),     
      SS_DataRead          => SS_DataRead_MEMORY      
   );

   ijoypad: entity work.joypad
   port map 
   (
      clk1x                => clk1x,
      clk2x                => clk2x,
      clk2xIndex           => clk2xIndex,
      ce                   => ce,   
      reset                => reset_intern,
      
      analogPad            => analogPad,
      
      irqRequest           => irq_PAD,
      
      KeyTriangle          => KeyTriangle,           
      KeyCircle            => KeyCircle,           
      KeyCross             => KeyCross,           
      KeySquare            => KeySquare,           
      KeySelect            => KeySelect,      
      KeyStart             => KeyStart,       
      KeyRight             => KeyRight,       
      KeyLeft              => KeyLeft,        
      KeyUp                => KeyUp,          
      KeyDown              => KeyDown,        
      KeyR1                => KeyR1,           
      KeyR2                => KeyR2,           
      KeyR3                => KeyR3,           
      KeyL1                => KeyL1,           
      KeyL2                => KeyL2,           
      KeyL3                => KeyL3,           
      Analog1XP1           => Analog1XP1,       
      Analog1YP1           => Analog1YP1,       
      Analog2XP1           => Analog2XP1,       
      Analog2YP1           => Analog2YP1,
      Analog1XP2           => Analog1XP2,
      Analog1YP2           => Analog1YP2,
      Analog2XP2           => Analog2XP2,
      Analog2YP2           => Analog2YP2,
      
      mem1_request         => memcard1_request,   
      mem1_BURSTCNT        => memcard1_BURSTCNT,  
      mem1_ADDR            => memcard1_ADDR,      
      mem1_DIN             => memcard1_DIN,       
      mem1_BE              => memcard1_BE,        
      mem1_WE              => memcard1_WE,        
      mem1_RD              => memcard1_RD,       
      mem1_ack             => memcard1_ack,       
      
      mem2_request         => memcard2_request,   
      mem2_BURSTCNT        => memcard2_BURSTCNT,  
      mem2_ADDR            => memcard2_ADDR,      
      mem2_DIN             => memcard2_DIN,       
      mem2_BE              => memcard2_BE,        
      mem2_WE              => memcard2_WE,        
      mem2_RD              => memcard2_RD,       
      mem2_ack             => memcard2_ack,  
      
      mem_DOUT             => ddr3_DOUT,      
      mem_DOUT_READY       => ddr3_DOUT_READY,
      
      bus_addr             => bus_pad_addr,     
      bus_dataWrite        => bus_pad_dataWrite,
      bus_read             => bus_pad_read,     
      bus_write            => bus_pad_write,    
      bus_writeMask        => bus_pad_writeMask,   
      bus_dataRead         => bus_pad_dataRead,
      
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(2 downto 0),      
      SS_wren              => SS_wren(5),     
      SS_rden              => SS_rden(5),     
      SS_DataRead          => SS_DataRead_JOYPAD,
      SS_idle              => SS_idle_pad
   );
   
   isio : entity work.sio
   port map
   (
      clk1x                => clk1x,
      ce                   => ce,   
      reset                => reset_intern,
      
      bus_addr             => bus_sio_addr,     
      bus_dataWrite        => bus_sio_dataWrite,
      bus_read             => bus_sio_read,     
      bus_write            => bus_sio_write,    
      bus_writeMask        => bus_sio_writeMask,
      bus_dataRead         => bus_sio_dataRead,
      
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(2 downto 0),      
      SS_wren              => SS_wren(11),     
      SS_rden              => SS_rden(11),     
      SS_DataRead          => SS_DataRead_SIO
   );
   
   irq_SIO       <= '0'; -- todo
   irq_LIGHTPEN  <= '0'; -- todo

   iirq : entity work.irq
   port map
   (
      clk1x                => clk1x,
      ce                   => ce,   
      reset                => reset_intern,
      
      irq_VBLANK           => irq_VBLANK,
      irq_GPU              => irq_GPU,     
      irq_CDROM            => irq_CDROM,   
      irq_DMA              => irq_DMA,     
      irq_TIMER0           => irq_TIMER0,  
      irq_TIMER1           => irq_TIMER1,  
      irq_TIMER2           => irq_TIMER2,  
      irq_PAD              => irq_PAD,     
      irq_SIO              => irq_SIO,     
      irq_SPU              => irq_SPU,     
      irq_LIGHTPEN         => irq_LIGHTPEN,
      
      bus_addr             => bus_irq_addr,     
      bus_dataWrite        => bus_irq_dataWrite,
      bus_read             => bus_irq_read,     
      bus_write            => bus_irq_write,    
      bus_dataRead         => bus_irq_dataRead,
      
      irqRequest           => irqRequest,

-- synthesis translate_off
      export_irq           => export_irq,
-- synthesis translate_on
      
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(0 downto 0),      
      SS_wren              => SS_wren(10),     
      SS_rden              => SS_rden(10),     
      SS_DataRead          => SS_DataRead_IRQ,
      SS_idle              => SS_idle_irq
   );
   
   idma : entity work.dma
   port map
   (
      clk1x                => clk1x,
      ce                   => ce,   
      reset                => reset_intern,
      
      REPRODUCIBLEDMATIMING=> REPRODUCIBLEDMATIMING,
      DMABLOCKATONCE       => DMABLOCKATONCE,
      
      cpuPaused            => cpuPaused,
      dmaOn                => dmaOn,
      irqOut               => irq_DMA,
      
      ram_refresh          => ram_refresh_dma,  
      ram_dataWrite        => ram_dma_dataWrite,
      ram_dataRead         => ram_dataRead, 
      ram_Adr              => ram_dma_Adr,      
      ram_be               => ram_dma_be,       
      ram_rnw              => ram_dma_rnw,      
      ram_ena              => ram_dma_ena,      
      ram_128              => ram_dma_128,      
      ram_done             => ram_dma_done, 
      ram_reqprocessed     => ram_reqprocessed,
      ram_idle             => ram_idle,
      
      gpu_dmaRequest       => gpu_dmaRequest,  
      DMA_GPU_waiting      => DMA_GPU_waiting,
      DMA_GPU_writeEna     => DMA_GPU_writeEna,
      DMA_GPU_readEna      => DMA_GPU_readEna, 
      DMA_GPU_write        => DMA_GPU_write,   
      DMA_GPU_read         => DMA_GPU_read,   
      
      mdec_dmaWriteRequest => mdec_dmaWriteRequest,
      mdec_dmaReadRequest  => mdec_dmaReadRequest, 
      DMA_MDEC_writeEna    => DMA_MDEC_writeEna,   
      DMA_MDEC_readEna     => DMA_MDEC_readEna,    
      DMA_MDEC_write       => DMA_MDEC_write,      
      DMA_MDEC_read        => DMA_MDEC_read,   

      DMA_CD_readEna       => DMA_CD_readEna,
      DMA_CD_read          => DMA_CD_read,   
      
      spu_dmaRequest       => spu_dmaRequest, 
      DMA_SPU_writeEna     => DMA_SPU_writeEna,   
      DMA_SPU_readEna      => DMA_SPU_readEna,    
      DMA_SPU_write        => DMA_SPU_write,    
      DMA_SPU_read         => DMA_SPU_read,
      
      bus_addr             => bus_dma_addr,     
      bus_dataWrite        => bus_dma_dataWrite,
      bus_read             => bus_dma_read,     
      bus_write            => bus_dma_write,    
      bus_dataRead         => bus_dma_dataRead,
      
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(5 downto 0),      
      SS_wren              => SS_wren(3),     
      SS_rden              => SS_rden(3),     
      SS_DataRead          => SS_DataRead_DMA,
      SS_idle              => SS_idle_dma
   );
   
   ram_refresh   <= ram_refresh_dma;
   
   ram_dataWrite <= ram_dma_dataWrite when (cpuPaused = '1') else ram_cpu_dataWrite;
   ram_Adr       <= ram_dma_Adr       when (cpuPaused = '1') else ram_cpu_Adr;      
   ram_be        <= ram_dma_be        when (cpuPaused = '1') else ram_cpu_be;       
   ram_rnw       <= ram_dma_rnw       when (cpuPaused = '1') else ram_cpu_rnw;      
   ram_ena       <= ram_dma_ena       when (cpuPaused = '1') else ram_cpu_ena;      
   ram_128       <= ram_dma_128       when (cpuPaused = '1') else ram_cpu_128;      
   
   process (clk1x)
   begin
      if rising_edge(clk1x) then
      
         if (ram_ena = '1') then
            ram_next_dma <= '0';
            ram_next_cpu <= '0';
            if (cpuPaused = '1') then
               ram_next_dma <= '1';
            else
               ram_next_cpu <= '1';
            end if;
         end if;
      
      end if;
   end process;
   
   ram_dma_done <= ram_done and ram_next_dma;
   ram_cpu_done <= ram_done and ram_next_cpu;
   
   itimer : entity work.timer
   port map
   (
      clk1x                => clk1x,
      ce                   => ce,   
      reset                => reset_intern,
      
      dotclock             => '0', -- todo
      hblank               => hblank_tmr,
      vblank               => vblank_intern,
      
      irqRequest0          => irq_TIMER0,
      irqRequest1          => irq_TIMER1,
      irqRequest2          => irq_TIMER2,
      
      bus_addr             => bus_tmr_addr,     
      bus_dataWrite        => bus_tmr_dataWrite,
      bus_read             => bus_tmr_read,     
      bus_write            => bus_tmr_write,       
      bus_dataRead         => bus_tmr_dataRead,
      
-- synthesis translate_off
      export_t_current0    => export_t_current0,
      export_t_current1    => export_t_current1,
      export_t_current2    => export_t_current2,
-- synthesis translate_on
      
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(3 downto 0),      
      SS_wren              => SS_wren(8),     
      SS_rden              => SS_rden(8),     
      SS_DataRead          => SS_DataRead_TIMER
   );
   
   icd_top : entity work.cd_top
   port map
   (
      clk1x                => clk1x,
      ce                   => ce,   
      reset                => reset_intern,
     
      INSTANTSEEK          => INSTANTSEEK,
      hasCD                => hasCD,
      fastCD               => fastCD,
      
      cdSlow               => cdSlow,
      error                => errorCD,
          
      irqOut               => irq_CDROM,
                            
      bus_addr             => bus_cd_addr,     
      bus_dataWrite        => bus_cd_dataWrite,
      bus_read             => bus_cd_read,     
      bus_write            => bus_cd_write,     
      bus_dataRead         => bus_cd_dataRead,
                            
      dma_read             => DMA_CD_readEna,
      dma_readdata         => DMA_CD_read,
      
      cdSize               => cd_Size,
      cd_req               => cd_req, 
      cd_addr              => cd_addr,
      cd_data              => cd_data,
      cd_done              => cd_done,
      
      cd_hps_on            => cd_hps_on,   
      cd_hps_req           => cd_hps_req,  
      cd_hps_lba           => cd_hps_lba,
      cd_hps_ack           => cd_hps_ack,
      cd_hps_write         => cd_hps_write,
      cd_hps_data          => cd_hps_data, 
      
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(13 downto 0),      
      SS_wren              => SS_wren(13),     
      SS_rden              => SS_rden(13),     
      SS_DataRead          => SS_DataRead_CD,
      SS_Idle              => SS_Idle_cd
   );
   
   
   hblank <= hblank_intern;
   
   igpu : entity work.gpu
   port map
   (
      clk1x                => clk1x,
      clk2x                => clk2x,
      clk2xIndex           => clk2xIndex,
      ce                   => ce,   
      reset                => reset_intern,
      
      ditherOff            => ditherOff,
      REPRODUCIBLEGPUTIMING=> REPRODUCIBLEGPUTIMING,
      videoout_on          => videoout_on,
      isPal                => isPal,
      fpscountOn           => fpscountOn,
      
      cdSlow               => cdSlow,
      
      errorOn              => errorOn,  
      errorEna             => errorEna, 
      errorCode            => errorCode,
      
      errorLINE            => errorLINE,
      errorRECT            => errorRECT,
      errorPOLY            => errorPOLY,
      errorGPU             => errorGPU, 
      
      bus_addr             => bus_gpu_addr,     
      bus_dataWrite        => bus_gpu_dataWrite,
      bus_read             => bus_gpu_read,     
      bus_write            => bus_gpu_write,    
      bus_dataRead         => bus_gpu_dataRead, 
      
      dmaOn                => dmaOn,
      gpu_dmaRequest       => gpu_dmaRequest,  
      DMA_GPU_waiting      => DMA_GPU_waiting,
      DMA_GPU_writeEna     => DMA_GPU_writeEna,
      DMA_GPU_readEna      => DMA_GPU_readEna, 
      DMA_GPU_write        => DMA_GPU_write,   
      DMA_GPU_read         => DMA_GPU_read,  
      
      irq_VBLANK           => irq_VBLANK,
      irq_GPU              => irq_GPU,
      
      vram_pause           => vram_pause, 
      vram_paused          => vram_paused,
      vram_BUSY            => ddr3_BUSY,       
      vram_DOUT            => ddr3_DOUT,       
      vram_DOUT_READY      => ddr3_DOUT_READY,
      vram_BURSTCNT        => vram_BURSTCNT,  
      vram_ADDR            => vram_ADDR,      
      vram_DIN             => vram_DIN,       
      vram_BE              => vram_BE,        
      vram_WE              => vram_WE,        
      vram_RD              => vram_RD, 

      hsync                => hsync, 
      vsync                => vsync, 
      hblank               => hblank_intern,
      hblank_tmr           => hblank_tmr,
      vblank               => vblank_intern,
      vblank_extern        => vblank,
      DisplayWidth         => DisplayWidth, 
      DisplayHeight        => DisplayHeight,
      DisplayOffsetX       => DisplayOffsetX,
      DisplayOffsetY       => DisplayOffsetY,
      
      video_ce              => video_ce,
      video_interlace       => video_interlace,
      video_r               => video_r, 
      video_g               => video_g, 
      video_b               => video_b, 
      
-- synthesis translate_off
      export_gtm           => export_gtm,
      export_line          => export_line,
      export_gpus          => export_gpus,
      export_gobj          => export_gobj,
-- synthesis translate_on
      
      loading_savestate    => loading_savestate,
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(2 downto 0),
      SS_wren_GPU          => SS_wren(1),     
      SS_wren_Timing       => SS_wren(2),      
      SS_rden_GPU          => SS_rden(1),     
      SS_rden_Timing       => SS_rden(2),
      SS_DataRead_GPU      => SS_DataRead_GPU,
      SS_DataRead_Timing   => SS_DataRead_GPUTiming,
      SS_Idle              => SS_Idle_gpu
   );
   
   imdec : entity work.mdec
   port map
   (
      clk1x                => clk1x,     
      clk2x                => clk2x,    
      clk2xIndex           => clk2xIndex,
      ce                   => ce,        
      reset                => reset_intern,     
      
      bus_addr             => bus_mdec_addr,     
      bus_dataWrite        => bus_mdec_dataWrite,
      bus_read             => bus_mdec_read,     
      bus_write            => bus_mdec_write,    
      bus_dataRead         => bus_mdec_dataRead, 
      
      dmaWriteRequest      => mdec_dmaWriteRequest,
      dmaReadRequest       => mdec_dmaReadRequest, 
      dma_write            => DMA_MDEC_writeEna,   
      dma_writedata        => DMA_MDEC_write,    
      dma_read             => DMA_MDEC_readEna,      
      dma_readdata         => DMA_MDEC_read,

      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(6 downto 0),      
      SS_wren              => SS_wren(6),     
      SS_rden              => SS_rden(6),     
      SS_DataRead          => SS_DataRead_MDEC,
      SS_Idle              => SS_Idle_mdec
   );
   
   irq_SPU <= irq_VBLANK and enaSPUirq;
   
   ispu : entity work.spu
   port map
   (
      clk1x                => clk1x,     
      ce                   => ce,        
      reset                => reset_intern,     
      
      --irqOut               => irq_SPU,
      
      FAKESPU              => FAKESPU,
      enaSPUirq            => enaSPUirq,
      
      bus_addr             => bus_spu_addr,     
      bus_dataWrite        => bus_spu_dataWrite,
      bus_read             => bus_spu_read,     
      bus_write            => bus_spu_write,    
      bus_dataRead         => bus_spu_dataRead, 
      
      spu_dmaRequest       => spu_dmaRequest, 
      dma_read             => DMA_SPU_readEna,      
      dma_readdata         => DMA_SPU_read, 
      dma_write            => DMA_SPU_writeEna, 
      dma_writedata        => DMA_SPU_write,
      
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(7 downto 0),  
      SS_wren              => SS_wren(9),     
      SS_rden              => SS_rden(9),     
      SS_DataRead          => SS_DataRead_SOUND,
      SS_idle              => SS_idle_spu
   );
   
   iexp2 : entity work.exp2
   port map
   (
      clk1x                => clk1x,
      ce                   => ce,   
      reset                => reset_intern,
      
      bus_addr             => bus_exp2_addr,     
      bus_dataWrite        => bus_exp2_dataWrite,
      bus_read             => bus_exp2_read,     
      bus_write            => bus_exp2_write,    
      bus_writeMask        => bus_exp2_writeMask, 
      bus_dataRead         => bus_exp2_dataRead
   );

   imemorymux : entity work.memorymux
   generic map
   (
      NOMEMWAIT => '1' --is_simu
   )
   port map
   (
      clk1x                => clk1x,
      ce                   => ce_cpu,   
      reset                => reset_intern,
      
      isIdle               => memMuxIdle,
         
      loadExe              => loadExe,
      reset_exe            => reset_exe,
      
      fastboot             => fastboot,
            
      ram_dataWrite        => ram_cpu_dataWrite,
      ram_dataRead         => ram_dataRead, 
      ram_Adr              => ram_cpu_Adr,  
      ram_be               => ram_cpu_be,        
      ram_rnw              => ram_cpu_rnw,      
      ram_ena              => ram_cpu_ena,      
      ram_128              => ram_cpu_128,      
      ram_done             => ram_cpu_done,     
      
      mem_request          => mem_request,  
      mem_rnw              => mem_rnw,      
      mem_isData           => mem_isData,      
      mem_isCache          => mem_isCache,      
      mem_addressInstr     => mem_addressInstr,  
      mem_addressData      => mem_addressData,  
      mem_reqsize          => mem_reqsize,  
      mem_writeMask        => mem_writeMask,
      mem_dataWrite        => mem_dataWrite,
      mem_dataRead         => mem_dataRead, 
      mem_dataCache        => mem_dataCache, 
      mem_done             => mem_done,

      --bus_exp1_addr        => bus_exp1_addr,   
      --bus_exp1_dataWrite   => bus_exp1_dataWrite,
      bus_exp1_read        => bus_exp1_read,   
      --bus_exp1_write       => bus_exp1_write,  
      bus_exp1_dataRead    => bus_exp1_dataRead,
      
      bus_memc_addr        => bus_memc_addr,     
      bus_memc_dataWrite   => bus_memc_dataWrite,
      bus_memc_read        => bus_memc_read,     
      bus_memc_write       => bus_memc_write,    
      bus_memc_dataRead    => bus_memc_dataRead,   
      
      bus_pad_addr         => bus_pad_addr,     
      bus_pad_dataWrite    => bus_pad_dataWrite,
      bus_pad_read         => bus_pad_read,     
      bus_pad_write        => bus_pad_write,    
      bus_pad_writeMask    => bus_pad_writeMask,
      bus_pad_dataRead     => bus_pad_dataRead,       
      
      bus_sio_addr         => bus_sio_addr,     
      bus_sio_dataWrite    => bus_sio_dataWrite,
      bus_sio_read         => bus_sio_read,     
      bus_sio_write        => bus_sio_write,    
      bus_sio_writeMask    => bus_sio_writeMask,
      bus_sio_dataRead     => bus_sio_dataRead, 

      bus_memc2_addr       => bus_memc2_addr,     
      bus_memc2_dataWrite  => bus_memc2_dataWrite,
      bus_memc2_read       => bus_memc2_read,     
      bus_memc2_write      => bus_memc2_write,    
      bus_memc2_dataRead   => bus_memc2_dataRead, 

      bus_irq_addr         => bus_irq_addr,     
      bus_irq_dataWrite    => bus_irq_dataWrite,
      bus_irq_read         => bus_irq_read,     
      bus_irq_write        => bus_irq_write,    
      bus_irq_dataRead     => bus_irq_dataRead,       
      
      bus_dma_addr         => bus_dma_addr,     
      bus_dma_dataWrite    => bus_dma_dataWrite,
      bus_dma_read         => bus_dma_read,     
      bus_dma_write        => bus_dma_write,    
      bus_dma_dataRead     => bus_dma_dataRead,     

      bus_tmr_addr         => bus_tmr_addr,     
      bus_tmr_dataWrite    => bus_tmr_dataWrite,
      bus_tmr_read         => bus_tmr_read,     
      bus_tmr_write        => bus_tmr_write,    
      bus_tmr_dataRead     => bus_tmr_dataRead,  

      bus_cd_addr          => bus_cd_addr,     
      bus_cd_dataWrite     => bus_cd_dataWrite,
      bus_cd_read          => bus_cd_read,     
      bus_cd_write         => bus_cd_write,    
      bus_cd_dataRead      => bus_cd_dataRead,      
      
      bus_gpu_addr         => bus_gpu_addr,     
      bus_gpu_dataWrite    => bus_gpu_dataWrite,
      bus_gpu_read         => bus_gpu_read,     
      bus_gpu_write        => bus_gpu_write,    
      bus_gpu_dataRead     => bus_gpu_dataRead,
      
      bus_mdec_addr        => bus_mdec_addr,     
      bus_mdec_dataWrite   => bus_mdec_dataWrite,
      bus_mdec_read        => bus_mdec_read,     
      bus_mdec_write       => bus_mdec_write,    
      bus_mdec_dataRead    => bus_mdec_dataRead, 
      
      bus_spu_addr         => bus_spu_addr,     
      bus_spu_dataWrite    => bus_spu_dataWrite,
      bus_spu_read         => bus_spu_read,     
      bus_spu_write        => bus_spu_write,    
      bus_spu_dataRead     => bus_spu_dataRead, 
      
      bus_exp2_addr        => bus_exp2_addr,     
      bus_exp2_dataWrite   => bus_exp2_dataWrite,
      bus_exp2_read        => bus_exp2_read,     
      bus_exp2_write       => bus_exp2_write,    
      bus_exp2_dataRead    => bus_exp2_dataRead, 
      bus_exp2_writeMask   => bus_exp2_writeMask,
      
      --bus_exp3_dataWrite   => bus_exp3_dataWrite,
      bus_exp3_read        => bus_exp3_read,     
      --bus_exp3_write       => bus_exp3_write,    
      bus_exp3_dataRead    => bus_exp3_dataRead, 
      
      loading_savestate    => loading_savestate,
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(18 downto 0),
      SS_wren_SDRam        => SS_wren(16),
      SS_rden_SDRam        => SS_rden(16)
   );
   
   icpu : entity work.cpu
   port map
   (
      clk1x             => clk1x,
      clk2x             => clk2x,
      ce                => ce_cpu,   
      ce_system         => ce,
      reset             => reset_intern,
         
      irqRequest        => irqRequest,
      
      error             => errorCPU,
         
      mem_request       => mem_request,  
      mem_rnw           => mem_rnw,      
      mem_isData        => mem_isData,      
      mem_isCache       => mem_isCache,      
      mem_addressInstr  => mem_addressInstr,  
      mem_addressData   => mem_addressData,  
      mem_reqsize       => mem_reqsize,  
      mem_writeMask     => mem_writeMask,
      mem_dataWrite     => mem_dataWrite,
      mem_dataRead      => mem_dataRead, 
      mem_dataCache     => mem_dataCache, 
      mem_done          => mem_done,
      
      stallNext         => stallNext,
      
      gte_busy          => gte_busy, 
      gte_readEna       => gte_readEna,
      gte_readAddr      => gte_readAddr, 
      gte_readData      => gte_readData, 
      gte_writeAddr     => gte_writeAddr,
      gte_writeData     => gte_writeData,
      gte_writeEna      => gte_writeEna, 
      gte_cmdData       => gte_cmdData,  
      gte_cmdEna        => gte_cmdEna, 

      SS_reset          => SS_reset,
      SS_DataWrite      => SS_DataWrite,
      SS_Adr            => SS_Adr(7 downto 0),   
      SS_wren_CPU       => SS_wren(0),     
      SS_wren_SCP       => SS_wren(12),  
      SS_rden_CPU       => SS_rden(0),     
      SS_rden_SCP       => SS_rden(12),        
      SS_DataRead_CPU   => SS_DataRead_CPU,
      SS_DataRead_SCP   => SS_DataRead_SCP,
      SS_idle           => SS_idle_cpu,
      
-- synthesis translate_off
      cpu_done          => cpu_done,  
      cpu_export        => cpu_export,
-- synthesis translate_on
      
      debug_firstGTE    => debug_firstGTE
   );
   
   igte : entity work.gte
   port map
   (
      clk1x                => clk1x,     
      clk2x                => clk2x,     
      clk2xIndex           => clk2xIndex,
      ce                   => ce,        
      reset                => reset_intern,     
      
      gte_busy             => gte_busy,     
      gte_readAddr         => gte_readAddr, 
      gte_readData         => gte_readData, 
      gte_readEna          => gte_readEna,
      gte_writeAddr_in     => gte_writeAddr,
      gte_writeData_in     => gte_writeData,
      gte_writeEna_in      => gte_writeEna, 
      gte_cmdData          => gte_cmdData,  
      gte_cmdEna           => gte_cmdEna,
      
      loading_savestate    => loading_savestate,
      SS_reset             => SS_reset,
      SS_DataWrite         => SS_DataWrite,
      SS_Adr               => SS_Adr(5 downto 0),
      SS_wren              => SS_wren(4),     
      SS_rden              => SS_rden(4),     
      SS_DataRead          => SS_DataRead_GTE,
      SS_idle              => SS_idle_gte,
      
      debug_firstGTE       => debug_firstGTE
   );
   
   ddr3_BURSTCNT <= ss_ram_BURSTCNT     when (ddr3_savestate = '1') else arbiter_BURSTCNT when (arbiter_active = '1') else  vram_BURSTCNT;  
   ddr3_ADDR     <= ss_ram_ADDR & "00"  when (ddr3_savestate = '1') else arbiter_ADDR     when (arbiter_active = '1') else  x"00" & vram_ADDR;      
   ddr3_DIN      <= ss_ram_DIN          when (ddr3_savestate = '1') else arbiter_DIN      when (arbiter_active = '1') else  vram_DIN;       
   ddr3_BE       <= ss_ram_BE           when (ddr3_savestate = '1') else arbiter_BE       when (arbiter_active = '1') else  vram_BE;        
   ddr3_WE       <= ss_ram_WE           when (ddr3_savestate = '1') else arbiter_WE       when (arbiter_active = '1') else  vram_WE;        
   ddr3_RD       <= ss_ram_RD           when (ddr3_savestate = '1') else arbiter_RD       when (arbiter_active = '1') else  vram_RD;        
   
   isavestates : entity work.savestates
   generic map
   (
      FASTSIM => is_simu
   )
   port map
   (
      clk1x                   => clk1x,
      clk2x                   => clk2x,
      clk2xIndex              => clk2xIndex,
      ce                      => ce,
      reset_in                => reset_in,
      reset_out               => reset_intern,
      ss_reset                => SS_reset,
      
      loadExe                 => loadExe,
           
      load_done               => state_loaded,
            
      increaseSSHeaderCount   => increaseSSHeaderCount,
      save                    => savestate_savestate,
      load                    => savestate_loadstate,
      savestate_address       => savestate_address,  
      savestate_busy          => savestate_busy,    

      SS_idle                 => SS_idle,
      system_paused           => pausing,
      savestate_pause         => savestate_pause,
      ddr3_savestate          => ddr3_savestate,
      
      SS_DataWrite            => SS_DataWrite,   
      SS_Adr                  => SS_Adr,         
      SS_wren                 => SS_wren,       
      SS_rden                 => SS_rden,       
      SS_DataRead_CPU         => SS_DataRead_CPU,
      SS_DataRead_GPU         => SS_DataRead_GPU,
      SS_DataRead_GPUTiming   => SS_DataRead_GPUTiming,
      SS_DataRead_DMA         => SS_DataRead_DMA,
      SS_DataRead_GTE         => SS_DataRead_GTE,
      SS_DataRead_JOYPAD      => SS_DataRead_JOYPAD,
      SS_DataRead_MDEC        => SS_DataRead_MDEC,
      SS_DataRead_MEMORY      => SS_DataRead_MEMORY,
      SS_DataRead_TIMER       => SS_DataRead_TIMER,
      SS_DataRead_SOUND       => SS_DataRead_SOUND,
      SS_DataRead_IRQ         => SS_DataRead_IRQ,
      SS_DataRead_SIO         => SS_DataRead_SIO,
      SS_DataRead_SCP         => SS_DataRead_SCP,
      SS_DataRead_CD          => SS_DataRead_CD,

      sdram_done              => ram_done,
      
      loading_savestate       => loading_savestate,
      saving_savestate        => open,
      sleep_savestate         => sleep_savestate,
            
      ddr3_BUSY               => ddr3_BUSY,      
      ddr3_DOUT               => ddr3_DOUT,      
      ddr3_DOUT_READY         => ddr3_DOUT_READY,
      ddr3_BURSTCNT           => ss_ram_BURSTCNT,
      ddr3_ADDR               => ss_ram_ADDR,    
      ddr3_DIN                => ss_ram_DIN,     
      ddr3_BE                 => ss_ram_BE,      
      ddr3_WE                 => ss_ram_WE,      
      ddr3_RD                 => ss_ram_RD,

      ram_done                => ram_cpu_done,   
      ram_data                => ram_dataRead(31 downto 0)
   );  

   istatemanager : entity work.statemanager
   generic map
   (
      Softmap_SaveState_ADDR   => 58720256,
      Softmap_Rewind_ADDR      => 33554432
   )
   port map
   (
      clk                 => clk2x,  
      ce                  => ce,  
      reset               => reset_in,
                         
      rewind_on           => rewind_on,    
      rewind_active       => rewind_active,
                        
      savestate_number    => savestate_number,
      save                => save_state,
      load                => load_state,
                       
      sleep_rewind        => sleep_rewind,
      vsync               => IRQ_VBlank,
      system_idle         => '1',
                 
      request_savestate   => savestate_savestate,
      request_loadstate   => savestate_loadstate,
      request_address     => savestate_address,  
      request_busy        => savestate_busy    
   );
   
   -- export
-- synthesis translate_off
   gexport : if is_simu = '1' generate
   begin
   
      new_export <= cpu_done; 
      
      iexport : entity work.export
      port map
      (
         clk               => clk1x,
         ce                => ce,
         reset             => reset_intern,
            
         new_export        => cpu_done,
         export_cpu        => cpu_export,
            
         export_irq        => export_irq,
            
         export_gtm        => export_gtm,
         export_line       => export_line,
         export_gpus       => export_gpus,
         export_gobj       => export_gobj,
         
         export_t_current0 => export_t_current0,
         export_t_current1 => export_t_current1,
         export_t_current2 => export_t_current2,
            
         export_8          => export_8,
         export_16         => export_16,
         export_32         => export_32
      );
   
   
   end generate;
-- synthesis translate_on
   
end architecture;






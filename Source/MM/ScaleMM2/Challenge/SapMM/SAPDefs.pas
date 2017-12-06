{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  SapMM defines

****************************************************************************************}

unit SAPDefs;

interface

{$Include SAPOptions.inc}

uses Windows;

const
  cAlignment = 8; // Alignment in bytes for all blocks
  cMagic = $6A;   // Magic number to distinguish own blocks

const 
// ----- Large (OS) blocks -----

  // Size of blocks, requested from OS
  cOSBlockSize = 64 * 1024 * 16; // Size of large (OS) memory block (in bytes), requested directly from OS

  // Free blocks global pool size (global pool for all threads)
  cMaxOSBlocksInPool   = 200;  // Maximum number of freed OS blocks, which are not returned to the OS 
			       // and stay in pool for future reuse. When free blocks pool size reaches cMaxOSBlocksInPool
			       // other freed blocks are returned directly to the OS
// ----- Small blocks -----
  
  cMaxSmallBlockSize = 1023 - 4; // Small blocks size limit
                               // If allocated size is smaller or equal to cMaxSmallBlockSize, 
                               // block is considered small
                          

  cSmallBlockStep = 32;   // step for small blocks allocation
                          // the smaller the step, the more small blocks lists are created

  cSmallBlockHeaderMagic = $ABCEADE;

  cSmallBlocksMaxPieceSize = 64 * 1024; 
                             // must be no more than 64 Kb,
                             // because 16-bit values are used to store block offset from the header
                             // If more than 64 Kb is needed, algorithm must be changed in accordance to
                             // the fact that both the header and the small block are 8-aligned,
                             // which makes 3 more bits available

// ----- Normal (medium) blocks -----

  cNormalBlockMagic = $AABCA61C;
  cNormalAlignment  = 16;

  cMinMediumBlock   = 1024;      // Normal block min size, must match small blocks 
			         // allocation algorithm
  cMaxMediumBlock   = 16 * 1024; // Normal blocks max size
  cMediumBlockStep  = 32;        // Step for normal blocks allocation


const // MM
  cMagicMM = $3EAD3EAD;

type
  PSAPThreadMM = ^ TSAPThreadMM;

  PSAPOSBlock = ^ TSAPOSBlock;

  PSAPFreeBlock = ^ TSAPFreeBlock;
  PSAPAllocatedBlock = ^ TSAPAllocatedBlock;

  PSAPSmallAllocatedBlock = ^ TSAPSmallAllocatedBlock;
  PSAPSmallFreeBlock = ^ TSAPSmallFreeBlock;
  PSAPSmallBlocksHeader = ^ TSAPSmallBlocksHeader;
  PSAPBlocksFreedInOtherThreads = ^ TSAPBlocksFreedInOtherThreads;

  TSAPTag = ( sap_allocated,          // allocated block
              sap_small_block,        // small block
              sap_os_block_first,     // first block in OS block
              sap_os_block_last,      // last block in ОС block
              sap_small_blocks_piece, // group of small blocks
              sap_used_in_leaks_reporting
             );

  TSAPTags = set of TSAPTag;

  TSAPSmallBlocksDesc = record
    headers: PSAPSmallBlocksHeader;
    block_size: LongWord;
    blocks_in_piece: LongWord;      // number of blocks in group, which will be allocated next time
    max_blocks_in_piece: LongWord;  // max number of blocks in group
  end;

//---------------------------------

  TSAPThreadMM = record
    magic: LongWord;

    // double-linked acyclic list of work MMs or "zombie" MMs, left from destroyed threads
    next: PSAPThreadMM; 
    prev: PSAPThreadMM; 

    os_blocks: PSAPOSBlock;     // double-linked list of blocks, allocated directly from OS
    free_blocks: PSAPFreeBlock; // list of free large blocks

  // Freed blocks from other threads
    blocks_to_free: PSAPBlocksFreedInOtherThreads;
    crit_sect_for_other_threads: Windows._RTL_CRITICAL_SECTION;

  // Statistics
    thread_id: LongWord;

  // Used for checking the existence of used blocks
    no_get: LongWord;
    no_free: LongWord;

  // Used for the optimization of returning blocks to the OS
    no_os_blocks: LongWord;

  // small blocks table
    small_table:
      array [0..cMaxSmallBlockSize div cSmallBlockStep]
        of TSAPSmallBlocksDesc; // PSAPSmallBlocksHeader;

  // Normal (medium) blocks table
    medium_table:
      array[cMinMediumBlock div cMediumBlockStep..cMaxMediumBlock div cMediumBlockStep]
        of PSAPFreeBlock;

{$IFDEF SAP_STAT}
    no_get_small: LongWord;
    no_get_normal: LongWord;
    no_get_os: LongWord;

    no_free_from_other_thread: LongWord;

    no_realloc: LongWord;
    no_realloc_extend: LongWord;
    no_realloc_shrink: LongWord;
    no_realloc_at_place: LongWord;
    no_realloc_copy_bytes: Int64;
    //
    no_os_alloc: LongWord;
    no_os_free: LongWord;
    //
    no_alloc_find_cycles: LongWord;
    // межпоточные освобождения
    no_inter_free: LongWord;
    no_inter_realloc: LongWord;
{$ENDIF}
  end;

  TIPs = array [1..8] of LongWord;

// Header of normal allocated block
// size must be 8-aligned
  TSAPAllocatedBlock = packed record
    size: LongWord;

    block_mm: Pointer;    // MM для блока
    block_magic: LongWord;

{$IFDEF SAP_STATIP}
    ips: TIPs;
{$ENDIF}

// 2 fields after "reserved" field are overlapped on allocated small block header
    reserved: Word; // not used, needed to overlap next 2 fields to small block header
    tags: TSAPTags;
    magic: Byte;
  end;
// Size of the header can be shrinked to 2 words, if "block_mm" is stored
// in OS block header, and only an offset from the beginning of the OS blockа is stored here
// But this requires finding place for needed size
// If we may limit OS block to 512К (19 bit) and block header is 8-aligned, 
// we may not store 3 lower bits
// This is not implemented, because taking in place that minimal block is 1 Kb,
// the overhead of 16 + 4 = 20 bytes is not essential.
// Don't want to lose extended check (block_magic) either

// Normal free block header
  TSAPFreeBlock = record
    block: TSAPAllocatedBlock;
    next: PSAPFreeBlock;
    prev: PSAPFreeBlock;
  end;

//-------------------------------------

  TSAPBlocksFreedInOtherThreads = record
    next: PSAPBlocksFreedInOtherThreads;
  end;

//-------------------------------------

// Small allocated block header
  TSAPSmallAllocatedBlock = packed record

{$IFDEF SAP_STATIP}
    ips: TIPs;
{$ENDIF}

    ofs: Word;
    tags: TSAPTags;
    magic: Byte;
  end;

// Small free block header
  TSAPSmallFreeBlock = record
    block: TSAPSmallAllocatedBlock;
    header: PSAPSmallBlocksHeader;
    next: PSAPSmallFreeBlock;
  end;

//-------------------------------------

// Size must be N*8 + 4: 20, 28, ...
  TSAPSmallBlocksHeader = record
    header_magic: LongWord;
    block_size: LongWord;

    blocks_no: Integer;   // number of blocks in group
    free_count: Integer;  // number of free blocks among the total of blocks_no blocks

    // single-linked list of free blocks in group
    blocks: PSAPSmallFreeBlock;

    // double-linked acyclic list of groups, which have free blocks
    next: PSAPSmallBlocksHeader;
    prev: PSAPSmallBlocksHeader;

    block_mm: Pointer; // MM for the block
    filler: LongWord;  // not used, needed for alignment
  end;

//-------------------------------------

// header of OS block. Size must be 16-aligned
// double-linked acyclic list
  TSAPOSBlock = record
    size: LongWord;
    next: PSAPOSBlock;
    prev: PSAPOSBlock;

    pool_next: PSAPOSBlock; // next free block in pool

//    fill: LongWord; // for the 16 alignment. Not used (nod needed in current ver)
  end;

const
  cBlockOverhead = SizeOf(TSAPAllocatedBlock) + SizeOf(LongWord);

var
  sap_no_free_oldMM: LongWord; // number of blocks, freed by the previous MM, replaced by SapMM
implementation

initialization
  assert( SizeOf(TSAPSmallBlocksHeader) mod 8 = 4);
  sap_no_free_oldMM := 0;
end.
{****************************************************************************************

  SAPMM v1.01

  SapMM error codes

****************************************************************************************}

unit SAPErrCodes;

interface

const
  cErrInvPtrAlignment  = 1; // wrong pointer alignment
  cErrWrongMagic       = 2; // wrong block header magic number
  cErrNotAllocated     = 3; // wrong allocated block mark
  cErrInvSizeAlignment = 4; // wrong block size alignment
  cErrGetSizeError     = 5; // get block size exception
  cErrGetLastSizeError = 6; // get block size at the end of the block exception
  cErrSizesAreDifferent= 7; // different size at the beggining and at the end of the block

  cErrWrongHeaderMagic =11; // wrong small block header magic number


// debug constants
  cErrDebugCheck1      =21;
  cErrDebugCheck2      =22;
  cErrDebugCheck3      =23;
  cErrDebugCheck4      =24;


implementation

end.

unit smmLogging;

interface

type
  TMemoryLogging = record
    //start/stop logging
    //log alloc(size), realloc(old, newsize), freed
  end;

  //note: "Real_" pointers are determined via .map file
  { TODO -oAM : Hook object creation (store allocation type, can also be used for GC later?)
    HookProc(@TObject.InitInstance, @NewInitInstance, @OldInitInstance, 'TObject.InitInstance');
    HookProc(Real_AfterConstruction, @New_AfterConstruction, @Old_AfterConstruction, '_AfterConstruction');
  }
  { TODO -oAM : Hook string creation (store allocation type, can also be used to be ignored by GC later?)
    HookProc(Real_NewAnsiString, @New_NewAnsiString, @Old_NewAnsiString, 'NewAnsiString');
  }
  { TODO -oAM : Hook array creation (store allocation type, can also be used to be partly ignored by GC later? (only if contains pointer types?))
    HookProc(@DynArraySetLength, @NewDynArraySetLength, @OldDynArraySetLength, 'DynArraySetLength');
  }

implementation

end.

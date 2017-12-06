unit LinkedListBenchmark;

interface

uses
  Classes, BenchmarkClassUnit, Math;

const
   cNB_LIST_ITEMS = 128*1024;

type

  TLinkedListBench = class(TFastcodeMMBenchmark)
  public
    constructor CreateBenchmark; override;
    destructor Destroy; override;
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetSpeedWeight: Double; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

{ TSmallResizeBench }

constructor TLinkedListBench.CreateBenchmark;
begin
  inherited;
end;

destructor TLinkedListBench.Destroy;
begin
  inherited;
end;

class function TLinkedListBench.GetBenchmarkDescription: string;
begin
  Result:= 'Allocates a linked list containers and then navigates back and '
          +'forth through it multiple times.';
end;

class function TLinkedListBench.GetBenchmarkName: string;
begin
  Result := 'Linked-list container benchmark';
end;

class function TLinkedListBench.GetCategory: TBenchmarkCategory;
begin
  Result := bmMemoryAccessSpeed;
end;

class function TLinkedListBench.GetSpeedWeight: Double;
begin
  {Speed is of the essence here}
  Result := 0.8;
end;

type
   TExternalRefObject1 = class
      Padding : array [0..50] of Integer;
   end;
   TExternalRefObject2 = class(TExternalRefObject1)
      Padding2 : array [0..50] of Integer;
   end;
   TExternalRefObject3 = class(TExternalRefObject2)
      Padding3 : array [0..50] of Integer;
   end;
   TExternalRefObject4 = class(TExternalRefObject3)
      Padding4 : array [0..50] of Integer;
   end;

   PLinkedListItem = ^TLinkedListItem;
   TLinkedListItem = record
      Next, Prev : PLinkedListItem;
      List : TList;
      ExternalRef : TExternalRefObject1;
   end;

   TLinkedList = class
      First, Last : PLinkedListItem;
   end;

procedure Dummy;
begin
end;

procedure TLinkedListBench.RunBenchmark;
var
   i : Integer;
   list : TLinkedList;
   current : PLinkedListItem;
begin
   inherited;
   // allocate the list
   list:=TLinkedList.Create;
   New(current);
   current.Next:=nil;
   current.Prev:=nil;
   current.ExternalRef:=TExternalRefObject1.Create;
   list.First:=current;
   list.Last:=list.First;
   RandSeed:=0;
   for i:=2 to cNB_LIST_ITEMS do begin
      New(current);
      current.Next:=nil;
      list.Last.Next:=current;
      current.Prev:=list.Last;
      list.Last:=current;
      case Random(4) of // allocate randomly from a small variety of external refs
         0 : current.ExternalRef:=TExternalRefObject1.Create;
         1 : current.ExternalRef:=TExternalRefObject2.Create;
         2 : current.ExternalRef:=TExternalRefObject3.Create;
         3 : current.ExternalRef:=TExternalRefObject4.Create;
      end;
   end;

   // peak usage reached now
   UpdateUsageStatistics;

   // do the bench
   for i:=1 to 100 do begin
      current:=list.First;
      while current<>nil do begin
         if current.ExternalRef.Padding[0]=-1 then Dummy; // access the ExternalRef
         current:=current.Next;
      end;
      current:=list.Last;
      while current<>nil do begin
         if current.ExternalRef.Padding[0]=-1 then Dummy; // access the ExternalRef
         current:=current.Prev;
      end;
   end;

   // cleanup
   current:=list.First;
   while current<>nil do begin
      list.First:=current.Next;
      current.ExternalRef.Free;
      Dispose(current);
      current:=list.First;
   end;
   list.Free;
end;

end.

unit OrderStructFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  CoreClasses, PascalStrings, DoStatusIO;

type
  TOrderStructForm = class(TForm)
    Memo: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure DoStatus_Backcall(Text_: SystemString; const ID: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TMyRecord = record
    a, b, c: Integer;
    obj: TObject;
  end;

  {  Torderptrstruct will automatically convert tmyrecord to pointer  }
  {  This structure is suitable for simple record  }
  TMyRecordOrder_Decl = TOrderPtrStruct<TMyRecord>;

  {  Torderptrstruct has its own release mechanism, which is convenient for demonstration. Here is an inheritance operation to release the interface event and release tmyrecord - > obj  }
  TMyRecordOrder = class(TMyRecordOrder_Decl)
  public
    procedure DoFree(data: TMyRecordOrder_Decl.PT_); override;
  end;

var
  OrderStructForm: TOrderStructForm;

implementation

{$R *.dfm}


procedure TMyRecordOrder.DoFree(data: TMyRecordOrder_Decl.PT_);
begin
  {  Torderptrstruct has its own release mechanism, which is convenient for demonstration. Here is an inheritance operation to release the interface event and release tmyrecord - > obj  }
  data^.obj.Free;
end;

procedure TOrderStructForm.Button1Click(Sender: TObject);
var
  order: TMyRecordOrder;
  i: Integer;
  tmp: TMyRecord;
begin
  order := TMyRecordOrder.Create;
  for i := 1 to 10 do
    begin
      tmp.a := i;
      tmp.b := i;
      tmp.c := i;
      tmp.obj := TObject.Create;
      {  Push tmyrecord data into the queue  }
      {  Push copies TMP back to a memory block  }
      {  By avoiding the use of linked lists and arrays, the push operation is very fast and can support highly intensive calls, such as 100 million times per second  }
      {  Push has no size limit, and the push mechanism can last until the memory is used up  }
      order.Push(tmp);
    end;

  if false then
    begin
      {  There are two ways to obtain data: 1. Directly judge whether current is nil  }
      while order.Current <> nil do
        begin
          with order.Current^.data^ do
              DoStatus('a:%d, b:%d, c:%d', [a, b, c]);

          order.Next; {  Next frees the current structure, frees memory, and points to the next structure  }
        end;
    end
  else
    begin
      {  There are two ways to obtain data: 2. Judge the current queue num  }
      while order.Num > 0 do
        begin
          with order.Current^.data^ do
              DoStatus('a:%d, b:%d, c:%d', [a, b, c]);

          order.Next; {  Next frees the current structure, frees memory, and points to the next structure  }
        end;
    end;

  order.Free;
end;

procedure TOrderStructForm.DoStatus_Backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

constructor TOrderStructForm.Create(AOwner: TComponent);
begin
  inherited;
  AddDoStatusHook(Self, DoStatus_Backcall);
end;

destructor TOrderStructForm.Destroy;
begin
  inherited;
end;

end.

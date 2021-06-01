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

  // TOrderPtrStruct会自动将TMyRecord转换成为指针
  // 该结构适用于简单record
  TMyRecordOrder_Decl = TOrderPtrStruct<TMyRecord>;

  // TOrderPtrStruct自带释放机制，处于方便演示，这里做了一个继承操作，作用是接口释放事件，把TMyRecord->obj释放掉
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
  // TOrderPtrStruct自带释放机制，处于方便演示，这里做了一个继承操作，作用是接口释放事件，把TMyRecord->obj释放掉
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
      // 将TMyRecord数据压入队列
      // push会将tmp重新拷贝到一个内存块
      // 由于避免了使用链表和数组，push操作是非常快的，并且可以支持高密集调用，例如每秒1亿次
      // push没有体量限制，push机制可以持续到内存用光
      order.Push(tmp);
    end;

  if false then
    begin
      // 两种获取数据的方式，1，直接判断current是否为nil
      while order.Current <> nil do
        begin
          with order.Current^.data^ do
              DoStatus('a:%d, b:%d, c:%d', [a, b, c]);

          order.Next; // next会释放当前结构，同时释放内存，并指向下一个结构
        end;
    end
  else
    begin
      // 两种获取数据的方式，2，判断当前队列num
      while order.Num > 0 do
        begin
          with order.Current^.data^ do
              DoStatus('a:%d, b:%d, c:%d', [a, b, c]);

          order.Next; // next会释放当前结构，同时释放内存，并指向下一个结构
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

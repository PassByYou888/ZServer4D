unit AC2ProgressFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  DoStatusIO,
  FMX.Layouts, FMX.Effects, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TAC2ProgressForm = class(TForm)
    MainLayout: TLayout;
    Memo: TMemo;
    AppNameLabel: TLabel;
    FloatAnimation5: TFloatAnimation;
    GlowEffect4: TGlowEffect;
    MemoGoInFloatAnimation: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MemoGoInFloatAnimationFinish(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    procedure Progress(deltaTime: Double);
  end;

var
  AC2ProgressForm: TAC2ProgressForm = nil;

implementation

{$R *.fmx}


uses AC2ClientGlobal;

procedure TAC2ProgressForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo.Lines.Add(AText);
  Memo.GoToTextEnd;
end;

procedure TAC2ProgressForm.MemoGoInFloatAnimationFinish(Sender: TObject);
begin
  Memo.GoToTextEnd;
end;

procedure TAC2ProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TAC2ProgressForm.FormCreate(Sender: TObject);
begin
  AppNameLabel.Text := AppName;

  AddDoStatusHook(self, DoStatusNear);

  ResetFormSize(self);
  ResetMainLayout(MainLayout, self);
end;

procedure TAC2ProgressForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TAC2ProgressForm.FormResize(Sender: TObject);
begin
  ResetMainLayout(MainLayout, self);
end;

procedure TAC2ProgressForm.FormShow(Sender: TObject);
begin
  ResetFormSize(self);
  ReStartAnimation(MainLayout);
end;

procedure TAC2ProgressForm.Progress(deltaTime: Double);
begin
  if Visible then
      Invalidate;
end;

end.

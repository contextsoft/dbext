unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CtxData, CtxDBAdapter, DB, ADODB, ADOExt, StdCtrls, Grids, DBGrids,
  CtxDataSet;

type
  TForm1 = class(TForm)
    CtxDataContainer1: TCtxDataContainer;
    ADOConnectionExt1: TADOConnectionExt;
    Button1: TButton;
    CtxDBAdapter1: TCtxDBAdapter;
    CtxDataSet1: TCtxDataSet;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses fDataContainerEditor;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DesignDataContainer(CtxDataContainer1, Self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CtxDataSet1.Active := True;
end;

end.

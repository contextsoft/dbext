unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBActns, ActnList, ImgList, Mask, DBCtrls, Db,
  dbRecord, ExtCtrls;

type
  TfrmDbRecordDemo = class(TForm)
    DBRecord1: TDBRecord;
    DataSource1: TDataSource;
    DBRecord1LastName: TStringField;
    DBRecord1FirstName: TStringField;
    DBRecord1City: TStringField;
    DBRecord1State: TStringField;
    DBRecord1Zip: TStringField;
    DBRecord1Street: TStringField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    ActionList1: TActionList;
    ImageList1: TImageList;
    DataSetCancel1: TDataSetCancel;
    DataSetEdit1: TDataSetEdit;
    DataSetPost1: TDataSetPost;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    DBRecord1Flag: TBooleanField;
    DBCheckBox1: TDBCheckBox;
    DBRecord1IntergerValue: TIntegerField;
    Label7: TLabel;
    DBEdit7: TDBEdit;
    DBRecord1Date: TDateField;
    Label8: TLabel;
    DBEdit8: TDBEdit;
    Label9: TLabel;
    Shape1: TShape;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDbRecordDemo: TfrmDbRecordDemo;

implementation

{$R *.DFM}

end.

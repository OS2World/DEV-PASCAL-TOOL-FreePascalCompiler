{$mode objfpc}
{$h+}

unit frmabout;

interface

uses fpgtk,gtk,classes,sysutils;

Type
  TAboutForm = Class (TFPGtkWindow)
    FAboutText : TFPGtkLabel;
    FSeparator : TFPGtkHSeparator;
    FVBox : TFPgtkVBox;
    FOK,
    FCancel : TFPGtkButton;
    FButtonBox: TFPgtkHBox;
    Constructor Create;
    Procedure CreateWindow;
  end;
  
Implementation

uses fpdemsg;

Constructor TAboutForm.Create;

begin
  Inherited Create(GTK_WINDOW_DIALOG);
  CreateWindow;
end;

Procedure TAboutForm.CreateWindow;

begin
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  // About text
  FAboutText:=TFPgtkLabel.Create(SAboutText);
  // button area
  FOK:=TFpGtkButton.CreateWithLabel(SOK);
  FOK.ConnectClicked(@CloseWithResult,IntToPointer(drOK));
  FCancel:=TFPgtkButton.CreateWithLabel(SCancel);
  FCancel.ConnectCLicked(@CloseWithResult,IntToPointer(drCancel));
  FSeparator:=TFPgtkHSeparator.Create;
  FButtonBox:=TfpGtkHBox.Create;
  FButtonBox.Spacing:=4;
  FButtonBox.PackEnd(FOK,false,false,4);
  FButtonBox.PackEnd(FCancel,false,false,4);
  // Add to window
  FVBox.PackStart(FAboutText,True,True,0);
  FVBox.PackStart(FSeparator,False,False,4);
  FVBox.PackStart(FButtonBox,false,false,0);
end;



end.  
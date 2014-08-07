{
    $Id: sysutilh.inc,v 1.1.2.10 2002/10/29 23:41:06 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



const
  PathDelim=System.DirectorySeparator;
  DriveDelim=System.DriveSeparator;
  PathSep=System.PathSeparator;

type
   { some helpful data types }

   tprocedure = procedure;

   tfilename = string;

   tsyscharset = set of char;
   tintegerset = set of 0..sizeof(integer)*8-1;

   longrec = packed record
      lo,hi : word;
   end;

   wordrec = packed record
      lo,hi : byte;
   end;

   TMethod = packed record
     Code, Data: Pointer;
   end;

  { FileRec/TextRec }
  {$i filerec.inc}
  {$i textrec.inc}

Type
  TFileRec=FileRec;
  TTextRec=TextRec;

  { Read String Handling functions declaration }
  {$i sysstrh.inc}

  {$i osutilsh.inc}

  { Read internationalization settings }
  {$i sysinth.inc}

  { Read date & Time function declarations }
  {$i datih.inc}

  { Read pchar handling functions declration }
  {$i syspchh.inc}

  { Read filename handling functions declaration }
  {$i finah.inc}

  { Read other file handling function declarations }
  {$i filutilh.inc}

  { Read disk function declarations }
  {$i diskh.inc}

{ ---------------------------------------------------------------------
    Exceptions.
  ---------------------------------------------------------------------}

Type
  
   Exception = class(TObject)
    private
      fmessage : string;
      fhelpcontext : longint;
    public
      constructor Create(const msg : string);
      constructor CreateFmt(const msg : string; const args : array of const);
      constructor CreateRes(ResString: PString);
      constructor CreateResFmt(ResString: PString; const Args: array of const);
      constructor CreateHelp(const Msg: string; AHelpContext: Integer);
      constructor CreateFmtHelp(const Msg: string; const Args: array of const;
        AHelpContext: Integer);
      constructor CreateResHelp(ResString: PString; AHelpContext: Integer);
      constructor CreateResFmtHelp(ResString: PString; const Args: array of const;
        AHelpContext: Integer);
      { !!!! }
      property helpcontext : longint read fhelpcontext write fhelpcontext;
      property message : string read fmessage write fmessage;
   end;

   ExceptClass = class of Exception;

   { integer math exceptions }
   EInterror    = Class(Exception);
   EDivByZero   = Class(EIntError);
   ERangeError  = Class(EIntError);
   EIntOverflow = Class(EIntError);

   { General math errors }
   EMathError  = Class(Exception);
   EInvalidOp  = Class(EMathError);
   EZeroDivide = Class(EMathError);
   EOverflow   = Class(EMathError);
   EUnderflow  = Class(EMathError);

   { Run-time and I/O Errors }
   EInOutError = class(Exception)
     public
     ErrorCode : Longint;
     end;
   EHeapMemoryError = class(Exception)
     protected
     AllowFree : boolean;
     procedure FreeInstance;override;
     end;

   EInvalidPointer  = Class(EHeapMemoryError);
   EOutOfMemory     = Class(EHeapMemoryError);
   EAccessViolation = Class(Exception);
   EInvalidCast = Class(Exception);

   { String conversion errors }
   EConvertError = class(Exception);

   { Other errors }
   EAbort           = Class(Exception);
   EAbstractError   = Class(Exception);
   EAssertionFailed = Class(Exception);

   { Exception handling routines }
   function ExceptObject: TObject;
   function ExceptAddr: Pointer;
   function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
                                  Buffer: PChar; Size: Integer): Integer;
   procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
   procedure Abort;
   procedure OutOfMemoryError;
   procedure Beep;
   function SysErrorMessage(ErrorCode: Integer): String;

type
  TTerminateProc = function: Boolean;
  
  procedure AddTerminateProc(TermProc: TTerminateProc);
  function CallTerminateProcs: Boolean;

Var
   OnShowException : Procedure (Msg : ShortString);

   procedure FreeAndNil(var obj);


{
  $Log: sysutilh.inc,v $
  Revision 1.1.2.10  2002/10/29 23:41:06  michael
  + Added lots of D4 functions

  Revision 1.1.2.9  2002/10/29 21:50:53  michael
  + Added AddTerminateProc

  Revision 1.1.2.8  2001/11/07 15:06:50  michael
  + Added PathDelim,DriveDelim,PathSep; Removed

  Revision 1.1.2.7  2001/08/12 22:14:03  peter
    * freeandnil added

  Revision 1.1.2.6  2001/06/03 15:23:33  peter
    * outofmemory and invalidpointer exceptions fixed

  Revision 1.1.2.5  2001/02/20 21:20:19  michael
  + Added osutilh.inc include

  Revision 1.1.2.4  2000/12/07 09:15:33  florian
    + tintegerset added

  Revision 1.1.2.3  2000/09/24 21:54:34  peter
    * ttextrec,tfilerec added

  Revision 1.1.2.2  2000/08/22 19:21:48  michael
  + Implemented syserrormessage. Made dummies for go32v2 and OS/2
  * Changed linux/errors.pp so it uses pchars for storage.

  Revision 1.1.2.1  2000/08/20 15:07:36  peter
    * sysutils.pp moved into target specific directory and merged
      disk.inc and filutil.inc in it

}

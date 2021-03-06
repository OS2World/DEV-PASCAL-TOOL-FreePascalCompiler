{
    $Id: objpas.inc,v 1.1.2.4 2003/01/05 17:43:23 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                  Internal Routines called from the Compiler
****************************************************************************}

    { the reverse order of the parameters make code generation easier }
    function int_do_is(aclass : tclass;aobject : tobject) : boolean;[public,alias: 'FPC_DO_IS'];
      begin
         int_do_is:=assigned(aobject) and assigned(aclass) and
           aobject.inheritsfrom(aclass);
      end;


    { the reverse order of the parameters make code generation easier }
    procedure int_do_as(aclass : tclass;aobject : tobject);[public,alias: 'FPC_DO_AS'];
      begin
         if assigned(aobject) and not(aobject.inheritsfrom(aclass)) then
           handleerror(219);
      end;


{****************************************************************************
                               TOBJECT
****************************************************************************}

      constructor TObject.Create;

        begin
        end;

      destructor TObject.Destroy;

        begin
        end;

      procedure TObject.Free;

        begin
           // the call via self avoids a warning
           if self<>nil then
             self.destroy;
        end;

      class function TObject.InstanceSize : LongInt;

        type
           plongint = ^longint;

        begin
           { type of self is class of tobject => it points to the vmt }
           { the size is saved at offset 0                            }
           InstanceSize:=plongint(pointer(self)+vmtInstanceSize)^;
        end;

      class function TObject.InitInstance(instance : pointer) : tobject;

        begin
           fillchar(instance^,self.instancesize,0);
           { insert VMT pointer into the new created memory area }
           { (in class methods self contains the VMT!)           }
           ppointer(instance)^:=pointer(self);
           InitInstance:=TObject(Instance);
        end;

      class function TObject.ClassParent : tclass;

        begin
           { type of self is class of tobject => it points to the vmt }
           { the parent vmt is saved at offset vmtParent              }
           classparent:=pclass(pointer(self)+vmtParent)^;
        end;

      class function TObject.NewInstance : tobject;

        var
           p : pointer;

        begin
           getmem(p,instancesize);
           if p <> nil then
              InitInstance(p);
           NewInstance:=TObject(p);
        end;

      procedure TObject.FreeInstance;

        var
           p : Pointer;

        begin
           CleanupInstance;

           { self is a register, so we can't pass it call by reference }
           p:=Pointer(Self);
           FreeMem(p,InstanceSize);
        end;

      function TObject.ClassType : TClass;

        begin
           ClassType:=TClass(Pointer(Self)^)
        end;

      type
         tmethodnamerec = packed record
            name : pshortstring;
            addr : pointer;
         end;

         tmethodnametable = packed record
           count : dword;
           entries : packed array[0..0] of tmethodnamerec;
         end;

         pmethodnametable =  ^tmethodnametable;

      class function TObject.MethodAddress(const name : shortstring) : pointer;

        var
       UName : ShortString;
           methodtable : pmethodnametable;
           i : dword;
           c : tclass;

        begin
       UName := UpCase(name);
           c:=self;
           while assigned(c) do
             begin
                methodtable:=pmethodnametable((Pointer(c)+vmtMethodTable)^);
                if assigned(methodtable) then
                  begin
                     for i:=0 to methodtable^.count-1 do
                       if UpCase(methodtable^.entries[i].name^)=UName then
                         begin
                            MethodAddress:=methodtable^.entries[i].addr;
                            exit;
                         end;
                  end;
                c:=c.ClassParent;
             end;
           MethodAddress:=nil;
        end;

      class function TObject.MethodName(address : pointer) : shortstring;

        var
           methodtable : pmethodnametable;
           i : dword;
           c : tclass;

        begin
           c:=self;
           while assigned(c) do
             begin
                methodtable:=pmethodnametable((Pointer(c)+vmtMethodTable)^);
                if assigned(methodtable) then
                  begin
                     for i:=0 to methodtable^.count-1 do
                       if methodtable^.entries[i].addr=address then
                         begin
                            MethodName:=methodtable^.entries[i].name^;
                            exit;
                         end;
                  end;
                c:=c.ClassParent;
             end;
           MethodName:='';
        end;




      function TObject.FieldAddress(const name : shortstring) : pointer;

    type
       PFieldInfo = ^TFieldInfo;
       TFieldInfo = packed record
         FieldOffset: LongWord;
         ClassTypeIndex: Word;
         Name: ShortString;
       end;

       PFieldTable = ^TFieldTable;
       TFieldTable = packed record
         FieldCount: Word;
         ClassTable: Pointer;
         { Fields: array[Word] of TFieldInfo;  Elements have variant size! }
       end;

        var
       UName: ShortString;
       CurClassType: TClass;
       FieldTable: PFieldTable;
       FieldInfo: PFieldInfo;
       i: Integer;

        begin
       if Length(name) > 0 then
       begin
         UName := UpCase(name);
         CurClassType := ClassType;
         while CurClassType <> nil do
         begin
           FieldTable := PFieldTable((Pointer(CurClassType) + vmtFieldTable)^);
           if FieldTable <> nil then
           begin
             FieldInfo := PFieldInfo(Pointer(FieldTable) + 6);
             for i := 0 to FieldTable^.FieldCount - 1 do
             begin
               if UpCase(FieldInfo^.Name) = UName then
           begin
             fieldaddress := Pointer(Self) + FieldInfo^.FieldOffset;
                 exit;
           end;
           Inc(Pointer(FieldInfo), 7 + Length(FieldInfo^.Name));
             end;
           end;
           { Try again with the parent class type }
           CurClassType := CurClassType.ClassParent;
         end;
       end;

           fieldaddress:=nil;
        end;

      function TObject.SafeCallException(exceptobject : tobject;
        exceptaddr : pointer) : longint;

        begin
           safecallexception:=0;
        end;

      class function TObject.ClassInfo : pointer;

        begin
           ClassInfo:=ppointer(Pointer(self)+vmtTypeInfo)^;
        end;

      class function TObject.ClassName : ShortString;

        begin
           ClassName:=PShortString((Pointer(Self)+vmtClassName)^)^;
        end;

      class function TObject.ClassNameIs(const name : string) : boolean;

        begin
           ClassNameIs:=Upcase(ClassName)=Upcase(name);
        end;

      class function TObject.InheritsFrom(aclass : TClass) : Boolean;

        var
           c : tclass;

        begin
           c:=self;
           while assigned(c) do
             begin
                if c=aclass then
                  begin
                     InheritsFrom:=true;
                     exit;
                  end;
                c:=c.ClassParent;
             end;
           InheritsFrom:=false;
        end;

      class function TObject.stringmessagetable : pstringmessagetable;

        type
           pdword = ^dword;

        begin
           stringmessagetable:=pstringmessagetable((pointer(Self)+vmtMsgStrPtr)^);
        end;

      type
         tmessagehandler = procedure(var msg) of object;
         tmessagehandlerrec = packed record
            proc : pointer;
            obj : pointer;
         end;


      procedure TObject.Dispatch(var message);

        type
           tmsgtable = record
              index : dword;
              method : pointer;
           end;

           pmsgtable = ^tmsgtable;

           pdword = ^dword;

        var
           index : dword;
           count,i : longint;
           msgtable : pmsgtable;
           p : pointer;
           vmt : tclass;
           msghandler : tmessagehandler;

        begin
           index:=dword(message);
           vmt:=ClassType;
           while assigned(vmt) do
             begin
                // See if we have messages at all in this class.
                p:=pointer(vmt)+vmtDynamicTable;
                If Assigned(p) and (Pdword(p)^<>0) then
                  begin
                  msgtable:=pmsgtable(pdword(P)^+4);
                  count:=pdword(pdword(P)^)^;
                  end
                else
                  Count:=0;
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if index=msgtable[i].index then
                       begin
                          p:=msgtable[i].method;
                          tmessagehandlerrec(msghandler).proc:=p;
                          tmessagehandlerrec(msghandler).obj:=self;
                          msghandler(message);
                          { we don't need any longer the assembler
                            solution
                          asm
                             pushl message
                             pushl %esi
                             movl p,%edi
                             call *%edi
                          end;
                          }
                          exit;
                       end;
                  end;
                vmt:=vmt.ClassParent;
             end;
           DefaultHandler(message);
        end;

      procedure TObject.DispatchStr(var message);

        type
           pdword = ^dword;

        var
           name : shortstring;
           count,i : longint;
           msgstrtable : pmsgstrtable;
           p : pointer;
           vmt : tclass;
           msghandler : tmessagehandler;

        begin
           name:=pshortstring(@message)^;
           vmt:=ClassType;
           while assigned(vmt) do
             begin
                p:=(pointer(vmt)+vmtMsgStrPtr);
                If (P<>Nil) and (PDWord(P)^<>0) then
                  begin
                  count:=pdword(pdword(p)^)^;
                  msgstrtable:=pmsgstrtable(pdword(P)^+4);
                  end
                else
                  Count:=0;
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if name=msgstrtable[i].name^ then
                       begin
                          p:=msgstrtable[i].method;
                          tmessagehandlerrec(msghandler).proc:=p;
                          tmessagehandlerrec(msghandler).obj:=self;
                          msghandler(message);
                          { we don't need any longer the assembler
                            solution
                          asm
                             pushl message
                             pushl %esi
                             movl p,%edi
                             call *%edi
                          end;
                          }
                          exit;
                       end;
                  end;
                vmt:=vmt.ClassParent;
             end;
           DefaultHandlerStr(message);
        end;

      procedure TObject.DefaultHandler(var message);

        begin
        end;

      procedure TObject.DefaultHandlerStr(var message);

        begin
        end;

      procedure TObject.CleanupInstance;

        var
           vmt : tclass;

        begin
           vmt:=ClassType;
           while vmt<>nil do
             begin
                if Assigned(Pointer((Pointer(vmt)+vmtInitTable)^)) then
                  Finalize(Pointer(Self),Pointer((Pointer(vmt)+vmtInitTable)^));
                vmt:=vmt.ClassParent;
             end;
        end;

      procedure TObject.AfterConstruction;

        begin
        end;

      procedure TObject.BeforeDestruction;

        begin
        end;

{****************************************************************************
                             Exception Support
****************************************************************************}

{$i except.inc}

{****************************************************************************
                                Initialize
****************************************************************************}

{
  $Log: objpas.inc,v $
  Revision 1.1.2.4  2003/01/05 17:43:23  peter
    * updated instancesize for stricter compiler check

  Revision 1.1.2.3  2002/04/01 21:09:11  carl
  * avoid crash if class instance cannot be allocated

  Revision 1.1.2.2  2000/12/20 21:36:40  florian
    * is-operator fixed

  Revision 1.1.2.1  2000/07/22 14:46:57  sg
  * Made TObject.MethodAddress case independent

  Revision 1.1  2000/07/13 06:30:48  michael
  + Initial import

  Revision 1.18  2000/07/08 21:27:42  sg
  * Fixed TObject.FieldAddress

  Revision 1.17  2000/07/08 07:24:24  sg
  * FieldAddress now returns immediately if name=''

  Revision 1.16  2000/06/29 16:32:50  sg
  * Implemented TObject.FieldAddress

  Revision 1.15  2000/05/16 08:06:14  michael
  + Fixed ClassNameIs so it is case insensitive

  Revision 1.14  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.13  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.12  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.11  1999/09/15 20:28:35  florian
    * fixed methodname/address: the loops must go from 0 to ...^.count-1

  Revision 1.10  1999/09/12 14:53:26  florian
    + tobject.methodaddress und tobject.methodname durchsucht nun auch
      die Elternklassen

  Revision 1.9  1999/09/12 08:01:00  florian
    + implementation of TObject.MethodName and TObject.MethodAddress (not
      in the compiler yet)

  Revision 1.8  1999/09/08 16:14:41  peter
    * pointer fixes

}

{
    $Id: tgen.pas,v 1.1.2.17 2001/09/17 13:47:33 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl, Carl Eric Codere

    This unit handles the temporary variables stuff for m68k

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit tgen;


  interface

    uses
       cobjects,globals,tree,hcodegen,verbose,files,aasm,cpubase,cpuasm;

    type
       tpushed = array[R_D0..R_FP7] of boolean;

    const
       { D2 to D7 usable as interngal registers }
       usablereg32 : byte = 6;
       { A2 to A4 usable as address registers }
       usableaddress: byte = 3;
       { FP2 to FP7 usable as FPU registers   }
       maxusablefloatreg = 6;
       usablefloatreg : byte = 6;

    function getregister32 : tregister;
    procedure ungetregister32(r : tregister);
    { return a free 32-bit address register }
    function getaddressreg: tregister;

    procedure ungetregister(r : tregister);

    procedure cleartempgen;
    procedure del_location(const l : tlocation);

    { returns a free floating point register }
    { used in real, fpu mode, otherwise we   }
    { must use standard register allocation  }
    function getfloatreg: tregister;
    procedure incrementregisterpushed(r : tregisterset);

    procedure del_reference(const ref : treference);
    procedure del_locref(const location : tlocation);

    function getexplicitregister32(r : tregister) : tregister;


    { save and restore registers }
    procedure saveusedregisters(var saved : tpushed; const tosave : tregisterset);
    procedure restoreusedregisters(const saved : tpushed);


    procedure clearregistercount;
    procedure resetusableregisters;
{$ifdef TEMPREGDEBUG}
    procedure testregisters32;
{$endif TEMPREGDEBUG}

    var
       unused,usableregs : tregisterset;
       c_usableregs : longint;

       usedinproc : tregisterset;

       { count, how much a register must be pushed if it is used as register }
       { variable                                                            }
       reg_pushes : array[R_D0..R_A6] of longint;
       is_reg_var : array[R_D0..R_A6] of boolean;
{$ifdef TEMPREGDEBUG}
       reg_user   : array[R_D0..R_FP7] of ptree;
       reg_releaser : array[R_D0..R_FP7] of ptree;
{$endif TEMPREGDEBUG}

    var
       { tries to hold the amount of times which the current tree is processed  }
       t_times : longint;

  implementation

   uses globtype;


    procedure incrementregisterpushed(r : tregisterset);

      var
         regi : tregister;

      begin
         for regi:=R_D0 to R_A6 do
           begin
              if regi in r then
                inc(reg_pushes[regi],t_times*2);
           end;
      end;


    function getusableaddr: byte;
    { Since address registers are different then data registers }
    { we check the unused register list to determine the number }
    { of address registers which are available.                 }
    var
      i: byte;
    Begin
      i:=0;
      if R_A2 in unused then
        Inc(i);
      if R_A3 in unused then
        Inc(i);
      if R_A4 in unused then
         Inc(i);
      getusableaddr:=i;
    end;


    procedure ungetregister(r : tregister);

      begin
           ungetregister32(r)
      end;


    procedure del_reference(const ref : treference);

      begin
         if ref.is_immediate then
           exit;
         ungetregister(ref.base);
         ungetregister32(ref.index);
      end;

    procedure del_locref(const location : tlocation);

      begin
         if (location.loc<>loc_mem) and (location.loc<>loc_reference) then
           exit;
         if location.reference.is_immediate then
           exit;
         ungetregister(location.reference.base);
         ungetregister32(location.reference.index);
      end;


    procedure ungetregister32(r : tregister);

      begin
         if (r in [R_NO,R_A5,R_A6,R_SP]) then
           exit;
{$ifdef DEBUG}
         if r in unused then
           internalerror(1010);
{$endif DEBUG}
         exprasmlist^.concat(new(pairegalloc,dealloc(r)));
         if r in [{R_D0,R_D1,}R_D2,R_D3,R_D4,R_D5,R_D6,R_D7] then
          begin
             unused:=unused+[r];
             inc(usablereg32);
{$ifdef TEMPREGDEBUG}
             if curptree=nil then
               reg_releaser[r]:=nil
             else
               reg_releaser[r]:=curptree^;
{$endif TEMPREGDEBUG}
             exit;
          end
         else
         if r in [R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7] then
         begin
              unused:=unused+[r];
              inc(usablefloatreg);
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_releaser[r]:=nil
              else
                reg_releaser[r]:=curptree^;
{$endif TEMPREGDEBUG}
              exit;
         end
         else
         if r in [{R_A0,R_A1,}R_A2,R_A3,R_A4] then
           begin
              unused:=unused+[r];
              inc(usableaddress);
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_releaser[r]:=nil
              else
                reg_releaser[r]:=curptree^;
{$endif TEMPREGDEBUG}
              exit;
           end;
        { other registers are RESERVED and should not be freed }
        if r in [R_D0,R_D1,R_A0,R_A1,R_FP0,R_FP1] then
          begin
              unused:=unused+[r];
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_releaser[r]:=nil
              else
                reg_releaser[r]:=curptree^;
{$endif TEMPREGDEBUG}
              exit;
          end;
          internalerror(18);
      end;


    function getfloatreg: tregister;
    { returns a free floating point register }
    { used in real, fpu mode, otherwise we   }
    { must use standard register allocation  }
    var
     i:tregister;
    begin
      if usablefloatreg = 0 then
       Message(cg_f_internal_error_in_getfloatreg);
      dec(usablefloatreg);
      for i:=R_FP2 to R_FP7 do
      begin
         if i in unused then
         begin
           unused := unused-[i];
           usedinproc:=usedinproc + [i];
           getfloatreg := i;
           exprasmlist^.concat(new(pairegalloc,alloc(i)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[i]:=nil
              else
                reg_user[i]:=curptree^;
{$endif TEMPREGDEBUG}
           exit;
         end;
      end;
      { if we are here, then there was an allocation failure }
      Message(cg_f_internal_error_in_getfloatreg);
    end;


    function getaddressreg: tregister;

     begin
         dec(usableaddress);
         if R_A2 in unused then
           begin
              unused:=unused-[R_A2];
              usedinproc:=usedinproc + [R_A2];
              getaddressreg:=R_A2;
              exprasmlist^.concat(new(pairegalloc,alloc(R_A2)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_A2]:=nil
              else
                reg_user[R_A2]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else
         if R_A3 in unused then
           begin
              unused:=unused-[R_A3];
              usedinproc:=usedinproc + [R_A3];
              getaddressreg:=R_A3;
              exprasmlist^.concat(new(pairegalloc,alloc(R_A3)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_A3]:=nil
              else
                reg_user[R_A3]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else
         if R_A4 in unused then
           begin
              unused:=unused-[R_A4];
              usedinproc:=usedinproc + [R_A4];
              getaddressreg:=R_A4;
              exprasmlist^.concat(new(pairegalloc,alloc(R_A4)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_A4]:=nil
              else
                reg_user[R_A4]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else
         begin
           internalerror(10);
         end;

     end;

    function getregister32 : tregister;
      begin
         dec(usablereg32);
         if R_D2 in unused then
           begin
              unused:=unused-[R_D2];
              usedinproc:=usedinproc + [R_D2];
              getregister32:=R_D2;
              exprasmlist^.concat(new(pairegalloc,alloc(R_D2)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_D2]:=nil
              else
                reg_user[R_D2]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else if R_D3 in unused then
           begin
              unused:=unused-[R_D3];
              usedinproc:=usedinproc + [R_D3];
              getregister32:=R_D3;
              exprasmlist^.concat(new(pairegalloc,alloc(R_D3)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_D3]:=nil
              else
                reg_user[R_D3]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else if R_D4 in unused then
           begin
              unused:=unused-[R_D4];
              usedinproc:=usedinproc + [R_D4];
              getregister32:=R_D4;
              exprasmlist^.concat(new(pairegalloc,alloc(R_D4)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_D4]:=nil
              else
                reg_user[R_D4]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else if R_D5 in unused then
           begin
             unused:=unused-[R_D5];
             usedinproc:=usedinproc + [R_D5];
             getregister32:=R_D5;
             exprasmlist^.concat(new(pairegalloc,alloc(R_D5)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_D5]:=nil
              else
                reg_user[R_D5]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else if R_D6 in unused then
           begin
             unused:=unused-[R_D6];
             usedinproc:=usedinproc + [R_D6];
             getregister32:=R_D6;
             exprasmlist^.concat(new(pairegalloc,alloc(R_D6)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_D6]:=nil
              else
                reg_user[R_D6]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else if R_D7 in unused then
           begin
             unused:=unused-[R_D7];
             usedinproc:=usedinproc + [R_D7];
             getregister32:=R_D7;
             exprasmlist^.concat(new(pairegalloc,alloc(R_D7)));
{$ifdef TEMPREGDEBUG}
              if curptree=nil then
                reg_user[R_D7]:=nil
              else
                reg_user[R_D7]:=curptree^;
{$endif TEMPREGDEBUG}
           end
         else
         begin
          internalerror(10);
         end;
      end;

    procedure cleartempgen;

      begin
         unused:=usableregs;
         usablereg32:=c_usableregs;
         usableaddress:=getusableaddr;
         usablefloatreg:=6;
      end;


   procedure clearregistercount;
     var
       regi : tregister;
     begin
       for regi:=R_D0 to R_A6 do
         begin
           reg_pushes[regi]:=0;
           is_reg_var[regi]:=false;
         end;
     end;



{$ifdef TEMPREGDEBUG}
    procedure testregisters32;
     var test : byte;
       begin
         {test:=0;
         if R_EAX in unused then
           inc(test);
         if R_EBX in unused then
           inc(test);
         if R_ECX in unused then
           inc(test);
         if R_EDX in unused then
           inc(test);
         if test<>usablereg32 then
           internalerror(10);}
         { Dummy for now PM }
       end;
{$endif TEMPREGDEBUG}

   procedure resetusableregisters;
     begin
       usableregs:=[R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,R_A0,R_A1,R_A2,R_A3,R_A4,
             R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7];
       c_usableregs:=6;
       usableaddress:=3;
       usablefloatreg:=6;
     end;



    procedure saveusedregisters(var saved : tpushed; const tosave : tregisterset);

      var
         r : tregister;

      begin
         usedinproc := usedinproc + tosave;
         { the following registers can be pushed }
         { D0, D1                                }
         { D2, D3, D4, D5, D6, D7, A0            }
         { A1, A2, A3, A4                        }
         for r:=R_D0 to R_A4 do
           begin
              saved[r]:=false;
              { if the register is used by the calling subroutine    }
              if r in tosave then
                begin
                   { and is present in use }
                   if not(r in unused) then
                     begin
                        { then save it }
                        { then save it on the stack }
                        exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,r,R_SPPUSH)));
                        exprasmlist^.concat(new(pairegalloc,tostack(r)));
                        { here was a big problem  !!!!!}
                        { you cannot do that for a register that is
                        globally assigned to a var
                        this also means that you must push it much more
                        often, but there must be a better way
                        maybe by putting the value back to the stack !! }
                        if not(is_reg_var[r]) then
                          unused:=unused+[r];
                        saved[r]:=true;
{$ifdef TEMPREGDEBUG}
                        if curptree=nil then
                          reg_releaser[r]:=nil
                        else
                          reg_releaser[r]:=curptree^;
{$endif TEMPREGDEBUG}
                     end;
                end;
           end;
         { now for the floating point registers }
         if not (cs_fp_emulation in aktmoduleswitches) then
           begin
             for r:=R_FP2 to R_FP7 do
              begin
                saved[r]:=false;
                { if the register is used by the calling subroutine    }
                if r in tosave then
                  begin
                     { and is present in use }
                     if not(r in unused) then
                       begin
                          { then save it }
                          { then save it on the stack }
                          exprasmlist^.concat(new(paicpu,op_reg_reg(A_FMOVE,S_FX,r,R_SPPUSH)));
                          exprasmlist^.concat(new(pairegalloc,tostack(r)));
                          { here was a big problem  !!!!!}
                          { you cannot do that for a register that is
                          globally assigned to a var
                          this also means that you must push it much more
                           often, but there must be a better way
                          maybe by putting the value back to the stack !! }
                          {if not(is_reg_var[r]) then
                           FP regs are not part of this array PM }
                             unused:=unused+[r];
                          saved[r]:=true;
{$ifdef TEMPREGDEBUG}
                        if curptree=nil then
                          reg_releaser[r]:=nil
                        else
                          reg_releaser[r]:=curptree^;
{$endif TEMPREGDEBUG}
                       end;
                  end;
              end;
           end;
      end;


    procedure restoreusedregisters(const saved : tpushed);

      var
         r : tregister;

      begin
         { start with floating point registers }
         { only if the hardware is present.    }
         if not (cs_fp_emulation in aktmoduleswitches) then
           begin
             for r:=R_FP7 downto R_FP2 do
               begin
                 if saved[r] then
                   begin
                    exprasmlist^.concat(new(pairegalloc,fromstack(r)));
                    exprasmlist^.concat(new(paicpu,op_reg_reg(A_FMOVE,S_FX,R_SPPULL,r)));
                    unused:=unused-[r];
{$ifdef TEMPREGDEBUG}
                    if curptree=nil then
                      reg_user[r]:=nil
                    else
                      reg_user[r]:=curptree^;
{$endif TEMPREGDEBUG}
                   end;
              end;
          end;
         for r:=R_A4 downto R_D0 do
          begin
           if saved[r] then
             begin
                exprasmlist^.concat(new(pairegalloc,fromstack(r)));
                exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_SPPULL,r)));
                unused:=unused-[r];
{$ifdef TEMPREGDEBUG}
                    if curptree=nil then
                      reg_user[r]:=nil
                    else
                      reg_user[r]:=curptree^;
{$endif TEMPREGDEBUG}
             end;
          end;
      end;



    function getexplicitregister32(r : tregister) : tregister;

      begin
         { verify the values of the scratch registers }
         if r in [R_D0,R_D1,R_A0,R_A1,R_FP0,R_FP1] then
           begin
             if r in unused then
               begin
                 exprasmlist^.concat(new(pairegalloc,alloc(r)));
                 unused := unused - [r];
                 usedinproc := usedinproc + [r];
                 getexplicitregister32 := r;
{$ifdef TEMPREGDEBUG}
                 if curptree=nil then
                   reg_user[r]:=nil
                 else
                   reg_user[r]:=curptree^;
{$endif TEMPREGDEBUG}
                 exit;
               end
             else
                internalerror(10); { should never happend }
           end;
{
         if r in unused then
           begin
              dec(usablereg32);
              if curptree^^.usableregs-usablereg32>curptree^^.registers32 then
                internalerror(10);
              reg_user[r]:=curptree^;
              unused:=unused-[r];
              usedinproc:=usedinproc or ($80 shr byte(r));
              exprasmlist^.concat(new(pairegalloc,alloc(r)));
              getexplicitregister32:=r;
              testregisters32;
           end
         else
           getexplicitregister32:=getregister32;
}
      end;


    procedure del_location(const l : tlocation);
      begin
        case l.loc of
          LOC_REGISTER :
            ungetregister(l.register);
          LOC_MEM,LOC_REFERENCE :
            del_reference(l.reference);
        end;
      end;

begin
  resetusableregisters;
end.
{
  $Log: tgen.pas,v $
  Revision 1.1.2.17  2001/09/17 13:47:33  pierre
   + check if register not already unget if DEBUG cond is set

  Revision 1.1.2.16  2001/09/13 13:13:51  pierre
   * forgot to commit a new const needed by cga unit

  Revision 1.1.2.15  2001/07/30 11:51:58  pierre
   * some tempregdebug code added

  Revision 1.1.2.14  2001/07/25 22:59:53  pierre
   * correct the internalerror(10) position

  Revision 1.1.2.13  2001/07/24 23:40:23  pierre
   * also add used fpu regs to usedinproc

  Revision 1.1.2.12  2001/07/24 14:30:55  pierre
   * consider FP1 also as a scratch register

  Revision 1.1.2.11  2001/07/19 16:43:33  pierre
   + improoved register allocation info

  Revision 1.1.2.10  2001/07/18 15:29:42  pierre
   * avoid range check errors

  Revision 1.1.2.9  2001/07/18 11:41:48  pierre
   * correct errors in usableaddress counting

  Revision 1.1.2.8  2001/07/17 07:18:17  pierre
   * usablefloatreg was not reset correctly in cleartempgen

  Revision 1.1.2.7  2001/05/21 03:32:50  carl
  + getexplicitregister32 can now give back an ie 10.

  Revision 1.1.2.6  2001/05/18 18:00:37  carl
  * because of internalerror(10) more FPU registers can now be used
  + getexplicitregister32 now really allocates the register

  Revision 1.1.2.5  2001/04/19 11:37:37  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.4  2001/04/17 03:25:50  carl
  * correction of scratch registers
  + implemented saveusedregisters()
  * numerous bug fixes and updates

  Revision 1.1.2.3  2001/04/03 03:03:48  carl
  + some stuff changed (register numbers)

  Revision 1.1.2.2  2001/04/02 02:21:57  carl
  + changed the number of registers which can be allocated by the cg. (D6 is no longer a scratch!)

  Revision 1.1.2.1  2001/02/25 04:19:10  carl
  + rename festival!

  Revision 1.1.2.1  2001/02/25 01:31:21  carl
  - imported from main directory

  Revision 1.1.2.1  2001/02/23 10:05:20  pierre
   * first bunch of m68k cpu updates

  Revision 1.1  2000/07/13 06:30:01  michael
  + Initial import

  Revision 1.7  2000/02/09 13:23:08  peter
    * log truncated

  Revision 1.6  2000/01/07 01:14:47  peter
    * updated copyright to 2000

  Revision 1.5  1999/09/16 23:05:57  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}

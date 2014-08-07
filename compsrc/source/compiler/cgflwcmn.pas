{
    $Id: cgflwcmn.pas,v 1.1.2.2 2001/03/03 12:40:19 jonas Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate common assembler for nodes that influence the flow

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
Unit cgflwcmn;

interface

    uses
      tree;

    procedure second_while_repeatn(var p : ptree);
    procedure secondifn(var p : ptree);
    procedure secondbreakn(var p : ptree);
    procedure secondcontinuen(var p : ptree);
    procedure secondgoto(var p : ptree);
    procedure secondlabel(var p : ptree);
    procedure secondfail(var p : ptree);

    type
       tenumflowcontrol = (fc_exit,fc_break,fc_continue);
       tflowcontrol = set of tenumflowcontrol;

    var
       flowcontrol : tflowcontrol;

implementation

    uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cga,tgen,tcflw;


{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure second_while_repeatn(var p : ptree);
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : pasmlabel;
         otlabel,oflabel : pasmlabel;

      begin
         getlabel(lloop);
         getlabel(lcont);
         getlabel(lbreak);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;

         { handling code at the end as it is much more efficient, and makes
           while equal to repeat loop, only the end true/false is swapped (PFV) }
         if p^.treetype=whilen then
          emitjmp(C_None,lcont);

         emitlab(lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
         cleartempgen;
         if assigned(p^.right) then
           secondpass(p^.right);
         emitlab(lcont);
         otlabel:=truelabel;
         oflabel:=falselabel;
         if p^.treetype=whilen then
          begin
            truelabel:=lloop;
            falselabel:=lbreak;
          end
         { repeatn }
         else
          begin
            truelabel:=lbreak;
            falselabel:=lloop;
          end;
         cleartempgen;
         secondpass(p^.left);
         maketojumpbool(p^.left);
         emitlab(lbreak);
         truelabel:=otlabel;
         falselabel:=oflabel;

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a while/repeat block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;

{*****************************************************************************
                               SecondIfN
*****************************************************************************}

    procedure secondifn(var p : ptree);

      var
         hl,otlabel,oflabel : pasmlabel;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         cleartempgen;
         secondpass(p^.left);
         maketojumpbool(p^.left);
         if assigned(p^.right) then
           begin
              emitlab(truelabel);
              cleartempgen;
              secondpass(p^.right);
           end;
         if assigned(p^.t1) then
           begin
              if assigned(p^.right) then
                begin
                   getlabel(hl);
                   { do go back to if line !! }
                   aktfilepos:=exprasmlist^.getlasttaifilepos^;
                   emitjmp(C_None,hl);
                end;
              emitlab(falselabel);
              cleartempgen;
              secondpass(p^.t1);
              if assigned(p^.right) then
                emitlab(hl);
           end
         else
           begin
              emitlab(falselabel);
           end;
         if not(assigned(p^.right)) then
           begin
              emitlab(truelabel);
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure secondbreakn(var p : ptree);
      begin
         include(flowcontrol,fc_break);
         if aktbreaklabel<>nil then
           emitjmp(C_None,aktbreaklabel)
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure secondcontinuen(var p : ptree);
      begin
         include(flowcontrol,fc_continue);
         if aktcontinuelabel<>nil then
           emitjmp(C_None,aktcontinuelabel)
         else
           CGMessage(cg_e_continue_not_allowed);
      end;

{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure secondgoto(var p : ptree);

       begin
         emitjmp(C_None,p^.labelnr);
         { the assigned avoids only crashes if the label isn't defined }
         if assigned(p^.labsym) and
           assigned(p^.labsym^.code) and
            (aktexceptblock<>ptree(p^.labsym^.code)^.exceptionblock) then
           CGMessage(cg_e_goto_inout_of_exception_block);
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure secondlabel(var p : ptree);
      begin
         emitlab(p^.labelnr);
         cleartempgen;
         secondpass(p^.left);
      end;


{*****************************************************************************
                             SecondFail
*****************************************************************************}

    procedure secondfail(var p : ptree);
      begin
        emitjmp(C_None,faillabel);
      end;

end.

{
  $Log: cgflwcmn.pas,v $
  Revision 1.1.2.2  2001/03/03 12:40:19  jonas
    * fixed cvs Log tag

 Revision 1.1.2.1  2001/03/02 02:17:57  carl
   + separated from cpu specific files


}
{
    $Id: syncob.inc,v 1.3 2002/09/07 15:15:25 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TCriticalSection.Enter;

  begin
     Acquire;
  end;

procedure TCriticalSection.Leave;

  begin
     Release;
  end;

{
  $Log: syncob.inc,v $
  Revision 1.3  2002/09/07 15:15:25  peter
    * old logs removed and tabs fixed

}

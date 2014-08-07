{****************************************************************************

    $Id: pmgpi.pas,v 1.1.2.1 2002/11/02 13:27:53 hajny Exp $

                            PMGPI interface unit
                     FPC Pascal Runtime Library for OS/2
                   Copyright (c) 1999-2000 by Florian Kl�mpfl
                    Copyright (c) 1999-2000 by Ramon Bosque

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal Compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

 ****************************************************************************}

{Warning: This code is alfa. Future versions of this unit will propably
 not be compatible.}

unit pmgpi;

interface

uses    os2def,pmbitmap;

const   GPI_ERROR                           =       0;
        GPI_OK                              =       1;
        GPI_ALTERROR                        =    (-1);

        CLR_NOINDEX                         =  (-254);

        PU_ARBITRARY                        =  $0004;
        PU_PELS                             =  $0008;
        PU_LOMETRIC                         =  $000C;
        PU_HIMETRIC                         =  $0010;
        PU_LOENGLISH                        =  $0014;
        PU_HIENGLISH                        =  $0018;
        PU_TWIPS                            =  $001C;
        GPIF_DEFAULT                        =      0;
        GPIF_SHORT                          =  $0100;
        GPIF_LONG                           =  $0200;
        GPIT_NORMAL                         =      0;
        GPIT_MICRO                          =  $1000;
        GPIA_NOASSOC                        =      0;
        GPIA_ASSOC                          =  $4000;
        HDC_ERROR                           =     -1;

        GRES_ATTRS                          =  $0001;
        GRES_SEGMENTS                       =  $0002;
        GRES_ALL                            =  $0004;
        PS_UNITS                            =  $00FC;
        PS_FORMAT                           =  $0F00;
        PS_TYPE                             =  $1000;
        PS_MODE                             =  $2000;
        PS_ASSOCIATE                        =  $4000;
        PS_NORESET                          =  $8000;
        GPIE_SEGMENT                        =      0;
        GPIE_ELEMENT                        =      1;
        GPIE_DATA                           =      2;
        DCTL_ERASE                          =      1;
        DCTL_DISPLAY                        =      2;
        DCTL_BOUNDARY                       =      3;
        DCTL_DYNAMIC                        =      4;
        DCTL_CORRELATE                      =      5;
        DCTL_ERROR                          =     -1;
        DCTL_OFF                            =      0;
        DCTL_ON                             =      1;
        SDW_ERROR                           =     -1;
        SDW_OFF                             =      0;
        SDW_ON                              =      1;
        DM_ERROR                            =      0;
        DM_DRAW                             =      1;
        DM_RETAIN                           =      2;
        DM_DRAWANDRETAIN                    =      3;

        PICKAP_DEFAULT                      =      0;
        PICKAP_REC                          =      2;
        PICKSEL_VISIBLE                     =      0;
        PICKSEL_ALL                         =      1;
        GPI_HITS                            =      2;

        DFORM_NOCONV                        =      0;
        DFORM_S370SHORT                     =      1;
        DFORM_PCSHORT                       =      2;
        DFORM_PCLONG                        =      4;
        ATTR_ERROR                          =   (-1);
        ATTR_DETECTABLE                     =      1;
        ATTR_VISIBLE                        =      2;
        ATTR_CHAINED                        =      6;
        ATTR_DYNAMIC                        =      8;
        ATTR_FASTCHAIN                      =      9;
        ATTR_PROP_DETECTABLE                =     10;
        ATTR_PROP_VISIBLE                   =     11;
        ATTR_OFF                            =      0;
        ATTR_ON                             =      1;
        LOWER_PRI                           =   (-1);
        HIGHER_PRI                          =      1;

        SEGEM_ERROR                         =      0;
        SEGEM_INSERT                        =      1;
        SEGEM_REPLACE                       =      2;

        CVTC_WORLD                          =      1;
        CVTC_MODEL                          =      2;
        CVTC_DEFAULTPAGE                    =      3;
        CVTC_PAGE                           =      4;
        CVTC_DEVICE                         =      5;
        TRANSFORM_REPLACE                   =      0;
        TRANSFORM_ADD                       =      1;
        TRANSFORM_PREEMPT                   =      2;

        MPATH_STROKE                        =      6;
        FPATH_ALTERNATE                     =      0;
        FPATH_WINDING                       =      2;
        FPATH_EXCL                          =      0;
        FPATH_INCL                          =      8;
        SCP_ALTERNATE                       =      0;
        SCP_WINDING                         =      2;
        SCP_AND                             =      4;
        SCP_RESET                           =      0;
        SCP_EXCL                            =      0;
        SCP_INCL                            =      8;

        LCOL_RESET                          =  $0001;
        LCOL_REALIZABLE                     =  $0002;
        LCOL_PURECOLOR                      =  $0004;
        LCOL_OVERRIDE_DEFAULT_COLORS        =  $0008;
        LCOL_REALIZED                       =  $0010;
        LCOLF_DEFAULT                       =      0;
        LCOLF_INDRGB                        =      1;
        LCOLF_CONSECRGB                     =      2;
        LCOLF_RGB                           =      3;
        LCOLF_PALETTE                       =      4;
        LCOLOPT_REALIZED                    =  $0001;
        LCOLOPT_INDEX                       =  $0002;
        QLCT_ERROR                          =   (-1);
        QLCT_RGB                            =   (-2);
        QLCT_NOTLOADED                      =   (-1);
        QCD_LCT_FORMAT                      =      0;
        QCD_LCT_LOINDEX                     =      1;
        QCD_LCT_HIINDEX                     =      2;
        QCD_LCT_OPTIONS                     =      3;
        PAL_ERROR                           =   (-1);
        PC_RESERVED                         =    $01;
        PC_EXPLICIT                         =    $02;
        PC_NOCOLLAPSE                       =    $04;

        CLR_false                           =   (-5);
        CLR_true                            =   (-4);
        CLR_error                           = (-255);
        CLR_default                         =   (-3);
        CLR_white                           =   (-2);
        CLR_black                           =   (-1);
        CLR_background                      =      0;
        CLR_blue                            =      1;
        CLR_red                             =      2;
        CLR_pink                            =      3;
        CLR_green                           =      4;
        CLR_cyan                            =      5;
        CLR_yellow                          =      6;
        CLR_neutral                         =      7;
        CLR_darkgray                        =      8;
        CLR_darkblue                        =      9;
        CLR_darkred                         =     10;
        CLR_darkpink                        =     11;
        CLR_darkgreen                       =     12;
        CLR_darkcyan                        =     13;
        CLR_brown                           =     14;
        CLR_palegray                        =     15;

        RGB_error                        =    (-255);
        RGB_black                        = $00000000;
        RGB_blue                         = $000000FF;
        RGB_green                        = $0000FF00;
        RGB_cyan                         = $0000FFFF;
        RGB_red                          = $00FF0000;
        RGB_pink                         = $00FF00FF;
        RGB_yellow                       = $00FFFF00;
        RGB_white                        = $00FFFFFF;

        BA_NOBOUNDARY                       =      0;
        BA_BOUNDARY                         =  $0001;
        BA_ALTERNATE                        =      0;
        BA_WINDING                          =  $0002;
        BA_EXCL                             =      0;
        BA_INCL                             =      8;
        DRO_FILL                            =      1;
        DRO_OUTLINE                         =      2;
        DRO_OUTLINEFILL                     =      3;
        PATSYM_ERROR                        =   (-1);
        PATSYM_DEFAULT                      =      0;
        PATSYM_DENSE1                       =      1;
        PATSYM_DENSE2                       =      2;
        PATSYM_DENSE3                       =      3;
        PATSYM_DENSE4                       =      4;
        PATSYM_DENSE5                       =      5;
        PATSYM_DENSE6                       =      6;
        PATSYM_DENSE7                       =      7;
        PATSYM_DENSE8                       =      8;
        PATSYM_VERT                         =      9;
        PATSYM_HORIZ                        =     10;
        PATSYM_DIAG1                        =     11;
        PATSYM_DIAG2                        =     12;
        PATSYM_DIAG3                        =     13;
        PATSYM_DIAG4                        =     14;
        PATSYM_NOSHADE                      =     15;
        PATSYM_SOLID                        =     16;
        PATSYM_HALFTONE                     =     17;
        PATSYM_HATCH                        =     18;
        PATSYM_DIAGHATCH                    =     19;
        PATSYM_BLANK                        =     64;
        LCID_ERROR                          =   (-1);
        LCID_DEFAULT                        =      0;

        AM_ERROR                            =   (-1);
        AM_PRESERVE                         =      0;
        AM_NOPRESERVE                       =      1;
        FM_ERROR                            =   (-1);
        FM_DEFAULT                          =      0;
        FM_OR                               =      1;
        FM_OVERPAINT                        =      2;
        FM_LEAVEALONE                       =      5;
        FM_XOR                              =      4;
        FM_AND                              =      6;
        FM_SUBTRACT                         =      7;
        FM_MASKSRCNOT                       =      8;
        FM_ZERO                             =      9;
        FM_NOTMERGESRC                      =     10;
        FM_NOTXORSRC                        =     11;
        FM_INVERT                           =     12;
        FM_MERGESRCNOT                      =     13;
        FM_NOTCOPYSRC                       =     14;
        FM_MERGENOTSRC                      =     15;
        FM_NOTMASKSRC                       =     16;
        FM_ONE                              =     17;
        BM_ERROR                            =   (-1);
        BM_DEFAULT                          =      0;
        BM_OR                               =      1;
        BM_OVERPAINT                        =      2;
        BM_LEAVEALONE                       =      5;
        BM_XOR                              =      4;
        BM_AND                              =      6;
        BM_SUBTRACT                         =      7;
        BM_MASKSRCNOT                       =      8;
        BM_ZERO                             =      9;
        BM_NOTMERGESRC                      =     10;
        BM_NOTXORSRC                        =     11;
        BM_INVERT                           =     12;
        BM_MERGESRCNOT                      =     13;
        BM_NOTCOPYSRC                       =     14;
        BM_MERGENOTSRC                      =     15;
        BM_NOTMASKSRC                       =     16;
        BM_ONE                              =     17;
        BM_SRCTRANSPARENT                   =     18;
        BM_DESTTRANSPARENT                  =     19;
        LINETYPE_ERROR                      =   (-1);
        LINETYPE_DEFAULT                    =      0;
        LINETYPE_DOT                        =      1;
        LINETYPE_SHORTDASH                  =      2;
        LINETYPE_DASHDOT                    =      3;
        LINETYPE_DOUBLEDOT                  =      4;
        LINETYPE_LONGDASH                   =      5;
        LINETYPE_DASHDOUBLEDOT              =      6;
        LINETYPE_SOLID                      =      7;
        LINETYPE_INVISIBLE                  =      8;
        LINETYPE_ALTERNATE                  =      9;
        LINEWIDTH_ERROR                     =   (-1);
        LINEWIDTH_DEFAULT                   =      0;
        LINEWIDTH_NORMAL                =  $00010000;
        LINEWIDTH_THICK                 =  $00020000;
        LINEWIDTHGEOM_ERROR             =       (-1);
        LINEEND_ERROR                       =   (-1);
        LINEEND_DEFAULT                     =      0;
        LINEEND_FLAT                        =      1;
        LINEEND_SQUARE                      =      2;
        LINEEND_ROUND                       =      3;
        LINEJOIN_ERROR                      =   (-1);
        LINEJOIN_DEFAULT                    =      0;
        LINEJOIN_BEVEL                      =      1;
        LINEJOIN_ROUND                      =      2;
        LINEJOIN_MITRE                      =      3;
        CHDIRN_ERROR                        =   (-1);
        CHDIRN_DEFAULT                      =      0;
        CHDIRN_LEFTRIGHT                    =      1;
        CHDIRN_TOPBOTTOM                    =      2;
        CHDIRN_RIGHTLEFT                    =      3;
        CHDIRN_BOTTOMTOP                    =      4;
        TA_NORMAL_HORIZ                     =  $0001;
        TA_LEFT                             =  $0002;
        TA_CENTER                           =  $0003;
        TA_RIGHT                            =  $0004;
        TA_STANDARD_HORIZ                   =  $0005;
        TA_NORMAL_VERT                      =  $0100;
        TA_TOP                              =  $0200;
        TA_HALF                             =  $0300;
        TA_BASE                             =  $0400;
        TA_BOTTOM                           =  $0500;
        TA_STANDARD_VERT                    =  $0600;
        CM_ERROR                            =   (-1);
        CM_DEFAULT                          =      0;
        CM_MODE1                            =      1;
        CM_MODE2                            =      2;
        CM_MODE3                            =      3;
        MARKSYM_ERROR                       =   (-1);
        MARKSYM_DEFAULT                     =      0;
        MARKSYM_CROSS                       =      1;
        MARKSYM_PLUS                        =      2;
        MARKSYM_DIAMOND                     =      3;
        MARKSYM_SQUARE                      =      4;
        MARKSYM_SIXPOINTSTAR                =      5;
        MARKSYM_EIGHTPOINTSTAR              =      6;
        MARKSYM_SOLIDDIAMOND                =      7;
        MARKSYM_SOLIDSQUARE                 =      8;
        MARKSYM_DOT                         =      9;
        MARKSYM_SMALLCIRCLE                 =     10;
        MARKSYM_BLANK                       =     64;
        CHS_OPAQUE                          =  $0001;
        CHS_VECTOR                          =  $0002;
        CHS_LEAVEPOS                        =  $0008;
        CHS_CLIP                            =  $0010;
        CHS_UNDERSCORE                      =  $0200;
        CHS_STRIKEOUT                       =  $0400;
        PRIM_LINE                           =      1;
        PRIM_CHAR                           =      2;
        PRIM_MARKER                         =      3;
        PRIM_AREA                           =      4;
        PRIM_IMAGE                          =      5;
        LBB_COLOR                           =  $0001;
        LBB_BACK_COLOR                      =  $0002;
        LBB_MIX_MODE                        =  $0004;
        LBB_BACK_MIX_MODE                   =  $0008;
        LBB_WIDTH                           =  $0010;
        LBB_GEOM_WIDTH                      =  $0020;
        LBB_TYPE                            =  $0040;
        LBB_END                             =  $0080;
        LBB_JOIN                            =  $0100;
        CBB_COLOR                           =  $0001;
        CBB_BACK_COLOR                      =  $0002;
        CBB_MIX_MODE                        =  $0004;
        CBB_BACK_MIX_MODE                   =  $0008;
        CBB_SET                             =  $0010;
        CBB_MODE                            =  $0020;
        CBB_BOX                             =  $0040;
        CBB_ANGLE                           =  $0080;
        CBB_SHEAR                           =  $0100;
        CBB_DIRECTION                       =  $0200;
        CBB_TEXT_ALIGN                      =  $0400;
        CBB_EXTRA                           =  $0800;
        CBB_BREAK_EXTRA                     =  $1000;
        MBB_COLOR                           =  $0001;
        MBB_BACK_COLOR                      =  $0002;
        MBB_MIX_MODE                        =  $0004;
        MBB_BACK_MIX_MODE                   =  $0008;
        MBB_SET                             =  $0010;
        MBB_SYMBOL                          =  $0020;
        MBB_BOX                             =  $0040;
        ABB_COLOR                           =  $0001;
        ABB_BACK_COLOR                      =  $0002;
        ABB_MIX_MODE                        =  $0004;
        ABB_BACK_MIX_MODE                   =  $0008;
        ABB_SET                             =  $0010;
        ABB_SYMBOL                          =  $0020;
        ABB_REF_POINT                       =  $0040;
        IBB_COLOR                           =  $0001;
        IBB_BACK_COLOR                      =  $0002;
        IBB_MIX_MODE                        =  $0004;
        IBB_BACK_MIX_MODE                   =  $0008;

        TXTBOX_TOPLEFT                      =      0;
        TXTBOX_BOTTOMLEFT                   =      1;
        TXTBOX_TOPRIGHT                     =      2;
        TXTBOX_BOTTOMRIGHT                  =      3;
        TXTBOX_CONCAT                       =      4;
        TXTBOX_COUNT                        =      5;
        PVIS_ERROR                          =      0;
        PVIS_INVISIBLE                      =      1;
        PVIS_VISIBLE                        =      2;
        RVIS_ERROR                          =      0;
        RVIS_INVISIBLE                      =      1;
        RVIS_PARTIAL                        =      2;
        RVIS_VISIBLE                        =      3;

        FONT_DEFAULT                        =      1;
        FONT_MATCH                          =      2;
        LCIDT_FONT                          =      6;
        LCIDT_BITMAP                        =      7;
        LCID_ALL                            =   (-1);

        FWEIGHT_DONT_CARE                   =      0;
        FWEIGHT_ULTRA_LIGHT                 =      1;
        FWEIGHT_EXTRA_LIGHT                 =      2;
        FWEIGHT_LIGHT                       =      3;
        FWEIGHT_SEMI_LIGHT                  =      4;
        FWEIGHT_NORMAL                      =      5;
        FWEIGHT_SEMI_BOLD                   =      6;
        FWEIGHT_BOLD                        =      7;
        FWEIGHT_EXTRA_BOLD                  =      8;
        FWEIGHT_ULTRA_BOLD                  =      9;
        FWIDTH_DONT_CARE                    =      0;
        FWIDTH_ULTRA_CONDENSED              =      1;
        FWIDTH_EXTRA_CONDENSED              =      2;
        FWIDTH_CONDENSED                    =      3;
        FWIDTH_SEMI_CONDENSED               =      4;
        FWIDTH_NORMAL                       =      5;
        FWIDTH_SEMI_EXPANDED                =      6;
        FWIDTH_EXPANDED                     =      7;
        FWIDTH_EXTRA_EXPANDED               =      8;
        FWIDTH_ULTRA_EXPANDED               =      9;
        FTYPE_ITALIC                        =  $0001;
        FTYPE_ITALIC_DONT_CARE              =  $0002;
        FTYPE_OBLIQUE                       =  $0004;
        FTYPE_OBLIQUE_DONT_CARE             =  $0008;
        FTYPE_ROUNDED                       =  $0010;
        FTYPE_ROUNDED_DONT_CARE             =  $0020;
        QFA_PUBLIC                          =      1;
        QFA_PRIVATE                         =      2;
        QFA_ERROR                      =GPI_ALTERROR;
        QF_PUBLIC                           =  $0001;
        QF_PRIVATE                          =  $0002;
        QF_NO_GENERIC                       =  $0004;
        QF_NO_DEVICE                        =  $0008;

        ROP_SRCCOPY                         =  $00CC;
        ROP_SRCPAINT                        =  $00EE;
        ROP_SRCAND                          =  $0088;
        ROP_SRCINVERT                       =  $0066;
        ROP_SRCERASE                        =  $0044;
        ROP_NOTSRCCOPY                      =  $0033;
        ROP_NOTSRCERASE                     =  $0011;
        ROP_MERGECOPY                       =  $00C0;
        ROP_MERGEPAINT                      =  $00BB;
        ROP_PATCOPY                         =  $00F0;
        ROP_PATPAINT                        =  $00FB;
        ROP_PATINVERT                       =  $005A;
        ROP_DSTINVERT                       =  $0055;
        ROP_ZERO                            =  $0000;
        ROP_ONE                             =  $00FF;
        BBO_OR                              =      0;
        BBO_AND                             =      1;
        BBO_IGNORE                          =      2;
        BBO_PAL_COLORS                      =      4;
        BBO_NO_COLOR_INFO                   =      8;
        FF_BOUNDARY                         =      0;
        FF_SURFACE                          =      1;
        HBM_ERROR                           =     -1;

        {Bitmaps}
        CBM_INIT                            =  $0004;
        BMB_ERROR                           =   (-1);

        {Regions}
        CRGN_OR                             =      1;
        CRGN_COPY                           =      2;
        CRGN_XOR                            =      4;
        CRGN_AND                            =      6;
        CRGN_DIFF                           =      7;
        RECTDIR_LFRT_TOPBOT                 =      1;
        RECTDIR_RTLF_TOPBOT                 =      2;
        RECTDIR_LFRT_BOTTOP                 =      3;
        RECTDIR_RTLF_BOTTOP                 =      4;
        RGN_ERROR                           =      0;
        RGN_NULL                            =      1;
        RGN_RECT                            =      2;
        RGN_COMPLEX                         =      3;
        PRGN_ERROR                          =      0;
        PRGN_OUTSIDE                        =      1;
        PRGN_INSIDE                         =      2;
        RRGN_ERROR                          =      0;
        RRGN_OUTSIDE                        =      1;
        RRGN_PARTIAL                        =      2;
        RRGN_INSIDE                         =      3;
        EQRGN_ERROR                         =      0;
        EQRGN_NOTEQUAL                      =      1;
        EQRGN_EQUAL                         =      2;
        HRGN_ERROR                          =     -1;

        {Metafiles}
        PMF_SEGBASE                         =      0;
        PMF_LOADTYPE                        =      1;
        PMF_RESOLVE                         =      2;
        PMF_LCIDS                           =      3;
        PMF_RESET                           =      4;
        PMF_SUPPRESS                        =      5;
        PMF_COLORTABLES                     =      6;
        PMF_COLORREALIZABLE                 =      7;
        PMF_DEFAULTS                        =      8;
        PMF_DELETEOBJECTS                   =      9;
        RS_DEFAULT                          =      0;
        RS_NODISCARD                        =      1;
        LC_DEFAULT                          =      0;
        LC_NOLOAD                           =      1;
        LC_LOADDISC                         =      3;
        LT_DEFAULT                          =      0;
        LT_NOMODIFY                         =      1;
        LT_ORIGINALVIEW                     =      4;
        RES_DEFAULT                         =      0;
        RES_NORESET                         =      1;
        RES_RESET                           =      2;
        SUP_DEFAULT                         =      0;
        SUP_NOSUPPRESS                      =      1;
        SUP_SUPPRESS                        =      2;
        CTAB_DEFAULT                        =      0;
        CTAB_NOMODIFY                       =      1;
        CTAB_REPLACE                        =      3;
        CTAB_REPLACEPALETTE                 =      4;
        CREA_DEFAULT                        =      0;
        CREA_REALIZE                        =      1;
        CREA_NOREALIZE                      =      2;
        CREA_DOREALIZE                      =      3;
        DDEF_DEFAULT                        =      0;
        DDEF_IGNORE                         =      1;
        DDEF_LOADDISC                       =      3;
        DOBJ_DEFAULT                        =      0;
        DOBJ_NODELETE                       =      1;
        DOBJ_DELETE                         =      2;
        RSP_DEFAULT                         =      0;
        RSP_NODISCARD                       =      1;

        {Polygons}
        POLYGON_NOBOUNDARY                  =      0;
        POLYGON_BOUNDARY                    =  $0001;
        POLYGON_ALTERNATE                   =      0;
        POLYGON_WINDING                     =  $0002;
        POLYGON_EXCL                        =      0;
        POLYGON_INCL                        =  $0008;

type    SizeL=record
            cx,cy:longint;
        end;
        PSizeL=^SizeL;
        TSizeL=SizeL;

        MatrixLF=record
            fxm11:longint;
            fxm12:longint;
            lm13:longint;
            fxm21:longint;
            fxm22:longint;
            lm23:longint;
            lm31:longint;
            lm32:longint;
            lm33:longint;
        end;
        PMatrixLF=^MatrixLF;
        TMatrixLF=MatrixLF;

        ArcParams=record
            lp,lq,lr,ls:longint;
        end;
        PArcParams=^ArcParams;
        TArcParams=ArcParams;

        SizeF=record
            cx,cy:longint;
        end;
        PSizeF=^SizeF;
        TSizeF=SizeF;

        GradientL=record
            x,y:longint;
        end;
        PGradientL=^GradientL;
        TGradientL=GradientL;

        LineBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            fxWidth:longint;
            lGeomWidth:longint;
            usType:word;
            usEnd:word;
            usJoin:word;
            usReserved:word;
        end;
        PLineBundle=^LineBundle;
        TLineBundle=LineBundle;

        CharBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            usSet:word;
            usPrecision:word;
            sizfxCell:sizef;
            ptlAngle:pointl;
            ptlShear:pointl;
            usDirection:word;
            usTextAlign:word;
            fxExtra:longint;
            fxBreakExtra:longint;
        end;
        PCharBundle=^CharBundle;
        TCharBundle=CharBundle;

        MarkerBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            usSet:word;
            usSymbol:word;
            sizFxCell:SizeF;
        end;
        PMarkerBundle=^MarkerBundle;
        TMarkerBundle=MarkerBundle;

        AreaBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            usSet:word;
            usSymbol:word;
            ptlRefPoint:pointl;
        end;
        PAreaBundle=^AreaBundle;
        TAreaBundle=AreaBundle;

        ImageBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
        end;
        PImageBundle=^ImageBundle;
        TImageBundle=ImageBundle;

        KerningPairs=record
            sFirstChar:integer;
            sSecondChar:integer;
            lKerningAmount:longint;
        end;
        PKerningPairs=^KerningPairs;
        TKerningPairs=KerningPairs;

        FaceNameDesc=record
            usSize:word;
            usWeightClass:word;
            usWidthClass:word;
            usReserved:word;
            flOptions:cardinal;
        end;
        PFaceNameDesc=^FaceNameDesc;
        TFaceNameDesc=FaceNameDesc;

        FFDescs=array[0..1,0..FaceSize-1] of char;
        PFFDescs=^FFDescs;
        TFFDescs = FFDescs;

        FFDescs2=record
            cbLength:cardinal;
            cbFacenameOffset:cardinal;
            abFamilyName:array[0..1-1] of byte;
        end;
        PFFDescs2=^FFDescs2;
        TFFDescs2=FFDescs2;

        RgnRect=record
            ircStart:cardinal;
            crc:cardinal;
            crcReturned:cardinal;
            ulDirection:cardinal;
        end;
        PRgnRect=^RgnRect;
        TRgnRect=RgnRect;

        Polygon=record
            ulPoints:cardinal;
            aPointl:Ppointl;
        end;
        PPolygon=^Polygon;
        TPolygon=Polygon;

        Polyset=record
            ulPolys:cardinal;
            aPolygon:array[0..1-1] of TPolygon;
        end;
        PPolyset=^Polyset;
        TPolyset=Polyset;

function GpiCreatePS(hab,hdc : cardinal;var psizlSize : SIZEL;flOptions : cardinal) : cardinal;cdecl;
function GpiDestroyPS(hps : cardinal) : longbool;cdecl;
function GpiAssociate(hps,hdc : cardinal) : longbool;cdecl;
function GpiRestorePS(hps : cardinal;lPSid : longint) : longbool;cdecl;
function GpiSavePS(hps : cardinal) : longint;cdecl;
function GpiErase(hps : cardinal) : longbool;cdecl;
function GpiQueryDevice(hps : cardinal) : cardinal;cdecl;
function GpiResetPS(hps,flOptions : cardinal) : longbool;cdecl;
function GpiSetPS(hps : cardinal;var psizlsize : SIZEL;flOptions : cardinal) : longbool;cdecl;
function GpiQueryPS(hps : cardinal;var psizlSize : SIZEL) : cardinal;cdecl;
function GpiErrorSegmentData(hps : cardinal;var plSegment,plContext : longint) : longint; cdecl;
function GpiQueryDrawControl(hps : cardinal;lControl : longint) : longint;cdecl;
function GpiSetDrawControl(hps : cardinal;lControl,lValue : longint) : longbool;cdecl;
function GpiQueryDrawingMode(hps : cardinal) : longint;cdecl;
function GpiSetDrawingMode(hps : cardinal;lMode : longint) : longbool;cdecl;
function GpiQueryStopDraw(hps : cardinal) : longint;cdecl;
function GpiSetStopDraw(hps : cardinal;lValue : longint) : longbool;cdecl;
function GpiCorrelateChain(hps : cardinal;lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var pl2 : longint) : longint;cdecl;
function GpiQueryTag(hps : cardinal;var plTag : longint) : longbool;cdecl;
function GpiSetTag(hps : cardinal;lTag : longint) : longbool;cdecl;
function GpiQueryPickApertureSize(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl;
function GpiSetPickApertureSize(hps : cardinal;lOptions : longint;var psizlSize : SIZEL) : longbool; cdecl;
function GpiQueryPickAperturePosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiSetPickAperturePosition(hps : cardinal;var pptlPick : POINTL) : longbool; cdecl;
function GpiQueryBoundaryData(hps : cardinal;var prclBoundary : RECTL) : longbool; cdecl;
function GpiResetBoundaryData(hps : cardinal) : longbool; cdecl;
function GpiCorrelateFrom(hps : cardinal;lFirstSegment,lLastSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var plSegTag : longint) : longint; cdecl;
function GpiCorrelateSegment(hps : cardinal;lSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var alSegTag : longint) : longint; cdecl;
function GpiOpenSegment(hps : cardinal;lSegment : longint) : longbool; cdecl;
function GpiCloseSegment(hps : cardinal) : longbool; cdecl;
function GpiDeleteSegment(hps : cardinal;lSegid : longint) : longbool; cdecl;
function GpiQueryInitialSegmentAttrs(hps : cardinal;lAttribute : longint) : longint; cdecl;
function GpiSetInitialSegmentAttrs(hps : cardinal;lAttribute,lValue : longint) : longbool; cdecl;
function GpiQuerySegmentAttrs(hps : cardinal;lSegid,lAttribute : longint) : longint; cdecl;
function GpiSetSegmentAttrs(hps : cardinal;lSegid,lAttribute,lValue : longint) : longbool; cdecl;
function GpiQuerySegmentPriority(hps : cardinal;lRefSegid,lOrder : longint) : longint; cdecl;
function GpiSetSegmentPriority(hps : cardinal;lSegid,lRefSegid,lOrder : longint) : longbool; cdecl;
function GpiDeleteSegments(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl;
function GpiQuerySegmentNames(hps : cardinal;lFirstSegid,lLastSegid,lMax : longint;var alSegids : longint) : longint; cdecl;
function GpiGetData(hps : cardinal;lSegid : longint;var plOffset : longint;lFormat,lLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiPutData(hps : cardinal;lFormat : longint;var plCount : longint;var pbData : BYTE) : longint; cdecl;
function GpiDrawChain(hps : cardinal) : longbool; cdecl;
function GpiDrawFrom(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl;
function GpiDrawSegment(hps : cardinal;lSegment : longint) : longbool; cdecl;
function GpiDrawDynamics(hps : cardinal) : longbool; cdecl;
function GpiRemoveDynamics(hps : cardinal;lFirstSegid,lLastSegid : longint) : longbool; cdecl;
function GpiBeginElement(hps : cardinal;lType : longint;pszDesc : pchar) : longbool; cdecl;
function GpiEndElement(hps : cardinal) : longbool; cdecl;
function GpiLabel(hps : cardinal;lLabel : longint) : longbool; cdecl;
function GpiElement(hps : cardinal;lType : longint;pszDesc : pchar;lLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiQueryElement(hps : cardinal;lOff,lMaxLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiDeleteElement(hps : cardinal) : longbool; cdecl;
function GpiDeleteElementRange(hps : cardinal;lFirstElement,lLastElement : longint) : longbool; cdecl;
function GpiDeleteElementsBetweenLabels(hps : cardinal;lFirstLabel,lLastLabel : longint) : longbool; cdecl;
function GpiQueryEditMode(hps : cardinal) : longint; cdecl;
function GpiSetEditMode(hps : cardinal;lMode : longint) : longbool; cdecl;
function GpiQueryElementPointer(hps : cardinal) : longint; cdecl;
function GpiSetElementPointer(hps : cardinal;lElement : longint) : longbool; cdecl;
function GpiOffsetElementPointer(hps : cardinal;loffset : longint) : longbool; cdecl;
function GpiQueryElementType(hps : cardinal;var plType : longint;lLength : longint;pszData : pchar) : longint; cdecl;
function GpiSetElementPointerAtLabel(hps : cardinal;lLabel : longint) : longbool; cdecl;
function GpiQuerySegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetSegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiConvert(hps : cardinal;lSrc,lTarg,lCount : longint;var aptlPoints : POINTL) : longbool; cdecl;
function GpiConvertWithMatrix(hps : cardinal;lCountp : longint;var aptlPoints : POINTL;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiQueryModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiCallSegmentMatrix(hps : cardinal;lSegment,lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longint; cdecl;
function GpiQueryDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiQueryPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl;
function GpiSetPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl;
function GpiQueryViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiTranslate(hps : cardinal;var pmatrixlf : MATRIXLF;long : longint;var ppointl : POINTL) : longbool; cdecl;
function GpiScale(hps : cardinal;var p1 : MATRIXLF;p2 : longint;var p3 : longint;var p4 : POINTL) : longbool; cdecl;
function GpiRotate(p1 : cardinal;var p2 : MATRIXLF;p3,p4 : longint;var p5 : POINTL) : longbool; cdecl;
function GpiSetGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl;
function GpiQueryGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl;
function GpiSetViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiQueryViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiBeginPath(hps : cardinal;lPath : longint) : longbool; cdecl;
function GpiEndPath(hps : cardinal) : longbool; cdecl;
function GpiCloseFigure(hps : cardinal) : longbool; cdecl;
function GpiModifyPath(hps : cardinal;lPath,lMode : longint) : longbool; cdecl;
function GpiFillPath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl;
function GpiSetClipPath(hps : cardinal;lPath,lOptions : longint) : longbool; cdecl;
function GpiOutlinePath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl;
function GpiPathToRegion(GpiH : cardinal;lPath,lOptions : longint) : cardinal; cdecl;
function GpiStrokePath(hps : cardinal;lPath : longint;flOptions : cardinal) : longint; cdecl;
function GpiCreateLogColorTable(hps,flOptions : cardinal;lFormat,lStart,lCount : longint;var alTable : longint) : longbool; cdecl;
function GpiQueryColorData(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl;
function GpiQueryLogColorTable(hps,flOptions : cardinal;lStart,lCount : longint;var alArray : longint) : longint; cdecl;
function GpiQueryRealColors(hps,flOptions : cardinal;lStart,lCount : longint;var alColors : longint) : longint; cdecl;
function GpiQueryNearestColor(hps,flOptions : cardinal;lRgbIn : longint) : longint; cdecl;
function GpiQueryColorIndex(hps,flOptions : cardinal;lRgbColor : longint) : longint; cdecl;
function GpiQueryRGBColor(hps,flOptions : cardinal;lColorIndex : longint) : longint; cdecl;
function GpiCreatePalette(hab,flOptions,ulFormat,ulCount : cardinal;var aulTable) : cardinal; cdecl;
function GpiDeletePalette(hpal : cardinal) : longbool; cdecl;
function GpiSelectPalette(hps,hpal : cardinal) : cardinal; cdecl;
function GpiAnimatePalette(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longint; cdecl;
function GpiSetPaletteEntries(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longbool; cdecl;
function GpiQueryPalette(hps : cardinal) : cardinal; cdecl;
function GpiQueryPaletteInfo(hpal,hps,flOptions,ulStart,ulCount : cardinal;var aulArray) : longint; cdecl;
function GpiSetColor(hps : cardinal;lColor : longint) : longbool; cdecl;
function GpiQueryColor(hps : cardinal) : longint; cdecl;
function GpiBox(hps : cardinal;lControl : longint;var pptlPoint : POINTL;lHRound,lVRound : longint) : longint; cdecl;
function GpiMove(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiLine(hps : cardinal;var pptlEndPoint : POINTL) : longint; cdecl;
function GpiPolyLine(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiPolyLineDisjoint(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiSetPattern(hps : cardinal;lPatternSymbol : longint) : longbool; cdecl;
function GpiQueryPattern(hps : cardinal) : longint;  cdecl;
function GpiBeginArea(hps,flOptions : cardinal) : longbool; cdecl;
function GpiEndArea(hps : cardinal) : longint; cdecl;
function GpiCharString(hps : cardinal;lCount : longint;pchString : pchar) : longint; cdecl;
function GpiCharStringAt(hps : cardinal;var pptlPoint : POINTL;lCount : longint;pchString : pchar) : longint; cdecl;
function GpiSetAttrMode(hps : cardinal;lMode : longint) : longbool; cdecl;
function GpiQueryAttrMode(hps : cardinal) : longint; cdecl;
function GpiSetAttrs(hps : cardinal;lPrimType : longint;flAttrMask,flDefMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl;
function GpiQueryAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longint; cdecl;
function GpiSetBackColor(hps : cardinal;lColor : longint) : longbool; cdecl;
function GpiQueryBackColor(hps : cardinal) : longint; cdecl;
function GpiSetMix(hps : cardinal;lMixMode : longint) : longbool; cdecl;
function GpiQueryMix(hps : cardinal) : longint; cdecl;
function GpiSetBackMix(hps : cardinal;lMixMode : longint) : longbool; cdecl;
function GpiQueryBackMix(hps : cardinal) : longint; cdecl;
function GpiSetLineType(hps : cardinal;lLineType : longint) : longbool; cdecl;
function GpiQueryLineType(hps : cardinal) : longint; cdecl;
function GpiSetLineWidth(hps : cardinal;fxLineWidth : longint) : longbool; cdecl;
function GpiQueryLineWidth(hps : cardinal) : longint; cdecl;
function GpiSetLineWidthGeom(hps : cardinal;lLineWidth : longint) : longbool; cdecl;
function GpiQueryLineWidthGeom(hps : cardinal) : longint; cdecl;
function GpiSetLineEnd(hps : cardinal;lLineEnd : longint) : longbool; cdecl;
function GpiQueryLineEnd(hps : cardinal) : longint; cdecl;
function GpiSetLineJoin(hps : cardinal;lLineJoin : longint) : longbool; cdecl;
function GpiQueryLineJoin(hps : cardinal) : longint; cdecl;
function GpiSetCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiQueryCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiSetArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiQueryArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiPointArc(hps : cardinal;var pptl2 : POINTL) : longint; cdecl;
function GpiFullArc(hps : cardinal;lControl,fxMultiplier : longint) : longint; cdecl;
function GpiPartialArc(hps : cardinal;var pptlCenter : POINTL;fxMultiplier,fxStartAngle,fxSweepAngle : longint) : longint; cdecl;
function GpiPolyFillet(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiPolySpline(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiPolyFilletSharp(hps : cardinal;lCount : longint;var aptlPoints : POINTL;var afxPoints : longint) : longint; cdecl;
function GpiSetPatternSet(hps : cardinal;lSet : longint) : longbool; cdecl;
function GpiQueryPatternSet(hps : cardinal) : longint; cdecl;
function GpiSetPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl;
function GpiQueryPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl;
function GpiQueryCharStringPos(hps,flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl;
function GpiQueryCharStringPosAt(hps : cardinal;var pptlStart : POINTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl;
function GpiQueryTextBox(hps : cardinal;lCount1 : longint;pchString : pchar;lCount2 : longint;var aptlPoints : POINTL) : longbool; cdecl;
function GpiQueryDefCharBox(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl;
function GpiSetCharSet(hps : cardinal;llcid : longint) : longbool; cdecl;
function GpiQueryCharSet(hps : cardinal) : longint; cdecl;
function GpiSetCharBox(hps : cardinal;var psizfxBox : SIZEF) : longbool; cdecl;
function GpiQueryCharBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl;
function GpiSetCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl;
function GpiQueryCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl;
function GpiSetCharShear(hps : cardinal;var pptlAngle : POINTL) : longbool; cdecl;
function GpiQueryCharShear(hps : cardinal;var pptlShear : POINTL) : longbool; cdecl;
function GpiSetCharDirection(hps : cardinal;lDirection : longint) : longbool; cdecl;
function GpiQueryCharDirection(hps : cardinal) : longint; cdecl;
function GpiSetCharMode(hps : cardinal;lMode : longint) : longbool; cdecl;
function GpiQueryCharMode(hps : cardinal) : longint; cdecl;
function GpiSetTextAlignment(hps : cardinal;lHoriz,lVert : longint) : longbool; cdecl;
function GpiQueryTextAlignment(hps : cardinal;var plHoriz,plVert : longint) : longbool; cdecl;
function GpiCharStringPos(hps : cardinal;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl;
function GpiCharStringPosAt(hps : cardinal;var pptlStart : POINTL;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl;
function GpiSetCharExtra(hps : cardinal;Extra : longint) : longbool;  cdecl;
function GpiSetCharBreakExtra(hps : cardinal;BreakExtra : longint) : longbool; cdecl;
function GpiQueryCharExtra(hps : cardinal;var Extra : longint) : longbool; cdecl;
function GpiQueryCharBreakExtra(hps : cardinal;var BreakExtra : longint) : longbool; cdecl;
function GpiMarker(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiPolyMarker(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiSetMarker(hps : cardinal;lSymbol : longint) : longbool; cdecl;
function GpiSetMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl;
function GpiSetMarkerSet(hps : cardinal;lSet : longint) : longbool; cdecl;
function GpiQueryMarker(hps : cardinal) : longint; cdecl;
function GpiQueryMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl;
function GpiQueryMarkerSet(hps : cardinal) : longint; cdecl;
function GpiImage(hps : cardinal;lFormat : longint;var psizlImageSize : SIZEL;lLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiPop(hps : cardinal;lCount : longint) : longbool; cdecl;
function GpiPtVisible(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiRectVisible(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl;
function GpiComment(hps : cardinal;lLength : longint;var pbData : BYTE) : longbool; cdecl;
function GpiCreateLogFont(hps : cardinal;var pName : STR8;lLcid : longint;var pfatAttrs : FATTRS) : longint; cdecl;
function GpiDeleteSetId(hps : cardinal;lLcid : longint) : longbool; cdecl;
function GpiLoadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl;
function GpiUnloadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl;
function GpiQueryFonts(hps,flOptions : cardinal;pszFacename : pchar;var plReqFonts : longint;lMetricsLength : longint;var afmMetrics : FONTMETRICS) : longint; cdecl;
function GpiQueryFontMetrics(hps : cardinal;lMetricsLength : longint;var pfmMetrics : FONTMETRICS) : longbool; cdecl;
function GpiQueryKerningPairs(hps : cardinal;lCount : longint;var akrnprData : KERNINGPAIRS) : longint; cdecl;
function GpiQueryWidthTable(hps : cardinal;lFirstChar,lCount : longint;var alData : longint) : longbool; cdecl;
function GpiQueryNumberSetIds(hps : cardinal) : longint; cdecl;
function GpiQuerySetIds(hps : cardinal;lCount : longint;var alTypes : longint;var aNames : STR8;var allcids : longint) : longbool; cdecl;
function GpiQueryFaceString(PS : cardinal;FamilyName : pchar;var attrs : FACENAMEDESC;length : longint;CompoundFaceName : pchar) : cardinal; cdecl;
function GpiQueryLogicalFont(PS : cardinal;lcid : longint;var name : STR8;var attrs : FATTRS;length : longint) : longbool; cdecl;
function GpiQueryFontAction(anchor,options : cardinal) : cardinal; cdecl;
function GpiLoadPublicFonts(p1 : cardinal;p2 : pchar):longbool; cdecl;
function GpiUnloadPublicFonts(p1 : cardinal;p2 : pchar) : longbool; cdecl;
function GpiSetCp(hps,ulCodePage : cardinal) : longbool;  cdecl;
function GpiQueryCp(hps : cardinal) : cardinal; cdecl;
function GpiQueryFontFileDescriptions(hab : cardinal;pszFilename : pchar;var plCount : longint;var affdescsNames : FFDESCS) : longint;  cdecl;
function GpiQueryFullFontFileDescs(hab : cardinal;pszFilename : pchar;var plCount : longint;pNames : pointer;var plNamesBuffLength : longint) : longint; cdecl;
function GpiBitBlt(hpsTarget,hpsSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl;
function GpiDeleteBitmap(hbm : cardinal) : longbool; cdecl;
function GpiLoadBitmap(hps,Resource,idBitmap : cardinal;lWidth,lHeight : longint) : cardinal; cdecl;
function GpiSetBitmap(hps,hbm : cardinal) : cardinal; cdecl;
function GpiWCBitBlt(hpsTarget,hbmSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl;
function GpiCreateBitmap(hps : cardinal;var pbmpNew : Tbitmapinfoheader2;flOptions : cardinal;var pbInitData : BYTE;var pbmiInfoTable : Tbitmapinfo2) : cardinal; cdecl;
function GpiSetBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable : Tbitmapinfo2) : longint; cdecl;
function GpiSetBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl;
function GpiSetBitmapId(hps,hbm : cardinal;lLcid : longint) : longbool; cdecl;
function GpiQueryBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable :Tbitmapinfo2) : longint; cdecl;
function GpiQueryBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl;
function GpiQueryBitmapHandle(hps : cardinal;lLcid : longint) : cardinal;  cdecl;
function GpiQueryBitmapParameters(hbm : cardinal;var pbmpData : Tbitmapinfoheader) : longbool; cdecl;
function GpiQueryBitmapInfoHeader(hbm : cardinal;var pbmpData : Tbitmapinfoheader2) : longbool; cdecl;
function GpiQueryDeviceBitmapFormats(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl;
function GpiSetPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiQueryPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiFloodFill(hps : cardinal;lOptions,lColor : longint) : longint; cdecl;
function GpiDrawBits(hps : cardinal;pBits : pointer;var pbmiInfoTable :Tbitmapinfo2;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl;
function GpiCombineRegion(hps,hrgnDest,hrgnSrc1,hrgnSrc2 : cardinal;lMode : longint) : longint;  cdecl;
function GpiCreateRegion(hps : cardinal;lCount : longint;var arclRectangles : RECTL) : cardinal;  cdecl;
function GpiDestroyRegion(hps,hrgn : cardinal) : longbool;  cdecl;
function GpiEqualRegion(hps,hrgnSrc1,hrgnSrc2 : cardinal) : longint; cdecl;
function GpiOffsetRegion(hps,Hrgn : cardinal;var pptlOffset : POINTL) : longbool; cdecl;
function GpiPaintRegion(hps,hrgn : cardinal) : longint; cdecl;
function GpiFrameRegion(hps,hrgn : cardinal;var thickness : SIZEL) : longint; cdecl;
function GpiPtInRegion(hps,hrgn : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiQueryRegionBox(hps,hrgn : cardinal;var prclBound : RECTL) : longint; cdecl;
function GpiQueryRegionRects(hps,hrgn : cardinal;var prclBound : RECTL;var prgnrcControl : RGNRECT;var prclRect : RECTL) : longbool; cdecl;
function GpiRectInRegion(hps,hrgn : cardinal;var prclRect : RECTL) : longint; cdecl;
function GpiSetRegion(hps,hrgn : cardinal;lcount : longint;var arclRectangles : RECTL) : longbool;cdecl;
function GpiSetClipRegion(hps,hrgn : cardinal;var phrgnOld : cardinal) : longint; cdecl;
function GpiQueryClipRegion(hps : cardinal) : cardinal;  cdecl;
function GpiQueryClipBox(hps : cardinal;var prclBound : RECTL) : longint; cdecl;
function GpiExcludeClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl;
function GpiIntersectClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl;
function GpiOffsetClipRegion(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiCopyMetaFile(hmf : cardinal) : cardinal; cdecl;
function GpiDeleteMetaFile(hmf : cardinal) : longbool; cdecl;
function GpiLoadMetaFile(hab : cardinal;pszFilename : pchar) : cardinal; cdecl;
function GpiPlayMetaFile(hps,hmf : cardinal;lCount1 : longint;var alOptarray,plSegCount : longint;lCount2 : longint;pszDesc : pchar) : longint;  cdecl;
function GpiQueryMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbData : BYTE) : longbool;  cdecl;
function GpiQueryMetaFileLength(hmf : cardinal) : longint;  cdecl;
function GpiSaveMetaFile(hmf : cardinal;pszFilename : pchar) : longbool; cdecl;
function GpiSetMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbBuffer : BYTE) : longbool; cdecl;
function GpiQueryDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiQueryDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl;
function GpiQueryDefTag(hps : cardinal;var plTag : longint) : longbool; cdecl;
function GpiQueryDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiSetDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiSetDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool;cdecl;
function GpiSetDefTag(hps : cardinal;lTag : longint) : longbool; cdecl;
function GpiSetDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiPolygons(hps,ulCount : cardinal;var paplgn : POLYGON;flOptions,flModel : cardinal) : longint; cdecl;

implementation

function GpiCreatePS(hab,hdc : cardinal;var psizlSize : SIZEL;flOptions : cardinal) : cardinal;cdecl;external 'pmgpi' index 369;
function GpiDestroyPS(hps : cardinal) : longbool;cdecl;external 'pmgpi' index 379;
function GpiAssociate(hps,hdc : cardinal) : longbool;cdecl;external 'pmgpi' index 351;
function GpiRestorePS(hps : cardinal;lPSid : longint) : longbool;cdecl;external 'pmgpi' index 499;
function GpiSavePS(hps : cardinal) : longint;cdecl;external 'pmgpi' index 501;
function GpiErase(hps : cardinal) : longbool;cdecl;external 'pmgpi' index 389;
function GpiQueryDevice(hps : cardinal) : cardinal;cdecl;external 'pmgpi' index 444;
function GpiResetPS(hps,flOptions : cardinal) : longbool;cdecl;external 'pmgpi' index 498;
function GpiSetPS(hps : cardinal;var psizlsize : SIZEL;flOptions : cardinal) : longbool;cdecl;external 'pmgpi' index 539;
function GpiQueryPS(hps : cardinal;var psizlSize : SIZEL) : cardinal;cdecl;external 'pmgpi' index 471;
function GpiErrorSegmentData(hps : cardinal;var plSegment,plContext : longint) : longint;cdecl;external 'pmgpi' index 390;
function GpiQueryDrawControl(hps : cardinal;lControl : longint) : longint;cdecl;external 'pmgpi' index 446;
function GpiSetDrawControl(hps : cardinal;lControl,lValue : longint) : longbool;cdecl;external 'pmgpi' index 521;
function GpiQueryDrawingMode(hps : cardinal) : longint;cdecl;external 'pmgpi' index 447;
function GpiSetDrawingMode(hps : cardinal;lMode : longint) : longbool;cdecl;external 'pmgpi' index 522;
function GpiQueryStopDraw(hps : cardinal) : longint;cdecl; external 'pmgpi' index 487;
function GpiSetStopDraw(hps : cardinal;lValue : longint) : longbool; cdecl; external 'pmgpi' index 550;
function GpiCorrelateChain(hps : cardinal;lType : longint;var pptlPick : POINTL;lMaxHits : longint;lMaxDepth : longint;var pl2 : longint) : longint; cdecl; external 'pmgpi' index 366;
function GpiQueryTag(hps : cardinal;var plTag : longint) : longbool; cdecl; external 'pmgpi' index 488;
function GpiSetTag(hps : cardinal;lTag : longint) : longbool; cdecl; external 'pmgpi' index 551;
function GpiQueryPickApertureSize(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl; external 'pmgpi' index 478;
function GpiSetPickApertureSize(hps : cardinal;lOptions : longint;var psizlSize : SIZEL) : longbool; cdecl; external 'pmgpi' index 589;
function GpiQueryPickAperturePosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 477;
function GpiSetPickAperturePosition(hps : cardinal;var pptlPick : POINTL) : longbool; cdecl; external 'pmgpi' index 545;
function GpiQueryBoundaryData(hps : cardinal;var prclBoundary : RECTL) : longbool; cdecl; external 'pmgpi' index 428;
function GpiResetBoundaryData(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 497;
function GpiCorrelateFrom(hps : cardinal;lFirstSegment,lLastSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var plSegTag : longint) : longint; cdecl; external 'pmgpi' index 367;
function GpiCorrelateSegment(hps : cardinal;lSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var alSegTag : longint) : longint; cdecl; external 'pmgpi' index 582;
function GpiOpenSegment(hps : cardinal;lSegment : longint) : longbool; cdecl; external 'pmgpi' index 408;
function GpiCloseSegment(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 361;
function GpiDeleteSegment(hps : cardinal;lSegid : longint) : longbool; cdecl; external 'pmgpi' index 376;
function GpiQueryInitialSegmentAttrs(hps : cardinal;lAttribute : longint) : longint; cdecl; external 'pmgpi' index 455;
function GpiSetInitialSegmentAttrs(hps : cardinal;lAttribute,lValue : longint) : longbool; cdecl; external 'pmgpi' index 527;
function GpiQuerySegmentAttrs(hps : cardinal;lSegid,lAttribute : longint) : longint; cdecl; external 'pmgpi' index 482;
function GpiSetSegmentAttrs(hps : cardinal;lSegid,lAttribute,lValue : longint) : longbool; cdecl; external 'pmgpi' index 547;
function GpiQuerySegmentPriority(hps : cardinal;lRefSegid,lOrder : longint) : longint; cdecl; external 'pmgpi' index 484;
function GpiSetSegmentPriority(hps : cardinal;lSegid,lRefSegid,lOrder : longint) : longbool; cdecl; external 'pmgpi' index 548;
function GpiDeleteSegments(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl; external 'pmgpi' index 377;
function GpiQuerySegmentNames(hps : cardinal;lFirstSegid,lLastSegid,lMax : longint;var alSegids : longint) : longint; cdecl; external 'pmgpi' index 483;
function GpiGetData(hps : cardinal;lSegid : longint;var plOffset : longint;lFormat,lLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 394;
function GpiPutData(hps : cardinal;lFormat : longint;var plCount : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 421;
function GpiDrawChain(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 380;
function GpiDrawFrom(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl; external 'pmgpi' index 382;
function GpiDrawSegment(hps : cardinal;lSegment : longint) : longbool; cdecl; external 'pmgpi' index 383;
function GpiDrawDynamics(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 381;
function GpiRemoveDynamics(hps : cardinal;lFirstSegid,lLastSegid : longint) : longbool; cdecl; external 'pmgpi' index 496;
function GpiBeginElement(hps : cardinal;lType : longint;pszDesc : pchar) : longbool; cdecl; external 'pmgpi' index 353;
function GpiEndElement(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 386;
function GpiLabel(hps : cardinal;lLabel : longint) : longbool; cdecl; external 'pmgpi' index 397;
function GpiElement(hps : cardinal;lType : longint;pszDesc : pchar;lLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 384;
function GpiQueryElement(hps : cardinal;lOff,lMaxLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 449;
function GpiDeleteElement(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 372;
function GpiDeleteElementRange(hps : cardinal;lFirstElement,lLastElement : longint) : longbool; cdecl; external 'pmgpi' index 373;
function GpiDeleteElementsBetweenLabels(hps : cardinal;lFirstLabel,lLastLabel : longint) : longbool; cdecl; external 'pmgpi' index 374;
function GpiQueryEditMode(hps : cardinal) : longint; cdecl; external 'pmgpi' index 448;
function GpiSetEditMode(hps : cardinal;lMode : longint) : longbool; cdecl; external 'pmgpi' index 523;
function GpiQueryElementPointer(hps : cardinal) : longint; cdecl; external 'pmgpi' index 450;
function GpiSetElementPointer(hps : cardinal;lElement : longint) : longbool; cdecl; external 'pmgpi' index 524;
function GpiOffsetElementPointer(hps : cardinal;loffset : longint) : longbool; cdecl; external 'pmgpi' index 406;
function GpiQueryElementType(hps : cardinal;var plType : longint;lLength : longint;pszData : pchar) : longint; cdecl; external 'pmgpi' index 451;
function GpiSetElementPointerAtLabel(hps : cardinal;lLabel : longint) : longbool; cdecl; external 'pmgpi' index 525;
function GpiQuerySegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 485;
function GpiSetSegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 549;
function GpiConvert(hps : cardinal;lSrc,lTarg,lCount : longint;var aptlPoints : POINTL) : longbool; cdecl; external 'pmgpi' index 364;
function GpiConvertWithMatrix(hps : cardinal;lCountp : longint;var aptlPoints : POINTL;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 618;
function GpiQueryModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 468;
function GpiSetModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 538;
function GpiCallSegmentMatrix(hps : cardinal;lSegment,lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longint; cdecl; external 'pmgpi' index 357;
function GpiQueryDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 443;
function GpiSetDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 520;
function GpiQueryPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl; external 'pmgpi' index 472;
function GpiSetPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl; external 'pmgpi' index 540;
function GpiQueryViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 491;
function GpiSetViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 553;
function GpiTranslate(hps : cardinal;var pmatrixlf : MATRIXLF;long : longint;var ppointl : POINTL) : longbool; cdecl; external 'pmgpi' index 564;
function GpiScale(hps : cardinal;var p1 : MATRIXLF;p2 : longint;var p3 : longint;var p4 : POINTL) : longbool; cdecl; external 'pmgpi' index 565;
function GpiRotate(p1 : cardinal;var p2 : MATRIXLF;p3,p4 : longint;var p5 : POINTL) : longbool; cdecl; external 'pmgpi' index 566;
function GpiSetGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl; external 'pmgpi' index 526;
function GpiQueryGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl; external 'pmgpi' index 454;
function GpiSetViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 552;
function GpiQueryViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 490;
function GpiBeginPath(hps : cardinal;lPath : longint) : longbool; cdecl; external 'pmgpi' index 354;
function GpiEndPath(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 387;
function GpiCloseFigure(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 360;
function GpiModifyPath(hps : cardinal;lPath,lMode : longint) : longbool; cdecl; external 'pmgpi' index 403;
function GpiFillPath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl; external 'pmgpi' index 392;
function GpiSetClipPath(hps : cardinal;lPath,lOptions : longint) : longbool; cdecl; external 'pmgpi' index 515;
function GpiOutlinePath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl; external 'pmgpi' index 563;
function GpiPathToRegion(GpiH : cardinal;lPath,lOptions : longint) : cardinal; cdecl; external 'pmgpi' index 559;
function GpiStrokePath(hps : cardinal;lPath : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 554;
function GpiCreateLogColorTable(hps,flOptions : cardinal;lFormat,lStart,lCount : longint;var alTable : longint) : longbool; cdecl; external 'pmgpi' index 592;
function GpiQueryColorData(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl; external 'pmgpi' index 438;
function GpiQueryLogColorTable(hps,flOptions : cardinal;lStart,lCount : longint;var alArray : longint) : longint; cdecl; external 'pmgpi' index 593;
function GpiQueryRealColors(hps,flOptions : cardinal;lStart,lCount : longint;var alColors : longint) : longint; cdecl; external 'pmgpi' index 480;
function GpiQueryNearestColor(hps,flOptions : cardinal;lRgbIn : longint) : longint; cdecl; external 'pmgpi' index 469;
function GpiQueryColorIndex(hps,flOptions : cardinal;lRgbColor : longint) : longint; cdecl; external 'pmgpi' index 439;
function GpiQueryRGBColor(hps,flOptions : cardinal;lColorIndex : longint) : longint; cdecl; external 'pmgpi' index 479;
function GpiCreatePalette(hab,flOptions,ulFormat,ulCount : cardinal;var aulTable) : cardinal; cdecl; external 'pmgpi' index 594;
function GpiDeletePalette(hpal : cardinal) : longbool; cdecl; external 'pmgpi' index 577;
function GpiSelectPalette(hps,hpal : cardinal) : cardinal; cdecl; external 'pmgpi' index 578;
function GpiAnimatePalette(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longint; cdecl; external 'pmgpi' index 595;
function GpiSetPaletteEntries(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longbool; cdecl; external 'pmgpi' index 596;
function GpiQueryPalette(hps : cardinal) : cardinal; cdecl; external 'pmgpi' index 579;
function GpiQueryPaletteInfo(hpal,hps,flOptions,ulStart,ulCount : cardinal;var aulArray) : longint; cdecl; external 'pmgpi' index 597;
function GpiSetColor(hps : cardinal;lColor : longint) : longbool; cdecl; external 'pmgpi' index 517;
function GpiQueryColor(hps : cardinal) : longint; cdecl; external 'pmgpi' index 437;
function GpiBox(hps : cardinal;lControl : longint;var pptlPoint : POINTL;lHRound,lVRound : longint) : longint; cdecl; external 'pmgpi' index 356;
function GpiMove(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 404;
function GpiLine(hps : cardinal;var pptlEndPoint : POINTL) : longint; cdecl; external 'pmgpi' index 398;
function GpiPolyLine(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 415;
function GpiPolyLineDisjoint(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 558;
function GpiSetPattern(hps : cardinal;lPatternSymbol : longint) : longbool; cdecl; external 'pmgpi' index 541;
function GpiQueryPattern(hps : cardinal) : longint; cdecl; external 'pmgpi' index 473;
function GpiBeginArea(hps,flOptions : cardinal) : longbool; cdecl; external 'pmgpi' index 352;
function GpiEndArea(hps : cardinal) : longint; cdecl; external 'pmgpi' index 385;
function GpiCharString(hps : cardinal;lCount : longint;pchString : pchar) : longint; cdecl; external 'pmgpi' index 358;
function GpiCharStringAt(hps : cardinal;var pptlPoint : POINTL;lCount : longint;pchString : pchar) : longint; cdecl; external 'pmgpi' index 359;
function GpiSetAttrMode(hps : cardinal;lMode : longint) : longbool; cdecl; external 'pmgpi' index 503;
function GpiQueryAttrMode(hps : cardinal) : longint; cdecl; external 'pmgpi' index 423;
function GpiSetAttrs(hps : cardinal;lPrimType : longint;flAttrMask,flDefMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl; external 'pmgpi' index 588;
function GpiQueryAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longint; cdecl; external 'pmgpi' index 583;
function GpiSetBackColor(hps : cardinal;lColor : longint) : longbool; cdecl; external 'pmgpi' index 504;
function GpiQueryBackColor(hps : cardinal) : longint; cdecl; external 'pmgpi' index 424;
function GpiSetMix(hps : cardinal;lMixMode : longint) : longbool; cdecl; external 'pmgpi' index 537;
function GpiQueryMix(hps : cardinal) : longint; cdecl; external 'pmgpi' index 467;
function GpiSetBackMix(hps : cardinal;lMixMode : longint) : longbool; cdecl; external 'pmgpi' index 505;
function GpiQueryBackMix(hps : cardinal) : longint; cdecl; external 'pmgpi' index 425;
function GpiSetLineType(hps : cardinal;lLineType : longint) : longbool; cdecl; external 'pmgpi' index 530;
function GpiQueryLineType(hps : cardinal) : longint; cdecl; external 'pmgpi' index 459;
function GpiSetLineWidth(hps : cardinal;fxLineWidth : longint) : longbool; cdecl; external 'pmgpi' index 531;
function GpiQueryLineWidth(hps : cardinal) : longint; cdecl; external 'pmgpi' index 460;
function GpiSetLineWidthGeom(hps : cardinal;lLineWidth : longint) : longbool; cdecl; external 'pmgpi' index 532;
function GpiQueryLineWidthGeom(hps : cardinal) : longint; cdecl; external 'pmgpi' index 461;
function GpiSetLineEnd(hps : cardinal;lLineEnd : longint) : longbool; cdecl; external 'pmgpi' index 528;
function GpiQueryLineEnd(hps : cardinal) : longint; cdecl; external 'pmgpi' index 457;
function GpiSetLineJoin(hps : cardinal;lLineJoin : longint) : longbool; cdecl; external 'pmgpi' index 529;
function GpiQueryLineJoin(hps : cardinal) : longint; cdecl; external 'pmgpi' index 458;
function GpiSetCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 519;
function GpiQueryCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 441;
function GpiSetArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 502;
function GpiQueryArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 422;
function GpiPointArc(hps : cardinal;var pptl2 : POINTL) : longint; cdecl; external 'pmgpi' index 412;
function GpiFullArc(hps : cardinal;lControl,fxMultiplier : longint) : longint; cdecl; external 'pmgpi' index 393;
function GpiPartialArc(hps : cardinal;var pptlCenter : POINTL;fxMultiplier,fxStartAngle,fxSweepAngle : longint) : longint; cdecl; external 'pmgpi' index 612;
function GpiPolyFillet(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 413;
function GpiPolySpline(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 417;
function GpiPolyFilletSharp(hps : cardinal;lCount : longint;var aptlPoints : POINTL;var afxPoints : longint) : longint; cdecl; external 'pmgpi' index 414;
function GpiSetPatternSet(hps : cardinal;lSet : longint) : longbool; cdecl; external 'pmgpi' index 543;
function GpiQueryPatternSet(hps : cardinal) : longint; cdecl; external 'pmgpi' index 475;
function GpiSetPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 542;
function GpiQueryPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 474;
function GpiQueryCharStringPos(hps,flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl; external 'pmgpi' index 584;
function GpiQueryCharStringPosAt(hps : cardinal;var pptlStart : POINTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl; external 'pmgpi' index 585;
function GpiQueryTextBox(hps : cardinal;lCount1 : longint;pchString : pchar;lCount2 : longint;var aptlPoints : POINTL) : longbool; cdecl; external 'pmgpi' index 489;
function GpiQueryDefCharBox(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl; external 'pmgpi' index 442;
function GpiSetCharSet(hps : cardinal;llcid : longint) : longbool; cdecl; external 'pmgpi' index 513;
function GpiQueryCharSet(hps : cardinal) : longint; cdecl; external 'pmgpi' index 433;
function GpiSetCharBox(hps : cardinal;var psizfxBox : SIZEF) : longbool; cdecl; external 'pmgpi' index 510;
function GpiQueryCharBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl; external 'pmgpi' index 430;
function GpiSetCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl; external 'pmgpi' index 509;
function GpiQueryCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl; external 'pmgpi' index 429;
function GpiSetCharShear(hps : cardinal;var pptlAngle : POINTL) : longbool; cdecl; external 'pmgpi' index 514;
function GpiQueryCharShear(hps : cardinal;var pptlShear : POINTL) : longbool; cdecl; external 'pmgpi' index 434;
function GpiSetCharDirection(hps : cardinal;lDirection : longint) : longbool; cdecl; external 'pmgpi' index 511;
function GpiQueryCharDirection(hps : cardinal) : longint; cdecl; external 'pmgpi' index 431;
function GpiSetCharMode(hps : cardinal;lMode : longint) : longbool; cdecl; external 'pmgpi' index 512;
function GpiQueryCharMode(hps : cardinal) : longint; cdecl; external 'pmgpi' index 432;
function GpiSetTextAlignment(hps : cardinal;lHoriz,lVert : longint) : longbool; cdecl; external 'pmgpi' index 649;
function GpiQueryTextAlignment(hps : cardinal;var plHoriz,plVert : longint) : longbool; cdecl; external 'pmgpi' index 648;
function GpiCharStringPos(hps : cardinal;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl; external 'pmgpi' index 580;
function GpiCharStringPosAt(hps : cardinal;var pptlStart : POINTL;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl; external 'pmgpi' index 581;
function GpiSetCharExtra(hps : cardinal;Extra : longint) : longbool; cdecl; external 'pmgpi' index 614;
function GpiSetCharBreakExtra(hps : cardinal;BreakExtra : longint) : longbool; cdecl; external 'pmgpi' index 616;
function GpiQueryCharExtra(hps : cardinal;var Extra : longint) : longbool; cdecl; external 'pmgpi' index 613;
function GpiQueryCharBreakExtra(hps : cardinal;var BreakExtra : longint) : longbool; cdecl; external 'pmgpi' index 615;
function GpiMarker(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 402;
function GpiPolyMarker(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 416;
function GpiSetMarker(hps : cardinal;lSymbol : longint) : longbool; cdecl; external 'pmgpi' index 533;
function GpiSetMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl; external 'pmgpi' index 534;
function GpiSetMarkerSet(hps : cardinal;lSet : longint) : longbool; cdecl; external 'pmgpi' index 535;
function GpiQueryMarker(hps : cardinal) : longint; cdecl; external 'pmgpi' index 462;
function GpiQueryMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl; external 'pmgpi' index 463;
function GpiQueryMarkerSet(hps : cardinal) : longint; cdecl; external 'pmgpi' index 464;
function GpiImage(hps : cardinal;lFormat : longint;var psizlImageSize : SIZEL;lLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 395;
function GpiPop(hps : cardinal;lCount : longint) : longbool; cdecl; external 'pmgpi' index 418;
function GpiPtVisible(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 420;
function GpiRectVisible(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl; external 'pmgpi' index 495;
function GpiComment(hps : cardinal;lLength : longint;var pbData : BYTE) : longbool; cdecl; external 'pmgpi' index 363;
function GpiCreateLogFont(hps : cardinal;var pName : STR8;lLcid : longint;var pfatAttrs : FATTRS) : longint; cdecl; external 'pmgpi' index 368;
function GpiDeleteSetId(hps : cardinal;lLcid : longint) : longbool; cdecl; external 'pmgpi' index 378;
function GpiLoadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl; external 'pmgpi' index 400;
function GpiUnloadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl; external 'pmgpi' index 555;
function GpiQueryFonts(hps,flOptions : cardinal;pszFacename : pchar;var plReqFonts : longint;lMetricsLength : longint;var afmMetrics : FONTMETRICS) : longint; cdecl; external 'pmgpi' index 586;
function GpiQueryFontMetrics(hps : cardinal;lMetricsLength : longint;var pfmMetrics : FONTMETRICS) : longbool; cdecl; external 'pmgpi' index 453;
function GpiQueryKerningPairs(hps : cardinal;lCount : longint;var akrnprData : KERNINGPAIRS) : longint; cdecl; external 'pmgpi' index 456;
function GpiQueryWidthTable(hps : cardinal;lFirstChar,lCount : longint;var alData : longint) : longbool; cdecl; external 'pmgpi' index 492;
function GpiQueryNumberSetIds(hps : cardinal) : longint; cdecl; external 'pmgpi' index 470;
function GpiQuerySetIds(hps : cardinal;lCount : longint;var alTypes : longint;var aNames : STR8;var allcids : longint) : longbool; cdecl; external 'pmgpi' index 486;
function GpiQueryFaceString(PS : cardinal;FamilyName : pchar;var attrs : FACENAMEDESC;length : longint;CompoundFaceName : pchar) : cardinal; cdecl; external 'pmgpi' index 575;
function GpiQueryLogicalFont(PS : cardinal;lcid : longint;var name : STR8;var attrs : FATTRS;length : longint) : longbool; cdecl; external 'pmgpi' index 574;
function GpiQueryFontAction(anchor,options : cardinal) : cardinal; cdecl; external 'pmgpi' index 576;
function GpiLoadPublicFonts(p1 : cardinal;p2 : pchar) : longbool; cdecl; external 'pmgpi' index 622;
function GpiUnloadPublicFonts(p1 : cardinal;p2 : pchar) : longbool; cdecl; external 'pmgpi' index 623;
function GpiSetCp(hps,ulCodePage : cardinal) : longbool; cdecl; external 'pmgpi' index 518;
function GpiQueryCp(hps : cardinal) : cardinal; cdecl; external 'pmgpi' index 440;
function GpiQueryFontFileDescriptions(hab : cardinal;pszFilename : pchar;var plCount : longint;var affdescsNames : FFDESCS) : longint; cdecl; external 'pmgpi' index 452;
function GpiQueryFullFontFileDescs(hab : cardinal;pszFilename : pchar;var plCount : longint;pNames : pointer;var plNamesBuffLength : longint) : longint; cdecl; external 'pmgpi' index 657;
function GpiBitBlt(hpsTarget,hpsSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 355;
function GpiDeleteBitmap(hbm : cardinal) : longbool; cdecl; external 'pmgpi' index 371;
function GpiLoadBitmap(hps,Resource,idBitmap:cardinal;lWidth,lHeight : longint) : cardinal; cdecl; external 'pmgpi' index 399;
function GpiSetBitmap(hps,hbm : cardinal) : cardinal; cdecl; external 'pmgpi' index 506;
function GpiWCBitBlt(hpsTarget,hbmSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 557;
function GpiCreateBitmap(hps : cardinal;var pbmpNew :Tbitmapinfoheader2;flOptions : cardinal;var pbInitData : BYTE;var pbmiInfoTable :Tbitmapinfo2) : cardinal; cdecl; external 'pmgpi' index 598;
function GpiSetBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable :Tbitmapinfo2) : longint; cdecl; external 'pmgpi' index 602;
function GpiSetBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl; external 'pmgpi' index 507;
function GpiSetBitmapId(hps,hbm : cardinal;lLcid : longint) : longbool; cdecl; external 'pmgpi' index 508;
function GpiQueryBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable :Tbitmapinfo2) : longint; cdecl; external 'pmgpi' index 599;
function GpiQueryBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl; external 'pmgpi' index 426;
function GpiQueryBitmapHandle(hps : cardinal;lLcid : longint) : cardinal; cdecl; external 'pmgpi' index 427;
function GpiQueryBitmapParameters(hbm : cardinal;var pbmpData :Tbitmapinfoheader) : longbool; cdecl; external 'pmgpi' index 573;
function GpiQueryBitmapInfoHeader(hbm : cardinal;var pbmpData :Tbitmapinfoheader2) : longbool; cdecl; external 'pmgpi' index 601;
function GpiQueryDeviceBitmapFormats(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl; external 'pmgpi' index 445;
function GpiSetPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 544;
function GpiQueryPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 476;
function GpiFloodFill(hps : cardinal;lOptions,lColor : longint) : longint; cdecl; external 'pmgpi' index 560;
function GpiDrawBits(hps : cardinal;pBits : pointer;var pbmiInfoTable :Tbitmapinfo2;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 603;
function GpiCombineRegion(hps,hrgnDest,hrgnSrc1,hrgnSrc2 : cardinal;lMode : longint) : longint; cdecl; external 'pmgpi' index 362;
function GpiCreateRegion(hps : cardinal;lCount : longint;var arclRectangles : RECTL) : cardinal; cdecl; external 'pmgpi' index 370;
function GpiDestroyRegion(hps,hrgn : cardinal) : longbool; cdecl; external 'pmgpi' index 611;
function GpiEqualRegion(hps,hrgnSrc1,hrgnSrc2 : cardinal) : longint; cdecl; external 'pmgpi' index 388;
function GpiOffsetRegion(hps,Hrgn : cardinal;var pptlOffset : POINTL) : longbool; cdecl; external 'pmgpi' index 407;
function GpiPaintRegion(hps,hrgn : cardinal) : longint; cdecl; external 'pmgpi' index 409;
function GpiFrameRegion(hps,hrgn : cardinal;var thickness : SIZEL) : longint; cdecl; external 'pmgpi' index 617;
function GpiPtInRegion(hps,hrgn : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 419;
function GpiQueryRegionBox(hps,hrgn : cardinal;var prclBound : RECTL) : longint; cdecl; external 'pmgpi' index 481;
function GpiQueryRegionRects(hps,hrgn : cardinal;var prclBound : RECTL;var prgnrcControl : RGNRECT;var prclRect : RECTL) : longbool; cdecl; external 'pmgpi' index 587;
function GpiRectInRegion(hps,hrgn : cardinal;var prclRect : RECTL) : longint; cdecl; external 'pmgpi' index 494;
function GpiSetRegion(hps,hrgn : cardinal;lcount : longint;var arclRectangles : RECTL) : longbool; cdecl; external 'pmgpi' index 546;
function GpiSetClipRegion(hps,hrgn : cardinal;var phrgnOld : cardinal) : longint; cdecl; external 'pmgpi' index 516;
function GpiQueryClipRegion(hps : cardinal) : cardinal; cdecl; external 'pmgpi' index 436;
function GpiQueryClipBox(hps : cardinal;var prclBound : RECTL) : longint; cdecl; external 'pmgpi' index 435;
function GpiExcludeClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl; external 'pmgpi' index 391;
function GpiIntersectClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl; external 'pmgpi' index 396;
function GpiOffsetClipRegion(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 405;
function GpiCopyMetaFile(hmf : cardinal) : cardinal; cdecl; external 'pmgpi' index 365;
function GpiDeleteMetaFile(hmf : cardinal) : longbool; cdecl; external 'pmgpi' index 375;
function GpiLoadMetaFile(hab : cardinal;pszFilename : pchar) : cardinal; cdecl; external 'pmgpi' index 401;
function GpiPlayMetaFile(hps,hmf : cardinal;lCount1 : longint;var alOptarray,plSegCount : longint;lCount2 : longint;pszDesc : pchar) : longint; cdecl; external 'pmgpi' index 411;
function GpiQueryMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbData : BYTE) : longbool; cdecl; external 'pmgpi' index 465;
function GpiQueryMetaFileLength(hmf : cardinal) : longint; cdecl; external 'pmgpi' index 466;
function GpiSaveMetaFile(hmf : cardinal;pszFilename : pchar) : longbool; cdecl; external 'pmgpi' index 500;
function GpiSetMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbBuffer : BYTE) : longbool; cdecl; external 'pmgpi' index 536;
function GpiQueryDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 567;
function GpiQueryDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl; external 'pmgpi' index 590;
function GpiQueryDefTag(hps : cardinal;var plTag : longint) : longbool; cdecl; external 'pmgpi' index 568;
function GpiQueryDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 569;
function GpiSetDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 570;
function GpiSetDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl; external 'pmgpi' index 591;
function GpiSetDefTag(hps : cardinal;lTag : longint) : longbool; cdecl; external 'pmgpi' index 571;
function GpiSetDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 572;
function GpiPolygons(hps,ulCount : cardinal;var paplgn : POLYGON;flOptions,flModel : cardinal) : longint; cdecl; external 'pmgpi' index 650;

end.
{
  $Log: pmgpi.pas,v $
  Revision 1.1.2.1  2002/11/02 13:27:53  hajny
    * Gpi*Palette* parameters corrected

  Revision 1.1  2000/07/13 06:31:06  michael
  + Initial import

  Revision 1.10  2000/02/09 16:59:33  peter
    * truncated log

  Revision 1.9  2000/01/09 20:48:04  hajny
    * FPK changed to FPC

  Revision 1.8  2000/01/07 16:41:48  daniel
    * copyright 2000

  Revision 1.7  2000/01/07 16:32:32  daniel
    * copyright 2000 added

  Revision 1.6  1999/08/04 15:51:07  hajny
    * merging changes by RB and DM :-(

}

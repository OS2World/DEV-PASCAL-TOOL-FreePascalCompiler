program ex3;

{$mode objfpc}

uses
 glib,gtk;

function newbutton(ALabel : PChar) : PGtkWidget;

begin
  Result:=gtk_button_new_with_label(ALabel);
  gtk_widget_show(result);
end;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

var
  window,
  totalbox,
  hbox,vbox : PgtkWidget;
   
begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  // Box to divide window in 2 halves:
  totalbox := gtk_vbox_new(true,10);
  gtk_widget_show(totalbox);
  // A box for each half of the screen:
  hbox := gtk_hbox_new(false,5);
  gtk_widget_show(hbox);
  vbox := gtk_vbox_new(true,5);
  gtk_widget_show(vbox);
  // Put boxes in their halves
  gtk_box_pack_start(GTK_BOX(totalbox),hbox,true,true,0);
  gtk_box_pack_start(GTK_BOX(totalbox),vbox,true,true,0);
  // Now fill boxes with buttons.
  // Horizontal box
  gtk_box_pack_start(GTK_BOX(hbox),newbutton('Button 1'),false,false,0);
  gtk_box_pack_start(GTK_BOX(hbox),newbutton('Button 2'),false,false,0);
  gtk_box_pack_start(GTK_BOX(hbox),newbutton('Button 3'),false,false,0);
  // Vertical box
  gtk_box_pack_start(GTK_BOX(vbox),newbutton('Button A'),true,true,0);
  gtk_box_pack_start(GTK_BOX(vbox),newbutton('Button B'),true,true,0);
  gtk_box_pack_start(GTK_BOX(vbox),newbutton('Button C'),true,true,0);
  // Put totalbox in window
  gtk_container_set_border_width(GTK_CONTAINER(Window),5);
  gtk_container_add(GTK_Container(window),totalbox);
  gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                      GTK_SIGNAL_FUNC (@destroy), NULL);
  gtk_widget_show (window);
  gtk_main ();
end.

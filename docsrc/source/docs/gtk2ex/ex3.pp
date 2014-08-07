program ex3;

{$mode objfpc}

uses
 glib,gtk,digit;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

var
  window,
  adigit,
  box,
  Button : PgtkWidget;
      
begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  adigit := gtkactivedigit_new;
  gtk_container_set_border_width(GTK_CONTAINER(Window),5);
  box:=gtk_vbox_new(true,10);
  button:=gtk_button_new_with_label('Quit');
  gtk_box_pack_start(pgtkbox(box),PGtkWidget(adigit),False,False,0);
  gtk_box_pack_start(pgtkbox(box),pgtkWidget(button),True,False,0);
  gtk_container_add(GTK_Container(window),box);
  gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                      GTK_SIGNAL_FUNC (@destroy), NULL);
  gtk_signal_connect_object(PgtkObject(button),'clicked',
                      GTK_SIGNAL_FUNC(@gtk_widget_destroy),
                      PGTKOBJECT(window));
  gtk_widget_show_all (window);
  gtkdigit_set_digit(pgtkdigit(adigit),2);
  gtk_main ();
end.

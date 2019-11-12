with AWS.Client;
with AWS.Response;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;

procedure AFollari is
 FoliData : JSON_Value := Create_Object;
 JSBody : Unbounded_String;
 Racks : JSON_Value;
 RacksTotal: Integer;
 BikesAvail: Integer;
 BikesTotal: Integer;
 Updated: Integer;
 T : Ada.Calendar.Time;

 type Rack is record
  StopCode : Unbounded_String;
  StopName : Unbounded_String;
  Bikes : Natural;
  Slots : Natural;
  Lat : Float range -90.0 .. 90.0;
  Lon : Float range -180.0 .. 180.0;
 end record;

 procedure RackCB(Name : UTF8_String; Value : JSON_Value) is
  R : Rack;
 begin
  if Value.Has_Field("stop_code") then
   R.StopCode:=Value.Get("stop_code");
  else
   R.StopCode:=To_Unbounded_String("N/A");
  end if;

  R.StopName:=Value.Get("name");

  if not Value.Has_Field("stop_code") then
   return;
  end if;

  R.Bikes:=Value.Get("bikes_avail");

  Put(R.StopCode);
  Put(" ");
  Put(R.Bikes, 3);
  Put(" ");
  Put(R.StopName);

  New_Line(1);
 end RackCB;

 function BikeLoad(Avail, Total: Integer) return Float is
 begin
  return 100.0-(Float(Avail)/Float(Total))*100.0;
 end BikeLoad;

 procedure ClearScreen is
 begin
  Put(ASCII.ESC & "[1;1H" & ASCII.ESC & "[2J");
 end ClearScreen;

 procedure PrintData is
 begin
  ClearScreen;

  Put("Updated: ");
  Put(Ada.Calendar.Formatting.Image(T));
  New_Line(1);

  Put("Total racks:");
  Put(RacksTotal);
  New_Line(1);

  Put("Total bikes available:");
  Put(BikesAvail);
  New_Line(1);

  Put("Bike Load (%):");
  Put(BikeLoad(BikesAvail, BikesTotal), 3, 2, 0);
  New_Line(2);

  Put_Line("ID  Bikes Location");

  if FoliData.Has_Field("racks") then
   Racks:=FoliData.Get("racks");
   Map_JSON_Object(Racks, RackCB'Access);
  end if;

  New_Line(1);
 end PrintData;

 procedure LoadData is

 begin
  JSBody:=AWS.Response.Message_Body(AWS.Client.Get(URL => "http://data.foli.fi/citybike"));
  FoliData:=Read(JSBody);

  RacksTotal:=FoliData.Get("racks_total");
  BikesAvail:=FoliData.Get("bikes_total_avail");
  BikesTotal:=300;

  Updated:=FoliData.Get("lastupdate");

  -- XXX: How to convert from Integer ???
  -- Updated:=To_Ada_Time();
  T:=Ada.Calendar.Time_Of (Year => 1970, Month => 1, Day => 1) + Duration(Updated);
 end LoadData;

begin
 loop
  LoadData;
  PrintData;
  delay Duration(5.0);
  New_Line(1);
 end loop;
end AFollari;

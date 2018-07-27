with AWS.Client;
with AWS.Response;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure AFollari is
 FoliData : JSON_Value := Create_Object;
 JSBody : Unbounded_String;
 Racks : JSON_Value;
 RacksTotal: Integer;
 BikesAvail: Integer;

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
  R.StopCode:=Value.Get("stop_code");
  R.StopName:=Value.Get("name");
  R.Bikes:=Value.Get("bikes_avail");

  Put(R.StopCode);
  Put(" ");
  Put(R.StopName);
  Put(" ");
  Put(R.Bikes);

  New_Line(1);
 end RackCB;

begin
 JSBody:=AWS.Response.Message_Body(AWS.Client.Get(URL => "http://data.foli.fi/citybike"));
 FoliData:=Read(JSBody);

 RacksTotal:=FoliData.Get("racks_total");
 BikesAvail:=FoliData.Get("bike_total_avail");

 Put("Total racks:");
 Put(RacksTotal);
 New_Line(1);

 Put("Total bikes available:");
 Put(BikesAvail);
 New_Line(1);

 if FoliData.Has_Field("racks") then
  Racks:=FoliData.Get("racks");
  Map_JSON_Object(Racks, RackCB'Access);
 end if;

 New_Line(1);
end AFollari;

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

 procedure RackCB(Name : UTF8_String; Value : JSON_Value) is
  StopCode : Unbounded_String;
  StopName : Unbounded_String;
  Bikes : Integer;
 begin
  StopCode:=Value.Get("stop_code");
  StopName:=Value.Get("name");
  Bikes:=Value.Get("bikes_avail");

  Put(StopCode);
  Put(" ");
  Put(StopName);
  Put(" ");
  Put(Bikes);

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

unit UDataMail;

{$mode ObjFPC}{$H+}

interface

type
  TCorreo = record
    ID          : LongInt;
    Remitente   : string;
    Destinatario: string;
    Estado      : string;
    Asunto      : string;
    Mensaje     : string;
  end;

implementation
end.


--  Copyright (c) 2016 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with Ahven.Framework; use Ahven.Framework;
with Ahven.Text_Runner;
with Ahven.XML_Runner;

with Test_Tokenizers;
with Test_Parsers;
with Test_Streams;
with Test_Images;

procedure Test_Bindings is
   Suite : Test_Suite := Create_Suite ("all");
begin
   Suite.Add_Test (new Test_Tokenizers.Test);
   Suite.Add_Test (new Test_Parsers.Test);
   Suite.Add_Test (new Test_Streams.Test);
   Suite.Add_Test (new Test_Images.Test);

   Ahven.Text_Runner.Run (Suite);
   Ahven.XML_Runner.Run (Suite);
end Test_Bindings;

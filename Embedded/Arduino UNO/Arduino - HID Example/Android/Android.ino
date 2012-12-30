/*
  Blink
  Turns on an LED on for one second, then off for one second, repeatedly.
 
  This example code is in the public domain.
 */
 
// Pin 13 has an LED connected on most Arduino boards.
// give it a name:
int pled = 13;

// the setup routine runs once when you press reset:
void setup() {                
  // initialize the digital pin as an output.
  Serial.begin(9600);
  pinMode(pled, OUTPUT); 
}

char lastReceive[32],lastTime[32];

void Write( char *s)
{
  Serial.println(s);
}  

void doTick()
{
  static int _led=0;
  static int seconds=0;
  static long lmillis;
  if (millis()>lmillis+1000)
  {
    _led = !_led;
    digitalWrite(pled,_led);
    lmillis = millis();
    seconds++;
    switch (seconds)
    {
        case 1:  Write("Hello World");
                 break;
        case 3:  strcpy(lastTime,lastReceive);
                 Write(lastTime);
                 break;
        case 5:  seconds=0;         
                 break;
     } 
  }   
}

void doRead()
{
  static int readp=0;
  while (Serial.available() > 0)
  {
    int c = Serial.read();
    lastReceive[readp]=c;
    if (c==0 || readp == 30) 
      readp=0;
    else
      readp++;  
  }     
}

// the loop routine runs over and over again forever:
void loop() {
    doTick();  
    doRead();  
}


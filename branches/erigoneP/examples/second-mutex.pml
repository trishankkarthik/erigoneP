bool wantp = false, wantq = false;
bool csp = false, csq = false;

active proctype p() {
    do 
	  :: !wantq;
       wantp = true;
       csp = true;
       csp = false;
       wantp = false;
    od
}

active proctype q() {
    do 
    :: !wantp;
       wantq = true;
       csq = true;
       csq = false;
       wantq = false;
    od
}

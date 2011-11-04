#ifndef NAN_H
#define NAN_H

// algorithm makes use of NaN values to signal "no data"
// since NaNs not yet supported by runtime IO, use sentinel value instead
#define MYNAN -667
#define ISNAN(x) ((x) == MYNAN)

#endif // NAN_H

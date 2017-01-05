# Code Book

## Subject

- The integer [1 - 30] that uniquely identifies the person being observed.

## Activity

 - The string that identifies whether the activity was:
  - standing
  - sitting
  - laying
  - walking
  - walking_downstairs
  - waking_upstairs

## Measurement Means

Each variable is the mean numeric value of the measurements for each of the subject-activity pairs. The variable names may be understood according to the following legend, as explained in the accompanying features_info.txt file.  (Note that the generated data set is the *mean* of the measurements from the original data.)

 - t: time domain
 - f: frequency domain
 - x, y, z: coordinate spacial dimensions
 - Acc - acceleration, attributable to the
  - Body, the body of the subject, or
  - Gravity.
 - Gyro - whether the measurement was angular
 - Jerk - time derivative of acceleration
 - Mag - the magnitude of the designated (x, y, z) vector.
 - mean() - Mean value
 - std() - Standard deviation
 

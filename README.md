# NNT Statistics Fun

This repository contains two main files: `6degs.hs` and `NNTXP.hs`

## 6degs.hs
This program finds links between actors at the Nottingham New Theatre in a 'Six Degrees Of Kevin Bacon' style.
So for instance, if Jeff was in Macbeth with Bev, and Bev was in Hamlet with Richard, Jeff would have 2 degrees of separation to Richard.
This only works for people who have *acted* together, it doesn't take into account backstage roles at all. For you guys, there's...

## NNTXP.hs
This takes a person's name and returns an 'XP' score.
This score is based on all of the backstage roles that person has done in their career at the NNT, from Directing to Operating and everything in between!

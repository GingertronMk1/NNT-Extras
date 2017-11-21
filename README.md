# NNT Statistics Fun

This repository contains two main files: `6degs.hs` and `NNTXP.hs`

## 6degs.hs
This program finds links between actors at the Nottingham New Theatre in a 'Six Degrees Of Kevin Bacon' style.
So for instance, if Jeff was in Macbeth with Bev, and Bev was in Hamlet with Richard, Jeff would have 2 degrees of separation to Richard.
This only works for people who have *acted* together, it doesn't take into account backstage roles at all. For you guys, there's...

## NNTXP.hs
This takes a person's name and returns an 'XP' score.
This score is based on all of the backstage roles that person has done in their career at the NNT, from Directing to Operating and everything in between!

The current Role -> XP table looks like this:

| Role                                  | XP Value  |
|---------------------------------------|----------:|
| Director                              | 100       |
| Executive Producer                    | 100       |
| Producer                              | 80        |
| Technical Director                    | 60        |
| Tech Master                           | 60        |
| Lighting Designer                     | 40        |
| Lighting Design                       | 40        |
| Set Design                            | 30        |
| Set Design / Construction             | 30        |
| Set Designer                          | 30        |
| Sound Designer                        | 30        |
| Venue Technician                      | 20        |
| Projection Design                     | 20        |
| Publicity Manager                     | 20        |
| Assistant Director                    | 15        |
| Publicity Designer                    | 15        |
| Poster Designer                       | 15        |
| Poster Design                         | 15        |
| Anything Video                        | 10        |
| Musician                              | 10        |
| Accent Coach                          | 10        |
| Sound                                 | 10        |
| Production Assistant                  | 10        |
| Costume Designer                      | 10        |
| Costumes                              | 10        |
| Hair and Make-Up                      | 10        |
| Make Up / Costumes                    | 10        |
| Make Up Artist / Hair                 | 10        |
| Make Up Artist/Costumes/Blood Effects | 10        |
| Make-Up Assistant                     | 10        |
| Make-Up Supervisor                    | 10        |
| Make-up Designer                      | 10        |
| Make-up and Masks                     | 10        |
| Make-Up                               | 10        |
| Make-up                               | 10        |
| Make Up Artist                        | 10        |
| FFActing                              | 10        |
| Photography                           | 10        |
| Set Construction                      | 5         |
| Design Assistant                      | 5         |
| Stage Manager                         | 5         |
| Technical Operator                    | 5         |
| Otherwise                             | 0         |

Shadow roles currently do not give a direct score, however confer a 2x bonus to the next time a person does the actual role, i.e. if Kelly Shadow TDs one show and then TDs another, they get 120XP for the show they TD.
These bonuses do not stack and Shadowing one role and then another does not erase the first bonus, so if Kelly Shadow LDs, then Shadow TDs, then LDs, they still get the 2x bonus for LDing.

If you think the balance is off, feel free to raise an issue!


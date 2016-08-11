                                   ECOWeB TM
                       Ecologists' Co-Operative Web Bank

ROCKEFELLER UNIVERSITY  BOX 20  1230 YORK AVENUE  NEW YORK, NY 10065-6399  USA

                                  VERSION 1.1
                      Copyright (c) 1989, 2010 by Joel E. Cohen

                                    CONTENTS
                                 December 2, 2010

Dear Colleague:

I. Terms of agreement

Thank you for your interest in ECOWeB(TM), the machine-readable data base of food webs.  We are pleased to make this data base available to you under terms and conditions set forth in this letter (the University's lawyer is speaking here).

Please review these terms and, if you wish to use the subject materials, send me the information requested, sign a copy of this letter enclosed in the place provided, and return the signed copy to me at the address above.  You may wish to make a copy for your own records.

The accompanying data base of more than 213 food webs contains four kinds of files:

1 README.txt: This introductory file with details about the nature and use of the data.

2 SourcesOf213FoodWebs.txt: The collected references to the original publications of all the food webs and the names of the people who contributed the webs to this collection.

3 DATFILES Predation Matrices, a folder containing 3 subfolders
	3a. Community food webs
	3b. Sink food webs
	3c. Source food webs
each of which contains text files of the form WEB*.DAT, where * is the serial number of the web, 1, 2, ...: Each WEB*.DAT file is the predation matrix of each web. ('Community', 'sink', and 'source' food webs were defined in section II of this document.)

4 PRNFILES Food Web & Trophic Species Names, a folder containing text files of the form WEB*.PRN, where * is the serial number of the web, 1, 2, ...: Each WEB*.PRN file lists the organisms in the web.  Some of the WEB*.PRN files contain remarks about details of the data or editorial decisions made.

All files are plain ASCII text.  The file structure is MS-DOS.  The first food web's citation, contributor, predation matrix, and list of organisms are attached to illustrate the format and content of the data files.
While every reasonable effort has been made to assure that the food webs we will send you represent the results of the original publication, supplemented in some cases by additional biological information, we make no guarantee of the accuracy of the original publication or of the accuracy of the edited food web we are providing you.

EXCEPT AS SPECIFICALLY PROVIDED ABOVE, THE ROCKEFELLER UNIVERSITY MAKES NO WARRANTY OR REPRESENTATION, EITHER EXPRESS OR IMPLIED, WITH RESPECT TO THIS DATA BASE OR ITS DOCUMENTATION, INCLUDING THEIR QUALITY, MERCHANTABILITY, PERFORMANCE OR FITNESS FOR A PARTICULAR PURPOSE.

Because the data base is extensive and inherently complex and may not be completely free of errors, you are advised to verify your work.  In no event will The Rockefeller University be liable for direct, indirect, special, incidental or consequential damages arising out of the use of or inability to use the data base or documentation, even if advised of the possibility of such damages.  In particular, The Rockefeller University is not responsible for any costs including, but not limited to, those incurred as a result of the loss of use of the media, loss of data, the cost of recovery of such media or data, the cost of substitute data or media, claims by third parties or other similar costs.

You agree to use the data base for your scientific research only.  You will not commercialize this data base or knowingly provide it to others who will.

In any scientific or other publication based on the use of any data from this data base, you agree to acknowledge the use of the data base by including a citation substantially equivalent to the following:

"Cohen, J. E. (compiler) 2010 [or year in which your version of the data base was prepared] Ecologists' Co-Operative Web Bank. Version 1.1 [or version number specified in README.txt].  Machine-readable data base of food webs.  New York: The Rockefeller University."

You agree to provide me with a copy of any scientific or other publication based on the use of any data from this data base.

You may make copies of this data base for backup.  You may share copies of this data base with research colleagues in your laboratory or department, provided that they also agree in writing to all the terms and conditions of this Agreement.  If you provide any other person with a copy of this data base, you accept responsibility for that person's fulfilling the terms of this Agreement.

We do not propose to profit from the dissemination of the data base and only wish to be reimbursed for our handling costs which we have tried to estimate as accurately as we can.  If you have the financial and administrative means to pay the handling fee for supply of the data base, it is $25 payable only by a check in United States currency made payable to The Rockefeller University, and drawn on an account in a bank in the United States. Your check should be mailed to JOEL E. COHEN, ROCKEFELLER UNIVERSITY * BOX 20 * 1230 YORK AVENUE * NEW YORK, NY 10065-6399 * USA*

Please inform me of any errors you believe you have identified in the data base and of your reason for believing they are errors. Thank you.

	Very truly yours,



	Joel E. Cohen

I agree to the terms and conditions set forth above. 



[your signature]
Date
	
Please print or type your name and address:




II. Description of data files

In each file called WEB*.DAT, the element in the first row and first column of each predation
matrix gives the serial number of the web. 

The other numbers in the first row identify the kinds of organisms that
correspond to each column.  The numbers, other than the first, in the first
column identify the kinds of organisms that correspond to each row.  The key to
these identifying numbers is the list of organisms (in the file WEB*.PRN where 
* indicates the same serial number) that accompanies each predation matrix.

Except for the first row and the first column of each predation matrix, an
entry of 1 or any other positive number in the predation matrix means that the
kind of organism corresponding to that column eats the kind of organism
corresponding to that row.  An entry of 0 means that the kind of organism
corresponding to that column does not eat the kind of organism corresponding to
that row.

When quantitative information about amounts or frequencies of feeding is
available, an entry of a positive number indicates the presence of a feeding
relation and an entry of 0 indicates the absence of a feeding relation.  When
the entries of the predation matrix are other than 0 or 1, a narrative
explanation of the meaning of the numbers follows the list of organisms.

In some webs, an entry of -1 or -2 results from a coding scheme of Cohen
(1978).  All -1 entries should be interpreted as 1 (these represent links
inferred from the descriptive text) and all -2 entries should be interpreted as
0.

WEB*.DAT files are sorted into three subdirectories: COMMUNITY, for community
food webs; SINK, for sink food webs; and SOURCE, for source webs.  According to
Cohen (1978, pp. 20-22), "a community food web is defined by picking, within a
habitat or set of habitats, a set of kinds of organisms.  The set of kinds of
organisms is chosen, without prior regard to the eating relations among them,
by taxonomy, size, location, or other criteria not explicitly dependent on
eating relations.... All the eating relations among those kinds of organisms
are [included in] the community food web.... A sink food web is a [portion] of
a community food web.  It includes one or more kinds of organisms (who are
called the 'consumers') plus all the kinds of organisms that the consumers
eat.  The prey of the prey of the consumers, and so on, are also among the
kinds of organisms in a sink food web.  The [trophic links] in a sink food web
are all the [links] that go from one point to another in the sink food web.  A
sink food web is so named because it is defined by the sinks for energy or
biomass flow....A source food web is also a [portion] of a community food web.
It includes one or more kinds of organisms (not necessarily primary producers
in the usual sense, but the sources of energy or biomass for that subweb), plus
all the kinds of organisms that eat those sources, plus all the predators of
those predators, and so on.  The [links] in a source food web are all the
[links] in its community food web that go from one point to another in the
source food web."

WEB*.PRN files are in the directory PRNFILES.  Each WEB*.PRN file gives the names of 
the organisms labeled by the numbers in the corresponding WEB*.DAT file.  The
WEB*.PRN files of community, sink, and source webs are all together in
PRNFILES.

Example: food web 1

The first food web's citation, contributor, predation matrix, and list of
organisms illustrate the format and content of the data files.

Extract from SourcesOf213FoodWebs.txt:


Web 1  Cochin backwater, India

S. Z. Qazim, Some problems related to the food chain in a tropical estuary.
    In: J. H. Steele, Ed., Marine Food Chains (Oliver and Boyd, Edinburgh,
    1970), pp. 46-51.

Contributor: F. Briand (1983)



Entire contents of WEB1.DAT:


      1   3   4   5   6   7   8   9
      1   1   1   1   1   0   0   0
      2   1   1   1   1   0   0   0
      3   0   0   0   0   1   1   1
      4   0   0   0   0   1   0   0
      5   0   0   0   0   1   1   0
      6   0   0   0   0   0   1   1
      7   0   0   0   0   0   1   0
      8   0   0   0   0   0   0   1



Entire contents of WEB1.PRN:


      1 Cochin backwater, India [Notice the repetition of the identifying web label.]
      1 basic food
      2 detritus
      3 prawns and shrimps
      4 benthos (micro, meio and macro)
      5 zooplankton herbivores
      6 fish herbivores
      7 other carnivores
      8 fish carnivores
      9 man



III. Properties and sources of the data

Properties of the data

The form of the predation matrices given here represents our best joint effort
at the time of publication.  Users are requested to inform us of errors.

These are unlumped food webs.  Depending on the practice of the original
reporter of the web and the compiler, organisms with identical sets of prey and
identical sets of predators may be listed separately, even though they belong
to the same trophic species.  (A trophic species is defined as a group of
organisms with identical predators and identical prey.)

The versions of webs 1-113 in this data base may differ from those published or
analyzed previously because errors were discovered as a result of converting
the webs to machine-readable form.  Because the original sources of these webs
are inconsistent in reporting cannibalism, we have usually suppressed reports
of cannibalism from the predation matrices of the first 113 webs.  Webs 1-113
should not be used to investigate cannibalism or properties of webs that are
sensitive to the presence or absence of cannibalism.

Webs 131-213 do not indicate any trophic link when one species kills a second
but does not eat the killed species.  Where specifically indicated in the notes
at the end of the species lists in WEB*.PRN, cannibalism and/or cycles have
been suppressed.  A number of species lists in the original publications use
abbreviations (such as "gen.n." and "n.sp.") without explanation; these
abbreviations are repeated here without change.  Trophic links based on a
single observation of a consumer eating a prey were consistently suppressed.
Where different stages in the development of a single biological species have
different diets or predators, these different stages are distinguished as
separate trophic species.  Several webs describe different successional stages
of a single community (e.g., webs 180-183 describe stages in the decay of logs;
webs 190-196 describes stages in the decay of rabbit carrion).  Webs that
describe successional stages can be identified from the web labels in
SourcesOf213FoodWebs.txt or the WEB*.PRN files.

Sources

Webs with a prior numbering from Cohen (1978) were first compiled in: J. E.
Cohen, 1978, Food Webs and Niche Space, Princeton: Princeton University Press.

Excepting the webs from Cohen (1978), webs 1-40 were first compiled in: F.
Briand, 1983, Environmental control of food web structure, Ecology 64, 253-263.

Webs 41-113 were first compiled in: Cohen, J. E., Briand, F. and Newman, C. M.,
1989, Community Food Webs: Data and Theory.  Biomathematics vol. 20.
Springer-Verlag, Heidelberg and New York.

Webs 114-130 were first compiled by Anthony King and Stuart L. Pimm in April
1983 as part of a larger collection. 

Webs 131-213 were first compiled by Kenneth G. Schoenly and Roger A. Beaver in
1988. 

Some prior analyses

Some or all of webs 1-113 have been analyzed previously:

Briand F. and Cohen, J. E., 1984.  Community food webs have scale-invariant
structure.  Nature 307:264-266 (Jan.).

Briand, F. and J. E. Cohen, 1987.  Environmental correlates of food chain
length.  Science 238:956-960 (13 Nov.); 1989, 243:239-240 (13 Jan.).

Cohen, J. E. and Briand, F., 1984.  Trophic links of community food webs.
Proceedings of the National Academy of Sciences U.S.A. 81:4105-4109, July.

Cohen, J. E., Briand, F. and Newman, C. M., 1986.  A stochastic theory of
community food webs.  III. Predicted and observed lengths of food chains. Proc.
Roy. Soc. (London) B. 228:317-353.

Cohen, J. E. and Newman, C. M., 1985.  A stochastic theory of community food
webs. I.  Models and aggregated data. Proc. Roy. Soc. (London) B 224:421-448.

Cohen, J. E., Newman, C. M. and Briand, F., 1985.  A stochastic theory of
community food webs. II. Individual webs. Proc. Roy. Soc. (London) B
224:449-461.

Cohen, J. E. and Palka, Z. J., 1989.  A stochastic theory of community food
webs. V. Intervality and triangulation in the trophic niche overlap graph.
Amer. Nat.

Newman, C. M. and Cohen, J. E., 1986.  A stochastic theory of community food
webs. IV. Theory of food chain lengths in large webs.  Proc. Roy. Soc. (London)
B. 228:355-377.

Pimm, S. L., 1982.  Food Webs.  London: Chapman and Hall.

Schoener, T. W., 1989.  Food webs from the small to the large: probes and
hypotheses.  Ecology, 1989.



Webs 131-213 have been analyzed previously:

Schoenly, K., Beaver, R. A., and Heumier, T. A. 1988.  On the trophic relations
of insects: a food web approach.  American Naturalist, submitted.

Sugihara, G., Schoenly, K. and Trombla, A. 1989.  Scale invariance in food web
properties.  Science 245:48-52.

4. Acknowledgments

This data base results from the work of the field ecologists who made the
original observations and those who compiled and edited the original reports.
Assembly, editing and preparation of the machine-readable form of this data
base were supported by a series of grants from the United States National
Science Foundation, including BSR 87-05047.

#'
#' Gender/Maternity discrimination
#' (commercial and administrative jobs in the financial sector)
#'
#' @description
#' The data were collected in January-March 2002 by Pascale Petit for her PhD
#' thesis (University of Paris I-Panthéon-Sorbonne, 2004). A candidate is
#' defined by the variables ("gender","age","child").
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{gender:} Woman or Man.
#' \item\bold{age:} 25 or 37 years old.
#' \item\bold{child:} number of children, 0 or 3.
#' \item\bold{educ:} education, BAC = Baccalauréat = A-level, BTS = 2 years of
#' vocational training after the A-level.
#' \item\bold{qual:} qualification required by the offer, Administrative or
#' Commercial.
#' \item\bold{date:} January 2002, February 2002 or March 2002.
#' \item\bold{cv:} CV template, A or B.
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#' LTC = long term contract (>1 year with no ending date).
#' \item\bold{popp:} promotion opportunity, Yes or No.
#' \item\bold{train:} training included, Yes or No.
#' \item\bold{negow:} negotiable wage, Yes or No.
#' \item\bold{incent:} wage depending on output, Yes or No.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' }
#' @docType data
#' @keywords datasets
#' @name gender1
#' @usage data(gender1)
#' @format A data frame with 942 rows and 14 variables
#' @references
#' Duguet E., Petit P., 2005. Hiring discrimination in the French financial
#' sector:an econometric analysis on field experiment data. Annals of
#' Economics and Statistics, 78:79-102.
#' @references
#' Petit P., 2007. The effects of age and family constraints on gender hiring
#' discrimination:A field experiment in the French financial sector. Labor
#' Economics, 14:371-391.
NULL

#'
#' Gender/Origin discrimination
#' (software developer)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between February and
#' April 2009. A candidate is defined by the variables ("gender","origin").
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{fname:} first name (forename).
#' \item\bold{lname:} last name (family name, surname).
#' \item\bold{gender:} Woman or Man.
#' \item\bold{origin:} all candidates are French, the origin is suggested by the
#'  name. F = French, S = Senegal, M = Morocco, V = Vietnam.
#' \item\bold{date:} date of the application.
#' \item\bold{sentorder:} order in which the application was sent.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{ansorder:} order in which the answer was received when positive, 9
#' otherwise.
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#' LTC = long term contract (>1 year with no ending date).
#' \item\bold{paris:} job located inside Paris, Yes or No.
#' }
#' @docType data
#' @keywords datasets
#' @name inter1
#' @usage data(inter1)
#' @format A data frame with 2480 rows and 11 variables
#' @references
#' Petit P., Duguet E., L'Horty Y., Du Parquet L., Sari F., 2013. Discrimination
#' à l'embauche :les effets du genre et de l'origine se cumulent-ils
#' systématiquement ? Economie et Statistique, 464-465-466:141-153.
#' @references
#' Duguet E., Du Parquet L, L'Horty Y., Petit P., 2015. New Evidence of Ethnic
#' and Gender discriminations in the French Labor Market using experimental
#' data:A ranking extension of responses from correspondence tests. Annals of
#' Economics and Statistics, 117-118:21-39.
NULL

#'
#' Labour market history discrimination
#' (accountants)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between February and
#' April 2015. A candidate is defined by the variable "hist".
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{date:} date of the application.
#' \item\bold{sentorder:} order in which the application was sent.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#' LTC = long term contract (>1 year with no ending date).
#' \item\bold{paris:} job located inside Paris, Yes or No.
#' \item\bold{hist:} history in the labour market, LTC = Long term contract,
#' LTU = Long term unemployment, STU = Short term unemployment,
#' STC = Short term contract, PTC = Part time contract.
#' }
#' @docType data
#' @keywords datasets
#' @name labour1
#' @usage data(labour1)
#' @format A data frame with 1475 rows and 7 variables
#' @references
#' Duguet E., Le Gall R., L’Horty Y., Petit P., 2018. How does labour market
#' history influence the access to hiring interviews? International Journal
#' of Manpower, 39(4), 519-533.
NULL

#'
#' Labour market history discrimination
#' (sales assistant)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between January and
#' April 2015. A candidate is defined by the variable "hist".
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{date:} date of the application.
#' \item\bold{sentorder:} order in which the application was sent.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#' LTC = long term contract (>1 year with no ending date).
#' \item\bold{paris:} job located inside Paris, Yes or No.
#' \item\bold{hist:} history in the labour market, LTC = Long term contract,
#' LTU = Long term unemployment, STU = Short term unemployment,
#' STC = Short term contract, PTC = Part time contract.
#' }
#' @docType data
#' @keywords datasets
#' @name labour2
#' @usage data(labour2)
#' @format A data frame with 1470 rows and 7 variables
#' @references
#' Duguet E., Le Gall R., L’Horty Y., Petit P., 2018. How does labour market
#' history influence the access to hiring interviews? International Journal
#' of Manpower, 39(4), 519-533.
NULL

#'
#' Origin/Gender discrimination and strongly negative mediatic exposure
#' (information technologist)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between December 2008
#' and January 2009. A candidate is defined by the variables ("gender","origin",
#' "mediaexp"). The variable "reput" creates a sample separation.
#'
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{date:} date of the application.
#' \item\bold{sentorder:} order in which the application was sent.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{fname:} first name (forename).
#' \item\bold{lname:} last name (family name, surname).
#' \item\bold{origin:} all the candidates are French, the origin is suggested by the
#' name, F = France, M = Morocco.
#' \item\bold{gender:} Woman or Man.
#' \item\bold{city:} candidate location.
#' \item\bold{reput:} reputation of the city, P = privileged, U = Unprivileged.
#' \item\bold{mediaexp:} strong negative mediatic exposure, Yes or No.
#' }
#' @docType data
#' @keywords datasets
#' @name address1
#' @usage data(address1)
#' @format A data frame with 3684 rows and 11 variables
#' @references
#' Duguet E., Gray D., L'Horty Y., Du Parquet L, Petit P., 2020. Labor market
#' effects of urban riots:an experimental assessment. Papers in Regional
#' Science, 99:787-806.
NULL

#'
#' Gender discrimination and mobility
#' (management controller)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between October 2008
#' and March 2009. A candidate is defined by the variables ("gender","licenses").
#' \itemize{
#' \item \bold{offer:} add number.
#' \item \bold{date:} date of the application.
#' \item \bold{sentorder:} order in which the application was sent.
#' \item \bold{callback:} TRUE if there was a non negative callback.
#' \item \bold{ansorder:} order in which the answer was received when positive, 5
#'  otherwise.
#' \item \bold{fname:} first name (forename).
#' \item \bold{lname:} last name (family name, surname).
#' \item \bold{gender:} Woman or Man.
#' \item \bold{licenses:} both moto and car licenses, Yes or No.
#' \item \bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#'  LTC = long term contract (>1 year with no ending date).
#' \item \bold{paris:} job located inside Paris, Yes or No.
#' \item \bold{cv:} CV template, A or B.
#' }
#' @docType data
#' @keywords datasets
#' @name mobility1
#' @usage data(mobility1)
#' @format A data frame with 1200 rows and 12 variables
#' @references
#' Duguet E., du Parquet L., L'Horty Y., Petit P., 2018. Counterproductive
#' hiring discrimination against women:evidence from a French correspondence
#' test. International Journal of Manpower, 39(1):37-50.
NULL

#'
#' Origin discrimination
#' (accountants)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between September and
#' November 2006. A candidate is defined by the variables ("nation","lnation",
#' "fnation"). The variables "educ" and "reput" create sample separations.
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{date:} September 2006, October 2006 or November 2006.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{fname:} first name (forename).
#' \item\bold{lname:} last name (family name, surname).
#' \item\bold{educ:} education, BAC = Baccalauréat = A-level, BTS = 2 years of
#'  vocational training after the A-level.
#' \item\bold{cartime:} commuting time by car (minutes).
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#'  LTC = long term contract (>1 year with no ending date).
#' \item\bold{paris:} job located inside Paris, Yes or No.
#' \item\bold{nation:} nationality, M = Moroccan, F = French.
#' \item\bold{fnation:} first name sounding, M = Moroccan, F = French.
#' \item\bold{lnation:} last name sounding, M = Moroccan, F = French.
#' \item\bold{origin:} summary variable made from nation, lnation and fnation.
#' Example:FMF = French nationality, Moroccan family name and French first
#' name.
#' \item\bold{city:} candidate location.
#' \item\bold{reput:} reputation of the city, P = privileged, U = Unprivileged.
#' \item\bold{cv:} CV template, A or B.
#' \item\bold{natemp:} add obtained from the national employment agency (ANPE at the
#'  time of the test, France Travail today).
#' \item\bold{subsid:} the firm is a subsidiary of a large corporation, Yes or No.
#' \item\bold{ansmode:} answering channel, email or ordinary mail for all the
#'  applications to the same add. M = email, P = postage prepaid envelope, R =
#'   Marianne stamp, C = Cubitus stamp (comics character).
#' \item\bold{email:} answered by email by all the candidates, 1 = yes, 0 = No.
#' }
#' @docType data
#' @keywords datasets
#' @name origin1
#' @usage data(origin1)
#' @format A data frame with 1097 rows and 20 variables
#' @references
#' Duguet E., Leandri N., L'Horty Y., Petit P., 2010. Are young French
#' jobseekers of ethnic immigrant origin discriminated against? A controlled
#' experiment in the Paris area. Annals of Economics and Statistics, 99-100:
#' 187-215.
NULL

#'
#' Origin discrimination
#' (waiters)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between September and
#' November 2006. A candidate is defined by the variables ("nation","lnation",
#' "fnation"). The variables "educ" and "reput" create sample separations.
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{date:} September 2006, October 2006 or November 2006.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{fname:} first name (forename).
#' \item\bold{lname:} last name (family name, surname).
#' \item\bold{educ:} education, BAC = Baccalauréat = A-level, BTS = 2 years of
#'  vocational training after the A-level.
#' \item\bold{cartime:} commuting time by car (minutes).
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#'  LTC = long term contract (>1 year with no ending date).
#' \item\bold{paris:} job located inside Paris, Yes or No.
#' \item\bold{nation:} nationality, M = Moroccan, F = French.
#' \item\bold{fnation:} first name sounding, M = Moroccan, F = French.
#' \item\bold{lnation:} last name sounding, M = Moroccan, F = French.
#' \item\bold{origin:} summary variable made from nation, lnation and fnation.
#'  Example:FMF = French nationality, Moroccan family name and French first
#'  name.
#' \item\bold{city:} candidate location.
#' \item\bold{reput:} reputation of the city, P = privileged, U = Unprivileged.
#' \item\bold{cv:} CV template, A or B.
#' \item\bold{natemp:} add obtained from the national employment agency (ANPE at the
#'  time of the test, France Travail today).
#' \item\bold{subsid:} the firm is a subsidiary of a large corporation, Yes or No.
#' \item\bold{ansmode:} answering channel, email or ordinary mail for all the
#'  applications to the same add. M = email, P = postage prepaid envelope, R =
#'   Marianne stamp, C = Cubitus stamp (comics character).
#' \item\bold{email:} answered by email by all the candidates, 1 = yes, 0 = No.
#' }
#' @docType data
#' @keywords datasets
#' @name origin2
#' @usage data(origin2)
#' @format A data frame with 936 rows and 20 variables
#' @references
#' Petit P., Duguet E., L'Horty Y., 2015. Discrimination résidentielle et
#' origine ethnique:une étude expérimentale sur les serveurs en Ile de France.
#' Economie et Prevision, 206-207:55-69.
NULL

#'
#' Gender/Maternity discrimination
#' (electricians)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between February and
#' July 2015. A candidate is defined by the variables ("gender","educ").
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{gender:} Woman or Man.
#' \item\bold{age:} 23 or 24 years old.
#' \item\bold{fname:} first name (forename).
#' \item\bold{lname:} last name (family name, surname).
#' \item\bold{educ:} education, CAP = vocational training certificate before the A
#' level, MAF = CAP + "One of the best French apprentice", OLY = CAP +
#' participation to the Worldskills Competition.
#' \item\bold{zip:} ZIP code.
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#'  LTC = long term contract (>1 year with no ending date).
#' \item\bold{recgender:} gender of the recruiter.
#' \item\bold{cv:} CV template, A or B.
#' \item\bold{ansmode:} answering channel, email or ordinary mail ("omail").
#' \item\bold{sentorder:} order in which the application was sent.
#' \item\bold{hours:} weekly work time.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{date:} between February and July 2015.
#' }
#' @docType data
#' @keywords datasets
#' @name gender2
#' @usage data(gender2)
#' @format A data frame with 564 rows and 15 variables
#' @references
#' Duguet E., du Parquet L., Petit P. (2022). Extracting the discrimination\cr
#' components from the callback rates". TEPP Working Paper 2022-15.
#' @references
#'  Duguet, E., du Parquet, L. & Petit, P. (2022). Révéler les composantes de
#'  la discrimination à partir des taux de rappel. Revue française d'économie,
#'  XXXVII, 233-268.
NULL

#'
#' Gender/Maternity discrimination
#' (masons)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between February and
#' July 2015. A candidate is defined by the variables ("gender","educ").
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{gender:} Woman or Man.
#' \item\bold{age:} 23 or 24 years old.
#' \item\bold{fname:} first name (forename).
#' \item\bold{lname:} last name (family name, surname).
#' \item\bold{educ:} education, CAP = vocational training certificate before the A
#' level, MAF = CAP + "One of the best French apprentice", OLY = CAP +
#' participation to the Worldskills Competition.
#' \item\bold{zip:} ZIP code.
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#'  LTC = long term contract (>1 year with no ending date).
#' \item\bold{recgender:} gender of the recruiter.
#' \item\bold{cv:} CV template, A or B.
#' \item\bold{ansmode:} answering channel, email or ordinary mail ("omail").
#' \item\bold{sentorder:} order in which the application was sent.
#' \item\bold{hours:} weekly work time.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{date:} between February and July 2015.
#' }
#' @docType data
#' @keywords datasets
#' @name gender3
#' @usage data(gender3)
#' @format A data frame with 532 rows and 15 variables
#' @references
#' Duguet E., du Parquet L., Petit P. (2022). Extracting the discrimination
#' components from the callback rates". TEPP Working Paper 2022-15.
#' @references
#'  Duguet, E., du Parquet, L. & Petit, P. (2022). Révéler les composantes de
#'  la discrimination à partir des taux de rappel. Revue française d'économie,
#'  XXXVII, 233-268.
NULL

#'
#' Gender/Maternity discrimination
#' (plumbers)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between February and
#' July 2015. A candidate is defined by the variables ("gender","educ").
#' \itemize{
#' \item\bold{offer:} add number.
#' \item\bold{gender:} Woman or Man.
#' \item\bold{age:} 23 or 24 years old.
#' \item\bold{fname:} first name (forename).
#' \item\bold{lname:} last name (family name, surname).
#' \item\bold{educ:} education, CAP = vocational training certificate before the A
#' level, MAF = CAP + "One of the best French apprentice", OLY = CAP +
#' participation to the Worldskills Competition.
#' \item\bold{zip:} ZIP code.
#' \item\bold{cont:} length of labour contract, STC = short term contract (<=1 year),
#'  LTC = long term contract (>1 year with no ending date).
#' \item\bold{recgender:} gender of the recruiter.
#' \item\bold{cv:} CV template, A or B.
#' \item\bold{ansmode:} answering channel, email or ordinary mail ("omail").
#' \item\bold{sentorder:} order in which the application was sent.
#' \item\bold{hours:} weekly work time.
#' \item\bold{callback:} TRUE if there was a non negative callback.
#' \item\bold{date:} between February and July 2015.
#' }
#' @docType data
#' @keywords datasets
#' @name gender4
#' @usage data(gender4)
#' @format A data frame with 1152 rows and 15 variables
#' @references
#' Duguet E., du Parquet L., Petit P. (2022). Extracting the discrimination
#' components from the callback rates". TEPP Working Paper 2022-15.
#' @references
#'  Duguet, E., du Parquet, L. & Petit, P. (2022). Révéler les composantes de
#'  la discrimination à partir des taux de rappel. Revue française d'économie,
#'  XXXVII, 233-268.
NULL

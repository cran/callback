#'
#' Gender/Maternity discrimination (commercial and administrative jobs in the
#' financial sector)
#'
#' @description
#' The data were collected in January-March 2002 by Pascale Petit for her PhD
#' thesis.
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{gender}\tab Woman or Man\cr
#' \code{age}\tab 25 or 37 years old\cr
#' \code{child}\tab number of children, 0 or 3\cr
#' \code{educ}\tab education, BAC = Baccalauréat = A-level, BTS = 2 years of
#' vocational training after the A-level\cr
#' \code{qual}\tab qualification required by the offer, Administrative or
#' Commercial \cr
#' \code{date}\tab January 2002, February 2002 or March 2002\cr
#' \code{cv}\tab CV template, A or B\cr
#' \code{cont}\tab length of labour contract, STC = short term contract (<=1
#' year), LTC = long term contract (>1 year with no ending date)\cr
#' \code{popp}\tab promotion opportunity, Yes or No\cr
#' \code{train}\tab training included, Yes or No\cr
#' \code{negow}\tab negotiable wage, Yes or No\cr
#' \code{incent}\tab wage depending on output, Yes or No\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' }
#' @docType data
#' @keywords datasets
#' @name gender1
#' @usage data(gender1)
#' @format A data frame with 942 rows and 14 variables
#' @references
#' Duguet E., Petit P., 2005. Hiring discrimination in the French financial
#' sector: an econometric analysis on field experiment data. Annals of
#' Economics and Statistics, 78: 79-102.
#' @references
#' Petit P., 2007. The effects of age and family constraints on gender hiring
#' discrimination: A field experiment in the French financial sector. Labor
#' Economics, 14: 371-391.
NULL

#'
#' Gender/Origin discrimination (software developer)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between February and
#' April 2009
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{fname}\tab first name (forename)\cr
#' \code{lname}\tab last name (family name, surname)\cr
#' \code{gender}\tab Woman or Man\cr
#' \code{origin}\tab all candidates are French, the origin is suggested by the
#' name. F = French, S = Senegal, M = Morocco, V = Vietnam\cr
#' \code{date}\tab date of the application\cr
#' \code{sentorder}\tab order in which the application was sent\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' \code{ansorder}\tab order in which the answer was received when positive, 9
#' otherwise\cr
#' \code{cont}\tab length of labour contract, STC = short term contract (<=1
#' year), LTC = long term contract (>1 year with no ending date)\cr
#' \code{paris}\tab job located inside Paris, Yes or No\cr
#' }
#' @docType data
#' @keywords datasets
#' @name inter1
#' @usage data(inter1)
#' @format A data frame with 2480 rows and 11 variables
#' @references
#' Petit P., Duguet E., L'Horty Y., Du Parquet L., Sari F., 2013. Discrimination
#' à l'embauche : les effets du genre et de l'origine se cumulent-ils
#' systématiquement ? Economie et Statistique, 464-465-466: 141-153.
#' @references
#' Duguet E., Du Parquet L, L'Horty Y., Petit P., 2015. New Evidence of Ethnic
#' and Gender discriminations in the French Labor Market using experimental
#' data: A ranking extension of responses from correspondence tests. Annals of
#' Economics and Statistics, 117-118: 21-39.
NULL

#'
#' Labour market history discrimination (accountants)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between February and
#' April 2015
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{date}\tab date of the application\cr
#' \code{sentorder}\tab order in which the application was sent\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' \code{cont}\tab length of labour contract, STC = short term contract (<=1
#' year), LTC = long term contract (>1 year with no ending date)\cr
#' \code{paris}\tab job located inside Paris, Yes or No\cr
#' \code{hist}\tab history in the labour market, LTC = Long term contract, LTU =
#'  Long term unemployment, STU = Short term unemployment, STC = Short term
#'  contract, PTC = Part time contract\cr
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
#' Labour market history discrimination (sales assistant)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between January and
#' April 2015
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{date}\tab date of the application\cr
#' \code{sentorder}\tab order in which the application was sent\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' \code{cont}\tab length of labour contract, STC = short term contract (<=1
#' year), LTC = long term contract (>1 year with no ending date)\cr
#' \code{paris}\tab job located inside Paris, Yes or No\cr
#' \code{hist}\tab history in the labour market, LTC = Long term contract, LTU =
#'  Long term unemployment, STU = Short term unemployment, STC = Short term
#'  contract, PTC = Part time contract\cr
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
#' and January 2009
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{date}\tab date of the application\cr
#' \code{sentorder}\tab order in which the application was sent\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' \code{fname}\tab first name (forename)\cr
#' \code{lname}\tab last name (family name, surname)\cr
#' \code{origin}\tab all the candidates are French, the origin is suggested by
#' the name, F = France, M = Morocco\cr
#' \code{gender}\tab Woman or Man\cr
#' \code{city}\tab candidate location\cr
#' \code{reput}\tab reputation of the city, P = privileged, U = Unprivileged\cr
#' \code{mediaexp}\tab strong negative mediatic exposure, Yes or No\cr
#' }
#' @docType data
#' @keywords datasets
#' @name media1
#' @usage data(media1)
#' @format A data frame with 3684 rows and 11 variables
#' @references
#' Duguet E., Gray D., L'Horty Y., Du Parquet L, Petit P., 2020. Labor market
#' effects of urban riots: an experimental assessment. Papers in Regional
#' Science, 99:787-806.
NULL

#'
#' Gender discrimination and mobility (management controller)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between October 2008
#' and March 2009
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{date}\tab date of the application\cr
#' \code{sentorder}\tab order in which the application was sent\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' \code{ansorder}\tab order in which the answer was received when positive, 5
#' otherwise\cr
#' \code{fname}\tab first name (forename)\cr
#' \code{lname}\tab last name (family name, surname)\cr
#' \code{gender}\tab Woman or Man\cr
#' \code{licenses}\tab both moto and car licenses, Yes or No\cr
#' \code{cont}\tab length of labour contract, STC = short term contract (<=1
#' year), LTC = long term contract (>1 year with no ending date)\cr
#' \code{paris}\tab job located inside Paris, Yes or No\cr
#' \code{cv}\tab CV template, A or B\cr
#' }
#' @docType data
#' @keywords datasets
#' @name mobility1
#' @usage data(mobility1)
#' @format A data frame with 1200 rows and 12 variables
#' @references
#' Duguet E., du Parquet L., L'Horty Y., Petit P., 2018. Counterproductive
#' hiring discrimination against women: evidence from a French correspondence
#' test. International Journal of Manpower, 39(1): 37-50.
NULL

#'
#' Origin discrimination (accountants)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between September and
#' November 2006
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{date}\tab September 2006, October 2006 or November 2006\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' \code{fname}\tab first name (forename)\cr
#' \code{lname}\tab last name (family name, surname)\cr
#' \code{educ}\tab education, BAC = Baccalauréat = A-level, BTS = 2 years of
#'vocational training after the A-level\cr
#' \code{cartime}\tab commuting time by car (minutes)\cr
#' \code{cont}\tab length of labour contract, STC = short term contract (<=1
#' year), LTC = long term contract (>1 year with no ending date)\cr
#' \code{paris}\tab job located inside Paris, Yes or No\cr
#' \code{nation}\tab nationality, M = Moroccan, F = French\cr
#' \code{fnation}\tab first name sounding, M = Moroccan, F = French\cr
#' \code{lnation}\tab last name sounding, M = Moroccan, F = French\cr
#' \code{origin}\tab summary variable made from Nation, Lnation and Fnation.
#' Example: FMF = French nationality, Moroccan family name and French first
#' name\cr
#' \code{city}\tab candidate location\cr
#' \code{reput}\tab reputation of the city, P = privileged, U = Unprivileged\cr
#' \code{cv}\tab CV template, A or B\cr
#' \code{natemp}\tab add obtained from the national employment agency (ANPE at
#' the time of the test, France Travail today)\cr
#' \code{subsid}\tab the firm is a subsidiary of a large corporation, Yes or No
#' \cr
#' \code{ansmode}\tab answering channel, email or ordinary mail for all the
#' applications to the same add. M = email, P = postage prepaid enveloppe, R =
#' Marianne stamp, C = Cubitus stamp (comics character)\cr
#' \code{email}\tab answered by email by all the candidates, 1 = yes, 0 = No\cr
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
#' Origin discrimination (waiters)
#'
#' @description
#' The data were collected by the TEPP team (FR CNRS 2042) between September and
#' November 2006
#'
#' \tabular{ll}{
#' \code{offer} \tab add number \cr
#' \code{date}\tab September 2006, October 2006 or November 2006\cr
#' \code{callback}\tab TRUE if there was a non negative callback\cr
#' \code{fname}\tab first name (forename)\cr
#' \code{lname}\tab last name (family name, surname)\cr
#' \code{educ}\tab education, BAC = Baccalauréat = A-level, BTS = 2 years of
#'vocational training after the A-level\cr
#' \code{cartime}\tab commuting time by car (minutes)\cr
#' \code{cont}\tab length of labour contract, STC = short term contract (<=1
#' year), LTC = long term contract (>1 year with no ending date)\cr
#' \code{paris}\tab job located inside Paris, Yes or No\cr
#' \code{nation}\tab nationality, M = Moroccan, F = French\cr
#' \code{fnation}\tab first name sounding, M = Moroccan, F = French\cr
#' \code{lnation}\tab last name sounding, M = Moroccan, F = French\cr
#' \code{origin}\tab summary variable made from Nation, Lnation and Fnation.
#' Example: FMF = French nationality, Moroccan family name and French first
#' name\cr
#' \code{city}\tab candidate location\cr
#' \code{reput}\tab reputation of the city, P = privileged, U = Unprivileged\cr
#' \code{cv}\tab CV template, A or B\cr
#' \code{natemp}\tab add obtained from the national employment agency (ANPE at
#' the time of the test, France Travail today)\cr
#' \code{subsid}\tab the firm is a subsidiary of a large corporation, Yes or No
#' \cr
#' \code{ansmode}\tab answering channel, email or ordinary mail for all the
#' applications to the same add. M = email, P = postage prepaid enveloppe, R =
#' Marianne stamp, C = Cubitus stamp (comics character)\cr
#' \code{email}\tab answered by email by all the candidates, 1 = yes, 0 = No\cr
#' }
#' @docType data
#' @keywords datasets
#' @name origin2
#' @usage data(origin2)
#' @format A data frame with 936 rows and 20 variables
#' @references
#' Petit P., Duguet E., L'Horty Y., 2015. Discrimination résidentielle et
#' origine ethnique: une étude expérimentale sur les serveurs en Ile de France.
#' Economie et Prevision, 206-207: 55-69.
NULL

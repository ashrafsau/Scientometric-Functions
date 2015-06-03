# options(stringsAsFactors = F)
# setwd("P:\\Functions")
# load(file = "data.RData")

get_country_pattern<-function(data_,year_,c.list=character(0)){
  #Input: authors vector and corresponding year values
  #Output: year-wise country output
  
  if(length(data_)!=length(year_))stop("author_ and year_ are not of same length!")
  if(mode(year_)!="numeric")stop("year_ must be numeric!")
  
  library(stringr)  
  data_<-data_[data_!=""]  
  C1<-data_
  C1<-str_replace_all(string = C1,pattern = "USA",replacement = "United States")
  C1<-str_replace_all(string = C1,pattern = "UK",replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = ignore.case("England"),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = ignore.case("Indiana"),replacement = "")
  C1<-str_replace_all(string = C1,pattern = ignore.case("Scotland"),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = "Wales",replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = ignore.case("Northern Ireland"),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = "UAE",replacement = "United Arab Emirates")
  
  if(length(C1)<1)stop("data_ is of length 0!")
  
  if(length(c.list)==0)c.list<-c("United States", "China", "United Kingdom", "Germany", "Japan", "France", "Canada",
            "Italy", "Spain", "India", "Australia", "Russia", "South Korea", "Netherlands", "Brazil",
            "Taiwan", "Switzerland", "Sweden", "Poland", "Turkey", "Belgium", "Israel", "Austria",
            "Denmark", "Iran", "Finland", "Greece", "Mexico", "Czech Republic", "Norway",
            "Singapore", "Portugal", "New Zealand", "South Africa", "Argentina", "Hungary", "Ukraine",
            "Ireland", "Malaysia", "Romania", "Egypt", "Thailand", "Chile", "Saudi Arabia", "Pakistan",
            "Croatia", "Slovakia", "Slovenia", "Bulgaria", "Nigeria", "Tunisia", "Colombia", "Serbia",
            "Morocco", "Venezuela", "Algeria", "Belarus", "Lithuania",  "Cuba", "Indonesia",  "Jordan",
            "Bangladesh",  "Estonia", "United Arab Emirates",  "Kenya", "Viet Nam", "Kuwait", "Lebanon",
            "Philippines", "Iceland", "Cyprus", "Latvia", "Uruguay", "Armenia", "Peru", "Sri Lanka",
            "Oman", "Ethiopia", "Tanzania", "Uzbekistan", "Cameroon", "Azerbaijan", "Uganda", "Ghana",
            "Luxembourg", "Costa Rica", "Nepal", "Iraq", "Qatar", "Macedonia", "Kazakhstan", "Zimbabwe",
            "Senegal", "Ecuador", "Moldova", "Bosnia and Herzegovina", "Sudan", "Syria",
            "Trinidad and Tobago", "Ivory Coast", "Panama", "Jamaica", "Botswana", "Burkina Faso",
            "Malawi", "Bahrain",  "Palestine", "Libya", "Zambia", "Bolivia", "Benin", "Malta",
            "Madagascar", "North Korea", "Mongolia", "Congo", "Mali", "Papua New Guinea", "Yemen",
            "Cambodia", "Albania", "Guatemala", "Fiji", "Gambia", "Mozambique", "Gabon", "Namibia",
            "Mauritius", "Brunei",  "Barbados", "Monaco", "Niger", "Laos",  "Montenegro", "Myanmar",
            "Guadeloupe", "Togo", "Kyrgyzstan", "Nicaragua", "Paraguay", "Rwanda", "Tajikistan",
            "El Salvador", "Dominican Republic", "Swaziland", "Honduras", "Guam", "Grenada",
            "Afghanistan", "Bermuda", "Martinique", "Angola", "Haiti", "Guyana", "Central African Republic",
            "Guinea", "Eritrea", "Mauritania", "Sierra Leone", "Faroe Islands", "Seychelles",
            "Guinea-Bissau", "Lesotho", "Bhutan", "Burundi", "Chad", "Bahamas", "Belize", "Solomon Islands",
            "Turkmenistan", "Vanuatu", "Suriname", "Samoa", "Dominica", "Federated States of Micronesia",
            "Maldives", "Djibouti", "Saint Kitts and Nevis", "Liberia", "San Marino", "Palau", "Andorra",
            "Cape Verde", "Tonga",  "Antigua and Barbuda", "Marshall Islands", "Mayotte", "Gibraltar",
            "Somalia", "Comoros", "Saint Lucia", "Timor-Leste", "Aruba", "Sao Tome and Principe",
            "Cook Islands", "Turks and Caicos Islands","Saint Vincent and The Grenadines", "Norfolk Island",
            "Tuvalu", "Nauru", "Anguilla", "Vatican City",  "Kiribati", "Niue")
  
  years<-sort(unique(year_))
  m<-matrix(data=0,nrow=length(c.list), ncol = length(years), dimnames = list(c.list,years))
  
  for(y in years){
    show(y)
    y_ind<-which(year_==y)
    for(i in y_ind){
      ind<-which(str_detect(string = C1[i],pattern = ignore.case(c.list)))    
      if(length(ind)>0){        
        m[ind,as.character(y)]<-m[ind,as.character(y)]+1
      }
    }
  }
  m<-m[rowSums(m)>0,]
  m<-m[order(rowSums(m),decreasing = T),]
}

#dfr<-get_country_pattern(data_ = data$C1,year_ = data$PY)

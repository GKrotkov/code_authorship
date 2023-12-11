linkedin <- read.csv("PATH")

linkedin$industry[linkedin$industry %in% c("Mining and logging", "Dairy", "Fishery", "Environmental Services", "Mining & Metals", "Oil & Energy", "Paper & Forest Products", "Renewables & Environment", "Tobacco", "Farming")] <-
  "Mining/Agriculture"
linkedin$industry[linkedin$industry %in% c("Architecture & Planning", "Civil Engineering", "Construction", "Glass, Ceramics & Concrete")] <-
  "Construction"
linkedin$industry[linkedin$industry %in% c("Automotive", "Aviation & Aerospace", "Chemicals", "Computer Hardware", "Defense & Space", "Electrical & Electronic Manufacturing", "Food & Beverages", "Food Production", "Furniture", "Industrial Automation", "Machinery", "Mechanical Or Industrial Engineering", "Pharmaceuticals", "Plastics", "Printing", "Railroad Manufacture", "Research", "Shipbuilding", "Textiles")] <-
  "Manufacturing"
linkedin$industry[linkedin$industry %in% c("Business Supplies & Equipment", "International Trade & Development", "Wholesale")] <-
  "Wholesale Trade"
linkedin$industry[linkedin$industry %in% c("Apparel & Fashion", "Building Materials", "Consumer Goods", "Cosmetics", "Luxury Goods & Jewelry", "Retail", "Sporting Goods", "Supermarkets", "Wine & Spirits")] <-
  "Retail Trade"
linkedin$industry[linkedin$industry %in% c("Airlines/Aviation", "Import & Export", "Logistics & Supply Chain", "Maritime", "Package/Freight Delivery", "Packaging & Containers", "Transportation/Trucking/Railroad", "Utilities", "Warehousing")] <-
  "Transportation, warehousing, and utilities"
linkedin$industry[linkedin$industry %in% c("Computer & Network Security", "Computer Networking", "Computer Software", "Consumer Electronics", "Information Technology & Services", "Internet", "Libraries", "Newspapers", "Online Media", "Outsourcing/Offshoring", "Publishing", "Semiconductors", "Telecommunications", "Translation & Localization", "Wireless", "Writing & Editing")] <-
  "Information/Information Technology"
linkedin$industry[linkedin$industry %in% c("Accounting", "Banking", "Capital Markets", "Financial Services", "Insurance", "Investment Banking", "Investment Management", "Market Research", "Marketing & Advertising", "Venture Capital & Private Equity")] <-
  "Finance/Insurance"
linkedin$industry[linkedin$industry %in% c("Commercial Real Estate", "Real Estate")] <-
  "Real Estate/Rental/Leasing"
linkedin$industry[linkedin$industry %in% c("Government Relations", "Human Resources", "Management Consulting", "Non-profit Organization Management", "Public Relations & Communications", "Staffing & Recruiting", "Think Tanks")] <-
  "Professional/Business Services"
linkedin$industry[linkedin$industry %in% c("E-learning", "Education Management", "Fundraising", "Higher Education", "Primary/Secondary Education")] <-
  "Educational Services"
linkedin$industry[linkedin$industry %in% c("Alternative Medicine", "Biotechnology", "Health, Wellness & Fitness", "Hospital & Health Care", "Individual & Family Services", "Medical Device", "Medical Practice", "Mental Health Care", "Philanthropy", "Professional Training & Coaching", "Veterinary")] <-
  "Health Care/Social Assistance"
linkedin$industry[linkedin$industry %in% c("Animation", "Broadcast Media", "Computer Games", "Design", "Entertainment", "Fine Art", "Graphic Design", "Media Production", "Museums & Institutions", "Music", "Performing Arts", "Photography", "Recreational Facilities & Services", "Sports")] <-
  "Arts/Entertainment/Recreation"
linkedin$industry[linkedin$industry %in% c("Consumer Services", "Events Services", "Gambling & Casinos", "Information Services", "Hospitality", "Leisure, Travel & Tourism", "Restaurants")] <-
  "Accommodation and Food Services"
linkedin$industry[linkedin$industry %in% c("Civic & Social Organization", "Facilities Services", "Law Practice", "Legal Services", "Religious Institutions")] <-
  "Other Services"
linkedin$industry[linkedin$industry %in% c("Executive Office", "Government Administration", "International Affairs", "Judiciary", "Law Enforcement", "Legislative Office", "Military", "Political Organization", "Public Safety", "Public Policy", "Security & Investigations")] <-
  "Government"
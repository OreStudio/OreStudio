"""
Generates the party source data for the ACME Corporation dataset:
party_contacts.json, party_countries.json, party_currencies.json and
party_counterparties.json, read by generate_sql.py in this same directory.

What these describe is what a party reads on its own page: where its offices
are and how to reach them, the countries and currencies it works in, and the
counterparties it may trade with.

- Each of the four legal entities has an office for each contact type. The
  legal address is the registered office and is the primary contact; the
  operations, settlement and billing offices are the departments that
  entity runs from its own premises.
- The countries and currencies of an entity are those it books in. The group
  works in every country and currency its subsidiaries do.
- The counterparties an entity may trade with are the banks it holds a master
  agreement with, read from netting_agreements.json so the two cannot
  disagree, and the banks its treasury also approves. The group sees its
  subsidiaries, and each subsidiary sees the group and its sister entities.
  The banks are real legal entities, named by the LEI the GLEIF counterparty
  set holds for them. The numbers and addresses are fictional: telephone
  numbers are the ranges set aside for drama and the domain is reserved.

Usage:
    python3 generate_party_data.py
"""
import json
import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# The legal entities: dataset company, LEI.
ENTITY_LEI = {
    "acme_group": "9695ACMEGROUP0000030",
    "acme_uk": "9695ACMEUK0000000047",
    "acme_us": "9695ACMEUS0000000043",
    "acme_hk": "9695ACMEHK0000000018",
}

# Each office: street, second line, city, state, country, postal code, the
# telephone prefix and the web page of the entity.
SITE = {
    "acme_group": {
        "street": "25 Cannon Street", "city": "London", "state": "", "country": "GB",
        "postal": "EC4M 5TA", "phone": "+44 20 7946 0", "digits": 3, "web": "https://www.acme-corp.example/group",
        "domain": "acme-corp.example",
    },
    "acme_uk": {
        "street": "1 Gresham Street", "city": "London", "state": "", "country": "GB",
        "postal": "EC2V 7BX", "phone": "+44 20 7946 0", "digits": 3, "web": "https://www.acme-corp.example/uk",
        "domain": "uk.acme-corp.example",
    },
    "acme_us": {
        "street": "200 Park Avenue", "city": "New York", "state": "NY", "country": "US",
        "postal": "10166", "phone": "+1 212 555 01", "digits": 2, "web": "https://www.acme-corp.example/us",
        "domain": "us.acme-corp.example",
    },
    "acme_hk": {
        "street": "88 Queen's Road Central", "city": "Hong Kong", "state": "", "country": "HK",
        "postal": "", "phone": "+852 5550 01", "digits": 2, "web": "https://www.acme-corp.example/hk",
        "domain": "hk.acme-corp.example",
    },
}

# The four offices of every entity: contact type, floor, the mailbox, the
# number of the line. The group and the UK entity share a London exchange, so
# the UK lines start at 100.
OFFICES = [
    ("Legal", "Registered office", "legal", 0),
    ("Operations", "Operations floor", "operations", 10),
    ("Settlement", "Settlement desk, operations floor", "settlement", 20),
    ("Billing", "Finance department", "billing", 30),
]

COUNTRIES = {
    "acme_uk": ["GB", "IE", "DE", "FR"],
    "acme_us": ["US", "CA"],
    "acme_hk": ["HK", "SG", "JP"],
}

# The currencies of an entity are its functional currency, the currencies its
# rates books are named for, and every currency in the pairs its FX books trade
# (OFFICE_TREE in generate_data.py): Cable and Sterling, the G10 majors, the
# Scandies and the emerging markets in London, the G10 and Latin America in New
# York, and the G10 and Asia in Hong Kong. Hong Kong trades the offshore
# renminbi, CNH.
CURRENCIES = {
    "acme_uk": ["GBP", "EUR", "USD", "CHF", "JPY", "CAD", "AUD", "NZD", "SEK", "NOK", "DKK",
                "ZAR", "TRY", "PLN", "CZK", "HUF", "ILS"],
    "acme_us": ["USD", "CAD", "EUR", "GBP", "JPY", "MXN", "BRL", "CLP", "COP"],
    "acme_hk": ["HKD", "USD", "JPY", "AUD", "NZD", "CNH", "SGD", "KRW", "INR", "TWD"],
}

# Banks a treasury approves besides those it holds a master agreement with.
APPROVED = {
    "acme_uk": ["213800LBQA1Y9L22JB70", "549300PPXHEU2JF0AM85", "2138005O9XJIJN4JPN90"],
    "acme_us": ["6SHGI4ZSSLCXXQSBB395", "8I5DZWZKVSZI1NUHU748", "IGJSJL3JD5P30I6NJZ34",
                "9DJT3UXIJIZJI4WXO774", "549300RQT6K4WXZL3083"],
    "acme_hk": ["MLU0ZO3ML4LN2LL2TL39", "6SHGI4ZSSLCXXQSBB395", "549300SZJ9VS8SGXAN81"],
    "acme_group": ["MLU0ZO3ML4LN2LL2TL39", "6SHGI4ZSSLCXXQSBB395", "549300SZJ9VS8SGXAN81"],
}


def union(*lists):
    seen = []
    for items in lists:
        for item in items:
            if item not in seen:
                seen.append(item)
    return seen


def write(name, records):
    path = os.path.join(SCRIPT_DIR, f"{name}.json")
    with open(path, "w") as f:
        json.dump(records, f, indent=2, ensure_ascii=False)
        f.write("\n")
    print(f"Wrote {path} ({len(records)} records)")


def contacts():
    records = []
    for company, site in SITE.items():
        for contact_type, floor, mailbox, tail in OFFICES:
            records.append({
                "company_code": company,
                "contact_type": contact_type,
                "street_line_1": site["street"],
                "street_line_2": floor,
                "city": site["city"],
                "state": site["state"],
                "country_code": site["country"],
                "postal_code": site["postal"],
                "phone": site["phone"] + str(tail + (100 if company == "acme_uk" else 0)).zfill(site["digits"]),
                "email": f"{mailbox}@{site['domain']}",
                "web_page": site["web"],
                "is_primary": contact_type == "Legal",
            })
    return records


def links(by_company, key):
    entries = {c: list(v) for c, v in by_company.items()}
    entries["acme_group"] = union(*(entries[c] for c in ("acme_uk", "acme_us", "acme_hk")))
    return [
        {"company_code": company, key: value}
        for company in ("acme_group", "acme_uk", "acme_us", "acme_hk")
        for value in entries[company]
    ]


def counterparties():
    with open(os.path.join(SCRIPT_DIR, "netting_agreements.json")) as f:
        agreements = json.load(f)
    held = {company: [] for company in ENTITY_LEI}
    for agreement in agreements:
        held[agreement["company_code"]].append(agreement["counterparty_lei"])
    records = []
    for company, own in ENTITY_LEI.items():
        sisters = [lei for c, lei in ENTITY_LEI.items() if c != company]
        wanted = union(held[company], APPROVED[company], sisters)
        for lei in wanted:
            if lei != own:
                records.append({"company_code": company, "counterparty_lei": lei})
    return records


def check_books_are_covered():
    """Every book of an entity is in a currency that entity works in."""
    with open(os.path.join(SCRIPT_DIR, "books.json")) as f:
        books = json.load(f)
    for book in books:
        company = book["company_code"]
        if company in CURRENCIES and book["functional_currency"] not in CURRENCIES[company]:
            raise SystemExit(
                f"{company} has book {book['name']} in {book['functional_currency']}, "
                "which is not one of its currencies")


def main():
    check_books_are_covered()
    write("party_contacts", contacts())
    write("party_countries", links(COUNTRIES, "country_alpha2_code"))
    write("party_currencies", links(CURRENCIES, "currency_iso_code"))
    write("party_counterparties", counterparties())


if __name__ == "__main__":
    main()

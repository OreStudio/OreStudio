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
- A treasury's approved banks are read from the vendored GLEIF extract, not
  written by hand, so the list grows with the data rather than with a person's
  patience. See approved_banks() for the rule.

Usage:
    python3 generate_party_data.py
"""
import csv
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

# The counterparties the treasury of an entity approves besides the ones it
# holds a master agreement with, read from the vendored GLEIF extract by
# approved_banks() under the rule stated there.
GLEIF_DIR = os.path.normpath(
    os.path.join(SCRIPT_DIR, "..", "..", "..", "..", "external", "lei"))
GLEIF_ENTITIES = "20260123-0800-gleif-goldencopy-lei2-golden-copy-subset-tiny.csv"
GLEIF_BIC = "lei-bic-20260130T000000.csv"


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


def gleif_banks_by_country():
    """
    The GLEIF entities that are banks, by legal-address country.

    A tradeable counterparty is read from the same extract the provisioning
    publishes, so every link resolves: the LEI import stages the subset and the
    counterparty publish joins each staged LEI to a counterparty. An entity
    qualifies as a bank when the GLEIF BIC mapping names it, which is the
    registry's own statement that the entity is a financial institution a
    payment can be addressed to, rather than an inferred one. The file also
    carries entities no counterparty publish reaches, such as a bank of a
    country the tiny subset does not carry, so the two files are joined rather
    than either being trusted alone.

    Active general entities only: a retired entity cannot be traded with, and a
    fund or a branch is not the legal entity a master agreement names.
    """
    with open(os.path.join(GLEIF_DIR, GLEIF_BIC), encoding="utf-8-sig") as f:
        bic_leis = {row["LEI"] for row in csv.DictReader(f) if row["LEI"]}

    by_country = {}
    with open(os.path.join(GLEIF_DIR, GLEIF_ENTITIES), encoding="utf-8-sig", newline="") as f:
        for row in csv.DictReader(f):
            if (row["LEI"] in bic_leis
                    and row["Entity.EntityCategory"] == "GENERAL"
                    and row["Entity.EntityStatus"] == "ACTIVE"):
                by_country.setdefault(row["Entity.LegalAddress.Country"], set()).add(row["LEI"])
    return by_country


def approved_banks(company, held, banks_by_country):
    """
    The banks the treasury of an entity approves, by the rule.

    A party trades with the banks of the jurisdictions it books in: the legal
    address of the bank is in one of the countries the party itself works in.
    A London entity books sterling, euro and dollar business, so it faces
    British, Irish, German and French banks, and a Hong Kong entity faces Hong
    Kong and Singapore banks. The rule reads the grid the entity already
    declares in COUNTRIES, so the two datasets cannot disagree.

    The list is sorted by LEI so that it is stable, and an LEI is derived from
    the entity itself and never reassigned, so a bank keeps its place when the
    extract is refreshed.
    """
    if company == "acme_group":
        countries = sorted({code for office in COUNTRIES for code in COUNTRIES[office]})
    else:
        countries = COUNTRIES[company]
    wanted = {lei for country in countries for lei in banks_by_country.get(country, ())}
    return sorted(wanted - set(held) - {ENTITY_LEI[company]})


def counterparties():
    with open(os.path.join(SCRIPT_DIR, "netting_agreements.json")) as f:
        agreements = json.load(f)
    held = {company: [] for company in ENTITY_LEI}
    for agreement in agreements:
        held[agreement["company_code"]].append(agreement["counterparty_lei"])
    banks_by_country = gleif_banks_by_country()
    records = []
    for company, own in ENTITY_LEI.items():
        sisters = [lei for c, lei in ENTITY_LEI.items() if c != company]
        approved = approved_banks(company, held[company], banks_by_country)
        wanted = union(held[company], approved, sisters)
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

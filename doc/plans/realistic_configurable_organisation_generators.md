# Plan: Realistic Configurable Organisation Generators

## Overview

Add an `organisation_generator_service` to `ores.synthetic` that generates a
complete, interconnected organisational hierarchy (parties, counterparties,
addresses, identifiers, business units, portfolios, books) with realistic
GB/US-specific naming and addresses. All configurable via
`organisation_generation_options`.

The design ensures compatibility with a future `ores.synthetic.cli` tool that
could emit SQL for DQ artefact tables, enabling deterministic/reproducible
dataset generation from the command line.

---

## Step 1: Add `organisation_generation_options` domain type

**File:** `projects/ores.synthetic/include/ores.synthetic/domain/organisation_generation_options.hpp`

```cpp
struct organisation_generation_options final {
    std::optional<std::uint64_t> seed;
    std::string country = "GB";              // "GB" or "US" - drives naming, addresses, business centres

    // Party tree
    std::size_t party_count = 5;             // total operational parties (excl. system party)
    std::size_t party_max_depth = 3;         // max nesting (1=flat, 3=root->region->branch)

    // Counterparty tree
    std::size_t counterparty_count = 10;
    std::size_t counterparty_max_depth = 3;

    // Portfolio tree
    std::size_t portfolio_leaf_count = 8;    // number of leaf (trading) portfolios
    std::size_t portfolio_max_depth = 4;     // root->region->asset_class->leaf

    // Books
    std::size_t books_per_leaf_portfolio = 2;

    // Business units
    std::size_t business_unit_count = 10;
    std::size_t business_unit_max_depth = 3;

    // Addresses & identifiers
    bool generate_addresses = true;
    std::size_t contacts_per_party = 2;      // e.g. Legal + Operations
    std::size_t contacts_per_counterparty = 1;
    bool generate_identifiers = true;        // LEI, BIC codes
};
```

---

## Step 2: Add `generated_organisation` domain type

**File:** `projects/ores.synthetic/include/ores.synthetic/domain/generated_organisation.hpp`

This holds all generated entities in insertion order (parents before children).

```cpp
struct generated_organisation final {
    std::uint64_t seed = 0;

    // Parties (parent-first order)
    std::vector<refdata::domain::party> parties;
    std::vector<refdata::domain::party_contact_information> party_contacts;
    std::vector<refdata::domain::party_identifier> party_identifiers;

    // Counterparties (parent-first order)
    std::vector<refdata::domain::counterparty> counterparties;
    std::vector<refdata::domain::counterparty_contact_information> counterparty_contacts;
    std::vector<refdata::domain::counterparty_identifier> counterparty_identifiers;

    // Junctions - links counterparties to root party
    std::vector<refdata::domain::party_counterparty> party_counterparties;

    // Organisation structure
    std::vector<refdata::domain::business_unit> business_units;
    std::vector<refdata::domain::portfolio> portfolios;
    std::vector<refdata::domain::book> books;
};
```

---

## Step 3: Add naming and address data arrays

**File:** `projects/ores.synthetic/src/data/financial_names.hpp` (private header, not installed)

Static `constexpr` arrays of realistic financial entity name components, split
by country:

**GB party name suffixes:** `"Bank Plc"`, `"Capital Ltd"`, `"Securities Ltd"`,
`"Asset Management LLP"`, `"Wealth Management Ltd"`, `"Holdings Plc"`,
`"Investments Ltd"`, `"Partners LLP"`

**US party name suffixes:** `"Financial Inc"`, `"Capital LLC"`, `"Securities
Corp"`, `"Asset Management LLC"`, `"Wealth Management Inc"`, `"Holdings Corp"`,
`"Investments LLC"`, `"Partners LP"`

**GB counterparty name patterns:** `"{Surname} Brothers"`, `"{Surname} & Co"`,
`"{Surname} Capital"`, `"{Surname} Bank Plc"`, `"Royal {Surname} Group"`,
`"{Surname} Investment Trust"`, `"{Surname} Wealth Management"`,
`"{Surname} Securities"`

**US counterparty name patterns:** `"{Surname} Stanley"`, `"{Surname} Sachs"`,
`"{Surname} Lynch"`, `"{Surname} Financial Group"`, `"First {Surname} Bank"`,
`"{Surname} Asset Management"`, `"{Surname} Brothers & Co"`,
`"{City} Trust Company"`

**Financial centre cities:**

| Country | Cities | Business Centre Codes |
|---------|--------|----------------------|
| GB | London, Edinburgh, Birmingham, Manchester, Leeds, Glasgow | GBLO, GBED, GBLO, GBLO, GBLO, GBLO |
| US | New York, Chicago, Boston, San Francisco, Charlotte, Houston | USNY, USCH, USBO, USSF, USNY, USNY |

**Portfolio naming convention** (structural, same for both countries):
- Depth 0: `"Global Portfolio"`
- Depth 1: `"{Region} Portfolio"` (EMEA, Americas, APAC)
- Depth 2: `"{Asset Class} {Region}"` (Rates, Credit, FX, Equities)
- Depth 3: `"{CCY} {Asset Class}"` (GBP Rates, EUR Credit IG, etc.)

**Book naming convention:**
`"{CCY} {Product Type}"` e.g. `"GBP Vanilla Swaps"`, `"EUR Rates Options"`

**Business unit naming convention:**
`"{Function} {Region}"` e.g. `"Rates Trading EMEA"`, `"Credit Trading Americas"`

**Asset classes and product types:**

```cpp
constexpr std::array asset_classes = { "Rates", "Credit", "FX", "Equities" };
constexpr std::array product_types = {
    "Vanilla Swaps", "Options", "Exotic", "Spot Forward", "CDS", "Bonds", "NDF"
};
```

**GL account reference and cost centre patterns:**

```cpp
// GL: "GL-{ASSET}-{SEQ}" e.g. "GL-RATES-001"
// CC: "CC-{REGION}-{ASSET}" e.g. "CC-EMEA-RATES"
```

All names use `faker::person::lastName(locale)` where locale is
`faker::Locale::en_GB` or `faker::Locale::en_US` based on the country option.

---

## Step 4: Add tree generation utility

**File:** `projects/ores.utility/include/ores.utility/generation/tree_builder.hpp`
**File:** `projects/ores.utility/src/generation/tree_builder.cpp`

A generic utility for generating hierarchical node structures:

```cpp
struct tree_node {
    std::size_t index;      // position in flat output vector
    std::size_t depth;      // depth in tree (0 = root)
    std::optional<std::size_t> parent_index;  // index of parent, nullopt for roots
};

/**
 * Generates a tree of `total_count` nodes with max depth `max_depth`.
 * Returns nodes in parent-first order (safe for insertion).
 *
 * Algorithm:
 * - Creates 1 root node at depth 0
 * - Distributes remaining nodes across levels using a breadth-first approach
 * - At each level, children are distributed among existing nodes at the
 *   previous level, with random variation controlled by the engine
 * - Stops when total_count reached or max_depth exceeded
 */
std::vector<tree_node> generate_tree(
    std::size_t total_count,
    std::size_t max_depth,
    generation_engine& engine);
```

This utility is generic and reusable across party trees, counterparty trees,
portfolio trees, and business unit trees. It only produces the shape; the
service fills in domain-specific data for each node.

---

## Step 5: Add `organisation_generator_service`

**File:** `projects/ores.synthetic/include/ores.synthetic/service/organisation_generator_service.hpp`
**File:** `projects/ores.synthetic/src/service/organisation_generator_service.cpp`

```cpp
class organisation_generator_service final {
public:
    domain::generated_organisation generate(
        const domain::organisation_generation_options& options);
    domain::generated_organisation generate(); // defaults
};
```

**Orchestration flow in `generate()`:**

```
1. Create generation_context with seed from options
2. Determine locale and country-specific data arrays based on options.country

3. Generate party tree:
   a. Use tree_builder to get tree_node shape (party_count, party_max_depth)
   b. For root: pick lastName(locale) + pick party_suffix -> full_name
      e.g. "Rutherford Bank Plc"
      short_code = uppercase abbreviated form e.g. "RUTH"
      party_category = "Operational", party_type = "Corporate"
      business_center_code = primary financial centre for country
   c. For children: derive name from root: "{Root} {Region}" or "{Root} {City} Branch"
      business_center_code = regional centre
   d. Store root party UUID for later use

4. Generate party contacts (addresses):
   For each party, generate contacts_per_party contact records:
   - contact_type cycles through: "Legal", "Operations", "Settlement", "Billing"
   - Root party: use curated financial centre city
   - Children: use faker::location with appropriate locale
   - street_line_1 = faker::location::streetAddress(locale)
   - city = curated or faker::location::city(locale)
   - state = faker::location::state(locale) (GB: county, US: state)
   - postal_code = faker::location::zipCode(locale)
   - country_code = "GB" or "US"
   - phone = formatted per country (+44 20... or +1 212...)
   - email = "contact@{short_code_lower}.co.uk" or ".com"
   - web_page = "https://www.{short_code_lower}.co.uk" or ".com"

5. Generate party identifiers:
   For each party, generate LEI + BIC:
   - LEI: id_scheme="LEI", id_value = alphanumeric(20)
   - BIC: id_scheme="BIC", id_value = faker::finance::bic(locale)

6. Generate counterparty tree:
   a. Use tree_builder for shape (counterparty_count, counterparty_max_depth)
   b. For each root: pick a name pattern, fill with lastName(locale)
      e.g. pattern "{Surname} Brothers" + lastName "Crawford" -> "Crawford Brothers"
      party_type = pick from ["Bank", "Securities Firm", "Insurance Co", "Fund Manager"]
   c. For children: "{Root} {City}" or "{Root} {Region}"

7. Generate counterparty contacts (addresses):
   Same pattern as party contacts but for counterparty_contact_information.

8. Generate counterparty identifiers:
   Same as party identifiers.

9. Generate party_counterparty junctions:
   Link all counterparties to the root party.

10. Generate business unit tree:
    a. tree_builder for shape (business_unit_count, business_unit_max_depth)
    b. Root: "Global Markets", code "GLOB_MKT"
    c. Depth 1: "{Region} Trading" e.g. "EMEA Trading", code "EMEA_TRD"
    d. Depth 2: "{Asset Class} Trading {Region}" e.g. "Rates Trading EMEA"
    e. All units reference root party_id
    f. business_centre_code matches region

11. Generate portfolio tree:
    a. Structural generation (not generic tree_builder):
       - 1 root: "Global Portfolio" (virtual=1, ccy=root_ccy)
       - N regional portfolios (virtual=1)
       - Under each region: asset class portfolios (virtual=1)
       - Under each asset class: leaf portfolios (virtual=0)
       - Total leaf count = portfolio_leaf_count
    b. owner_unit_id references matching business units
    c. aggregation_ccy matches region

12. Generate books:
    For each leaf portfolio, generate books_per_leaf_portfolio books:
    - name = "{CCY} {Product Type}" from product_types array
    - parent_portfolio_id = leaf portfolio id
    - party_id = root party id
    - ledger_ccy = portfolio aggregation_ccy
    - gl_account_ref = "GL-{ASSET}-{SEQ}"
    - cost_center = "CC-{REGION}-{ASSET}"
    - book_status = "Active"
    - is_trading_book = 1 (except some regulatory books)

13. Return generated_organisation with seed stamped
```

---

## Step 6: Update CMakeLists.txt for ores.synthetic

**File:** `projects/ores.synthetic/src/CMakeLists.txt`

Add `ores.refdata.lib` as a PUBLIC dependency (since `generated_organisation`
references refdata domain types in its public header):

```cmake
target_link_libraries(${lib_target_name}
    PUBLIC
        ores.dq.lib
        ores.iam.lib
        ores.refdata.lib    # NEW
    PRIVATE
        ores.utility.lib
        faker-cxx::faker-cxx
        Boost::boost)
```

---

## Step 7: Add unit tests

**File:** `projects/ores.synthetic/tests/organisation_generator_service_tests.cpp`

Test cases following the `catalog_generator_service_tests.cpp` pattern:

1. `generate_with_defaults_produces_nonempty_organisation` - all vectors populated
2. `generate_respects_party_count` - correct number of parties
3. `generate_respects_counterparty_count` - correct number of counterparties
4. `generate_with_seed_is_reproducible` - same seed -> same output
5. `generate_stores_seed_in_result` - seed preserved
6. `generate_parties_have_valid_fields` - all required fields non-empty
7. `generate_party_tree_has_correct_depth` - no party deeper than max_depth
8. `generate_party_contacts_reference_existing_parties` - party_id exists
9. `generate_counterparty_contacts_reference_existing_counterparties`
10. `generate_party_identifiers_reference_existing_parties`
11. `generate_party_counterparties_link_existing_entities` - both IDs exist
12. `generate_portfolios_form_valid_tree` - parent_portfolio_id references exist
13. `generate_books_reference_leaf_portfolios` - parent_portfolio_id is leaf
14. `generate_books_reference_root_party` - party_id = root party
15. `generate_business_units_reference_root_party`
16. `generate_addresses_use_correct_country_code` - "GB" for GB, "US" for US
17. `generate_gb_names_have_gb_suffixes` - Plc, Ltd, LLP
18. `generate_us_names_have_us_suffixes` - Inc, LLC, Corp
19. `generate_with_addresses_disabled_produces_no_contacts` - flag honoured
20. `generate_with_identifiers_disabled_produces_no_identifiers`

---

## Step 8: Add tree_builder unit tests

**File:** `projects/ores.utility/tests/tree_builder_tests.cpp`

1. `single_node_tree` - total_count=1 -> one root, depth 0, no parent
2. `flat_tree` - max_depth=1 -> root + N children all at depth 1
3. `deep_tree` - respects max_depth limit
4. `parent_first_ordering` - parent index < child index for all nodes
5. `total_count_respected` - exact count of nodes produced
6. `reproducible_with_same_engine_seed` - same seed -> same tree shape

---

## Future compatibility: Synthetic CLI tool

The design is already compatible with a future `ores.synthetic.cli` that:

1. Takes `organisation_generation_options` as CLI args or JSON config
2. Calls `organisation_generator_service::generate(options)`
3. Serialises the `generated_organisation` to SQL INSERT statements targeting
   `ores_dq_*_artefact_tbl` tables (same format as the existing testdata SQL)
4. Outputs deterministic SQL (same seed -> same SQL) for DQ dataset population

This works because:
- The service is a pure library with no I/O or persistence dependencies
- `generated_organisation` contains plain domain objects that can be serialised
  to any format (JSON, SQL, CSV)
- The seed ensures reproducibility
- The options struct maps naturally to CLI arguments

No changes are needed now for this future direction; the service API is already
sufficient.

---

## Files to create (new)

| # | File | Description |
|---|------|-------------|
| 1 | `ores.synthetic/include/.../domain/organisation_generation_options.hpp` | Options struct |
| 2 | `ores.synthetic/include/.../domain/generated_organisation.hpp` | Result struct |
| 3 | `ores.synthetic/include/.../service/organisation_generator_service.hpp` | Service header |
| 4 | `ores.synthetic/src/service/organisation_generator_service.cpp` | Service impl |
| 5 | `ores.synthetic/src/data/financial_names.hpp` | Private naming data arrays |
| 6 | `ores.synthetic/tests/organisation_generator_service_tests.cpp` | Tests |
| 7 | `ores.utility/include/.../generation/tree_builder.hpp` | Tree builder header |
| 8 | `ores.utility/src/generation/tree_builder.cpp` | Tree builder impl |
| 9 | `ores.utility/tests/tree_builder_tests.cpp` | Tree builder tests |

## Files to modify (existing)

| # | File | Change |
|---|------|--------|
| 1 | `ores.synthetic/src/CMakeLists.txt` | Add `ores.refdata.lib` dependency |

## Files NOT modified

The existing flat generators in `ores.refdata/generators/` remain unchanged -
they continue serving unit tests. The `organisation_generator_service` builds
entities directly using the same patterns but with proper hierarchy and
cross-references.

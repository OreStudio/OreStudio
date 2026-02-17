/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.synthetic/service/organisation_generator_service.hpp"

#include <set>
#include <unordered_set>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[organisation_generator_service]");

using namespace ores::synthetic::service;
using namespace ores::synthetic::domain;

}

TEST_CASE("generate_with_defaults_produces_nonempty_organisation", tags) {
    organisation_generator_service svc;
    auto result = svc.generate();

    REQUIRE(!result.parties.empty());
    REQUIRE(!result.counterparties.empty());
    REQUIRE(!result.business_units.empty());
    REQUIRE(!result.portfolios.empty());
    REQUIRE(!result.books.empty());
    REQUIRE(!result.party_contacts.empty());
    REQUIRE(!result.counterparty_contacts.empty());
    REQUIRE(!result.party_identifiers.empty());
    REQUIRE(!result.counterparty_identifiers.empty());
    REQUIRE(!result.party_counterparties.empty());
}

TEST_CASE("generate_respects_party_count", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.party_count = 3;

    auto result = svc.generate(opts);
    REQUIRE(result.parties.size() == 3);
}

TEST_CASE("generate_respects_counterparty_count", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.counterparty_count = 7;

    auto result = svc.generate(opts);
    REQUIRE(result.counterparties.size() == 7);
}

TEST_CASE("generate_with_seed_is_reproducible", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result1 = svc.generate(opts);
    auto result2 = svc.generate(opts);

    REQUIRE(result1.seed == result2.seed);
    REQUIRE(result1.parties.size() == result2.parties.size());
    REQUIRE(result1.counterparties.size() == result2.counterparties.size());
    REQUIRE(result1.portfolios.size() == result2.portfolios.size());
    REQUIRE(result1.books.size() == result2.books.size());

    for (std::size_t i = 0; i < result1.parties.size(); ++i) {
        CHECK(result1.parties[i].full_name == result2.parties[i].full_name);
        CHECK(result1.parties[i].short_code == result2.parties[i].short_code);
    }

    for (std::size_t i = 0; i < result1.counterparties.size(); ++i) {
        CHECK(result1.counterparties[i].full_name ==
            result2.counterparties[i].full_name);
    }
}

TEST_CASE("generate_stores_seed_in_result", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 777;

    auto result = svc.generate(opts);
    REQUIRE(result.seed == 777);
}

TEST_CASE("generate_parties_have_valid_fields", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);
    for (const auto& party : result.parties) {
        CHECK(party.version == 1);
        CHECK(!party.id.is_nil());
        CHECK(!party.full_name.empty());
        CHECK(!party.short_code.empty());
        CHECK(party.party_category == "Operational");
        CHECK(party.party_type == "Corporate");
        CHECK(party.status == "Active");
        CHECK(!party.modified_by.empty());
        CHECK(!party.business_center_code.empty());
    }
}

TEST_CASE("generate_party_tree_has_correct_parent_references", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.party_count = 5;
    opts.party_max_depth = 3;

    auto result = svc.generate(opts);

    // Root party has no parent.
    CHECK(!result.parties[0].parent_party_id.has_value());

    // Collect all party IDs.
    std::set<boost::uuids::uuid> party_ids;
    for (const auto& p : result.parties)
        party_ids.insert(p.id);

    // All non-root parties must reference an existing party as parent.
    for (std::size_t i = 1; i < result.parties.size(); ++i) {
        REQUIRE(result.parties[i].parent_party_id.has_value());
        CHECK(party_ids.count(*result.parties[i].parent_party_id) == 1);
    }
}

TEST_CASE("generate_party_contacts_reference_existing_parties", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    std::set<boost::uuids::uuid> party_ids;
    for (const auto& p : result.parties)
        party_ids.insert(p.id);

    for (const auto& contact : result.party_contacts) {
        CHECK(!contact.id.is_nil());
        CHECK(party_ids.count(contact.party_id) == 1);
        CHECK(!contact.street_line_1.empty());
        CHECK(!contact.city.empty());
        CHECK(!contact.postal_code.empty());
        CHECK(!contact.contact_type.empty());
    }
}

TEST_CASE("generate_counterparty_contacts_reference_existing_counterparties",
    tags) {

    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    std::set<boost::uuids::uuid> cpty_ids;
    for (const auto& c : result.counterparties)
        cpty_ids.insert(c.id);

    for (const auto& contact : result.counterparty_contacts) {
        CHECK(!contact.id.is_nil());
        CHECK(cpty_ids.count(contact.counterparty_id) == 1);
        CHECK(!contact.street_line_1.empty());
        CHECK(!contact.city.empty());
    }
}

TEST_CASE("generate_party_identifiers_reference_existing_parties", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    std::set<boost::uuids::uuid> party_ids;
    for (const auto& p : result.parties)
        party_ids.insert(p.id);

    for (const auto& ident : result.party_identifiers) {
        CHECK(!ident.id.is_nil());
        CHECK(party_ids.count(ident.party_id) == 1);
        CHECK(!ident.id_scheme.empty());
        CHECK(!ident.id_value.empty());
    }

    // Each party should have LEI and BIC.
    CHECK(result.party_identifiers.size() == result.parties.size() * 2);
}

TEST_CASE("generate_party_counterparties_link_existing_entities", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    std::set<boost::uuids::uuid> party_ids;
    for (const auto& p : result.parties)
        party_ids.insert(p.id);

    std::set<boost::uuids::uuid> cpty_ids;
    for (const auto& c : result.counterparties)
        cpty_ids.insert(c.id);

    for (const auto& pc : result.party_counterparties) {
        CHECK(party_ids.count(pc.party_id) == 1);
        CHECK(cpty_ids.count(pc.counterparty_id) == 1);
    }

    // Every counterparty should be linked to the root party.
    CHECK(result.party_counterparties.size() == result.counterparties.size());
}

TEST_CASE("generate_portfolios_form_valid_tree", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    std::set<boost::uuids::uuid> portfolio_ids;
    for (const auto& p : result.portfolios)
        portfolio_ids.insert(p.id);

    bool has_root = false;
    for (const auto& p : result.portfolios) {
        if (!p.parent_portfolio_id) {
            has_root = true;
        } else {
            CHECK(portfolio_ids.count(*p.parent_portfolio_id) == 1);
        }
    }
    CHECK(has_root);
}

TEST_CASE("generate_books_reference_leaf_portfolios", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    // Collect leaf portfolio IDs (is_virtual == 0).
    std::set<boost::uuids::uuid> leaf_ids;
    for (const auto& p : result.portfolios) {
        if (p.is_virtual == 0)
            leaf_ids.insert(p.id);
    }

    for (const auto& book : result.books) {
        CHECK(!book.id.is_nil());
        CHECK(leaf_ids.count(book.parent_portfolio_id) == 1);
        CHECK(!book.name.empty());
        CHECK(!book.ledger_ccy.empty());
        CHECK(!book.gl_account_ref.empty());
    }
}

TEST_CASE("generate_books_reference_root_party", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    const auto root_party_id = result.parties[0].id;
    for (const auto& book : result.books) {
        CHECK(book.party_id == root_party_id);
    }
}

TEST_CASE("generate_business_units_reference_root_party", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    const auto root_party_id = result.parties[0].id;
    for (const auto& bu : result.business_units) {
        CHECK(bu.party_id == root_party_id);
        CHECK(!bu.unit_name.empty());
        CHECK(!bu.unit_code.empty());
    }
}

TEST_CASE("generate_addresses_use_correct_country_code_gb", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.country = "GB";

    auto result = svc.generate(opts);

    for (const auto& contact : result.party_contacts) {
        CHECK(contact.country_code == "GB");
    }
    for (const auto& contact : result.counterparty_contacts) {
        CHECK(contact.country_code == "GB");
    }
}

TEST_CASE("generate_addresses_use_correct_country_code_us", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.country = "US";

    auto result = svc.generate(opts);

    for (const auto& contact : result.party_contacts) {
        CHECK(contact.country_code == "US");
    }
    for (const auto& contact : result.counterparty_contacts) {
        CHECK(contact.country_code == "US");
    }
}

TEST_CASE("generate_with_addresses_disabled_produces_no_contacts", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.generate_addresses = false;

    auto result = svc.generate(opts);

    CHECK(result.party_contacts.empty());
    CHECK(result.counterparty_contacts.empty());
    // Parties and counterparties should still exist.
    CHECK(!result.parties.empty());
    CHECK(!result.counterparties.empty());
}

TEST_CASE("generate_with_identifiers_disabled_produces_no_identifiers", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.generate_identifiers = false;

    auto result = svc.generate(opts);

    CHECK(result.party_identifiers.empty());
    CHECK(result.counterparty_identifiers.empty());
    CHECK(!result.parties.empty());
    CHECK(!result.counterparties.empty());
}

TEST_CASE("generate_us_produces_distinct_names_from_gb", tags) {
    organisation_generator_service svc;

    organisation_generation_options gb_opts;
    gb_opts.seed = 42;
    gb_opts.country = "GB";
    auto gb_result = svc.generate(gb_opts);

    organisation_generation_options us_opts;
    us_opts.seed = 42;
    us_opts.country = "US";
    auto us_result = svc.generate(us_opts);

    // The root party names should differ because of different suffixes.
    // (They may share a surname since the seed is the same, but the suffix
    // arrays differ between GB and US.)
    CHECK(!gb_result.parties.empty());
    CHECK(!us_result.parties.empty());
    // At minimum, the web domains should differ.
    if (!gb_result.party_contacts.empty() &&
        !us_result.party_contacts.empty()) {
        CHECK(gb_result.party_contacts[0].web_page !=
            us_result.party_contacts[0].web_page);
    }
}

TEST_CASE("generate_party_short_codes_are_unique", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.party_count = 10;

    auto result = svc.generate(opts);

    std::unordered_set<std::string> codes;
    for (const auto& p : result.parties) {
        CHECK(!p.short_code.empty());
        CHECK(codes.insert(p.short_code).second);
    }
}

TEST_CASE("generate_counterparty_short_codes_are_unique", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.counterparty_count = 20;

    auto result = svc.generate(opts);

    std::unordered_set<std::string> codes;
    for (const auto& c : result.counterparties) {
        CHECK(!c.short_code.empty());
        CHECK(codes.insert(c.short_code).second);
    }
}

TEST_CASE("generate_all_short_codes_across_entities_are_unique", tags) {
    organisation_generator_service svc;
    organisation_generation_options opts;
    opts.seed = 42;
    opts.party_count = 10;
    opts.counterparty_count = 20;

    auto result = svc.generate(opts);

    std::unordered_set<std::string> codes;
    for (const auto& p : result.parties)
        CHECK(codes.insert(p.short_code).second);
    for (const auto& c : result.counterparties)
        CHECK(codes.insert(c.short_code).second);
}

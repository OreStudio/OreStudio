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
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/generation/tree_builder.hpp"

#include <algorithm>
#include <array>
#include <iomanip>
#include <sstream>
#include <unordered_set>

#include "ores.utility/string/short_code_generator.hpp"
#include "data/financial_names.hpp"

namespace ores::synthetic::service {

using ores::utility::generation::generation_context;
using ores::utility::generation::generation_keys;
using ores::utility::generation::generate_tree;

namespace {

// ============================================================================
// Country-specific data selectors
// ============================================================================

std::string_view domain_suffix_for(const std::string& country) {
    return country == "US" ? data::us_domain_suffix : data::gb_domain_suffix;
}

std::string_view phone_prefix_for(const std::string& country) {
    return country == "US" ? data::us_phone_prefix : data::gb_phone_prefix;
}

const auto& financial_centres_for(const std::string& country) {
    return country == "US" ? data::us_financial_centres
        : data::gb_financial_centres;
}

const auto& party_suffixes_for(const std::string& country) {
    return country == "US" ? data::us_party_suffixes : data::gb_party_suffixes;
}

const auto& counterparty_patterns_for(const std::string& country) {
    return country == "US" ? data::us_counterparty_patterns
        : data::gb_counterparty_patterns;
}

const auto& surnames_for(const std::string& country) {
    return country == "US" ? data::us_surnames : data::gb_surnames;
}

const auto& street_names_for(const std::string& country) {
    return country == "US" ? data::us_street_names : data::gb_street_names;
}

std::string_view bic_country_suffix_for(const std::string& country) {
    return country == "US" ? data::us_bic_suffix : data::gb_bic_suffix;
}

// ============================================================================
// Name generation helpers
// ============================================================================

/**
 * @brief Applies a name pattern, replacing {0} with surname and {1} with city.
 */
std::string apply_pattern(std::string_view pattern,
    const std::string& surname, const std::string& city) {
    std::string result;
    result.reserve(pattern.size() + surname.size() + city.size());
    for (std::size_t i = 0; i < pattern.size(); ++i) {
        if (i + 2 < pattern.size() && pattern[i] == '{') {
            if (pattern[i + 1] == '0' && pattern[i + 2] == '}') {
                result += surname;
                i += 2;
                continue;
            }
            if (pattern[i + 1] == '1' && pattern[i + 2] == '}') {
                result += city;
                i += 2;
                continue;
            }
        }
        result += pattern[i];
    }
    return result;
}

using utility::string::generate_unique_short_code;

std::string make_slug(const std::string& name) {
    std::string slug;
    for (char c : name) {
        if (std::isalpha(static_cast<unsigned char>(c)))
            slug += static_cast<char>(
                std::tolower(static_cast<unsigned char>(c)));
        else if (c == ' ' && !slug.empty() && slug.back() != '-')
            slug += '-';
    }
    while (!slug.empty() && slug.back() == '-')
        slug.pop_back();
    return slug;
}

std::string make_phone(const std::string& country,
    generation_context& ctx) {
    const auto prefix = phone_prefix_for(country);
    std::ostringstream os;
    os << prefix << " " << ctx.random_int(100, 999)
       << " " << ctx.random_int(1000, 9999);
    return os.str();
}

/**
 * @brief Generates a deterministic street address from curated arrays.
 */
std::string make_street_address(const std::string& country,
    generation_context& ctx) {
    const auto number = ctx.random_int(1, 200);
    const auto& street = ctx.pick(street_names_for(country));
    return std::to_string(number) + " " + std::string(street);
}

/**
 * @brief Generates a deterministic postal code from the centre's prefix.
 */
std::string make_postal_code(const std::string& country,
    const data::financial_centre& centre,
    generation_context& ctx) {
    if (country == "GB") {
        // UK format: "EC2V 8AS"
        return std::string(centre.postal_prefix)
            + std::to_string(ctx.random_int(1, 9))
            + std::string(1, static_cast<char>('A' + ctx.random_int(0, 25)))
            + " " + std::to_string(ctx.random_int(1, 9))
            + std::string(1, static_cast<char>('A' + ctx.random_int(0, 25)))
            + std::string(1, static_cast<char>('A' + ctx.random_int(0, 25)));
    }
    // US format: "10001"
    return std::string(centre.postal_prefix)
        + std::to_string(ctx.random_int(100, 999));
}

/**
 * @brief Generates a deterministic BIC code.
 */
std::string make_bic(const std::string& country,
    generation_context& ctx) {
    const auto& prefix = ctx.pick(data::bic_prefixes);
    return std::string(prefix) + std::string(bic_country_suffix_for(country));
}

// ============================================================================
// Audit field helpers
// ============================================================================

struct audit_fields {
    std::string tenant_id;
    std::string modified_by;
};

audit_fields get_audit(generation_context& ctx) {
    return {
        ctx.env().get_or(generation_keys::tenant_id, "system"),
        ctx.env().get_or(generation_keys::modified_by, "system")
    };
}

// ============================================================================
// Party generation
// ============================================================================

void generate_parties(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result,
    std::unordered_set<std::string>& used_codes) {

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto& centres = financial_centres_for(options.country);
    const auto& suffixes = party_suffixes_for(options.country);
    const auto& surnames = surnames_for(options.country);

    auto nodes = generate_tree(
        options.party_count, options.party_max_depth, ctx.engine());

    const auto root_surname = std::string(ctx.pick(surnames));

    for (const auto& node : nodes) {
        refdata::domain::party p;
        p.version = 1;
        p.tenant_id = tenant_id;
        p.id = ctx.generate_uuid();
        p.party_category = "Operational";
        p.party_type = "Corporate";
        p.status = "Active";
        p.modified_by = modified_by;
        p.performed_by = modified_by;
        p.change_reason_code = "system.new_record";
        p.change_commentary = "Generated organisation data";
        p.recorded_at = ctx.past_timepoint();

        const auto& centre = centres[node.index % centres.size()];

        if (node.depth == 0) {
            const auto& suffix = ctx.pick(suffixes);
            p.full_name = root_surname + " " + std::string(suffix);
            p.short_code = generate_unique_short_code(p.full_name, used_codes);
            p.business_center_code =
                std::string(centres[0].business_centre_code);
            p.parent_party_id = std::nullopt;
        } else if (node.depth == 1) {
            const auto region_idx =
                (node.index - 1) % data::region_names.size();
            p.full_name = root_surname + " "
                + std::string(data::region_names[region_idx]);
            p.short_code = generate_unique_short_code(p.full_name, used_codes);
            p.business_center_code = std::string(centre.business_centre_code);
            p.parent_party_id = result.parties[*node.parent_index].id;
        } else {
            p.full_name = root_surname + " "
                + std::string(centre.city) + " Branch";
            p.short_code = generate_unique_short_code(p.full_name, used_codes);
            p.business_center_code = std::string(centre.business_centre_code);
            p.parent_party_id = result.parties[*node.parent_index].id;
        }

        result.parties.push_back(std::move(p));
    }
}

// ============================================================================
// Counterparty generation
// ============================================================================

void generate_counterparties(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result,
    std::unordered_set<std::string>& used_codes) {

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto& centres = financial_centres_for(options.country);
    const auto& patterns = counterparty_patterns_for(options.country);
    const auto& surnames = surnames_for(options.country);

    auto nodes = generate_tree(
        options.counterparty_count, options.counterparty_max_depth,
        ctx.engine());

    for (const auto& node : nodes) {
        refdata::domain::counterparty c;
        c.version = 1;
        c.tenant_id = tenant_id;
        c.id = ctx.generate_uuid();
        c.party_type = std::string(ctx.pick(data::counterparty_types));
        c.status = "Active";
        c.modified_by = modified_by;
        c.performed_by = modified_by;
        c.change_reason_code = "system.new_record";
        c.change_commentary = "Generated organisation data";
        c.recorded_at = ctx.past_timepoint();

        const auto& centre = centres[node.index % centres.size()];

        if (!node.parent_index) {
            const auto surname = std::string(ctx.pick(surnames));
            const auto city = std::string(centre.city);
            const auto& pattern = ctx.pick(patterns);
            c.full_name = apply_pattern(pattern, surname, city);
            c.short_code = generate_unique_short_code(c.full_name, used_codes);
            c.business_center_code =
                std::string(centre.business_centre_code);
            c.parent_counterparty_id = std::nullopt;
        } else {
            const auto& parent = result.counterparties[*node.parent_index];
            auto space_pos = parent.full_name.find(' ');
            auto prefix = (space_pos != std::string::npos)
                ? parent.full_name.substr(0, space_pos) : parent.full_name;
            c.full_name = prefix + " " + std::string(centre.city);
            c.short_code = generate_unique_short_code(c.full_name, used_codes);
            c.business_center_code =
                std::string(centre.business_centre_code);
            c.parent_counterparty_id = parent.id;
        }

        result.counterparties.push_back(std::move(c));
    }
}

// ============================================================================
// Contact information (addresses) generation
// ============================================================================

void generate_party_contacts(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result) {

    if (!options.generate_addresses)
        return;

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto& centres = financial_centres_for(options.country);
    const auto domain_suffix = domain_suffix_for(options.country);

    for (std::size_t pi = 0; pi < result.parties.size(); ++pi) {
        const auto& party = result.parties[pi];
        const auto slug = make_slug(party.full_name);
        const auto& centre = centres[pi % centres.size()];

        for (std::size_t ci = 0; ci < options.contacts_per_party; ++ci) {
            refdata::domain::party_contact_information pci;
            pci.version = 1;
            pci.tenant_id = tenant_id;
            pci.id = ctx.generate_uuid();
            pci.party_id = party.id;
            pci.contact_type = std::string(
                data::contact_types[ci % data::contact_types.size()]);
            pci.street_line_1 =
                make_street_address(options.country, ctx);
            pci.street_line_2 =
                pi == 0 ? std::string("Floor ")
                    + std::to_string(ctx.random_int(1, 30))
                : "";
            pci.city = std::string(centre.city);
            pci.state = std::string(centre.state);
            pci.country_code = options.country;
            pci.postal_code =
                make_postal_code(options.country, centre, ctx);
            pci.phone = make_phone(options.country, ctx);
            pci.email = "contact@" + slug + "." + std::string(domain_suffix);
            pci.web_page = "https://www." + slug + "."
                + std::string(domain_suffix);
            pci.modified_by = modified_by;
            pci.performed_by = modified_by;
            pci.change_reason_code = "system.new_record";
            pci.change_commentary = "Generated organisation data";
            pci.recorded_at = ctx.past_timepoint();

            result.party_contacts.push_back(std::move(pci));
        }
    }
}

void generate_counterparty_contacts(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result) {

    if (!options.generate_addresses)
        return;

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto& centres = financial_centres_for(options.country);
    const auto domain_suffix = domain_suffix_for(options.country);

    for (std::size_t ci = 0; ci < result.counterparties.size(); ++ci) {
        const auto& cpty = result.counterparties[ci];
        const auto slug = make_slug(cpty.full_name);
        const auto& centre = centres[ci % centres.size()];

        for (std::size_t ti = 0;
             ti < options.contacts_per_counterparty; ++ti) {
            refdata::domain::counterparty_contact_information cci;
            cci.version = 1;
            cci.tenant_id = tenant_id;
            cci.id = ctx.generate_uuid();
            cci.counterparty_id = cpty.id;
            cci.contact_type = std::string(
                data::contact_types[ti % data::contact_types.size()]);
            cci.street_line_1 =
                make_street_address(options.country, ctx);
            cci.street_line_2 = "";
            cci.city = std::string(centre.city);
            cci.state = std::string(centre.state);
            cci.country_code = options.country;
            cci.postal_code =
                make_postal_code(options.country, centre, ctx);
            cci.phone = make_phone(options.country, ctx);
            cci.email = "info@" + slug + "." + std::string(domain_suffix);
            cci.web_page = "https://www." + slug + "."
                + std::string(domain_suffix);
            cci.modified_by = modified_by;
            cci.performed_by = modified_by;
            cci.change_reason_code = "system.new_record";
            cci.change_commentary = "Generated organisation data";
            cci.recorded_at = ctx.past_timepoint();

            result.counterparty_contacts.push_back(std::move(cci));
        }
    }
}

// ============================================================================
// Identifier generation
// ============================================================================

void generate_party_identifiers(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result) {

    if (!options.generate_identifiers)
        return;

    const auto [tenant_id, modified_by] = get_audit(ctx);

    for (const auto& party : result.parties) {
        // LEI identifier
        {
            refdata::domain::party_identifier pid;
            pid.version = 1;
            pid.tenant_id = tenant_id;
            pid.id = ctx.generate_uuid();
            pid.party_id = party.id;
            pid.id_scheme = "LEI";
            pid.id_value = ctx.alphanumeric(20);
            pid.description = "Legal Entity Identifier";
            pid.modified_by = modified_by;
            pid.performed_by = modified_by;
            pid.change_reason_code = "system.new_record";
            pid.change_commentary = "Generated organisation data";
            pid.recorded_at = ctx.past_timepoint();
            result.party_identifiers.push_back(std::move(pid));
        }
        // BIC identifier
        {
            refdata::domain::party_identifier pid;
            pid.version = 1;
            pid.tenant_id = tenant_id;
            pid.id = ctx.generate_uuid();
            pid.party_id = party.id;
            pid.id_scheme = "BIC";
            pid.id_value = make_bic(options.country, ctx);
            pid.description = "Business Identifier Code";
            pid.modified_by = modified_by;
            pid.performed_by = modified_by;
            pid.change_reason_code = "system.new_record";
            pid.change_commentary = "Generated organisation data";
            pid.recorded_at = ctx.past_timepoint();
            result.party_identifiers.push_back(std::move(pid));
        }
    }
}

void generate_counterparty_identifiers(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result) {

    if (!options.generate_identifiers)
        return;

    const auto [tenant_id, modified_by] = get_audit(ctx);

    for (const auto& cpty : result.counterparties) {
        // LEI identifier
        {
            refdata::domain::counterparty_identifier cid;
            cid.version = 1;
            cid.tenant_id = tenant_id;
            cid.id = ctx.generate_uuid();
            cid.counterparty_id = cpty.id;
            cid.id_scheme = "LEI";
            cid.id_value = ctx.alphanumeric(20);
            cid.description = "Legal Entity Identifier";
            cid.modified_by = modified_by;
            cid.performed_by = modified_by;
            cid.change_reason_code = "system.new_record";
            cid.change_commentary = "Generated organisation data";
            cid.recorded_at = ctx.past_timepoint();
            result.counterparty_identifiers.push_back(std::move(cid));
        }
        // BIC identifier
        {
            refdata::domain::counterparty_identifier cid;
            cid.version = 1;
            cid.tenant_id = tenant_id;
            cid.id = ctx.generate_uuid();
            cid.counterparty_id = cpty.id;
            cid.id_scheme = "BIC";
            cid.id_value = make_bic(options.country, ctx);
            cid.description = "Business Identifier Code";
            cid.modified_by = modified_by;
            cid.performed_by = modified_by;
            cid.change_reason_code = "system.new_record";
            cid.change_commentary = "Generated organisation data";
            cid.recorded_at = ctx.past_timepoint();
            result.counterparty_identifiers.push_back(std::move(cid));
        }
    }
}

// ============================================================================
// Party-counterparty junction generation
// ============================================================================

void generate_party_counterparty_links(
    generation_context& ctx,
    domain::generated_organisation& result) {

    if (result.parties.empty() || result.counterparties.empty())
        return;

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto& root_party = result.parties[0];

    for (const auto& cpty : result.counterparties) {
        refdata::domain::party_counterparty pc;
        pc.version = 1;
        pc.tenant_id = tenant_id;
        pc.party_id = root_party.id;
        pc.counterparty_id = cpty.id;
        pc.modified_by = modified_by;
        pc.performed_by = modified_by;
        pc.change_reason_code = "system.new_record";
        pc.change_commentary = "Generated organisation data";
        pc.recorded_at = ctx.past_timepoint();
        result.party_counterparties.push_back(std::move(pc));
    }
}

// ============================================================================
// Business unit generation
// ============================================================================

void generate_business_units(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result) {

    if (result.parties.empty())
        return;

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto& centres = financial_centres_for(options.country);
    const auto root_party_id = result.parties[0].id;

    auto nodes = generate_tree(
        options.business_unit_count, options.business_unit_max_depth,
        ctx.engine());

    int unit_seq = 0;
    for (const auto& node : nodes) {
        refdata::domain::business_unit bu;
        bu.version = 1;
        bu.tenant_id = tenant_id;
        bu.id = ctx.generate_uuid();
        bu.party_id = root_party_id;
        bu.modified_by = modified_by;
        bu.performed_by = modified_by;
        bu.change_reason_code = "system.new_record";
        bu.change_commentary = "Generated organisation data";
        bu.recorded_at = ctx.past_timepoint();

        const auto& centre = centres[node.index % centres.size()];
        bu.business_centre_code = std::string(centre.business_centre_code);

        if (node.depth == 0) {
            bu.unit_name = "Global Markets";
            bu.unit_code = "GLOB_MKT";
            bu.parent_business_unit_id = std::nullopt;
        } else if (node.depth == 1) {
            const auto region_idx =
                (node.index - 1) % data::region_names.size();
            const auto region_round =
                (node.index - 1) / data::region_names.size();
            bu.unit_name = std::string(data::region_names[region_idx])
                + " Trading"
                + (region_round > 0
                    ? " " + std::to_string(region_round + 1) : "");
            bu.unit_code = std::string(
                data::region_names[region_idx]).substr(0, 4) + "_TRD"
                + (region_round > 0
                    ? std::to_string(region_round + 1) : "");
            std::transform(bu.unit_code.begin(), bu.unit_code.end(),
                bu.unit_code.begin(), [](unsigned char c) {
                    return std::toupper(c);
                });
            bu.parent_business_unit_id =
                result.business_units[*node.parent_index].id;
        } else {
            const auto asset_idx = unit_seq % data::asset_classes.size();
            const auto& parent = result.business_units[*node.parent_index];
            auto space_pos = parent.unit_name.find(' ');
            auto region = (space_pos != std::string::npos)
                ? parent.unit_name.substr(0, space_pos) : parent.unit_name;
            bu.unit_name = std::string(data::asset_classes[asset_idx])
                + " Trading " + region
                + " " + std::to_string(unit_seq + 1);
            bu.unit_code = std::string(
                data::asset_classes[asset_idx]).substr(0, 4) + "_"
                + region.substr(0, 4)
                + std::to_string(unit_seq + 1);
            std::transform(bu.unit_code.begin(), bu.unit_code.end(),
                bu.unit_code.begin(), [](unsigned char c) {
                    return std::toupper(c);
                });
            bu.parent_business_unit_id = parent.id;
            ++unit_seq;
        }

        result.business_units.push_back(std::move(bu));
    }
}

// ============================================================================
// Portfolio tree generation
// ============================================================================

void generate_portfolios(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result) {

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto root_ccy = options.country == "US" ? "USD" : "GBP";

    // Extract the root party's first word to make portfolio names unique
    // across different generation runs within the same tenant.
    std::string org_prefix;
    if (!result.parties.empty()) {
        const auto& root_name = result.parties[0].full_name;
        auto sp = root_name.find(' ');
        org_prefix = (sp != std::string::npos)
            ? root_name.substr(0, sp) : root_name;
    }

    const auto region_count = options.portfolio_leaf_count >= 8 ? 3u
        : options.portfolio_leaf_count >= 4 ? 2u : 1u;

    std::vector<std::size_t> leaves_per_region(region_count, 0);
    for (std::size_t i = 0; i < options.portfolio_leaf_count; ++i)
        leaves_per_region[i % region_count]++;

    auto make_portfolio = [&](const std::string& name,
        std::optional<boost::uuids::uuid> parent_id,
        std::optional<boost::uuids::uuid> owner_unit_id,
        const std::string& purpose, const std::string& ccy,
        int is_virtual) -> refdata::domain::portfolio {

        refdata::domain::portfolio p;
        p.version = 1;
        p.tenant_id = tenant_id;
        p.id = ctx.generate_uuid();
        p.name = name;
        p.parent_portfolio_id = parent_id;
        p.owner_unit_id = owner_unit_id;
        p.purpose_type = purpose;
        p.aggregation_ccy = ccy;
        p.is_virtual = is_virtual;
        p.modified_by = modified_by;
        p.performed_by = modified_by;
        p.change_reason_code = "system.new_record";
        p.change_commentary = "Generated organisation data";
        p.recorded_at = ctx.past_timepoint();
        return p;
    };

    auto find_unit_id = [&](const std::string& region_hint)
        -> std::optional<boost::uuids::uuid> {
        for (const auto& bu : result.business_units) {
            if (bu.unit_name.find(region_hint) != std::string::npos)
                return bu.id;
        }
        return result.business_units.empty() ? std::nullopt
            : std::optional(result.business_units[0].id);
    };

    auto root = make_portfolio(org_prefix + " Global Portfolio", std::nullopt,
        find_unit_id("Global"), "Risk", root_ccy, 1);
    const auto root_id = root.id;
    result.portfolios.push_back(std::move(root));

    const std::array<const std::array<std::string_view, 4>*, 3>
        leaf_currencies = {{
            &data::emea_leaf_currencies,
            &data::americas_leaf_currencies,
            &data::apac_leaf_currencies
        }};

    for (std::size_t ri = 0; ri < region_count; ++ri) {
        const auto& region = data::region_names[ri];
        const auto& region_ccy = data::region_currencies[ri];

        auto regional = make_portfolio(
            org_prefix + " " + std::string(region) + " Portfolio",
            root_id, find_unit_id(std::string(region)),
            "Risk", std::string(region_ccy), 1);
        const auto regional_id = regional.id;
        result.portfolios.push_back(std::move(regional));

        const auto leaves_in_region = leaves_per_region[ri];
        const auto ac_count = std::min(
            data::asset_classes.size(), leaves_in_region);

        std::vector<std::size_t> leaves_per_ac(ac_count, 0);
        for (std::size_t i = 0; i < leaves_in_region; ++i)
            leaves_per_ac[i % ac_count]++;

        for (std::size_t ai = 0; ai < ac_count; ++ai) {
            const auto& asset_class = data::asset_classes[ai];
            const auto& lc = *leaf_currencies[ri];

            auto ac_portfolio = make_portfolio(
                org_prefix + " " + std::string(asset_class)
                    + " " + std::string(region),
                regional_id, find_unit_id(std::string(asset_class)),
                "Risk", std::string(lc[ai]), 1);
            const auto ac_id = ac_portfolio.id;
            result.portfolios.push_back(std::move(ac_portfolio));

            for (std::size_t li = 0; li < leaves_per_ac[ai]; ++li) {
                auto leaf = make_portfolio(
                    org_prefix + " " + std::string(lc[ai])
                        + " " + std::string(asset_class)
                        + (li > 0 ? " " + std::to_string(li + 1) : ""),
                    ac_id, find_unit_id(std::string(asset_class)),
                    "Risk", std::string(lc[ai]), 0);
                result.portfolios.push_back(std::move(leaf));
            }
        }
    }
}

// ============================================================================
// Book generation
// ============================================================================

void generate_books(
    const domain::organisation_generation_options& options,
    generation_context& ctx,
    domain::generated_organisation& result) {

    if (result.parties.empty())
        return;

    const auto [tenant_id, modified_by] = get_audit(ctx);
    const auto root_party_id = result.parties[0].id;

    int book_seq = 0;
    for (const auto& portfolio : result.portfolios) {
        if (portfolio.is_virtual != 0)
            continue;

        for (std::size_t bi = 0; bi < options.books_per_leaf_portfolio; ++bi) {
            refdata::domain::book bk;
            bk.version = 1;
            bk.tenant_id = tenant_id;
            bk.id = ctx.generate_uuid();
            bk.party_id = root_party_id;
            bk.parent_portfolio_id = portfolio.id;
            bk.ledger_ccy = portfolio.aggregation_ccy;
            bk.book_status = "Active";
            bk.is_trading_book = 1;
            bk.modified_by = modified_by;
            bk.performed_by = modified_by;
            bk.change_reason_code = "system.new_record";
            bk.change_commentary = "Generated organisation data";
            bk.recorded_at = ctx.past_timepoint();

            const auto& product = data::product_types[
                book_seq % data::product_types.size()];
            bk.name = portfolio.aggregation_ccy + " " + std::string(product);

            std::ostringstream gl;
            gl << "GL-" << portfolio.aggregation_ccy << "-"
               << std::setfill('0') << std::setw(3) << (book_seq + 1);
            bk.gl_account_ref = gl.str();

            // Derive cost centre from parent portfolio's region.
            std::string cc_region = "GEN";
            for (const auto& parent_p : result.portfolios) {
                if (parent_p.id == portfolio.parent_portfolio_id) {
                    for (const auto& r : data::region_names) {
                        if (parent_p.name.find(
                            std::string(r)) != std::string::npos) {
                            cc_region = std::string(r).substr(0, 4);
                            std::transform(
                                cc_region.begin(), cc_region.end(),
                                cc_region.begin(), [](unsigned char c) {
                                    return std::toupper(c);
                                });
                            break;
                        }
                    }
                    break;
                }
            }
            bk.cost_center = "CC-" + cc_region;

            result.books.push_back(std::move(bk));
            ++book_seq;
        }
    }
}

} // anonymous namespace

// ============================================================================
// Public API
// ============================================================================

domain::generated_organisation
organisation_generator_service::generate(
    const domain::organisation_generation_options& options) {

    generation_context ctx(
        options.seed.value_or(std::random_device{}()));

    domain::generated_organisation result;
    result.seed = ctx.seed();

    // Track used short codes to prevent collisions across both entity types.
    std::unordered_set<std::string> used_codes;

    generate_parties(options, ctx, result, used_codes);
    generate_party_contacts(options, ctx, result);
    generate_party_identifiers(options, ctx, result);

    generate_counterparties(options, ctx, result, used_codes);
    generate_counterparty_contacts(options, ctx, result);
    generate_counterparty_identifiers(options, ctx, result);

    generate_party_counterparty_links(ctx, result);

    generate_business_units(options, ctx, result);

    generate_portfolios(options, ctx, result);
    generate_books(options, ctx, result);

    return result;
}

domain::generated_organisation
organisation_generator_service::generate() {
    return generate(domain::organisation_generation_options{});
}

}

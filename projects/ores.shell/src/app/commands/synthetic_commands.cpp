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
#include "ores.shell/app/commands/synthetic_commands.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include <chrono>
#include <cli/cli.h>
#include <functional>
#include <ostream>
#include <rfl/json.hpp>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// Generation walks the whole organisation tree server-side; mirror the
// wizard's generous timeout.
constexpr std::chrono::minutes generate_timeout(10);

}

void synthetic_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto synthetic_menu = std::make_unique<cli::Menu>("synthetic");

    synthetic_menu->Insert(
        "generate",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_generate(std::ref(out), std::ref(session), args);
        },
        "Generate a synthetic organisation (parties, counterparties, portfolios, "
        "books, business units)",
        {"[--country GB|US] [--party-count N] [--party-max-depth N] "
         "[--counterparty-count N] [--counterparty-max-depth N] "
         "[--portfolio-leaf-count N] [--portfolio-max-depth N] "
         "[--books-per-portfolio N] [--business-unit-count N] "
         "[--business-unit-max-depth N] [--contacts-per-party N] "
         "[--contacts-per-counterparty N] [--no-addresses] [--no-identifiers] "
         "[--seed N]"});

    root_menu.Insert(std::move(synthetic_menu));
}

std::vector<flag_spec> synthetic_commands::generate_flag_specs() {
    // Defaults mirror generate_organisation_request, which mirrors the
    // tenant provisioning wizard's synthetic page.
    synthetic::messaging::generate_organisation_request defaults;
    return {
        {.name = "country", .requires_value = true, .default_value = defaults.country},
        {.name = "party-count", .requires_value = true,
         .default_value = std::to_string(defaults.party_count)},
        {.name = "party-max-depth", .requires_value = true,
         .default_value = std::to_string(defaults.party_max_depth)},
        {.name = "counterparty-count", .requires_value = true,
         .default_value = std::to_string(defaults.counterparty_count)},
        {.name = "counterparty-max-depth", .requires_value = true,
         .default_value = std::to_string(defaults.counterparty_max_depth)},
        {.name = "portfolio-leaf-count", .requires_value = true,
         .default_value = std::to_string(defaults.portfolio_leaf_count)},
        {.name = "portfolio-max-depth", .requires_value = true,
         .default_value = std::to_string(defaults.portfolio_max_depth)},
        {.name = "books-per-portfolio", .requires_value = true,
         .default_value = std::to_string(defaults.books_per_leaf_portfolio)},
        {.name = "business-unit-count", .requires_value = true,
         .default_value = std::to_string(defaults.business_unit_count)},
        {.name = "business-unit-max-depth", .requires_value = true,
         .default_value = std::to_string(defaults.business_unit_max_depth)},
        {.name = "contacts-per-party", .requires_value = true,
         .default_value = std::to_string(defaults.contacts_per_party)},
        {.name = "contacts-per-counterparty", .requires_value = true,
         .default_value = std::to_string(defaults.contacts_per_counterparty)},
        {.name = "no-addresses"},
        {.name = "no-identifiers"},
        {.name = "seed", .requires_value = true, .default_value = ""}
    };
}

std::optional<synthetic::messaging::generate_organisation_request>
synthetic_commands::build_generate_request(std::ostream& out,
                                           const parsed_args& parsed) {
    synthetic::messaging::generate_organisation_request req;
    req.country = parsed.flag("country");

    // Numeric knobs: validate each as an unsigned integer.
    const std::vector<std::pair<std::string, std::uint32_t*>> knobs = {
        {"party-count", &req.party_count},
        {"party-max-depth", &req.party_max_depth},
        {"counterparty-count", &req.counterparty_count},
        {"counterparty-max-depth", &req.counterparty_max_depth},
        {"portfolio-leaf-count", &req.portfolio_leaf_count},
        {"portfolio-max-depth", &req.portfolio_max_depth},
        {"books-per-portfolio", &req.books_per_leaf_portfolio},
        {"business-unit-count", &req.business_unit_count},
        {"business-unit-max-depth", &req.business_unit_max_depth},
        {"contacts-per-party", &req.contacts_per_party},
        {"contacts-per-counterparty", &req.contacts_per_counterparty}};
    for (const auto& [name, target] : knobs) {
        auto v = parse_uint32(parsed.flag(name));
        if (!v) {
            fail(out) << "Flag --" << name << " must be an unsigned integer: "
                      << parsed.flag(name) << std::endl;
            return std::nullopt;
        }
        *target = *v;
    }

    req.generate_addresses = !parsed.flag_set("no-addresses");
    req.generate_identifiers = !parsed.flag_set("no-identifiers");

    if (!parsed.flag("seed").empty()) {
        auto seed = parse_uint64(parsed.flag("seed"));
        if (!seed) {
            fail(out) << "Flag --seed must be an unsigned integer: "
                      << parsed.flag("seed") << std::endl;
            return std::nullopt;
        }
        req.seed = *seed;
    }
    return req;
}

bool synthetic_commands::generate(
    std::ostream& out, nats_client& session,
    const synthetic::messaging::generate_organisation_request& req) {
    BOOST_LOG_SEV(lg(), info) << "Generating synthetic organisation: "
                              << rfl::json::write(req);
    out << "Generating synthetic organisation (country " << req.country << ", "
        << req.party_count << " parties)..." << std::endl;

    try {
        auto reply = session.authenticated_request(std::string(req.nats_subject),
                                                   rfl::json::write(req),
                                                   generate_timeout);
        auto result =
            rfl::json::read<synthetic::messaging::generate_organisation_response>(
                ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what()
                      << std::endl;
            return false;
        }
        if (!result->success) {
            fail(out) << "Generation failed: " << result->error_message << std::endl;
            return false;
        }

        out << "✓ Synthetic organisation generated (seed " << result->seed << "):"
            << std::endl;
        out << "  parties:             " << result->parties_count << std::endl;
        out << "  counterparties:      " << result->counterparties_count << std::endl;
        out << "  business unit types: " << result->business_unit_types_count << std::endl;
        out << "  business units:      " << result->business_units_count << std::endl;
        out << "  portfolios:          " << result->portfolios_count << std::endl;
        out << "  books:               " << result->books_count << std::endl;
        out << "  contacts:            " << result->contacts_count << std::endl;
        out << "  identifiers:         " << result->identifiers_count << std::endl;
        out << "Reproduce with: synthetic generate --seed " << result->seed << std::endl;
        BOOST_LOG_SEV(lg(), info) << "Synthetic generation complete; seed " << result->seed;
        return true;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return false;
    }
}

void synthetic_commands::process_generate(std::ostream& out,
                                          nats_client& session,
                                          const std::vector<std::string>& args) {
    auto parsed = parse_args(args, generate_flag_specs());
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "synthetic generate takes no positional arguments; see help."
                  << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    auto req = build_generate_request(out, *parsed);
    if (!req)
        return;
    generate(out, session, *req);
}

}

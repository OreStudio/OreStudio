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
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <cli/cli.h>
#include <functional>
#include <map>
#include <ostream>
#include <rfl/json.hpp>
#include <set>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// Generation walks the whole organisation tree server-side; mirror the
// wizard's generous timeout.
constexpr std::chrono::minutes generate_timeout(10);

std::optional<boost::uuids::uuid>
parse_uuid(std::ostream& out, const std::string& value, std::string_view what) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(value);
    } catch (const boost::bad_lexical_cast&) {
        fail(out) << "Invalid " << what << " format. Expected UUID: " << value << std::endl;
        return std::nullopt;
    }
}

std::string scope_label(synthetic::domain::scope value) {
    switch (value) {
        case synthetic::domain::scope::system:
            return "system";
        case synthetic::domain::scope::tenant:
            return "tenant";
        case synthetic::domain::scope::party:
            return "party";
    }
    return "unknown";
}

std::string binding_mode_label(synthetic::domain::binding_mode value) {
    switch (value) {
        case synthetic::domain::binding_mode::bound:
            return "bound";
        case synthetic::domain::binding_mode::sandboxed:
            return "sandboxed";
    }
    return "unknown";
}

std::optional<synthetic::domain::scope> parse_scope(std::ostream& out, const std::string& value) {
    if (value == "system")
        return synthetic::domain::scope::system;
    if (value == "tenant")
        return synthetic::domain::scope::tenant;
    if (value == "party")
        return synthetic::domain::scope::party;
    fail(out) << "Invalid --scope value '" << value << "'; expected system, tenant or party."
              << std::endl;
    return std::nullopt;
}

// Parse "folder <token>" / "feed <token>" into (target, token); reports
// usage. Folder tokens are UUIDs, exact names or standard codename
// paths; feed tokens are UUIDs, ore keys or source_names -- the
// executors resolve them (see resolve_folder_id/resolve_feed).
// Multi-word names are quoted at the shell ("2026 Realistic"), as
// everywhere in the REPL; the cli tokenizer passes them through as a
// single token.
std::optional<std::pair<std::string, std::string>> parse_target(
    std::ostream& out, const std::vector<std::string>& positionals, std::string_view verb) {
    if (positionals.size() != 2) {
        fail(out) << "Usage: synthetic " << verb
                  << " folder <folder-id|\"folder name\"> | feed <feed-id|ore-key|source-name>"
                  << std::endl;
        return std::nullopt;
    }
    if (positionals[0] != "folder" && positionals[0] != "feed") {
        fail(out) << "Unknown target '" << positionals[0] << "'; expected folder or feed."
                  << std::endl;
        return std::nullopt;
    }
    return std::pair{positionals[0], positionals[1]};
}

// Parse a UUID, silently returning nullopt for non-UUID tokens (which
// may be names) -- unlike parse_uuid, which reports an error.
std::optional<boost::uuids::uuid> try_uuid(const std::string& value) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(value);
    } catch (const boost::bad_lexical_cast&) {
        return std::nullopt;
    }
}

// Report an ambiguous token match, listing the candidate labels (folder
// paths, feed source_names, ...) so the user can pick one.
void report_ambiguous(std::ostream& out,
                      std::string_view what,
                      const std::string& token,
                      const std::vector<std::string>& labels) {
    fail(out) << "Ambiguous " << what << " token '" << token << "' matches " << labels.size()
              << " entries:" << std::endl;
    for (const auto& label : labels)
        out << "  " << label << std::endl;
}

// Walk a folder's ancestor chain, returning each folder's name from the
// tree root down to @p id.
std::vector<std::string>
folder_path_parts(const std::map<std::string, synthetic::domain::folder>& by_id,
                  const std::string& id) {
    std::vector<std::string> parts;
    std::string cur = id;
    while (!cur.empty() && parts.size() < 32) { // cycle guard
        auto it = by_id.find(cur);
        if (it == by_id.end())
            break;
        parts.push_back(it->second.name);
        cur = it->second.parent_id ? boost::uuids::to_string(*it->second.parent_id) : std::string{};
    }
    std::reverse(parts.begin(), parts.end());
    return parts;
}

// Slug form of a display name, mirroring the source_name derivation in
// publish_from_dq ("2026 Realistic" -> "2026realistic"): lowercased,
// spaces removed. The slug is a folder's codename: the component of its
// standard path.
std::string slugify(const std::string& value) {
    std::string slug;
    slug.reserve(value.size());
    for (const char c : value) {
        if (c == ' ')
            continue;
        slug.push_back(static_cast<char>(std::tolower(static_cast<unsigned char>(c))));
    }
    return slug;
}

// Render a folder's codename path from the tree root
// ("synthetic/2026realistic/fx") -- the standard, filesystem-style
// token for addressing it, and the label ambiguity reports use.
std::string folder_slug_path(const std::map<std::string, synthetic::domain::folder>& by_id,
                             const std::string& id) {
    std::string path;
    for (const auto& part : folder_path_parts(by_id, id)) {
        if (!path.empty())
            path += '/';
        path += slugify(part);
    }
    return path;
}

// Match a bare codename ("fx", "2026realistic") against folders at any
// depth; several matches (e.g. the codename "fx" under two collections)
// are ambiguous for the caller.
std::vector<std::string>
match_codename(const std::map<std::string, synthetic::domain::folder>& by_id,
               const std::string& token) {
    const auto needle = slugify(token);
    std::vector<std::string> matched;
    for (const auto& [id, f] : by_id)
        if (slugify(f.name) == needle)
            matched.push_back(id);
    return matched;
}

// Walk the tree from its root(s), matching each "/"-separated component
// of the token against the codenames of the current folder itself or
// its children (slugify applied to both sides, so "2026 Realistic/FX"
// and "2026Realistic/fx" both work). Matching the current folder lets
// paths carry the root as their first component, so absolute and
// relative forms are equivalent: "synthetic/2026realistic/fx",
// "/synthetic/2026realistic/fx" and "2026realistic/fx" all resolve to
// the same folder. Leading, trailing and repeated slashes are ignored.
// Returns every folder reachable by the full chain; empty when the
// token is empty or a component matches nothing.
std::vector<std::string>
match_folder_path(const std::map<std::string, synthetic::domain::folder>& by_id,
                  const std::string& token) {
    if (token.empty())
        return {};

    // Split the token into slugified components.
    std::vector<std::string> components;
    std::string current;
    for (const char c : token) {
        if (c == '/') {
            if (!current.empty()) {
                components.push_back(slugify(current));
                current.clear();
            }
        } else {
            current.push_back(c);
        }
    }
    if (!current.empty())
        components.push_back(slugify(current));

    // Start at the roots (folders whose parent is not visible), then
    // descend one level per component.
    std::vector<std::string> level;
    for (const auto& [id, f] : by_id)
        if (!f.parent_id || !by_id.contains(boost::uuids::to_string(*f.parent_id)))
            level.push_back(id);

    for (const auto& component : components) {
        std::vector<std::string> next;
        for (const auto& cur : level)
            for (const auto& [id, f] : by_id) {
                if (slugify(f.name) != component)
                    continue;
                const auto is_self = id == cur;
                const auto is_child = f.parent_id && boost::uuids::to_string(*f.parent_id) == cur;
                if (is_self || is_child)
                    next.push_back(id);
            }
        level = std::move(next);
        if (level.empty())
            return {}; // a component matched nothing anywhere
    }
    return level;
}

// Match a folder token against the visible tree, in order: exact name,
// then a codename -- a bare codename ("fx") matches folders at any
// depth, a standard path ("2026realistic/fx") is walked from the root
// (see match_codename/match_folder_path). Returns every folder matched
// by the first form that matches anything -- several matches (e.g. the
// name "FX" under two collections) are ambiguous for the caller; empty
// when nothing matches.
std::vector<std::string>
match_folders(const std::map<std::string, synthetic::domain::folder>& by_id,
              const std::string& token) {
    std::vector<std::string> matched;
    for (const auto& [id, f] : by_id)
        if (f.name == token)
            matched.push_back(id);
    if (!matched.empty())
        return matched;
    return token.find('/') == std::string::npos ? match_codename(by_id, token) :
                                                  match_folder_path(by_id, token);
}

// Resolve a folder token -- UUID, exact name or standard codename path
// -- to a folder id. The match must be unique within the visible tree;
// a token shared by several folders (e.g. the codename of an asset
// class under different collections) is reported as ambiguous, listing
// each folder's codename path.
std::optional<std::string>
resolve_folder_id(std::ostream& out, nats_client& session, const std::string& token) {
    if (const auto id = try_uuid(token))
        return boost::uuids::to_string(*id);

    synthetic::messaging::get_folders_request req{.offset = 0, .limit = 1000};
    auto result = do_auth_request<synthetic::messaging::get_folders_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return std::nullopt;
    if (!result->success) {
        fail(out) << "Failed to list folders: " << result->message << std::endl;
        return std::nullopt;
    }

    std::map<std::string, synthetic::domain::folder> by_id;
    for (const auto& f : result->folders)
        by_id.emplace(boost::uuids::to_string(f.id), f);

    const auto matched = match_folders(by_id, token);
    if (matched.size() == 1)
        return matched.front();
    if (matched.size() > 1) {
        std::vector<std::string> labels;
        for (const auto& id : matched)
            labels.push_back(folder_slug_path(by_id, id));
        report_ambiguous(out, "folder", token, labels);
        return std::nullopt;
    }
    fail(out) << "No folder matching '" << token << "'." << std::endl;
    return std::nullopt;
}

// The identity of one feed config, of any asset class. The shell never
// needs the kind: start/stop send config_id and the server resolves it.
struct resolved_feed {
    std::string config_id;
    std::string source_name;
    // FX feeds only; IR curve configs have no ore key. validate_vintage
    // prints it.
    std::optional<std::string> ore_key;
};

// The result of probing one feed family for an exact name match: a
// single resolved feed, or several matching configs. Several matches
// are ambiguous -- already reported to the user -- and the probe stops
// there.
struct exact_match_result {
    std::optional<resolved_feed> feed;
    bool ambiguous = false;
};

// Resolve an exact name match within one feed family. A single match
// yields the feed identity; several matches are reported ambiguous,
// listing each matching feed's source_name, and yield nothing.
template <typename Config, typename NameMatch, typename OreKeyOf>
exact_match_result
exact_feed_match(std::ostream& out, const std::vector<Config>& configs,
                 const std::string& token, NameMatch is_name_match, OreKeyOf ore_key_of) {
    std::vector<Config> exact;
    for (const auto& c : configs)
        if (is_name_match(c))
            exact.push_back(c);
    if (exact.size() == 1)
        return exact_match_result{resolved_feed{boost::uuids::to_string(exact.front().id),
                                                exact.front().source_name,
                                                ore_key_of(exact.front())}};
    if (exact.size() > 1) {
        std::vector<std::string> labels;
        for (const auto& c : exact)
            labels.push_back(c.source_name);
        report_ambiguous(out, "feed", token, labels);
        return exact_match_result{std::nullopt, true};
    }
    return exact_match_result{};
}

// Find the config whose id matches the needle within one feed family.
template <typename Config, typename OreKeyOf>
std::optional<resolved_feed>
feed_by_id(const std::vector<Config>& configs, const std::string& needle, OreKeyOf ore_key_of) {
    for (const auto& c : configs)
        if (boost::uuids::to_string(c.id) == needle)
            return resolved_feed{needle, c.source_name, ore_key_of(c)};
    return std::nullopt;
}

// Resolve a feed token -- UUID, exact ore key or exact source_name --
// to its config identity, of any asset class. The shell probes the FX
// config family first, then the IR curve family -- the server's own
// kind resolution probes the repositories in the same order, so a
// token matching both families, config ids and source_names alike,
// resolves to the feed the server would start. A token shared by
// several visible feeds (e.g. the same ore key under different
// collections) is reported as ambiguous, listing each feed's
// source_name.
std::optional<resolved_feed>
resolve_feed(std::ostream& out, nats_client& session, const std::string& token) {
    const auto id = try_uuid(token);

    synthetic::messaging::get_fx_spot_generation_configs_request fx_req{.offset = 0,
                                                                        .limit = 1000};
    auto fx_result = do_auth_request<synthetic::messaging::get_fx_spot_generation_configs_response>(
        out, session, std::string(fx_req.nats_subject), fx_req);
    if (!fx_result)
        return std::nullopt;
    if (!fx_result->success) {
        fail(out) << "Failed to list feeds: " << fx_result->message << std::endl;
        return std::nullopt;
    }

    if (id) {
        if (const auto found = feed_by_id(fx_result->fx_spot_generation_configs,
                                          boost::uuids::to_string(*id),
                                          [](const auto& c) { return c.ore_key; }))
            return found;
    } else {
        const auto matched = exact_feed_match(
            out, fx_result->fx_spot_generation_configs, token,
            [&token](const auto& c) { return c.ore_key == token || c.source_name == token; },
            [](const auto& c) { return c.ore_key; });
        if (matched.feed || matched.ambiguous)
            return matched.feed;
    }

    synthetic::messaging::get_ir_curve_generation_configs_request ir_req{.offset = 0,
                                                                         .limit = 1000};
    auto ir_result = do_auth_request<synthetic::messaging::get_ir_curve_generation_configs_response>(
        out, session, std::string(ir_req.nats_subject), ir_req);
    if (!ir_result)
        return std::nullopt;
    if (!ir_result->success) {
        fail(out) << "Failed to list feeds: " << ir_result->message << std::endl;
        return std::nullopt;
    }

    if (id) {
        if (const auto found = feed_by_id(ir_result->ir_curve_generation_configs,
                                          boost::uuids::to_string(*id),
                                          [](const auto&) { return std::nullopt; }))
            return found;
        fail(out) << "Feed not found: " << token << std::endl;
        return std::nullopt;
    }

    const auto matched = exact_feed_match(
        out, ir_result->ir_curve_generation_configs, token,
        [&token](const auto& c) { return c.source_name == token; },
        [](const auto&) { return std::nullopt; });
    if (matched.feed || matched.ambiguous)
        return matched.feed;
    fail(out) << "No feed matching '" << token << "'." << std::endl;
    return std::nullopt;
}

// Render one folder row; collection rows additionally show the
// market_data_generation_config they represent.
void print_folder(std::ostream& out, const synthetic::domain::folder& f) {
    out << "  " << f.name << " [" << f.kind << "] " << boost::uuids::to_string(f.id);
    if (f.collection_id)
        out << " (config " << boost::uuids::to_string(*f.collection_id) << ")";
    out << std::endl;
}

// Recursively print a folder and its children (depth-first, indented),
// returning the number of nodes printed.
std::size_t print_folder_tree(std::ostream& out,
                              const std::string& folder_id,
                              const std::map<std::string, synthetic::domain::folder>& by_id,
                              std::set<std::string>& visited,
                              int depth) {
    if (!visited.insert(folder_id).second)
        return 0; // parent cycles are not expected, but never loop forever
    auto it = by_id.find(folder_id);
    if (it == by_id.end())
        return 0;

    if (depth > 0)
        out << std::string(static_cast<std::size_t>(depth) * 2, ' ');
    print_folder(out, it->second);

    std::size_t printed = 1;
    for (const auto& [id, child] : by_id)
        if (child.parent_id && boost::uuids::to_string(*child.parent_id) == folder_id)
            printed += print_folder_tree(out, id, by_id, visited, depth + 1);
    return printed;
}

} // namespace

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

    // Market simulator operations, mirroring the Qt Market Simulator window.
    auto list_menu = std::make_unique<cli::Menu>("list");
    list_menu->Insert("folders",
                      [&session](std::ostream& out, std::vector<std::string> args) {
                          process_list_folders(std::ref(out), std::ref(session), args);
                      },
                      "List the synthetic folder hierarchy visible to the logged-in party",
                      {"[--config-id <collection-id>] [--name <folder-name>]"});
    list_menu->Insert("configs",
                      [&session](std::ostream& out, std::vector<std::string> args) {
                          process_list_configs(std::ref(out), std::ref(session), args);
                      },
                      "List market_data_generation_configs visible to the logged-in party/tenant",
                      {"[--scope system|tenant|party]"});
    list_menu->Insert("feeds",
                      [&session](std::ostream& out, std::vector<std::string> args) {
                          process_list_feeds(std::ref(out), std::ref(session), args);
                      },
                      "List the synthetic feeds running in the service, of every asset class",
                      {});
    synthetic_menu->Insert(std::move(list_menu));

    synthetic_menu->Insert("start",
                           [&session](std::ostream& out, std::vector<std::string> args) {
                               process_start(std::ref(out), std::ref(session), args);
                           },
                           "Start feeds under a folder or an individual feed",
                           {"folder <folder-id|folder-name> | feed <feed-id|ore-key|source-name>"});

    synthetic_menu->Insert("stop",
                           [&session](std::ostream& out, std::vector<std::string> args) {
                               process_stop(std::ref(out), std::ref(session), args);
                           },
                           "Stop feeds under a folder or an individual feed",
                           {"folder <folder-id|folder-name> | feed <feed-id|ore-key|source-name>"});

    synthetic_menu->Insert("validate-vintage",
                           [&session](std::ostream& out, std::vector<std::string> args) {
                               process_validate_vintage(std::ref(out), std::ref(session), args);
                           },
                           "Validate vintage data availability for a feed",
                           {"feed <feed-id|ore-key|source-name>"});

    root_menu.Insert(std::move(synthetic_menu));
}

std::vector<flag_spec> synthetic_commands::generate_flag_specs() {
    // Defaults mirror generate_organisation_request, which mirrors the
    // tenant provisioning wizard's synthetic page.
    synthetic::messaging::generate_organisation_request defaults;
    return {{.name = "country", .requires_value = true, .default_value = defaults.country},
            {.name = "party-count",
             .requires_value = true,
             .default_value = std::to_string(defaults.party_count)},
            {.name = "party-max-depth",
             .requires_value = true,
             .default_value = std::to_string(defaults.party_max_depth)},
            {.name = "counterparty-count",
             .requires_value = true,
             .default_value = std::to_string(defaults.counterparty_count)},
            {.name = "counterparty-max-depth",
             .requires_value = true,
             .default_value = std::to_string(defaults.counterparty_max_depth)},
            {.name = "portfolio-leaf-count",
             .requires_value = true,
             .default_value = std::to_string(defaults.portfolio_leaf_count)},
            {.name = "portfolio-max-depth",
             .requires_value = true,
             .default_value = std::to_string(defaults.portfolio_max_depth)},
            {.name = "books-per-portfolio",
             .requires_value = true,
             .default_value = std::to_string(defaults.books_per_leaf_portfolio)},
            {.name = "business-unit-count",
             .requires_value = true,
             .default_value = std::to_string(defaults.business_unit_count)},
            {.name = "business-unit-max-depth",
             .requires_value = true,
             .default_value = std::to_string(defaults.business_unit_max_depth)},
            {.name = "contacts-per-party",
             .requires_value = true,
             .default_value = std::to_string(defaults.contacts_per_party)},
            {.name = "contacts-per-counterparty",
             .requires_value = true,
             .default_value = std::to_string(defaults.contacts_per_counterparty)},
            {.name = "no-addresses"},
            {.name = "no-identifiers"},
            {.name = "seed", .requires_value = true, .default_value = ""}};
}

std::optional<synthetic::messaging::generate_organisation_request>
synthetic_commands::build_generate_request(std::ostream& out, const parsed_args& parsed) {
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
            fail(out) << "Flag --" << name << " must be an unsigned integer: " << parsed.flag(name)
                      << std::endl;
            return std::nullopt;
        }
        *target = *v;
    }

    req.generate_addresses = !parsed.flag_set("no-addresses");
    req.generate_identifiers = !parsed.flag_set("no-identifiers");

    if (!parsed.flag("seed").empty()) {
        auto seed = parse_uint64(parsed.flag("seed"));
        if (!seed) {
            fail(out) << "Flag --seed must be an unsigned integer: " << parsed.flag("seed")
                      << std::endl;
            return std::nullopt;
        }
        req.seed = *seed;
    }
    return req;
}

bool synthetic_commands::generate(std::ostream& out,
                                  nats_client& session,
                                  const synthetic::messaging::generate_organisation_request& req) {
    BOOST_LOG_SEV(lg(), info) << "Generating synthetic organisation: " << rfl::json::write(req);
    out << "Generating synthetic organisation (country " << req.country << ", " << req.party_count
        << " parties)..." << std::endl;

    try {
        auto result = do_auth_request<synthetic::messaging::generate_organisation_response>(
            out, session, std::string(req.nats_subject), req, generate_timeout);
        if (!result)
            return false;
        if (!result->success) {
            fail(out) << "Generation failed: " << result->error_message << std::endl;
            return false;
        }

        out << "✓ Synthetic organisation generated (seed " << result->seed << "):" << std::endl;
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
        fail(out) << "synthetic generate takes no positional arguments; see help." << std::endl;
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

void synthetic_commands::process_list_folders(std::ostream& out,
                                              nats_client& session,
                                              const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "config-id", .requires_value = true, .default_value = ""},
                              {.name = "name", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "synthetic list folders takes no positional arguments; see help." << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& config_id = parsed->flag("config-id");
    if (!config_id.empty() && !parse_uuid(out, config_id, "config ID"))
        return;
    list_folders(out, session, config_id, parsed->flag("name"));
}

bool synthetic_commands::list_folders(std::ostream& out,
                                      nats_client& session,
                                      const std::string& collection_id,
                                      const std::string& folder_name) {
    BOOST_LOG_SEV(lg(), debug) << "Listing synthetic folders.";

    synthetic::messaging::get_folders_request req{.offset = 0, .limit = 1000};
    auto result = do_auth_request<synthetic::messaging::get_folders_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (!result->success) {
        fail(out) << "Failed to list folders: " << result->message << std::endl;
        return false;
    }

    // Index by id for parent/child resolution and subtree collection.
    std::map<std::string, synthetic::domain::folder> by_id;
    for (const auto& f : result->folders)
        by_id.emplace(boost::uuids::to_string(f.id), f);

    // --config-id narrows the listing to one collection and its
    // descendants: find the collection folder representing that config,
    // then print only its subtree.
    std::string subtree_root;
    if (!collection_id.empty()) {
        for (const auto& [id, f] : by_id)
            if (f.collection_id && boost::uuids::to_string(*f.collection_id) == collection_id)
                subtree_root = id;
        if (subtree_root.empty()) {
            fail(out) << "No collection folder for config " << collection_id << std::endl;
            return false;
        }
    }

    // --name narrows the listing to the subtree(s) rooted at the
    // folder(s) matching the token (exact name, display path, slugified
    // name or slugged path -- see match_folders). Multiple matches
    // print each subtree.
    std::vector<std::string> subtree_roots;
    if (!folder_name.empty()) {
        subtree_roots = match_folders(by_id, folder_name);
        if (subtree_roots.empty()) {
            fail(out) << "No folder matching '" << folder_name << "'." << std::endl;
            return false;
        }
    }

    // Roots are folders without a parent, or whose parent is not
    // visible (RLS never hides a folder from its own party, but be
    // defensive and still print such orphans).
    std::vector<std::string> roots;
    for (const auto& [id, f] : by_id)
        if (!f.parent_id || !by_id.contains(boost::uuids::to_string(*f.parent_id)))
            roots.push_back(id);

    std::size_t printed = 0;
    std::set<std::string> visited;
    if (!subtree_root.empty())
        printed = print_folder_tree(out, subtree_root, by_id, visited, 0);
    else if (!subtree_roots.empty())
        for (const auto& root : subtree_roots)
            printed += print_folder_tree(out, root, by_id, visited, 0);
    else
        for (const auto& root : roots)
            printed += print_folder_tree(out, root, by_id, visited, 0);

    out << printed << " of " << result->total_available_count << " folders shown." << std::endl;
    return true;
}

void synthetic_commands::process_list_configs(std::ostream& out,
                                              nats_client& session,
                                              const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, {{.name = "scope", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "synthetic list configs takes no positional arguments; see help." << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    std::optional<synthetic::domain::scope> scope_filter;
    const auto& scope = parsed->flag("scope");
    if (!scope.empty()) {
        scope_filter = parse_scope(out, scope);
        if (!scope_filter)
            return;
    }
    list_configs(out, session, scope_filter);
}

bool synthetic_commands::list_configs(std::ostream& out,
                                      nats_client& session,
                                      std::optional<synthetic::domain::scope> scope_filter) {
    BOOST_LOG_SEV(lg(), debug) << "Listing market data generation configs.";

    synthetic::messaging::get_market_data_generation_configs_request req{.offset = 0,
                                                                         .limit = 1000};
    auto result =
        do_auth_request<synthetic::messaging::get_market_data_generation_configs_response>(
            out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (!result->success) {
        fail(out) << "Failed to list configs: " << result->message << std::endl;
        return false;
    }

    std::size_t shown = 0;
    for (const auto& c : result->market_data_generation_configs) {
        if (scope_filter && c.scope != *scope_filter)
            continue;
        ++shown;
        out << "  " << c.name << " (" << boost::uuids::to_string(c.id)
            << ") scope=" << scope_label(c.scope)
            << " binding=" << binding_mode_label(c.binding_mode)
            << " enabled=" << (c.enabled ? "true" : "false");
        if (c.party_id)
            out << " party_id=" << boost::uuids::to_string(*c.party_id);
        out << std::endl;
    }
    out << shown << " of " << result->total_available_count << " configs shown." << std::endl;
    return true;
}

void synthetic_commands::process_list_feeds(std::ostream& out,
                                            nats_client& session,
                                            const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "synthetic list feeds takes no positional arguments; see help." << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    list_feeds(out, session);
}

bool synthetic_commands::list_feeds(std::ostream& out, nats_client& session) {
    BOOST_LOG_SEV(lg(), debug) << "Listing running synthetic feeds.";

    synthetic::messaging::list_feeds_request req;
    auto result = do_auth_request<synthetic::messaging::list_feeds_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    // The list response carries no message field; the failure reason is
    // generic.
    if (!result->success) {
        fail(out) << "Failed to list feeds." << std::endl;
        return false;
    }

    for (const auto& source_name : result->running_source_names)
        out << "  " << source_name << std::endl;
    out << result->running_source_names.size() << " running feed(s)." << std::endl;
    return true;
}

void synthetic_commands::process_start(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    auto target = parse_target(out, parsed->positionals, "start");
    if (!target)
        return;
    if (target->first == "folder")
        start_folder(out, session, target->second);
    else
        start_feed(out, session, target->second);
}

bool synthetic_commands::start_folder(std::ostream& out,
                                      nats_client& session,
                                      const std::string& token) {
    const auto folder_id = resolve_folder_id(out, session, token);
    if (!folder_id)
        return false;
    BOOST_LOG_SEV(lg(), info) << "Starting feeds under folder " << *folder_id << ".";
    marketdata::messaging::start_feeds_under_folder_request req{.folder_id = *folder_id};
    auto result = do_auth_request<marketdata::messaging::start_feeds_under_folder_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (!result->success) {
        fail(out) << "Start failed: " << result->message << std::endl;
        return false;
    }
    out << "✓ Started " << result->started << " feed(s) under folder " << *folder_id << " ("
        << result->already_running << " already running, " << result->skipped << " skipped)."
        << std::endl;
    return true;
}

bool synthetic_commands::start_feed(std::ostream& out,
                                    nats_client& session,
                                    const std::string& token) {
    // The request is keyed by config_id; the server resolves the config,
    // its children, and the refdata context.
    const auto feed = resolve_feed(out, session, token);
    if (!feed)
        return false;
    const auto& feed_id = feed->config_id;

    synthetic::messaging::start_feed_request req{.config_id = feed_id};
    BOOST_LOG_SEV(lg(), info) << "Starting feed " << feed_id << ".";
    auto result = do_auth_request<synthetic::messaging::start_feed_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (!result->success) {
        fail(out) << "Start failed: " << result->message << std::endl;
        return false;
    }
    out << "✓ Started feed " << feed_id << "." << std::endl;
    return true;
}

void synthetic_commands::process_stop(std::ostream& out,
                                      nats_client& session,
                                      const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    auto target = parse_target(out, parsed->positionals, "stop");
    if (!target)
        return;
    if (target->first == "folder")
        stop_folder(out, session, target->second);
    else
        stop_feed(out, session, target->second);
}

bool synthetic_commands::stop_folder(std::ostream& out,
                                     nats_client& session,
                                     const std::string& token) {
    const auto folder_id = resolve_folder_id(out, session, token);
    if (!folder_id)
        return false;
    BOOST_LOG_SEV(lg(), info) << "Stopping feeds under folder " << *folder_id << ".";
    marketdata::messaging::stop_feeds_under_folder_request req{.folder_id = *folder_id};
    auto result = do_auth_request<marketdata::messaging::stop_feeds_under_folder_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (!result->success) {
        fail(out) << "Stop failed: " << result->message << std::endl;
        return false;
    }
    out << "✓ Stopped " << result->stopped << " feed(s) under folder " << *folder_id << "."
        << std::endl;
    return true;
}

bool synthetic_commands::stop_feed(std::ostream& out,
                                   nats_client& session,
                                   const std::string& token) {
    // The stop request is keyed by config_id; the server resolves it to
    // the config's source_name.
    const auto feed = resolve_feed(out, session, token);
    if (!feed)
        return false;
    const auto& feed_id = feed->config_id;

    synthetic::messaging::stop_feed_request req{.config_id = feed_id};
    BOOST_LOG_SEV(lg(), info) << "Stopping feed " << feed_id << ".";
    auto result = do_auth_request<synthetic::messaging::stop_feed_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (!result->success) {
        fail(out) << "Stop failed: " << result->message << std::endl;
        return false;
    }
    out << "✓ Stopped feed " << feed_id << "." << std::endl;
    return true;
}

void synthetic_commands::process_validate_vintage(std::ostream& out,
                                                  nats_client& session,
                                                  const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: synthetic validate-vintage <feed-id|ore-key|source-name>" << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    validate_vintage(out, session, parsed->positionals[0]);
}

bool synthetic_commands::validate_vintage(std::ostream& out,
                                          nats_client& session,
                                          const std::string& token) {
    // Resolve the feed (id, ore key or source name), then look up its
    // live server-side vintage validity entry. Entries are keyed by fx
    // config id, so only FX feeds match one; IR curve feeds report no
    // entry. The ore key renders only for FX feeds.
    const auto feed = resolve_feed(out, session, token);
    if (!feed)
        return false;
    const auto& feed_id = feed->config_id;
    const std::string ore_key = feed->ore_key ? " (" + *feed->ore_key + ")" : "";

    marketdata::messaging::get_vintage_validity_request req;
    auto result = do_auth_request<marketdata::messaging::get_vintage_validity_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (!result->success) {
        fail(out) << "Vintage validity check failed: " << result->message << std::endl;
        return false;
    }

    for (const auto& e : result->entries) {
        if (e.fx_spot_generation_config_id != feed_id)
            continue;
        if (!e.applicable) {
            out << "⚠ Vintage check not applicable to feed " << feed->source_name << ore_key
                << " (fixed price source)." << std::endl;
            return true;
        }
        if (e.valid) {
            out << "✓ Vintage data available for feed " << feed->source_name << ore_key << "."
                << std::endl;
            return true;
        }
        fail(out) << "Vintage data missing for feed " << feed->source_name << ore_key << "."
                  << std::endl;
        return false;
    }
    fail(out) << "No vintage validity entry for feed " << feed->source_name << "." << std::endl;
    return false;
}

}

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
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_process_parameter_value_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_template_entry_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/preview_ir_curve_shape_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_ir_curve_paths_protocol.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_type_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <cctype>
#include <chrono>
#include <cli/cli.h>
#include <format>
#include <functional>
#include <map>
#include <memory>
#include <ostream>
#include <rfl/json.hpp>
#include <set>
#include <string_view>
#include <utility>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

namespace synthetic_ms = ores::synthetic::messaging;

// Generation walks the whole organisation tree server-side; mirror the
// wizard's generous timeout.

std::optional<boost::uuids::uuid>
parse_uuid(std::ostream& out, const std::string& value, std::string_view what) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(value);
    } catch (const boost::bad_lexical_cast&) {
        fail(out) << "Invalid " << what << " format. Expected UUID: " << value << std::endl;
        return std::nullopt;
    }
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
    // The depth cap breaks any parent cycle.
    while (!cur.empty() && parts.size() < 32) {
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
        // No component matched anywhere.
        if (level.empty())
            return {};
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

    synthetic::messaging::list_folders_request req{.offset = 0, .limit = 1000};
    auto result = do_auth_request<synthetic::messaging::list_folders_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return std::nullopt;
    if (result->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list folders: " << result->result.message << std::endl;
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
exact_match_result exact_feed_match(std::ostream& out,
                                    const std::vector<Config>& configs,
                                    const std::string& token,
                                    NameMatch is_name_match,
                                    OreKeyOf ore_key_of) {
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

    synthetic::messaging::list_fx_spot_generation_configs_request fx_req{.offset = 0,
                                                                         .limit = 1000};
    auto fx_result =
        do_auth_request<synthetic::messaging::list_fx_spot_generation_configs_response>(
            out, session, std::string(fx_req.nats_subject), fx_req);
    if (!fx_result)
        return std::nullopt;
    if (fx_result->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list feeds: " << fx_result->result.message << std::endl;
        return std::nullopt;
    }

    if (id) {
        if (const auto found = feed_by_id(fx_result->fx_spot_generation_configs,
                                          boost::uuids::to_string(*id),
                                          [](const auto& c) { return c.ore_key; }))
            return found;
    } else {
        const auto matched = exact_feed_match(
            out,
            fx_result->fx_spot_generation_configs,
            token,
            [&token](const auto& c) { return c.ore_key == token || c.source_name == token; },
            [](const auto& c) { return c.ore_key; });
        if (matched.feed || matched.ambiguous)
            return matched.feed;
    }

    synthetic::messaging::list_ir_curve_generation_configs_request ir_req{.offset = 0,
                                                                          .limit = 1000};
    auto ir_result =
        do_auth_request<synthetic::messaging::list_ir_curve_generation_configs_response>(
            out, session, std::string(ir_req.nats_subject), ir_req);
    if (!ir_result)
        return std::nullopt;
    if (ir_result->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list feeds: " << ir_result->result.message << std::endl;
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
        out,
        ir_result->ir_curve_generation_configs,
        token,
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
    // Parent cycles are not expected, but never loop forever.
    if (!visited.insert(folder_id).second)
        return 0;
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

bool is_running(const std::vector<std::string>& running, const std::string& source_name) {
    return std::ranges::find(running, source_name) != running.end();
}

// The service keys a running feed by its source name, which is the only name
// a feed binding carries, so the source name is the whole of the join.
std::size_t count_gmm_components(const feed_listing& listing, const std::string& fx_config_id) {
    return std::ranges::count_if(listing.gmm_components, [&](const auto& c) {
        return boost::uuids::to_string(c.fx_spot_config_id) == fx_config_id;
    });
}

std::size_t count_template_entries(const feed_listing& listing, const std::string& config_id) {
    return std::ranges::count_if(listing.template_entries, [&](const auto& e) {
        return boost::uuids::to_string(e.ir_curve_config_id) == config_id;
    });
}

std::size_t count_parameter_values(const feed_listing& listing, const std::string& config_id) {
    return std::ranges::count_if(listing.parameter_values, [&](const auto& v) {
        return boost::uuids::to_string(v.config_id) == config_id;
    });
}

/**
 * @brief The FX spot class: its own config's state, and its GMM components.
 *
 * A GMM price process with no components has no distribution to step, so a
 * config with none is the partial feed this listing exists to show.
 */
class fx_spot_assets final : public feed_assets {
public:
    std::vector<feed_state> states(const feed_listing& listing,
                                   const std::vector<std::string>& running) const override {
        std::vector<feed_state> out;
        for (const auto& c : listing.fx_spot) {
            feed_state feed;
            feed.asset_class = "fx_spot";
            feed.source_name = c.source_name;
            feed.config_id = boost::uuids::to_string(c.id);
            feed.enabled = c.enabled;
            feed.auto_start = c.auto_start;
            feed.running = is_running(running, c.source_name);
            const auto components = count_gmm_components(listing, feed.config_id);
            feed.child_rows = std::format("gmm {}", components);
            feed.partial = !complete(listing, feed);
            out.push_back(std::move(feed));
        }
        return out;
    }

    bool complete(const feed_listing& listing, const feed_state& feed) const override {
        return count_gmm_components(listing, feed.config_id) > 0;
    }
};

/**
 * @brief The IR curve class: its own config's state, its Curve Template
 * entries and its process parameter values.
 *
 * A curve with no template publishes no instruments, and one with no
 * parameter values cannot be stepped at all, so either absence is partial.
 */
class ir_curve_assets final : public feed_assets {
public:
    std::vector<feed_state> states(const feed_listing& listing,
                                   const std::vector<std::string>& running) const override {
        std::vector<feed_state> out;
        for (const auto& c : listing.ir_curve) {
            feed_state feed;
            feed.asset_class = "ir_curve";
            feed.source_name = c.source_name;
            feed.config_id = boost::uuids::to_string(c.id);
            feed.enabled = c.enabled;
            feed.auto_start = c.auto_start;
            feed.running = is_running(running, c.source_name);
            feed.child_rows = std::format("template {}, params {}",
                                          count_template_entries(listing, feed.config_id),
                                          count_parameter_values(listing, feed.config_id));
            feed.partial = !complete(listing, feed);
            out.push_back(std::move(feed));
        }
        return out;
    }

    bool complete(const feed_listing& listing, const feed_state& feed) const override {
        return count_template_entries(listing, feed.config_id) > 0 &&
               count_parameter_values(listing, feed.config_id) > 0;
    }
};

const std::vector<std::unique_ptr<feed_assets>>& feed_asset_classes() {
    static const std::vector<std::unique_ptr<feed_assets>> classes = [] {
        std::vector<std::unique_ptr<feed_assets>> made;
        made.push_back(std::make_unique<fx_spot_assets>());
        made.push_back(std::make_unique<ir_curve_assets>());
        return made;
    }();
    return classes;
}

/**
 * @brief Everything a stateless IR preview states: the config's process type,
 * its named parameter values and its Curve Template entries.
 *
 * Both IR preview verbs state the same three because a curve is the same
 * shape whether it is priced or stepped. A definition name missing from the
 * catalogue, an empty template or an absent parameter value is reported here
 * rather than sent as a request the service would refuse.
 */
struct ir_preview_inputs {
    std::string process_type;
    std::vector<synthetic_ms::parameter_spec> parameters;
    std::vector<synthetic::domain::ir_curve_template_entry> entries;
};

std::optional<ir_preview_inputs>
ir_preview(std::ostream& out, const feed_listing& listing, const std::string& config_id) {
    const synthetic::domain::ir_curve_generation_config* config = nullptr;
    for (const auto& c : listing.ir_curve)
        if (boost::uuids::to_string(c.id) == config_id)
            config = &c;
    if (!config) {
        fail(out) << "Feed " << config_id
                  << " is not an IR curve feed, so it has no curve to preview." << std::endl;
        return std::nullopt;
    }

    ir_preview_inputs preview;
    preview.process_type = config->process_type;

    // A preview states parameter names, because the wire carries the name the
    // editor already has on screen; the store joins them onto definitions.
    std::map<std::string, std::string> name_of_definition;
    for (const auto& d : listing.definitions)
        name_of_definition[boost::uuids::to_string(d.id)] = d.parameter_name;

    for (const auto& v : listing.parameter_values) {
        if (boost::uuids::to_string(v.config_id) != config_id)
            continue;
        const auto it = name_of_definition.find(boost::uuids::to_string(v.parameter_definition_id));
        if (it == name_of_definition.end()) {
            fail(out) << "Parameter definition "
                      << boost::uuids::to_string(v.parameter_definition_id)
                      << " is not in the process parameter catalogue." << std::endl;
            return std::nullopt;
        }
        preview.parameters.push_back(
            {.parameter_name = it->second, .parameter_value = v.parameter_value});
    }
    if (preview.parameters.empty()) {
        fail(out) << "Feed " << config->source_name
                  << " has no process parameter values to preview." << std::endl;
        return std::nullopt;
    }

    for (const auto& e : listing.template_entries)
        if (boost::uuids::to_string(e.ir_curve_config_id) == config_id)
            preview.entries.push_back(e);
    if (preview.entries.empty()) {
        fail(out) << "Feed " << config->source_name << " has no Curve Template entries to preview."
                  << std::endl;
        return std::nullopt;
    }
    std::ranges::sort(preview.entries, {}, [](const auto& e) { return e.sequence_index; });

    return preview;
}

// Every row the setup flow writes is a new row, so one reason code covers the
// whole run and the commentary names the feed it authored.
ores::utility::domain::change_intent setup_intent(const std::string& source_name) {
    return {.reason_code = std::string(dq::domain::change_reason_constants::codes::new_record),
            .commentary = "synthetic setup " + source_name};
}

std::vector<std::string> split_on(const std::string& text, char separator) {
    std::vector<std::string> parts;
    std::string current;
    for (const char c : text) {
        if (c == separator) {
            parts.push_back(current);
            current.clear();
        } else {
            current.push_back(c);
        }
    }
    parts.push_back(current);
    return parts;
}

/**
 * @brief The parameters a process type cannot be built without.
 *
 * The service's own map_parameters_to_yield_curve_process states the same set
 * and refuses a config missing one; naming it here is what lets the setup
 * flow refuse before it writes anything rather than half-authoring a feed.
 */
std::vector<std::string> expected_parameter_names(const std::string& process_type) {
    if (process_type == "TWO_FACTOR_GAUSSIAN")
        return {"kappa_x", "kappa_y", "theta", "sigma_x", "sigma_y", "rho", "initial_rate"};
    if (process_type == "VASICEK" || process_type == "COX_INGERSOLL_ROSS" ||
        process_type == "HULL_WHITE" || process_type == "BLACK_KARASINSKI")
        return {"kappa", "theta", "sigma", "initial_rate"};
    return {};
}

/// A fresh id for one row the setup flow mints; the server never generates one.
std::string random_id() {
    static boost::uuids::random_generator generator;
    return boost::uuids::to_string(generator());
}

/// Reads a whole-number flag, or says which flag was not one.
std::optional<std::uint32_t>
uint_flag(std::ostream& out, const parsed_args& parsed, const std::string& name) {
    auto value = parse_uint32(parsed.flag(name));
    if (!value)
        fail(out) << "--" << name << " must be a whole number: '" << parsed.flag(name) << "'."
                  << std::endl;
    return value;
}

/**
 * @brief Send one planned row, as the entity's own generated `add` verb would:
 * the same subject, carrying the same request body.
 */
template <typename Request>
bool send_row(std::ostream& out, nats_client& session, const synthetic_commands::setup_row& row) {
    auto body = rfl::json::read<Request>(row.body);
    if (!body) {
        fail(out) << "Cannot decode the planned " << row.label << ": " << body.error().what()
                  << std::endl;
        return false;
    }
    auto result =
        do_auth_request<typename Request::response_type>(out, session, row.subject, *body);
    if (!result)
        return false;
    if (result->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to write " << row.label << ": " << result->result.message << std::endl;
        return false;
    }
    return true;
}

/// The request that travels on the subject a planned row states.
bool send_planned_row(std::ostream& out,
                      nats_client& session,
                      const synthetic_commands::setup_row& row) {
    if (row.subject == synthetic_ms::put_market_data_generation_config_request::nats_subject)
        return send_row<synthetic_ms::put_market_data_generation_config_request>(out, session, row);
    if (row.subject == synthetic_ms::put_folder_request::nats_subject)
        return send_row<synthetic_ms::put_folder_request>(out, session, row);
    if (row.subject == synthetic_ms::put_fx_spot_generation_config_request::nats_subject)
        return send_row<synthetic_ms::put_fx_spot_generation_config_request>(out, session, row);
    if (row.subject == synthetic_ms::put_gmm_component_request::nats_subject)
        return send_row<synthetic_ms::put_gmm_component_request>(out, session, row);
    if (row.subject == synthetic_ms::put_ir_curve_generation_config_request::nats_subject)
        return send_row<synthetic_ms::put_ir_curve_generation_config_request>(out, session, row);
    if (row.subject == synthetic_ms::put_ir_curve_template_entry_request::nats_subject)
        return send_row<synthetic_ms::put_ir_curve_template_entry_request>(out, session, row);
    if (row.subject ==
        synthetic_ms::put_ir_curve_generation_config_process_parameter_value_request::nats_subject)
        return send_row<
            synthetic_ms::put_ir_curve_generation_config_process_parameter_value_request>(
            out, session, row);
    fail(out) << "No request is known for subject '" << row.subject << "'." << std::endl;
    return false;
}

/// The live client as the setup flow sees it.
class setup_session_adapter final : public synthetic_commands::setup_session {
public:
    explicit setup_session_adapter(nats_client& session)
        : session_(session) {}

    [[nodiscard]] bool is_logged_in() const override {
        return session_.is_logged_in();
    }

    [[nodiscard]] std::string party_id() const override {
        return session_.auth().default_party_id;
    }

    [[nodiscard]] std::optional<feed_listing> read_context(std::ostream& out) override {
        return synthetic_commands::read_feed_context(out, session_);
    }

    [[nodiscard]] bool send(std::ostream& out, const synthetic_commands::setup_row& row) override {
        return send_planned_row(out, session_, row);
    }

private:
    nats_client& session_;
};

/**
 * @brief A folder row, built as the generated `folders add` verb builds it.
 */
std::string folder_body(const std::string& id,
                        const std::optional<std::string>& parent_id,
                        const std::string& name,
                        const std::string& kind,
                        const std::optional<std::string>& collection_id) {
    synthetic_ms::folder_change change;
    change.write.id = boost::lexical_cast<boost::uuids::uuid>(id);
    if (parent_id)
        change.write.parent_id = boost::lexical_cast<boost::uuids::uuid>(*parent_id);
    change.write.name = name;
    change.write.kind = kind;
    if (collection_id)
        change.write.collection_id = boost::lexical_cast<boost::uuids::uuid>(*collection_id);
    change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
    synthetic_ms::put_folder_request req;
    req.change = change;
    req.intent = {.reason_code =
                      std::string(dq::domain::change_reason_constants::codes::new_record),
                  .commentary = "synthetic setup folder " + name};
    return rfl::json::write(req);
}

} // namespace

void synthetic_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto synthetic_menu = std::make_unique<cli::Menu>("synthetic");

    // Market simulator operations, mirroring the Qt Market Simulator window.
    auto list_menu = std::make_unique<cli::Menu>("list");
    list_menu->Insert("folders",
                      [&session](std::ostream& out, std::vector<std::string> args) {
                          process_list_folders(std::ref(out), std::ref(session), args);
                      },
                      "List the synthetic folder hierarchy visible to the logged-in party",
                      {"[--config-id <collection-id>] [--name <folder-name>]"});
    list_menu->Insert("feeds",
                      [&session](std::ostream& out, std::vector<std::string> args) {
                          process_list_feeds(std::ref(out), std::ref(session), args);
                      },
                      "List every configured feed with its class, source, enabled, auto-start and "
                      "running state",
                      {});
    synthetic_menu->Insert(std::move(list_menu));

    // The three dry-run operations the service already offers, one verb each:
    // a reader asks for one thing at a time, and each one takes a different
    // feed family.
    auto preview_menu = std::make_unique<cli::Menu>("preview");
    preview_menu->Insert(
        "fx-spot",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_preview_fx_spot(std::ref(out), std::ref(session), args);
        },
        "Simulate the sample paths an FX spot feed would produce",
        {"<feed-id|ore-key|source-name> [--ticks <n>] [--paths <n>] [--seed <n>]"});
    preview_menu->Insert("ir-shape",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_preview_ir_shape(std::ref(out), std::ref(session), args);
                         },
                         "Show the curve an IR curve feed would publish, entry by entry",
                         {"<feed-id|source-name> [--seed <n>]"});
    preview_menu->Insert("ir-paths",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_preview_ir_paths(std::ref(out), std::ref(session), args);
                         },
                         "Simulate the short-rate paths an IR curve feed would step",
                         {"<feed-id|source-name> [--ticks <n>] [--paths <n>] [--seed <n>]"});
    synthetic_menu->Insert(std::move(preview_menu));

    synthetic_menu->Insert("setup",
                           [&session](std::ostream& out, std::vector<std::string> args) {
                               process_setup(std::ref(out), std::ref(session), args);
                           },
                           "Author a whole feed in one pass, refusing when a reference is missing",
                           {"--name <name> --source <source-name> [--kind <fx|ir>] ..."});

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

    ores::shell::app::insert_menu(root_menu, std::move(synthetic_menu));
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

    synthetic::messaging::list_folders_request req{.offset = 0, .limit = 1000};
    auto result = do_auth_request<synthetic::messaging::list_folders_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return false;
    if (result->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list folders: " << result->result.message << std::endl;
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

    out << printed << " of " << result->total << " folders shown." << std::endl;
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
    BOOST_LOG_SEV(lg(), debug) << "Listing configured synthetic feeds.";

    auto listing = read_feed_listing(out, session);
    if (!listing)
        return false;

    const auto feeds = feed_states(*listing);
    for (const auto& feed : feeds)
        out << format_feed_state(feed) << std::endl;

    const auto running = std::ranges::count_if(feeds, [](const auto& f) { return f.running; });
    const auto partial = std::ranges::count_if(feeds, [](const auto& f) { return f.partial; });
    out << feeds.size() << " feed(s), " << running << " running, " << partial << " partial."
        << std::endl;
    // A listing of nothing is indistinguishable from a listing that failed
    // unless the command says which it is.
    if (feeds.empty())
        out << "No feed configs are visible to this party." << std::endl;
    return true;
}

std::string synthetic_commands::format_feed_state(const feed_state& feed) {
    return std::format("  {:8} {:32} [{:3}] {:8} {:9}{} {}",
                       feed.asset_class,
                       feed.source_name,
                       feed.enabled ? "on" : "off",
                       feed.auto_start ? "[auto]" : "[manual]",
                       feed.running ? "[running]" : "[stopped]",
                       feed.partial ? " [partial]" : "",
                       feed.child_rows);
}

std::optional<feed_listing> synthetic_commands::read_feed_listing(std::ostream& out,
                                                                  nats_client& session) {
    feed_listing listing;

    synthetic_ms::list_market_data_generation_configs_request configs_req{.offset = 0,
                                                                          .limit = 1000};
    auto configs = do_auth_request<synthetic_ms::list_market_data_generation_configs_response>(
        out, session, std::string(configs_req.nats_subject), configs_req);
    if (!configs)
        return std::nullopt;
    if (configs->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list generation configs: " << configs->result.message << std::endl;
        return std::nullopt;
    }
    listing.configs = std::move(configs->market_data_generation_configs);

    synthetic_ms::list_fx_spot_generation_configs_request fx_req{.offset = 0, .limit = 1000};
    auto fx = do_auth_request<synthetic_ms::list_fx_spot_generation_configs_response>(
        out, session, std::string(fx_req.nats_subject), fx_req);
    if (!fx)
        return std::nullopt;
    if (fx->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list FX spot configs: " << fx->result.message << std::endl;
        return std::nullopt;
    }
    listing.fx_spot = std::move(fx->fx_spot_generation_configs);

    synthetic_ms::list_ir_curve_generation_configs_request ir_req{.offset = 0, .limit = 1000};
    auto ir = do_auth_request<synthetic_ms::list_ir_curve_generation_configs_response>(
        out, session, std::string(ir_req.nats_subject), ir_req);
    if (!ir)
        return std::nullopt;
    if (ir->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list IR curve configs: " << ir->result.message << std::endl;
        return std::nullopt;
    }
    listing.ir_curve = std::move(ir->ir_curve_generation_configs);

    synthetic_ms::list_gmm_components_request gmm_req{.offset = 0, .limit = 1000};
    auto gmm = do_auth_request<synthetic_ms::list_gmm_components_response>(
        out, session, std::string(gmm_req.nats_subject), gmm_req);
    if (!gmm)
        return std::nullopt;
    if (gmm->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list GMM components: " << gmm->result.message << std::endl;
        return std::nullopt;
    }
    listing.gmm_components = std::move(gmm->gmm_components);

    synthetic_ms::list_ir_curve_template_entries_request entries_req{.offset = 0, .limit = 1000};
    auto entries = do_auth_request<synthetic_ms::list_ir_curve_template_entries_response>(
        out, session, std::string(entries_req.nats_subject), entries_req);
    if (!entries)
        return std::nullopt;
    if (entries->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list curve template entries: " << entries->result.message
                  << std::endl;
        return std::nullopt;
    }
    listing.template_entries = std::move(entries->ir_curve_template_entries);

    synthetic_ms::list_ir_curve_generation_config_process_parameter_values_request values_req{
        .offset = 0, .limit = 1000};
    auto values = do_auth_request<
        synthetic_ms::list_ir_curve_generation_config_process_parameter_values_response>(
        out, session, std::string(values_req.nats_subject), values_req);
    if (!values)
        return std::nullopt;
    if (values->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list process parameter values: " << values->result.message
                  << std::endl;
        return std::nullopt;
    }
    listing.parameter_values = std::move(values->process_parameter_values);

    // What is running is the one fact no config row carries: a config states
    // that it is enabled and auto-startable, and this states that the service
    // holds it now.
    synthetic_ms::list_feeds_request running_req;
    auto running = do_auth_request<synthetic_ms::list_feeds_response>(
        out, session, std::string(running_req.nats_subject), running_req);
    if (!running)
        return std::nullopt;
    // The list response carries no message field; the failure reason is generic.
    if (!running->success) {
        fail(out) << "Failed to list running feeds." << std::endl;
        return std::nullopt;
    }
    listing.running = std::move(running->running_source_names);

    return listing;
}

std::optional<feed_listing> synthetic_commands::read_feed_context(std::ostream& out,
                                                                  nats_client& session) {
    auto listing = read_feed_listing(out, session);
    if (!listing)
        return std::nullopt;

    synthetic_ms::list_yield_curve_process_types_request types_req{.offset = 0, .limit = 1000};
    auto types = do_auth_request<synthetic_ms::list_yield_curve_process_types_response>(
        out, session, std::string(types_req.nats_subject), types_req);
    if (!types)
        return std::nullopt;
    if (types->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list yield curve process types: " << types->result.message
                  << std::endl;
        return std::nullopt;
    }
    listing->process_types = std::move(types->process_types);

    // The catalogue a preview and the setup flow both read: a process
    // parameter is named by its definition, which is what the wire carries.
    synthetic_ms::list_yield_curve_process_parameter_definitions_request defs_req{.offset = 0,
                                                                                  .limit = 1000};
    auto defs =
        do_auth_request<synthetic_ms::list_yield_curve_process_parameter_definitions_response>(
            out, session, std::string(defs_req.nats_subject), defs_req);
    if (!defs)
        return std::nullopt;
    if (defs->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to list process parameter definitions: " << defs->result.message
                  << std::endl;
        return std::nullopt;
    }
    listing->definitions = std::move(defs->parameter_definitions);

    return listing;
}

std::vector<feed_state> synthetic_commands::feed_states(const feed_listing& listing) {
    std::vector<feed_state> feeds;
    for (const auto& assets : feed_asset_classes()) {
        auto states = assets->states(listing, listing.running);
        feeds.insert(feeds.end(),
                     std::make_move_iterator(states.begin()),
                     std::make_move_iterator(states.end()));
    }
    return feeds;
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
    // live server-side vintage validity entry. Entries are keyed by
    // feed config id, of every asset class. The ore key renders only for
    // FX feeds.
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
        if (e.config_id != feed_id)
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

void synthetic_commands::process_preview_fx_spot(std::ostream& out,
                                                 nats_client& session,
                                                 const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "ticks", .requires_value = true, .default_value = "100"},
                              {.name = "paths", .requires_value = true, .default_value = "5"},
                              {.name = "seed", .requires_value = true, .default_value = "1"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: synthetic preview fx-spot <feed-id|ore-key|source-name> "
                     "[--ticks <n>] [--paths <n>] [--seed <n>]"
                  << std::endl;
        return;
    }
    const auto ticks = uint_flag(out, *parsed, "ticks");
    const auto paths = uint_flag(out, *parsed, "paths");
    const auto seed = uint_flag(out, *parsed, "seed");
    if (!ticks || !paths || !seed)
        return;
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto feed = resolve_feed(out, session, parsed->positionals[0]);
    if (!feed)
        return;
    auto listing = read_feed_context(out, session);
    if (!listing)
        return;

    const synthetic::domain::fx_spot_generation_config* config = nullptr;
    for (const auto& c : listing->fx_spot)
        if (boost::uuids::to_string(c.id) == feed->config_id)
            config = &c;
    if (!config) {
        fail(out) << "Feed " << feed->source_name
                  << " is not an FX spot feed, so it has no FX spot paths to simulate."
                  << std::endl;
        return;
    }
    // A vintage-started feed takes its opening price from a stored
    // observation; the dry run has no vintage to read, so it says which
    // price source it needs rather than inventing one.
    if (config->price_source != "fixed") {
        fail(out) << "Feed " << feed->source_name << " takes its initial price from a vintage ('"
                  << config->price_source << "'); the preview needs a fixed price source."
                  << std::endl;
        return;
    }

    std::vector<const synthetic::domain::gmm_component*> components;
    for (const auto& c : listing->gmm_components)
        if (boost::uuids::to_string(c.fx_spot_config_id) == feed->config_id)
            components.push_back(&c);
    if (components.empty()) {
        fail(out) << "Feed " << feed->source_name << " has no GMM components to simulate."
                  << std::endl;
        return;
    }
    std::ranges::sort(components, {}, [](const auto* c) { return c->component_index; });

    synthetic_ms::simulate_fx_spot_paths_request req;
    req.process_type = config->process_type;
    req.initial_price = config->gmm_initial_price;
    for (const auto* c : components) {
        req.gmm_means.push_back(c->mean);
        req.gmm_stdevs.push_back(c->stdev);
        req.gmm_weights.push_back(c->weight);
    }
    req.num_ticks = static_cast<int>(*ticks);
    req.num_paths = static_cast<int>(*paths);
    req.seed = *seed;

    auto result = do_auth_request<synthetic_ms::simulate_fx_spot_paths_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Simulation failed: " << result->message << std::endl;
        return;
    }

    out << "Feed " << feed->source_name << " (" << req.process_type << ", " << components.size()
        << " GMM component(s), initial " << req.initial_price << ") over " << req.num_ticks
        << " tick(s), " << result->paths.size() << " path(s):" << std::endl;
    for (std::size_t i = 0; i < result->paths.size(); ++i) {
        const auto& path = result->paths[i];
        out << "  path " << i + 1 << ": " << path.size() << " point(s) from " << path.front()
            << " to " << path.back() << std::endl;
    }
}

void synthetic_commands::process_preview_ir_shape(std::ostream& out,
                                                  nats_client& session,
                                                  const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args,
                   {{.name = "seed", .requires_value = true, .default_value = "1"},
                    {.name = "frequency", .requires_value = true, .default_value = "Annual"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: synthetic preview ir-shape <feed-id|source-name> [--seed <n>] "
                     "[--frequency <code>]"
                  << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto feed = resolve_feed(out, session, parsed->positionals[0]);
    if (!feed)
        return;
    auto listing = read_feed_context(out, session);
    if (!listing)
        return;
    auto context = ir_preview(out, *listing, feed->config_id);
    if (!context)
        return;

    synthetic_ms::preview_ir_curve_shape_request req;
    req.process_type = context->process_type;
    req.parameters = context->parameters;
    req.fixed_leg_payment_frequency_code = parsed->flag("frequency");
    for (const auto& e : context->entries) {
        req.entries.push_back({.sequence_index = e.sequence_index,
                               .start_tenor_code = e.start_tenor_code,
                               .end_tenor_code = e.end_tenor_code,
                               .instrument_code = e.instrument_code});
    }

    auto result = do_auth_request<synthetic_ms::preview_ir_curve_shape_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Preview failed: " << result->message << std::endl;
        return;
    }

    out << "Feed " << feed->source_name << " (" << req.process_type << ") would publish "
        << result->points.size() << " point(s):" << std::endl;
    for (const auto& point : result->points)
        out << "  " << point.sequence_index << "  " << point.start_tenor_code << " -> "
            << point.end_tenor_code << "  " << point.rate << std::endl;
}

void synthetic_commands::process_preview_ir_paths(std::ostream& out,
                                                  nats_client& session,
                                                  const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "ticks", .requires_value = true, .default_value = "100"},
                              {.name = "paths", .requires_value = true, .default_value = "5"},
                              {.name = "seed", .requires_value = true, .default_value = "1"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: synthetic preview ir-paths <feed-id|source-name> [--ticks <n>] "
                     "[--paths <n>] [--seed <n>]"
                  << std::endl;
        return;
    }
    const auto ticks = uint_flag(out, *parsed, "ticks");
    const auto paths = uint_flag(out, *parsed, "paths");
    const auto seed = uint_flag(out, *parsed, "seed");
    if (!ticks || !paths || !seed)
        return;
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto feed = resolve_feed(out, session, parsed->positionals[0]);
    if (!feed)
        return;
    auto listing = read_feed_context(out, session);
    if (!listing)
        return;
    auto context = ir_preview(out, *listing, feed->config_id);
    if (!context)
        return;

    synthetic_ms::simulate_ir_curve_paths_request req;
    req.process_type = context->process_type;
    req.parameters = context->parameters;
    req.num_ticks = static_cast<int>(*ticks);
    req.num_paths = static_cast<int>(*paths);
    req.seed = *seed;

    auto result = do_auth_request<synthetic_ms::simulate_ir_curve_paths_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Simulation failed: " << result->message << std::endl;
        return;
    }

    out << "Feed " << feed->source_name << " (" << req.process_type << ") over " << req.num_ticks
        << " tick(s), " << result->paths.size() << " path(s):" << std::endl;
    for (std::size_t i = 0; i < result->paths.size(); ++i) {
        const auto& path = result->paths[i];
        out << "  path " << i + 1 << ": " << path.size() << " point(s) from " << path.front()
            << " to " << path.back() << std::endl;
    }
}

std::vector<synthetic_commands::setup_row>
synthetic_commands::plan_fx(const std::string& sub_config_id,
                            const std::string& container_id,
                            const std::string& folder_id,
                            const std::string& party_id,
                            const std::string& source_name,
                            const std::string& base,
                            const std::string& quote,
                            const std::string& process_type,
                            std::uint32_t ticks_per_hour,
                            double initial_price,
                            const std::vector<std::array<double, 3>>& components) {
    synthetic_ms::fx_spot_generation_config_write write;
    write.id = boost::lexical_cast<boost::uuids::uuid>(sub_config_id);
    write.party_id = boost::lexical_cast<boost::uuids::uuid>(party_id);
    write.config_id = boost::lexical_cast<boost::uuids::uuid>(container_id);
    write.base_currency_code = base;
    write.quote_currency_code = quote;
    write.source_name = source_name;
    write.ore_key = "FX/RATE/" + base + "/" + quote;
    // The preview reads the opening price from the config, so the authored feed
    // states it rather than depending on a vintage the setup cannot read.
    write.price_source = "fixed";
    write.gmm_initial_price = initial_price;
    write.ticks_per_hour = static_cast<int>(ticks_per_hour);
    write.process_type = process_type;
    write.enabled = true;
    write.auto_start = false;
    write.folder_id = boost::lexical_cast<boost::uuids::uuid>(folder_id);

    synthetic_ms::put_fx_spot_generation_config_request config_req;
    config_req.change.write = write;
    config_req.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
    config_req.intent = setup_intent(source_name);

    std::vector<setup_row> rows;
    rows.push_back({.label = "fx_spot_generation_config " + source_name,
                    .subject = std::string(config_req.nats_subject),
                    .body = rfl::json::write(config_req)});

    int index = 0;
    for (const auto& component : components) {
        synthetic_ms::gmm_component_write gmm;
        gmm.id = boost::lexical_cast<boost::uuids::uuid>(random_id());
        gmm.party_id = boost::lexical_cast<boost::uuids::uuid>(party_id);
        gmm.fx_spot_config_id = boost::lexical_cast<boost::uuids::uuid>(sub_config_id);
        gmm.component_index = index;
        gmm.description = "synthetic setup component " + std::to_string(index);
        gmm.weight = component[0];
        gmm.mean = component[1];
        gmm.stdev = component[2];
        synthetic_ms::put_gmm_component_request gmm_req;
        gmm_req.change.write = gmm;
        gmm_req.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
        gmm_req.intent = setup_intent(gmm.description);
        rows.push_back({.label = "gmm_component " + std::to_string(index),
                        .subject = std::string(gmm_req.nats_subject),
                        .body = rfl::json::write(gmm_req)});
        ++index;
    }
    return rows;
}

std::vector<synthetic_commands::setup_row> synthetic_commands::plan_ir(
    const std::string& sub_config_id,
    const std::string& container_id,
    const std::string& folder_id,
    const std::string& party_id,
    const std::string& source_name,
    const std::vector<synthetic::domain::yield_curve_process_parameter_definition>& definitions,
    const std::string& currency,
    const std::string& index_family,
    const std::string& tenor,
    const std::string& role,
    const std::string& process_type,
    std::uint32_t ticks_per_hour,
    const std::vector<synthetic_ms::parameter_spec>& parameters,
    const std::vector<std::string>& curve_keys) {
    synthetic_ms::ir_curve_generation_config_write write;
    write.id = boost::lexical_cast<boost::uuids::uuid>(sub_config_id);
    write.party_id = boost::lexical_cast<boost::uuids::uuid>(party_id);
    write.config_id = boost::lexical_cast<boost::uuids::uuid>(container_id);
    write.currency_code = currency;
    write.index_family = index_family;
    write.tenor = tenor;
    write.role = role;
    write.process_type = process_type;
    write.ticks_per_hour = static_cast<int>(ticks_per_hour);
    write.enabled = true;
    write.auto_start = false;
    write.folder_id = boost::lexical_cast<boost::uuids::uuid>(folder_id);
    write.source_name = source_name;
    // The SQL check admits only 'fixed' or 'vintage', and the store copies the
    // request verbatim rather than applying the domain default. The setup flow
    // authors a fixed feed, so it states that and leaves the vintage fields empty.
    write.price_source = "fixed";
    // Not consulted for Deposit entries, but required and validated, and the
    // Swap entries the resolver builds need it.
    write.fixed_leg_payment_frequency_code = "Annual";

    synthetic_ms::put_ir_curve_generation_config_request config_req;
    config_req.change.write = write;
    config_req.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
    config_req.intent = setup_intent(source_name);

    std::vector<setup_row> rows;
    rows.push_back({.label = "ir_curve_generation_config " + source_name,
                    .subject = std::string(config_req.nats_subject),
                    .body = rfl::json::write(config_req)});

    int index = 0;
    for (const auto& key : curve_keys) {
        synthetic_ms::ir_curve_template_entry_write entry;
        entry.id = boost::lexical_cast<boost::uuids::uuid>(random_id());
        entry.party_id = boost::lexical_cast<boost::uuids::uuid>(party_id);
        entry.ir_curve_config_id = boost::lexical_cast<boost::uuids::uuid>(sub_config_id);
        entry.sequence_index = index;
        // A caller states one tenor per entry; the entry takes it as both ends,
        // which is the shape one pillar has.
        entry.start_tenor_code = key;
        entry.end_tenor_code = key;
        entry.instrument_code = "ir_swap";
        synthetic_ms::put_ir_curve_template_entry_request entry_req;
        entry_req.change.write = entry;
        entry_req.change.precondition.kind =
            ores::utility::domain::precondition_kind::must_not_exist;
        entry_req.intent = setup_intent(source_name);
        rows.push_back({.label = "ir_curve_template_entry " + key,
                        .subject = std::string(entry_req.nats_subject),
                        .body = rfl::json::write(entry_req)});
        ++index;
    }

    for (const auto& parameter : parameters) {
        const synthetic::domain::yield_curve_process_parameter_definition* definition = nullptr;
        for (const auto& d : definitions)
            if (d.process_type_code == process_type && d.parameter_name == parameter.parameter_name)
                definition = &d;
        // A parameter the catalogue does not define has no row to point at, so
        // the plan carries a label with no subject and the flow stops on it.
        if (!definition) {
            rows.push_back({.label = "process parameter " + parameter.parameter_name,
                            .subject = "",
                            .body = ""});
            continue;
        }
        synthetic_ms::ir_curve_generation_config_process_parameter_value_write value;
        value.id = boost::lexical_cast<boost::uuids::uuid>(random_id());
        value.config_id = boost::lexical_cast<boost::uuids::uuid>(sub_config_id);
        value.parameter_definition_id = definition->id;
        value.parameter_value = parameter.parameter_value;
        synthetic_ms::put_ir_curve_generation_config_process_parameter_value_request value_req;
        value_req.change.write = value;
        value_req.change.precondition.kind =
            ores::utility::domain::precondition_kind::must_not_exist;
        value_req.intent = setup_intent(source_name);
        rows.push_back({.label = "process_parameter_value " + parameter.parameter_name,
                        .subject = std::string(value_req.nats_subject),
                        .body = rfl::json::write(value_req)});
    }
    return rows;
}

void synthetic_commands::process_setup(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    setup_session_adapter adapter(session);
    process_setup(out, adapter, args);
}

void synthetic_commands::process_setup(std::ostream& out,
                                       setup_session& session,
                                       const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args,
                   {{.name = "kind", .requires_value = true, .default_value = ""},
                    {.name = "name", .requires_value = true, .default_value = ""},
                    {.name = "source", .requires_value = true, .default_value = ""},
                    {.name = "base", .requires_value = true, .default_value = ""},
                    {.name = "currency", .requires_value = true, .default_value = ""},
                    {.name = "quote", .requires_value = true, .default_value = ""},
                    {.name = "initial-price", .requires_value = true, .default_value = "1.0"},
                    {.name = "ticks-per-hour", .requires_value = true, .default_value = "60"},
                    {.name = "price-process", .requires_value = true, .default_value = "geometric"},
                    {.name = "index-family", .requires_value = true, .default_value = ""},
                    {.name = "tenor", .requires_value = true, .default_value = ""},
                    {.name = "role", .requires_value = true, .default_value = "self_discounting"},
                    {.name = "process", .requires_value = true, .default_value = ""},
                    {.name = "gmm", .requires_value = true, .repeatable = true},
                    {.name = "param", .requires_value = true, .repeatable = true},
                    {.name = "curve-key", .requires_value = true, .repeatable = true}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "synthetic setup takes no positional arguments; see help." << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& kind = parsed->flag("kind");
    if (kind != "fx" && kind != "ir") {
        fail(out) << "Usage: synthetic setup --kind <fx|ir> --name <name> --source <source-name> "
                     "[--base <ccy> --quote <ccy> --gmm <weight,mean,stdev>]..."
                  << std::endl;
        return;
    }
    const auto& name = parsed->flag("name");
    const auto& source_name = parsed->flag("source");
    if (name.empty() || source_name.empty()) {
        fail(out) << "synthetic setup needs --name and --source." << std::endl;
        return;
    }

    // Parse the flag values before the first request, so a badly typed number
    // is refused before anything is read either.
    std::vector<std::array<double, 3>> gmm_components;
    std::vector<synthetic_ms::parameter_spec> parameters;
    std::vector<std::string> curve_keys;
    try {
        for (const auto& spec : parsed->values("gmm")) {
            const auto parts = split_on(spec, ',');
            if (parts.size() != 3) {
                fail(out) << "--gmm takes <weight>,<mean>,<stdev>: " << spec << std::endl;
                return;
            }
            gmm_components.push_back({from_token<double>(parts[0], "weight"),
                                      from_token<double>(parts[1], "mean"),
                                      from_token<double>(parts[2], "stdev")});
        }
        for (const auto& spec : parsed->values("param")) {
            const auto parts = split_on(spec, '=');
            if (parts.size() != 2) {
                fail(out) << "--param takes <name>=<value>: " << spec << std::endl;
                return;
            }
            parameters.push_back(
                {.parameter_name = parts[0],
                 .parameter_value = from_token<double>(parts[1], "parameter value")});
        }
        curve_keys = parsed->values("curve-key");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    const auto ticks_per_hour = uint_flag(out, *parsed, "ticks-per-hour");
    if (!ticks_per_hour)
        return;

    auto listing = session.read_context(out);
    if (!listing)
        return;

    // Every reference the run depends on, checked before the first write, so a
    // missing one leaves the store as it found it.
    if (kind == "fx") {
        if (parsed->flag("base").empty() || parsed->flag("quote").empty()) {
            fail(out) << "An FX feed needs --base and --quote." << std::endl;
            return;
        }
        if (gmm_components.empty()) {
            fail(out) << "An FX feed needs at least one --gmm <weight>,<mean>,<stdev>; a GMM "
                         "process with no components has no price to step."
                      << std::endl;
            return;
        }
    } else {
        const auto& process = parsed->flag("process");
        bool process_known = false;
        for (const auto& t : listing->process_types)
            if (t.code == process)
                process_known = true;
        if (!process_known) {
            fail(out) << "Unknown --process '" << process
                      << "'; the yield curve process type catalogue holds no such code."
                      << std::endl;
            return;
        }
        const auto expected = expected_parameter_names(process);
        for (const auto& want : expected) {
            const bool held = std::ranges::any_of(
                parameters, [&](const auto& p) { return p.parameter_name == want; });
            if (!held) {
                fail(out) << "Process type '" << process << "' is missing required parameter '"
                          << want << "'." << std::endl;
                return;
            }
            const bool defined = std::ranges::any_of(listing->definitions, [&](const auto& d) {
                return d.process_type_code == process && d.parameter_name == want;
            });
            if (!defined) {
                fail(out) << "The process parameter catalogue defines no '" << want
                          << "' for process type '" << process << "'." << std::endl;
                return;
            }
        }
        if (curve_keys.empty()) {
            fail(out) << "An IR curve feed needs at least one --curve-key; a curve with no Curve "
                         "Template entry publishes no instrument."
                      << std::endl;
            return;
        }
    }

    // The shared flow: one container, one folder chain, and the config the
    // class's own rows hang from. The class supplies the rows below that, and
    // nothing else about the flow changes with it. Every config row is party
    // scoped and the party comes from the session rather than from an
    // argument, so a session that selected no party names the one command that
    // gives it one.
    const auto party_id = session.party_id();
    if (party_id.empty()) {
        fail(out) << "This session holds no party, which every config row needs. Run "
                     "'accounts set-default-party <party-id-or-name>' and log in again."
                  << std::endl;
        return;
    }
    const auto collection = random_id();
    const auto collection_folder = random_id();
    const auto asset_folder = random_id();
    const auto instrument_folder = random_id();
    const auto feed_config = random_id();
    const auto intent = setup_intent(source_name);

    synthetic_ms::put_market_data_generation_config_request container_req;
    container_req.change.write.id = boost::lexical_cast<boost::uuids::uuid>(collection);
    container_req.change.write.scope = synthetic::domain::scope::party;
    container_req.change.write.binding_mode = synthetic::domain::binding_mode::bound;
    container_req.change.write.name = name;
    container_req.change.write.description = "Authored by synthetic setup";
    container_req.change.write.enabled = true;
    container_req.change.precondition.kind =
        ores::utility::domain::precondition_kind::must_not_exist;
    container_req.intent = intent;

    std::vector<setup_row> rows;
    rows.push_back({.label = "market_data_generation_config " + name,
                    .subject = std::string(container_req.nats_subject),
                    .body = rfl::json::write(container_req)});
    rows.push_back(
        {.label = "folder " + name,
         .subject = std::string(synthetic_ms::put_folder_request::nats_subject),
         .body = folder_body(collection_folder, std::nullopt, name, "collection", collection)});
    const auto asset_name = kind == "fx" ? std::string("FX") : std::string("IR");
    rows.push_back({.label = "folder " + asset_name,
                    .subject = std::string(synthetic_ms::put_folder_request::nats_subject),
                    .body = folder_body(
                        asset_folder, collection_folder, asset_name, "asset_class", std::nullopt)});
    const auto instrument_name = kind == "fx" ? std::string("FX Rates") : std::string("IR Curves");
    rows.push_back(
        {.label = "folder " + instrument_name,
         .subject = std::string(synthetic_ms::put_folder_request::nats_subject),
         .body = folder_body(
             instrument_folder, asset_folder, instrument_name, "instrument_type", std::nullopt)});

    if (kind == "fx") {
        double initial_price = 0.0;
        try {
            initial_price = from_token<double>(parsed->flag("initial-price"), "initial price");
        } catch (const std::exception& e) {
            fail(out) << e.what() << std::endl;
            return;
        }
        const auto fx_rows = plan_fx(feed_config,
                                     collection,
                                     instrument_folder,
                                     party_id,
                                     source_name,
                                     parsed->flag("base"),
                                     parsed->flag("quote"),
                                     parsed->flag("price-process"),
                                     *ticks_per_hour,
                                     initial_price,
                                     gmm_components);
        rows.insert(rows.end(), fx_rows.begin(), fx_rows.end());
    } else {
        const auto ir_rows = plan_ir(feed_config,
                                     collection,
                                     instrument_folder,
                                     party_id,
                                     source_name,
                                     listing->definitions,
                                     parsed->flag("currency"),
                                     parsed->flag("index-family"),
                                     parsed->flag("tenor"),
                                     parsed->flag("role"),
                                     parsed->flag("process"),
                                     *ticks_per_hour,
                                     parameters,
                                     curve_keys);
        rows.insert(rows.end(), ir_rows.begin(), ir_rows.end());
    }

    for (std::size_t i = 0; i < rows.size(); ++i) {
        const auto& row = rows[i];
        if (row.subject.empty()) {
            fail(out) << "Cannot author " << row.label
                      << ": the process parameter catalogue has no such definition." << std::endl;
            return;
        }
        if (!session.send(out, row)) {
            out << (rows.size() - i - 1) << " further row(s) were not written." << std::endl;
            return;
        }
        out << "✓ " << row.label << std::endl;
    }

    out << "✓ feed " << source_name << " (" << kind << ") is authored under " << instrument_folder
        << "." << std::endl;
}

}

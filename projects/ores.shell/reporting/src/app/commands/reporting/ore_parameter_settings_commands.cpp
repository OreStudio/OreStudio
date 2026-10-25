/*
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
 */
#include "ores.shell/app/commands/reporting/ore_parameter_settings_commands.hpp"
#include "ores.reporting.api/messaging/ore_parameter_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_setting_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_value_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <cli/cli.h>
#include <iomanip>
#include <map>
#include <optional>
#include <ostream>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace messaging = ores::reporting::messaging;
namespace domain = ores::reporting::domain;

namespace {

constexpr std::uint32_t page_size = 500;

template <typename Request, typename Response, typename Accessor>
auto fetch_all(nats_client& session, Accessor accessor)
    -> std::optional<std::decay_t<decltype(accessor(std::declval<Response&>()))>> {
    using rows_type = std::decay_t<decltype(accessor(std::declval<Response&>()))>;
    rows_type all;
    std::uint32_t offset = 0;
    while (true) {
        Request request;
        request.offset = offset;
        request.limit = page_size;
        std::ostringstream errors;
        auto response = do_auth_request<Response>(
            errors, session, std::string(Request::nats_subject), request);
        if (!response)
            return std::nullopt;
        auto& rows = accessor(*response);
        for (auto& row : rows)
            all.push_back(std::move(row));
        if (all.size() >= (*response).total || rows.empty())
            break;
        offset += static_cast<std::uint32_t>(rows.size());
    }
    return all;
}

struct model {
    std::vector<domain::ore_parameter> parameters;
    std::vector<domain::ore_parameter_setting_value_set> links;
    std::vector<domain::ore_parameter_value_set> sets;
    std::vector<domain::ore_parameter_value_set_value> values;
};

std::optional<model> load(nats_client& session) {
    model loaded;
    auto parameters = fetch_all<messaging::list_ore_parameters_request,
                                messaging::list_ore_parameters_response>(
        session, [](auto& response) -> auto& { return response.parameters; });
    auto links = fetch_all<messaging::list_ore_parameter_setting_value_sets_request,
                           messaging::list_ore_parameter_setting_value_sets_response>(
        session, [](auto& response) -> auto& { return response.setting_value_sets; });
    auto sets = fetch_all<messaging::list_ore_parameter_value_sets_request,
                          messaging::list_ore_parameter_value_sets_response>(
        session, [](auto& response) -> auto& { return response.sets; });
    auto values = fetch_all<messaging::list_ore_parameter_value_set_values_request,
                            messaging::list_ore_parameter_value_set_values_response>(
        session, [](auto& response) -> auto& { return response.values; });
    if (!parameters || !links || !sets || !values)
        return std::nullopt;
    loaded.parameters = std::move(*parameters);
    loaded.links = std::move(*links);
    loaded.sets = std::move(*sets);
    loaded.values = std::move(*values);
    return loaded;
}

bool is_parameter_scope(const std::string& scope) {
    return scope == "ModelParameters" || scope == "EngineParameters" ||
           scope == "GlobalParameters";
}

std::string scope_of(const std::string& raw) {
    if (raw == "model")
        return "ModelParameters";
    if (raw == "engine")
        return "EngineParameters";
    if (raw == "global")
        return "GlobalParameters";
    return raw;
}

struct row {
    std::string scope;
    std::string name;
    std::string domain;
    std::string kind;
    std::string set_code;
    std::string catalogue;
    std::string pattern;
    std::optional<double> minimum;
    std::optional<double> maximum;
    std::vector<const domain::ore_parameter_value_set_value*> values;
};

/** Whether a set's kind can hold a value of the domain the contract states. */
bool compatible(const std::string& kind, const std::string& domain) {
    if (kind.empty() || domain.empty())
        return true;
    if (domain == "boolean")
        return kind == "boolean";
    if (domain == "integer")
        return kind == "integer";
    if (domain == "double")
        return kind == "number" || kind == "range";
    if (domain == "string")
        return kind == "text" || kind == "enumerated" || kind == "catalogue" ||
               kind == "regex" || kind == "range";
    return true;
}

std::string choices_of(const row& entry) {
    if (entry.kind == "boolean")
        return "true, false";
    if (entry.kind == "catalogue")
        return "catalogue:" + entry.catalogue;
    if (entry.values.empty())
        return "-";
    std::string choices;
    for (const auto* value : entry.values) {
        if (!choices.empty())
            choices += ", ";
        choices += value->value;
    }
    return choices;
}

std::vector<row> rows_of(const model& data) {
    std::map<std::string, const domain::ore_parameter_value_set*> sets;
    for (const auto& set : data.sets)
        sets.emplace(set.code, &set);
    std::map<std::string, std::vector<const domain::ore_parameter_value_set_value*>> values;
    for (const auto& value : data.values)
        if (value.is_active)
            values[value.value_set_code].push_back(&value);
    for (auto& [code, list] : values) {
        std::sort(list.begin(), list.end(), [](const auto* left, const auto* right) {
            return left->display_order < right->display_order;
        });
    }
    std::map<std::string, const domain::ore_parameter_setting_value_set*> links;
    for (const auto& link : data.links)
        links.emplace(link.scope + "|" + link.name, &link);

    std::map<std::pair<std::string, std::string>, std::string> contract;
    for (const auto& parameter : data.parameters)
        if (is_parameter_scope(parameter.scope))
            contract.emplace(std::make_pair(parameter.scope, parameter.name),
                             parameter.ore_parameter_value_domain_code);

    std::vector<row> rows;
    for (const auto& [key, domain] : contract) {
        const auto& [scope, name] = key;
        row entry{.scope = scope, .name = name, .domain = domain};
        const auto found = links.find(scope + "|" + name);
        if (found != links.end()) {
            entry.set_code = found->second->value_set_code;
            const auto set = sets.find(entry.set_code);
            if (set != sets.end()) {
                entry.kind = set->second->source_kind;
                entry.catalogue = set->second->catalogue_code;
                entry.pattern = set->second->pattern;
                entry.minimum = set->second->minimum;
                entry.maximum = set->second->maximum;
            }
            const auto list = values.find(entry.set_code);
            if (list != values.end())
                entry.values = list->second;
        }
        rows.push_back(std::move(entry));
    }
    return rows;
}

void print_rows(std::ostream& out, const std::vector<row>& rows) {
    out << std::left << std::setw(18) << "SCOPE" << std::setw(36) << "NAME" << std::setw(12)
        << "KIND" << std::setw(34) << "SET" << "CHOICES" << '\n';
    for (const auto& entry : rows) {
        out << std::left << std::setw(18) << entry.scope << std::setw(36) << entry.name
            << std::setw(12) << entry.kind << std::setw(34) << entry.set_code
            << choices_of(entry) << '\n';
    }
    out << rows.size() << " parameter(s)." << '\n';
}

void print_one(std::ostream& out, const row& entry) {
    out << std::left << std::setw(12) << "Scope" << entry.scope << '\n'
        << std::setw(12) << "Name" << entry.name << '\n'
        << std::setw(12) << "Set" << entry.set_code << '\n'
        << std::setw(12) << "Kind" << entry.kind << '\n'
        << std::setw(12) << "Catalogue" << (entry.catalogue.empty() ? "-" : entry.catalogue)
        << '\n'
        << std::setw(12) << "Pattern" << (entry.pattern.empty() ? "-" : entry.pattern) << '\n'
        << std::setw(12) << "Minimum"
        << (entry.minimum ? std::to_string(*entry.minimum) : "-") << '\n'
        << std::setw(12) << "Maximum"
        << (entry.maximum ? std::to_string(*entry.maximum) : "-") << '\n'
        << std::setw(12) << "Choices" << choices_of(entry) << '\n';
}

}

void ore_parameter_settings_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("ore_parameter_settings");

    menu->Insert(
        "list",
        [&session](std::ostream& out, std::vector<std::string> args) {
            const std::vector<flag_spec> specs{
                {.name = "scope", .requires_value = true, .default_value = ""},
                {.name = "kind", .requires_value = true, .default_value = ""},
            };
            const auto parsed = parse_args(args, specs);
            if (!parsed) {
                fail(out) << parsed.error() << '\n';
                return;
            }
            const auto data = load(session);
            if (!data) {
                fail(out) << "Could not read the parameter model." << '\n';
                return;
            }
            const auto wanted_scope = scope_of(parsed->flag("scope"));
            const auto wanted_kind = parsed->flag("kind");
            std::vector<row> rows;
            for (const auto& entry : rows_of(*data)) {
                if (!wanted_scope.empty() && entry.scope != wanted_scope)
                    continue;
                if (!wanted_kind.empty() && entry.kind != wanted_kind)
                    continue;
                rows.push_back(entry);
            }
            print_rows(out, rows);
        },
        "list [--scope <scope>] [--kind <kind>]");

    menu->Insert(
        "show",
        [&session](std::ostream& out, std::vector<std::string> args) {
            const std::vector<flag_spec> specs{
                {.name = "scope", .requires_value = true, .default_value = ""},
            };
            const auto parsed = parse_args(args, specs);
            if (!parsed) {
                fail(out) << parsed.error() << '\n';
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Expected 1 argument, got " << parsed->positionals.size() << "."
                          << '\n';
                return;
            }
            const auto data = load(session);
            if (!data) {
                fail(out) << "Could not read the parameter model." << '\n';
                return;
            }
            const auto wanted_scope = scope_of(parsed->flag("scope"));
            const auto& wanted = parsed->positionals.front();
            std::vector<row> matches;
            for (const auto& entry : rows_of(*data)) {
                if (entry.name != wanted)
                    continue;
                if (!wanted_scope.empty() && entry.scope != wanted_scope)
                    continue;
                matches.push_back(entry);
            }
            if (matches.empty()) {
                fail(out) << "No parameter named " << wanted << "." << '\n';
                return;
            }
            for (const auto& entry : matches)
                print_one(out, entry);
        },
        "show <name> [--scope <scope>]");

    menu->Insert(
        "check",
        [&session](std::ostream& out, std::vector<std::string> args) {
            const std::vector<flag_spec> specs{};
            const auto parsed = parse_args(args, specs);
            if (!parsed) {
                fail(out) << parsed.error() << '\n';
                return;
            }
            const auto data = load(session);
            if (!data) {
                fail(out) << "Could not read the parameter model." << '\n';
                return;
            }
            auto rows = rows_of(*data);
            std::size_t without_set = 0;
            std::size_t empty_enum = 0;
            std::size_t mismatched = 0;
            for (const auto& entry : rows) {
                if (entry.set_code.empty()) {
                    ++without_set;
                    out << "no set       " << entry.scope << " " << entry.name << '\n';
                } else if (entry.kind == "enumerated" && entry.values.empty()) {
                    ++empty_enum;
                    out << "empty enum   " << entry.scope << " " << entry.name << " ("
                        << entry.set_code << ")" << '\n';
                } else if (!compatible(entry.kind, entry.domain)) {
                    ++mismatched;
                    out << "kind/domain  " << entry.scope << " " << entry.name << " ("
                        << entry.kind << " set, " << entry.domain << " domain)" << '\n';
                }
            }
            std::set<std::string> contract;
            for (const auto& entry : rows)
                contract.insert(entry.scope + "|" + entry.name);
            std::size_t outside = 0;
            for (const auto& link : data->links) {
                if (!contract.contains(link.scope + "|" + link.name)) {
                    ++outside;
                    out << "outside      " << link.scope << " " << link.name << " ("
                        << link.value_set_code << ")" << '\n';
                }
            }
            out << "parameters   " << rows.size() << '\n'
                << "with a set   " << rows.size() - without_set << '\n'
                << "without set  " << without_set << '\n'
                << "links outside " << outside << '\n'
                << "empty enums  " << empty_enum << '\n'
                << "kind/domain  " << mismatched << '\n';
            if (without_set == 0 && outside == 0 && empty_enum == 0 && mismatched == 0)
                out << "OK" << '\n';
            else
                out << "FAIL" << '\n';
        },
        "check");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

}

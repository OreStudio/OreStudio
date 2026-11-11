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
#include "ores.http.core/site/site_configuration.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <regex>
#include <set>
#include <sstream>

namespace ores::http::site {

namespace {

std::optional<std::string> non_empty(const std::optional<std::string>& value) {
    if (!value)
        return std::nullopt;
    const auto first = value->find_first_not_of(" \t\r\n");
    if (first == std::string::npos)
        return std::nullopt;
    return value->substr(first, value->find_last_not_of(" \t\r\n") - first + 1);
}

std::string ids_of(const site_configuration& configuration) {
    std::string out;
    for (const auto& e : configuration.environments)
        out += (out.empty() ? "" : ", ") + e.id;
    return out;
}

void check(const site_configuration& configuration, std::string_view source) {
    const auto refuse = [&](const std::string& why) {
        throw site_configuration_error(std::string(source) +
                                       " is not a usable site configuration: " + why);
    };
    if (configuration.environments.empty())
        refuse("it names no environment");
    static const std::regex id_form("^[a-z][a-z0-9_]*$");
    std::set<std::string> ids;
    for (const auto& e : configuration.environments) {
        if (!std::regex_match(e.id, id_form))
            refuse("the environment id '" + e.id + "' is not lower case letters, digits and _");
        if (e.displayName.empty() || e.host.empty() || e.subjectPrefix.empty())
            refuse("the environment '" + e.id + "' needs a displayName, a host and a subjectPrefix");
        if (e.port < 1 || e.port > 65535)
            refuse("the environment '" + e.id + "' has no port between 1 and 65535");
        if (!ids.insert(e.id).second)
            refuse("the environment id '" + e.id + "' is duplicated");
    }
    for (const auto& a : configuration.developerAccounts)
        if (a.username.empty())
            refuse("a developer account has no username");
    if (!configuration.active.empty() && !ids.contains(configuration.active))
        refuse("active environment '" + configuration.active + "' is not defined; known: " +
               ids_of(configuration));
}

}

site_configuration parse_site(std::string_view text, std::string_view source) {
    auto parsed = ores::utility::codec::json::read_lenient<site_configuration>(text);
    if (!parsed)
        throw site_configuration_error(std::string(source) + " is not valid: " + parsed.error());
    check(*parsed, source);
    return std::move(*parsed);
}

loaded_site
load_site(const std::string& path, const std::string& environment_id, const variable_lookup& lookup) {
    const auto source_path = !path.empty() ? std::optional<std::string>(path) :
                                             non_empty(lookup(site_config_variable));
    if (!source_path)
        throw site_configuration_error("No site configuration: pass one, or set " +
                                       std::string(site_config_variable) + ".");
    const auto source = std::filesystem::absolute(*source_path).string();

    std::ifstream in(source);
    if (!in)
        throw site_configuration_error("Cannot read the site configuration at " + source + ".");
    std::ostringstream text;
    text << in.rdbuf();
    auto configuration = parse_site(text.str(), source);

    auto checkout = non_empty(lookup("ORES_ENV_NAME"));
    if (checkout)
        std::ranges::replace(*checkout, '-', '_');
    std::optional<std::string> requested =
        !environment_id.empty() ? std::optional<std::string>(environment_id) :
        non_empty(lookup("ORES_WEB_ENV")) ? non_empty(lookup("ORES_WEB_ENV")) :
        checkout                          ? checkout :
        !configuration.active.empty()     ? std::optional<std::string>(configuration.active) :
                                            std::nullopt;

    const auto& environments = configuration.environments;
    const auto chosen = requested ? std::ranges::find(environments, *requested, &environment::id) :
                                    environments.begin();
    if (chosen == environments.end())
        throw site_configuration_error("No environment '" + *requested + "' in " + source +
                                       ". Available: " + ids_of(configuration));

    if (const auto overridden = lookup("ORES_WEB_DEVELOPER_TOOLS")) {
        auto lowered = *overridden;
        std::ranges::transform(lowered, lowered.begin(), [](unsigned char c) {
            return static_cast<char>(std::tolower(c));
        });
        configuration.developerTools = lowered == "1" || lowered == "true";
    }

    loaded_site out;
    out.served = *chosen;
    configuration.active = chosen->id;
    out.configuration = std::move(configuration);
    out.source = source;
    return out;
}

}

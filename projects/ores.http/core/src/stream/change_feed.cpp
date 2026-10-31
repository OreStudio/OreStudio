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
#include "ores.http.core/stream/change_feed.hpp"
#include "ores.nats/domain/headers.hpp"
#include <string>
#include <vector>

namespace ores::http::stream {

namespace {

/// The suffixes a store puts on its events collection and its schema version.
constexpr std::string_view events_suffix = "_events";

[[nodiscard]] std::vector<std::string_view> split(std::string_view subject) {
    std::vector<std::string_view> tokens;
    std::string_view::size_type start = 0;
    while (start <= subject.size()) {
        const auto dot = subject.find('.', start);
        const auto end = dot == std::string_view::npos ? subject.size() : dot;
        tokens.push_back(subject.substr(start, end - start));
        if (dot == std::string_view::npos)
            break;
        start = dot + 1;
    }
    return tokens;
}

/// The header's value, or nothing when the message does not carry it.
[[nodiscard]] std::string header_of(const ores::nats::message& message, std::string_view name) {
    const auto found = message.headers.find(std::string(name));
    return found == message.headers.end() ? std::string() : found->second;
}

}

std::optional<entity_event_address> parse_entity_event_subject(std::string_view subject) {
    const auto tokens = split(subject);
    if (tokens.size() < 4)
        return std::nullopt;

    const auto& action = tokens[tokens.size() - 1];
    const auto& collection = tokens[tokens.size() - 2];
    const auto& version = tokens[tokens.size() - 3];
    const auto& component = tokens[tokens.size() - 4];

    if (action.empty() || component.empty())
        return std::nullopt;
    if (!version.starts_with('v') || version.size() < 2)
        return std::nullopt;
    if (!collection.ends_with(events_suffix) || collection.size() == events_suffix.size())
        return std::nullopt;

    return entity_event_address{
        .component = std::string(component),
        .collection = std::string(collection.substr(0, collection.size() - events_suffix.size())),
        .action = std::string(action),
    };
}

bool change_feed::notify(const ores::nats::message& message) {
    const auto address = parse_entity_event_subject(message.subject);
    if (!address)
        return false;

    const auto tenant = header_of(message, ores::nats::headers::x_tenant_id);
    if (tenant.empty())
        return false;

    coalescer_.record({
        .tenant = tenant,
        .party = header_of(message, ores::nats::headers::x_party_id),
        .component = address->component,
        .entity = address->collection,
    });
    return true;
}

}

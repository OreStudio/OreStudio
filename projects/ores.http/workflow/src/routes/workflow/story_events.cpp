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
#include "ores.http/routes/workflow/story_events.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <algorithm>
#include <exception>
#include <map>
#include <set>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;

/// The trail the stream leaves when it drops a closing it cannot place in time.
auto& story_events_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.story_events");
    return instance;
}

/// The name a link's history gives to its own row: both keys, parent first.
constexpr std::string_view link_id_field = "ID";

/// The end of a link's validity, empty while the link is open.
constexpr std::string_view closed_at_field = "Closed At";
constexpr std::string_view closed_by_field = "Closed By";

/// The fields that belong to a link's closing and not to the versions beside it.
bool is_closing_field(const std::string& name) {
    return name == closed_at_field || name == closed_by_field;
}

/// One field of one version, as the history layer stamped it.
std::string stamped_field(const ores::http::net::history::version& version, std::string_view name) {
    for (const auto& field : version.fields)
        if (field.name == name)
            return field.value;
    return {};
}

/// One field of an event, as the screen reads it.
std::string field_json(const std::pair<std::string, std::string>& field) {
    return "{\"name\":" + json::write(field.first) + ",\"value\":" + json::write(field.second) +
           "}";
}

/// The fields of one event, without the ones this view states elsewhere.
std::vector<std::pair<std::string, std::string>>
fields_without(const ores::http::net::history::version& version, bool with_provenance) {
    std::vector<std::pair<std::string, std::string>> kept;
    kept.reserve(version.fields.size());
    for (const auto& field : version.fields) {
        if (is_closing_field(field.name))
            continue;
        if (!with_provenance &&
            (field.name == ores::history::domain::provenance_fields::modified_by ||
             field.name == ores::history::domain::provenance_fields::performed_by ||
             field.name == ores::history::domain::provenance_fields::change_reason_code ||
             field.name == ores::history::domain::provenance_fields::change_commentary))
            continue;
        kept.emplace_back(field.name, field.value);
    }
    return kept;
}

}

int story_kind_rank(std::string_view kind) {
    if (kind == "raised")
        return 10;
    if (kind == "changed")
        return 20;
    if (kind == "asked")
        return 30;
    if (kind == "told")
        return 40;
    if (kind == "decided")
        return 50;
    if (kind == "granted")
        return 60;
    if (kind == "signed_in" || kind == "sign_in_failed" || kind == "signed_out" ||
        kind == "refreshed" || kind == "noticed")
        return 70;
    if (kind == "closed")
        return 80;
    return 0;
}

void order_story(std::vector<story_event>& events) {
    std::ranges::stable_sort(events, [](const story_event& left, const story_event& right) {
        if (left.at != right.at)
            return left.at > right.at;
        const auto rank = story_kind_rank(right.kind) - story_kind_rank(left.kind);
        if (rank != 0)
            return rank < 0;
        if (left.entity_id != right.entity_id)
            return left.entity_id < right.entity_id;
        return right.version < left.version;
    });
}

std::string story_event_json(const story_event& event) {
    std::vector<std::string> written;
    written.reserve(event.fields.size());
    for (const auto& field : event.fields)
        written.push_back(field_json(field));
    return "{\"entityType\":" + json::write(event.entity_type) +
           ",\"entityId\":" + json::write(event.entity_id) +
           ",\"kind\":" + json::write(event.kind) + ",\"at\":" + json::write(event.at) +
           ",\"actor\":" + json::write(event.actor) +
           ",\"version\":" + std::to_string(event.version) +
           ",\"reasonCode\":" + json::write(event.reason_code) +
           ",\"commentary\":" + json::write(event.commentary) +
           ",\"fields\":" + ores::http::net::record_routes::array_of(written) + "}";
}

std::string story_field(const story_event& event, std::string_view name) {
    for (const auto& field : event.fields)
        if (field.first == name)
            return field.second;
    return {};
}

std::vector<story_event>
story_record_events(std::string_view entity_type,
                    std::string entity_id,
                    const std::vector<ores::http::net::history::version>& versions) {
    std::vector<story_event> events;
    events.reserve(versions.size());
    for (const auto& version : versions) {
        story_event event;
        event.entity_type = std::string(entity_type);
        event.entity_id = entity_id;
        event.kind = version.version == 1 ? "raised" : "changed";
        event.at = version.recorded_at;
        event.actor = version.modified_by;
        event.version = static_cast<int>(version.version);
        for (const auto& field : version.fields) {
            if (field.name == ores::history::domain::provenance_fields::change_reason_code)
                event.reason_code = field.value;
            if (field.name == ores::history::domain::provenance_fields::change_commentary)
                event.commentary = field.value;
            event.fields.emplace_back(field.name, field.value);
        }
        events.push_back(std::move(event));
    }
    return events;
}

std::vector<story_event>
story_link_events(std::string_view entity_type,
                  const std::vector<ores::http::net::history::version>& versions) {
    std::map<std::string, std::vector<const ores::http::net::history::version*>> links;
    for (const auto& version : versions) {
        std::string id;
        for (const auto& field : version.fields)
            if (field.name == link_id_field)
                id = field.value;
        links[id].push_back(&version);
    }

    std::vector<story_event> events;
    for (const auto& [id, own] : links) {
        std::vector<ores::http::net::history::version> kept;
        kept.reserve(own.size());
        for (const auto* version : own) {
            auto copy = *version;
            copy.fields.clear();
            for (const auto& field : version->fields)
                if (!is_closing_field(field.name))
                    copy.fields.push_back(field);
            kept.push_back(std::move(copy));
        }
        for (auto& event : story_record_events(entity_type, id, kept))
            events.push_back(std::move(event));

        for (const auto* version : own) {
            const auto closed_at = stamped_field(*version, closed_at_field);
            if (closed_at.empty())
                continue;
            story_event closing;
            closing.entity_type = std::string(entity_type);
            closing.entity_id = id;
            closing.kind = "closed";
            try {
                closing.at = ores::utility::codec::text_form<
                    std::chrono::system_clock::time_point>::from_text(closed_at);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(story_events_lg(), ores::logging::warn)
                    << "A link of " << entity_type << " states a closing this view cannot place in "
                    << "time, so it is not shown: " << e.what();
                continue;
            }
            closing.actor = stamped_field(*version, closed_by_field);
            closing.version = static_cast<int>(version->version);
            closing.fields = fields_without(*version, false);
            events.push_back(std::move(closing));
        }
    }
    return events;
}

}

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
#ifndef ORES_HTTP_ROUTES_REPORTING_ENGINE_SETTINGS_ROWS_HPP
#define ORES_HTTP_ROUTES_REPORTING_ENGINE_SETTINGS_ROWS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/domain/engine_settings.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

/**
 * @brief One setting of one group: the column its value travels under and the
 * ORE parameter that documents it.
 */
struct engine_setting_row final {
    std::string column;
    std::string ore;

    /**
     * @brief Whether the setting moves the numbers a run computes, and is not
     * only operational.
     */
    bool changes_numbers = false;
};

/**
 * @brief One group of settings, with the settings the data places in it.
 */
struct engine_group_row final {
    /// The code of the group, which is the word its part travels under.
    std::string code;

    /**
     * @brief Whether a write to this group moves the numbers a run computes,
     * which is the declaration its settings carry.
     */
    bool changes_numbers = false;

    std::vector<engine_setting_row> settings;
};

/**
 * @brief One part of one configuration: the row that holds a group's values.
 *
 * A part is one row of a table whose columns are the settings the data names, so
 * a value travels beside the row's own members rather than as a field of this
 * type. A column that is null is the engine default for that setting.
 */
struct engine_part_row final {
    std::string id;
    int version = 0;
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
    std::string change_reason;
    std::string change_commentary;

    /**
     * @brief Every member of the row, by name, as the JSON it renders to.
     */
    std::map<std::string, std::string> columns;
};

/**
 * @brief The parts of every configuration, by group and then by the
 * configuration each belongs to.
 */
using engine_parts = std::map<std::string, std::map<std::string, engine_part_row>>;

/**
 * @brief A party the caller works in, as the IAM service names it.
 */
struct engine_party_name final {
    std::string name;
    std::string short_code;
};

/**
 * @brief One report definition that binds a configuration.
 */
struct engine_bound_report final {
    std::string id;
    std::string name;
};

/**
 * @brief The groups and the settings of the engine settings, and the rows a
 * screen joins onto them, read from the services.
 *
 * A group and its settings are rows, never a change: the model states them, so
 * the screens and the writes both read them here rather than restating them.
 * The part a group's values live in is the service's wiring and not the model,
 * so a group the data holds and this gateway does not name is not offered: its
 * rows could not be read or written.
 */
namespace engine_settings_rows {

/**
 * @brief The groups whose part the gateway reads and writes, in the words a part
 * travels under.
 */
[[nodiscard]] std::span<const std::string_view> wired_groups();

/**
 * @brief The groups the data states, in the order it places them, with the
 * settings of each in the order it places those.
 */
[[nodiscard]] std::vector<engine_group_row> read_groups(const ores::http::domain::http_request& req,
                                                        ores::nats::service::nats_client& session);

/**
 * @brief The parts of the configurations named, or of every configuration when
 * none is named.
 */
[[nodiscard]] engine_parts read_parts(const ores::http::domain::http_request& req,
                                      ores::nats::service::nats_client& session,
                                      const std::vector<boost::uuids::uuid>& ids);

/**
 * @brief Every configuration the caller's parties hold, in the service's order.
 */
[[nodiscard]] std::vector<ores::reporting::domain::engine_settings>
read_engine_settings(const ores::http::domain::http_request& req,
                     ores::nats::service::nats_client& session);

/**
 * @brief One configuration, or a 404 when the service holds none with this id.
 */
[[nodiscard]] ores::reporting::domain::engine_settings
read_central(const ores::http::domain::http_request& req,
             ores::nats::service::nats_client& session,
             const boost::uuids::uuid& id);

/**
 * @brief The parties the caller works in, by id. A read that fails leaves the
 * names empty, so a page still states the configuration it could read.
 */
[[nodiscard]] std::map<std::string, engine_party_name>
read_party_names(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session);

/**
 * @brief The report definitions that bind one configuration, or nothing when the
 * binding cannot be read.
 *
 * A binding that cannot be read must not hide the settings, so a failure is
 * stated as an unknown list and not as a refusal of the page.
 */
[[nodiscard]] std::optional<std::vector<engine_bound_report>>
read_bound_reports(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session,
                   const boost::uuids::uuid& configuration_id);

/**
 * @brief Every version of one group's part row, in the order the service states
 * them.
 */
[[nodiscard]] std::vector<engine_part_row>
read_part_versions(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session,
                   std::string_view group_code,
                   const boost::uuids::uuid& part_id);

}

}

#endif

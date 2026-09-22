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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct tenor_convention_resolution_key {
    std::string convention_code;
    std::string tenor_code;
};

struct tenor_convention_resolution_lookup {
    tenor_convention_resolution_key key;
    std::optional<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolution;
};

struct tenor_convention_resolutions_filter {
    std::optional<std::string> convention_code;
};

struct tenor_convention_resolution_event {
    boost::uuids::uuid event_id;
    tenor_convention_resolution_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_tenor_convention_resolutions_request {
    using response_type = struct list_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_convention_resolutions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_convention_resolutions_filter> filter;
};

struct list_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolutions;
    std::uint64_t total;
};

struct get_tenor_convention_resolution_request {
    using response_type = struct get_tenor_convention_resolution_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_convention_resolutions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_convention_resolution_key key;
};

struct get_tenor_convention_resolution_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolution;
};

struct get_many_tenor_convention_resolutions_request {
    using response_type = struct get_many_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_convention_resolution_key> keys;
};

struct get_many_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
    std::vector<tenor_convention_resolution_lookup> entries;
};

struct list_by_convention_code_tenor_convention_resolutions_request {
    using response_type = struct list_by_convention_code_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.list_by_convention_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string convention_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_convention_resolutions_filter> filter;
};

struct list_by_convention_code_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolutions;
    std::uint64_t total;
};

}

#endif

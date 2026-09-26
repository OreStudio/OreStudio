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
#ifndef ORES_COMPUTE_API_MESSAGING_APP_VERSION_PLATFORM_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_APP_VERSION_PLATFORM_PROTOCOL_HPP

#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct app_version_platform_key {
    boost::uuids::uuid app_version_id;
    boost::uuids::uuid platform_id;
};

struct app_version_platform_write {
    boost::uuids::uuid app_version_id;
    boost::uuids::uuid platform_id;
    std::string package_uri;
    std::string sha256;
};

struct app_version_platform_change {
    app_version_platform_write write;
    ores::utility::domain::precondition precondition;
};

struct app_version_platform_removal {
    app_version_platform_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct app_version_platform_lookup {
    app_version_platform_key key;
    std::optional<ores::compute::domain::app_version_platform> app_version_platform;
};

struct app_version_platforms_filter {
    std::optional<boost::uuids::uuid> app_version_id;
};

struct list_app_version_platforms_request {
    using response_type = struct list_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.list";
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
    std::optional<app_version_platforms_filter> filter;
};

struct list_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;
    std::uint64_t total;
};

struct get_app_version_platform_request {
    using response_type = struct get_app_version_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_version_platform_key key;
};

struct get_app_version_platform_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::app_version_platform> app_version_platform;
};

struct get_many_app_version_platforms_request {
    using response_type = struct get_many_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_version_platform_key> keys;
};

struct get_many_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<app_version_platform_lookup> entries;
};

struct put_app_version_platform_request {
    using response_type = struct put_app_version_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_version_platform_change change;
    ores::utility::domain::change_intent intent;
};

struct put_app_version_platform_response {
    ores::utility::domain::result result;
    ores::compute::domain::app_version_platform app_version_platform;
};

struct put_many_app_version_platforms_request {
    using response_type = struct put_many_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_version_platform_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;
};

struct delete_app_version_platform_request {
    using response_type = struct delete_app_version_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_version_platform_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_app_version_platform_response {
    ores::utility::domain::result result;
};

struct delete_many_app_version_platforms_request {
    using response_type = struct delete_many_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_version_platform_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_app_version_platforms_response {
    ores::utility::domain::result result;
};

struct list_by_app_version_id_app_version_platforms_request {
    using response_type = struct list_by_app_version_id_app_version_platforms_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.list_by_app_version_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid app_version_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<app_version_platforms_filter> filter;
};

struct list_by_app_version_id_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;
    std::uint64_t total;
};

}

#endif

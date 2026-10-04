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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_TYPE_CONFIGURATION_TYPE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_TYPE_CONFIGURATION_TYPE_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_type_configuration_type.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_type_configuration_type_key {
    std::string report_type_code;
    std::string configuration_type_code;
};

struct report_type_configuration_type_write {
    std::string report_type_code;
    std::string configuration_type_code;
};

struct report_type_configuration_type_change {
    report_type_configuration_type_write write;
    ores::utility::domain::precondition precondition;
};

struct report_type_configuration_type_removal {
    report_type_configuration_type_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_type_configuration_type_lookup {
    report_type_configuration_type_key key;
    std::optional<ores::reporting::domain::report_type_configuration_type>
        report_type_configuration_type;
};

struct report_type_configuration_types_filter {
    std::optional<std::string> report_type_code;
    std::optional<std::vector<std::string>> report_type_code_one_of;
};

struct list_report_type_configuration_types_request {
    using response_type = struct list_report_type_configuration_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.list";
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
    std::optional<report_type_configuration_types_filter> filter;
};

struct list_report_type_configuration_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_type_configuration_type>
        report_type_configuration_types;
    std::uint64_t total;
};

struct get_report_type_configuration_type_request {
    using response_type = struct get_report_type_configuration_type_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_type_configuration_type_key key;
};

struct get_report_type_configuration_type_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_type_configuration_type>
        report_type_configuration_type;
};

struct get_many_report_type_configuration_types_request {
    using response_type = struct get_many_report_type_configuration_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_type_configuration_type_key> keys;
};

struct get_many_report_type_configuration_types_response {
    ores::utility::domain::result result;
    std::vector<report_type_configuration_type_lookup> entries;
};

struct put_report_type_configuration_type_request {
    using response_type = struct put_report_type_configuration_type_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_type_configuration_type_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_type_configuration_type_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_type_configuration_type>
        report_type_configuration_type;
};

struct put_many_report_type_configuration_types_request {
    using response_type = struct put_many_report_type_configuration_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_type_configuration_type_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_type_configuration_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_type_configuration_type>
        report_type_configuration_types;
};

struct delete_report_type_configuration_type_request {
    using response_type = struct delete_report_type_configuration_type_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_type_configuration_type_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_type_configuration_type_response {
    ores::utility::domain::result result;
};

struct delete_many_report_type_configuration_types_request {
    using response_type = struct delete_many_report_type_configuration_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_type_configuration_type_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_type_configuration_types_response {
    ores::utility::domain::result result;
};

struct list_by_report_type_code_report_type_configuration_types_request {
    using response_type = struct list_by_report_type_code_report_type_configuration_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_type_configuration_types.list_by_report_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_type_configuration_types_filter> filter;
};

struct list_by_report_type_code_report_type_configuration_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_type_configuration_type>
        report_type_configuration_types;
    std::uint64_t total;
};

}

#endif

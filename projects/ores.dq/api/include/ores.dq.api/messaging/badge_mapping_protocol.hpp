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
#ifndef ORES_DQ_API_MESSAGING_BADGE_MAPPING_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_BADGE_MAPPING_PROTOCOL_HPP

#include "ores.dq.api/domain/badge_mapping.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct badge_mapping_key {
    std::string code_domain_code;
    std::string entity_code;
};

struct badge_mapping_write {
    std::string code_domain_code;
    std::string entity_code;
    std::string badge_code;
};

struct badge_mapping_change {
    badge_mapping_write write;
    ores::utility::domain::precondition precondition;
};

struct badge_mapping_removal {
    badge_mapping_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct badge_mapping_lookup {
    badge_mapping_key key;
    std::optional<ores::dq::domain::badge_mapping> badge_mapping;
};

struct badge_mappings_filter {
    std::optional<std::string> code_domain_code;
    std::optional<std::vector<std::string>> code_domain_code_one_of;
};

struct list_badge_mappings_request {
    using response_type = struct list_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.list";
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
    std::optional<badge_mappings_filter> filter;
};

struct list_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_mapping> badge_mappings;
    std::uint64_t total;
};

struct get_badge_mapping_request {
    using response_type = struct get_badge_mapping_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_mapping_key key;
};

struct get_badge_mapping_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::badge_mapping> badge_mapping;
};

struct get_many_badge_mappings_request {
    using response_type = struct get_many_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_mapping_key> keys;
};

struct get_many_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<badge_mapping_lookup> entries;
};

struct put_badge_mapping_request {
    using response_type = struct put_badge_mapping_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_mapping_change change;
    ores::utility::domain::change_intent intent;
};

struct put_badge_mapping_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::badge_mapping> badge_mapping;
};

struct put_many_badge_mappings_request {
    using response_type = struct put_many_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_mapping_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_mapping> badge_mappings;
};

struct delete_badge_mapping_request {
    using response_type = struct delete_badge_mapping_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_mapping_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_badge_mapping_response {
    ores::utility::domain::result result;
};

struct delete_many_badge_mappings_request {
    using response_type = struct delete_many_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_mapping_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_badge_mappings_response {
    ores::utility::domain::result result;
};

struct list_by_code_domain_code_badge_mappings_request {
    using response_type = struct list_by_code_domain_code_badge_mappings_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.badge_mappings.list_by_code_domain_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string code_domain_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<badge_mappings_filter> filter;
};

struct list_by_code_domain_code_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_mapping> badge_mappings;
    std::uint64_t total;
};

}

#endif

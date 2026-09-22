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
#ifndef ORES_REFDATA_API_MESSAGING_PORTFOLIO_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PORTFOLIO_PROTOCOL_HPP

#include "ores.refdata.api/domain/portfolio.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct portfolio_key {
    boost::uuids::uuid id;
};

struct portfolio_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    std::optional<boost::uuids::uuid> parent_portfolio_id;
    std::optional<boost::uuids::uuid> owner_unit_id;
    std::string purpose_type;
    std::string aggregation_ccy;
    bool is_virtual;
    std::string status;
};

struct portfolio_change {
    portfolio_write write;
    ores::utility::domain::precondition precondition;
};

struct portfolio_removal {
    portfolio_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct portfolio_lookup {
    portfolio_key key;
    std::optional<ores::refdata::domain::portfolio> portfolio;
};

struct portfolio_event {
    boost::uuids::uuid event_id;
    portfolio_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct portfolio_version_key {
    portfolio_key portfolio;
    std::uint32_t version;
};

struct portfolio_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_portfolios_request {
    using response_type = struct list_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.list";
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
};

struct list_portfolios_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::portfolio> portfolios;
    std::uint64_t total;
};

struct get_portfolio_request {
    using response_type = struct get_portfolio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_key key;
};

struct get_portfolio_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::portfolio> portfolio;
};

struct get_many_portfolios_request {
    using response_type = struct get_many_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<portfolio_key> keys;
};

struct get_many_portfolios_response {
    ores::utility::domain::result result;
    std::vector<portfolio_lookup> entries;
};

struct put_portfolio_request {
    using response_type = struct put_portfolio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_change change;
    ores::utility::domain::change_intent intent;
};

struct put_portfolio_response {
    ores::utility::domain::result result;
    ores::refdata::domain::portfolio portfolio;
};

struct put_many_portfolios_request {
    using response_type = struct put_many_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<portfolio_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_portfolios_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::portfolio> portfolios;
};

struct delete_portfolio_request {
    using response_type = struct delete_portfolio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_portfolio_response {
    ores::utility::domain::result result;
};

struct delete_many_portfolios_request {
    using response_type = struct delete_many_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<portfolio_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_portfolios_response {
    ores::utility::domain::result result;
};

struct list_portfolio_versions_request {
    using response_type = struct list_portfolio_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<portfolio_versions_filter> filter;
};

struct list_portfolio_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::portfolio> versions;
    std::uint64_t total;
};

struct get_portfolio_version_request {
    using response_type = struct get_portfolio_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_version_key key;
};

struct get_portfolio_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::portfolio version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace portfolio_event_subjects {
inline constexpr std::string_view created = "refdata.v1.portfolios_events.created";
inline constexpr std::string_view updated = "refdata.v1.portfolios_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.portfolios_events.deleted";
}

}

#endif

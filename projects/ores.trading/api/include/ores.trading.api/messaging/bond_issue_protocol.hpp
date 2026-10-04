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
#ifndef ORES_TRADING_API_MESSAGING_BOND_ISSUE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_ISSUE_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct bond_issue_key {
    boost::uuids::uuid issue_id;
};

struct bond_issue_write {
    boost::uuids::uuid issue_id;
    std::string security_id;
    std::string issuer;
    std::optional<ores::utility::decimal::decimal> face_value;
    std::optional<std::chrono::year_month_day> issue_date;
    int settlement_days;
    std::optional<std::string> calendar;
    std::optional<std::string> credit_curve_id;
    std::optional<std::string> reference_curve_id;
    std::optional<std::string> income_curve_id;
    std::optional<std::string> credit_group;
    std::optional<std::string> volatility_curve_id;
    std::optional<std::string> price_quote_method;
    std::optional<std::string> price_quote_base_value;
    std::optional<std::string> sub_type;
    std::optional<std::string> price_type;
    std::optional<std::string> payer;
    std::optional<std::string> credit_risk;
};

struct bond_issue_change {
    bond_issue_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_issue_removal {
    bond_issue_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_issue_lookup {
    bond_issue_key key;
    std::optional<ores::trading::domain::bond_issue> bond_issue;
};

struct bond_issues_filter {
    std::optional<std::vector<boost::uuids::uuid>> issue_id_one_of;
};

struct bond_issue_event {
    boost::uuids::uuid event_id;
    bond_issue_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_issue_version_key {
    bond_issue_key bond_issue;
    std::uint32_t version;
};

struct bond_issue_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_issues_request {
    using response_type = struct list_bond_issues_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.list";
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
    std::optional<bond_issues_filter> filter;
};

struct list_bond_issues_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_issue> issues;
    std::uint64_t total;
};

struct get_bond_issue_request {
    using response_type = struct get_bond_issue_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_key key;
};

struct get_bond_issue_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_issue> bond_issue;
};

struct get_many_bond_issues_request {
    using response_type = struct get_many_bond_issues_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_issue_key> keys;
};

struct get_many_bond_issues_response {
    ores::utility::domain::result result;
    std::vector<bond_issue_lookup> entries;
};

struct put_bond_issue_request {
    using response_type = struct put_bond_issue_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_issue_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_issue> bond_issue;
};

struct put_many_bond_issues_request {
    using response_type = struct put_many_bond_issues_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_issue_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_issues_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_issue> issues;
};

struct delete_bond_issue_request {
    using response_type = struct delete_bond_issue_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_issue_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_issues_request {
    using response_type = struct delete_many_bond_issues_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_issue_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_issues_response {
    ores::utility::domain::result result;
};

struct list_bond_issue_versions_request {
    using response_type = struct list_bond_issue_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_issue_versions_filter> filter;
};

struct list_bond_issue_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_issue> versions;
    std::uint64_t total;
};

struct get_bond_issue_version_request {
    using response_type = struct get_bond_issue_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_version_key key;
};

struct get_bond_issue_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_issue> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_issue_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_issues_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_issues_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_issues_events.deleted";
}

}

#endif
